#!/usr/bin/env Rscript

# Stream one bounded AOI from a USGS EPT endpoint to a temporary LAZ.
# The AOI is buffered so downstream normalisation/metric edge cells have data.
suppressPackageStartupMessages({
  library(sf)
  library(jsonlite)
})

usage <- paste(
  "Usage: Rscript scripts/extract_ept_block.R",
  "--ept-url URL --aoi FILE --layer NAME --output FILE",
  "[--buffer-m 60] [--requests 16] [--retries 3] [--block-id ID]",
  sep = "\n"
)

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opts <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
required <- c("ept-url", "aoi", "layer", "output")
if (!all(required %in% names(opts))) stop(usage, call. = FALSE)
pdal_bin <- Sys.getenv("PDAL_BIN", unset = Sys.which("pdal"))
if (!nzchar(pdal_bin) || !file.exists(pdal_bin)) {
  stop("PDAL is required; set PDAL_BIN or add pdal to PATH.", call. = FALSE)
}
if (file.exists(opts[["output"]])) stop("Refusing to overwrite: ", opts[["output"]], call. = FALSE)
partial_output <- paste0(opts[["output"]], ".partial")
if (file.exists(partial_output)) stop("Refusing to overwrite partial output: ", partial_output, call. = FALSE)

opt_or <- function(name, default) if (name %in% names(opts)) opts[[name]] else default
buffer_m <- as.numeric(opt_or("buffer-m", "60"))
requests <- as.integer(opt_or("requests", "16"))
retries <- as.integer(opt_or("retries", "3"))
if (!is.finite(buffer_m) || buffer_m < 0 || is.na(requests) || requests < 4 || is.na(retries) || retries < 1) {
  stop("buffer-m must be >= 0, requests >= 4, and retries >= 1.", call. = FALSE)
}

aoi <- st_read(opts[["aoi"]], layer = opts[["layer"]], quiet = TRUE)
if ("block-id" %in% names(opts)) {
  if (!"block_id" %in% names(aoi)) stop("AOI has no block_id field.", call. = FALSE)
  aoi <- aoi[aoi$block_id == opts[["block-id"]], ]
}
if (nrow(aoi) != 1L) stop("Extraction requires exactly one bounded AOI feature.", call. = FALSE)
if (is.na(st_crs(aoi))) stop("AOI CRS is missing.", call. = FALSE)
aoi <- st_make_valid(aoi)
# A source-boundary job can contain narrow slivers or touching rings. EPT is
# served in EPSG:3857, so use a buffered Web-Mercator bounding window instead
# of PDAL's brittle WKT-polygon parser. The exact core is masked after metrics;
# the window only adds a small amount of harmless halo data.
stream_area <- st_buffer(aoi, buffer_m)
stream_area <- st_make_valid(stream_area)
stream_area <- st_transform(stream_area, 3857)
bounds <- st_bbox(stream_area)
ept_bounds <- sprintf("([%.3f, %.3f], [%.3f, %.3f])", bounds[["xmin"]], bounds[["xmax"]], bounds[["ymin"]], bounds[["ymax"]])

dir.create(dirname(opts[["output"]]), recursive = TRUE, showWarnings = FALSE)
pipeline <- list(pipeline = list(
  list(type = "readers.ept", filename = opts[["ept-url"]], bounds = ept_bounds, requests = requests),
  list(type = "filters.expression", expression = "Classification != 7 && Classification != 9"),
  # Commit only a fully completed download, so interrupted PDAL runs never
  # look like valid cached LAZ inputs to later pipeline stages.
  list(type = "writers.las", filename = partial_output, compression = "laszip")
))
pipeline_path <- tempfile(fileext = ".json")
write_json(pipeline, pipeline_path, auto_unbox = TRUE, pretty = TRUE)
status <- 1L
for (attempt in seq_len(retries)) {
  if (file.exists(partial_output)) unlink(partial_output)
  status <- system2(pdal_bin, c("pipeline", pipeline_path))
  if (status == 0L && file.exists(partial_output) && file.rename(partial_output, opts[["output"]])) break
  if (attempt < retries) {
    message("EPT extraction attempt ", attempt, " failed; retrying after ", attempt * 5, " seconds.")
    Sys.sleep(attempt * 5)
  }
}
if (!file.exists(opts[["output"]])) {
  if (file.exists(partial_output)) unlink(partial_output)
  stop("PDAL EPT extraction failed after ", retries, " attempts.", call. = FALSE)
}
message("Wrote streamed block: ", opts[["output"]])
