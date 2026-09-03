#!/usr/bin/env Rscript

# Extract a buffered processing job from one or more local USGS delivery LAZ
# tiles. The result has the same contract as an EPT raw block and therefore
# enters the existing normalization and metric stages unchanged.
suppressPackageStartupMessages({ library(sf); library(jsonlite) })

usage <- paste(
  "Usage: Rscript scripts/extract_laz_block.R --source-files 'A.laz|B.laz'",
  "--source-crs EPSG --aoi JOBS.gpkg --layer NAME --block-id ID --output FILE",
  "[--buffer-m 60]",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opt <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
required <- c("source-files", "source-crs", "aoi", "layer", "block-id", "output")
if (!all(required %in% names(opt))) stop(usage, call. = FALSE)
pdal_bin <- Sys.getenv("PDAL_BIN", unset = Sys.which("pdal"))
if (!nzchar(pdal_bin) || !file.exists(pdal_bin)) stop("PDAL is required; set PDAL_BIN or add it to PATH.", call. = FALSE)
if (file.exists(opt[["output"]])) stop("Refusing to overwrite: ", opt[["output"]], call. = FALSE)
partial <- paste0(opt[["output"]], ".partial")
if (file.exists(partial)) unlink(partial)

files <- strsplit(opt[["source-files"]], "|", fixed = TRUE)[[1]]
files <- unique(files[nzchar(files)])
if (!length(files) || any(!file.exists(files))) stop("Every direct-LAZ source file must exist locally.", call. = FALSE)
source_crs <- suppressWarnings(st_crs(as.integer(opt[["source-crs"]])))
if (is.na(source_crs)) source_crs <- suppressWarnings(st_crs(opt[["source-crs"]]))
if (is.na(source_crs)) stop("source-crs is not readable: ", opt[["source-crs"]], call. = FALSE)
buffer_m <- as.numeric(if ("buffer-m" %in% names(opt)) opt[["buffer-m"]] else 60)

aoi <- st_read(opt[["aoi"]], layer = opt[["layer"]], quiet = TRUE)
aoi <- aoi[aoi$block_id == opt[["block-id"]], ]
if (nrow(aoi) != 1L) stop("Extraction requires exactly one job feature.", call. = FALSE)
bounds <- st_bbox(st_transform(st_buffer(st_make_valid(aoi), buffer_m), source_crs))
crop_bounds <- sprintf("([%.3f, %.3f], [%.3f, %.3f])", bounds[["xmin"]], bounds[["xmax"]], bounds[["ymin"]], bounds[["ymax"]])

readers <- lapply(seq_along(files), function(i) list(type = "readers.las", filename = files[i], tag = paste0("reader", i)))
stages <- readers
input_tags <- paste0("reader", seq_along(files))
if (length(files) > 1L) stages <- c(stages, list(list(type = "filters.merge", inputs = input_tags, tag = "merged")))
crop_input <- if (length(files) > 1L) "merged" else input_tags
stages <- c(stages, list(
  list(type = "filters.crop", bounds = crop_bounds, inputs = crop_input, tag = "cropped"),
  list(type = "filters.expression", expression = "Classification != 7 && Classification != 9", inputs = "cropped"),
  list(type = "writers.las", filename = partial, compression = "laszip")
))
pipeline_path <- tempfile(fileext = ".json")
on.exit(unlink(pipeline_path), add = TRUE)
write_json(list(pipeline = stages), pipeline_path, auto_unbox = TRUE, pretty = TRUE)
dir.create(dirname(opt[["output"]]), recursive = TRUE, showWarnings = FALSE)
status <- system2(pdal_bin, c("pipeline", pipeline_path))
if (status != 0L || !file.exists(partial) || !file.rename(partial, opt[["output"]])) {
  if (file.exists(partial)) unlink(partial)
  stop("PDAL direct-LAZ extraction failed.", call. = FALSE)
}
message("Wrote direct-LAZ block: ", opt[["output"]])
