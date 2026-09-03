#!/usr/bin/env Rscript

# Build a clean normalized LAZ for 3-D inspection from cached processing blocks.
# Each normalized block is clipped to its non-buffered core before merging, so
# neighboring 60 m halos do not create duplicate points in the viewer cloud.
suppressPackageStartupMessages({ library(sf); library(jsonlite) })

usage <- paste(
  "Usage: Rscript scripts/make_viewer_pointcloud.R --blocks BLOCKS.gpkg --layer NAME",
  "--cache-dir WORK/cache --output OUTPUT.laz --las-epsg EPSG [--clip-only true|false] [--merge-only true|false]",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opts <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
required <- c("blocks", "layer", "cache-dir", "output", "las-epsg")
if (!all(required %in% names(opts))) stop(usage, call. = FALSE)
is_true <- function(x) tolower(x) %in% c("true", "t", "1", "yes")
clip_only <- is_true(if ("clip-only" %in% names(opts)) opts[["clip-only"]] else "false")
merge_only <- is_true(if ("merge-only" %in% names(opts)) opts[["merge-only"]] else "false")
if (clip_only && merge_only) stop("Use only one of --clip-only or --merge-only.", call. = FALSE)

pdal_bin <- Sys.getenv("PDAL_BIN", unset = Sys.which("pdal"))
if (!nzchar(pdal_bin) || !file.exists(pdal_bin)) stop("PDAL is required; set PDAL_BIN or add it to PATH.", call. = FALSE)
if (file.exists(opts[["output"]])) stop("Refusing to overwrite: ", opts[["output"]], call. = FALSE)

blocks <- st_read(opts[["blocks"]], layer = opts[["layer"]], quiet = TRUE)
if (!"block_id" %in% names(blocks) || !nrow(blocks)) stop("Blocks must contain block_id values.", call. = FALSE)
las_epsg <- as.integer(opts[["las-epsg"]])
if (is.na(las_epsg)) stop("--las-epsg must be an integer EPSG code.", call. = FALSE)
blocks <- st_transform(blocks, las_epsg)

cache_dir <- opts[["cache-dir"]]
norm_files <- file.path(cache_dir, paste0(blocks$block_id, "_norm.laz"))
if (!all(file.exists(norm_files))) stop("Missing normalized cache files: ", paste(basename(norm_files[!file.exists(norm_files)]), collapse = ", "), call. = FALSE)

clip_dir <- file.path(dirname(opts[["output"]]), paste0(tools::file_path_sans_ext(basename(opts[["output"]])), "_cores"))
dir.create(clip_dir, recursive = TRUE, showWarnings = FALSE)
clip_files <- file.path(clip_dir, paste0(blocks$block_id, ".laz"))

if (!merge_only) {
  for (i in seq_len(nrow(blocks))) {
    if (file.exists(clip_files[i])) next
    polygon <- st_as_text(st_geometry(blocks[i, ]))
    pipeline <- list(pipeline = list(
      list(type = "readers.las", filename = norm_files[i]),
      list(type = "filters.crop", polygon = polygon),
      list(type = "writers.las", filename = clip_files[i], compression = "laszip")
    ))
    pipeline_path <- tempfile(fileext = ".json")
    write_json(pipeline, pipeline_path, auto_unbox = TRUE)
    status <- system2(pdal_bin, c("pipeline", pipeline_path))
    unlink(pipeline_path)
    if (status != 0L || !file.exists(clip_files[i])) stop("Core clip failed for ", blocks$block_id[i], call. = FALSE)
  }
}

if (clip_only) {
  message("Wrote core clips: ", clip_dir)
  quit(status = 0L)
}
if (!all(file.exists(clip_files))) stop("Not all core clips are complete; rerun without --merge-only.", call. = FALSE)

partial <- file.path(dirname(opts[["output"]]), paste0(
  tools::file_path_sans_ext(basename(opts[["output"]])), ".partial.laz"
))
status <- system2(pdal_bin, c("merge", clip_files, partial))
if (status != 0L || !file.exists(partial) || !file.rename(partial, opts[["output"]])) {
  if (file.exists(partial)) unlink(partial)
  stop("PDAL merge failed.", call. = FALSE)
}
unlink(clip_files)
unlink(clip_dir, recursive = TRUE)
message("Wrote core-clipped normalized viewer cloud: ", opts[["output"]])
