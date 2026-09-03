#!/usr/bin/env Rscript

# Split a single core AOI into stable, square processing blocks.
suppressPackageStartupMessages(library(sf))

usage <- paste(
  "Usage: Rscript scripts/make_processing_grid.R --input CORE.gpkg --layer NAME",
  "--block-m 1000 --output BLOCKS.gpkg --output-layer NAME",
  "[--filter-field FIELD --filter-value VALUE]",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opts <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
required <- c("input", "layer", "block-m", "output", "output-layer")
if (!all(required %in% names(opts))) stop(usage, call. = FALSE)
if (file.exists(opts[["output"]])) stop("Refusing to overwrite: ", opts[["output"]], call. = FALSE)

core <- st_read(opts[["input"]], layer = opts[["layer"]], quiet = TRUE)
if (xor("filter-field" %in% names(opts), "filter-value" %in% names(opts))) {
  stop("Provide both --filter-field and --filter-value.", call. = FALSE)
}
if ("filter-field" %in% names(opts)) {
  field <- opts[["filter-field"]]
  if (!field %in% names(core)) stop("AOI has no field: ", field, call. = FALSE)
  # Make copied CLI values resilient to trailing spaces and typographic dashes.
  normalize_text <- function(x) trimws(gsub("[\u2010\u2011\u2012\u2013\u2014\u2212]", "-", as.character(x)))
  core <- core[normalize_text(core[[field]]) == normalize_text(opts[["filter-value"]]), ]
}
if (!nrow(core) || is.na(st_crs(core))) stop("input must contain at least one feature with a CRS.", call. = FALSE)
block_m <- as.numeric(opts[["block-m"]])
if (!is.finite(block_m) || block_m <= 0) stop("block-m must be positive.", call. = FALSE)

core <- st_union(st_make_valid(core))
grid <- st_make_grid(core, cellsize = block_m, square = TRUE, what = "polygons")
blocks <- st_intersection(st_as_sf(data.frame(block_id = seq_along(grid)), geometry = grid), core)
blocks <- blocks[as.numeric(st_area(blocks)) > 0, ]
blocks$block_id <- sprintf("block_%03d", seq_len(nrow(blocks)))
blocks$core_area_m2 <- as.numeric(st_area(blocks))
st_write(blocks, opts[["output"]], layer = opts[["output-layer"]], quiet = TRUE)
message("Wrote ", nrow(blocks), " processing blocks: ", opts[["output"]])
