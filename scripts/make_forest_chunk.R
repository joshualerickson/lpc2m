#!/usr/bin/env Rscript

# Make a bounded square test core inside one named forest AOI.
suppressPackageStartupMessages(library(sf))

usage <- paste(
  "Usage: Rscript scripts/make_forest_chunk.R --input AOI.gpkg --layer NAME",
  "--forestname TEXT --side-m 2000 --output CHUNK.gpkg --output-layer NAME",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opts <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
required <- c("input", "layer", "forestname", "side-m", "output", "output-layer")
if (!all(required %in% names(opts))) stop(usage, call. = FALSE)
if (file.exists(opts[["output"]])) stop("Refusing to overwrite: ", opts[["output"]], call. = FALSE)

aoi <- st_read(opts[["input"]], layer = opts[["layer"]], quiet = TRUE)
hit <- aoi[tolower(aoi$forestname) == tolower(opts[["forestname"]]), ]
if (nrow(hit) != 1L) stop("Expected one forest feature; found ", nrow(hit), call. = FALSE)
side_m <- as.numeric(opts[["side-m"]])
if (!is.finite(side_m) || side_m <= 0) stop("side-m must be positive.", call. = FALSE)

anchor <- st_point_on_surface(hit)
xy <- st_coordinates(anchor)
square <- st_as_sfc(st_bbox(c(xmin = xy[1] - side_m / 2, ymin = xy[2] - side_m / 2,
                               xmax = xy[1] + side_m / 2, ymax = xy[2] + side_m / 2), crs = st_crs(hit)))
core <- st_intersection(hit["forestname"], square)
if (st_is_empty(core) || as.numeric(st_area(core)) < 0.95 * side_m^2) {
  stop("Could not create a nearly complete square inside the target forest.", call. = FALSE)
}
core$block_id <- paste0(gsub("[^a-z0-9]+", "_", tolower(opts[["forestname"]])), "_", side_m, "m")
core$core_area_m2 <- as.numeric(st_area(core))
st_write(core, opts[["output"]], layer = opts[["output-layer"]], quiet = TRUE)
message("Wrote ", opts[["output"]], " (", round(core$core_area_m2 / 1e6, 2), " km2)")
