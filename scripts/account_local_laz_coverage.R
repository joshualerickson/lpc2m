#!/usr/bin/env Rscript

# Account for a local LAZ tile index against an alternate-acquisition footprint.
suppressPackageStartupMessages(library(sf))
usage <- paste(
  "Usage: Rscript scripts/account_local_laz_coverage.R --holes HOLES.gpkg --holes-layer NAME",
  "--tile-index TILES.shp --tile-layer NAME --output ACCOUNTING.gpkg",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opts <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
required <- c("holes", "holes-layer", "tile-index", "tile-layer", "output")
if (!all(required %in% names(opts))) stop(usage, call. = FALSE)
if (file.exists(opts[["output"]])) stop("Refusing to overwrite: ", opts[["output"]], call. = FALSE)

holes <- st_read(opts[["holes"]], layer = opts[["holes-layer"]], quiet = TRUE)
tiles <- st_read(opts[["tile-index"]], layer = opts[["tile-layer"]], quiet = TRUE)
if (!nrow(holes) || !nrow(tiles) || is.na(st_crs(holes)) || is.na(st_crs(tiles))) stop("holes and tile index must be non-empty with CRS.", call. = FALSE)
holes <- st_make_valid(st_transform(holes, st_crs(tiles)))
tiles <- st_make_valid(tiles)
tile_field <- if ("location" %in% names(tiles)) "location" else names(st_drop_geometry(tiles))[1]
# Keep the input feature attributes (for example forest name) in the overlap
# layer. Dissolve overlapping AOIs before this step when a unioned area total
# is required; that operation can be expensive for a state-scale multipart AOI.
hit_index <- st_intersects(tiles, holes)
tiles <- tiles[lengths(hit_index) > 0, ]
if (!nrow(tiles)) stop("No local tiles intersect the holes.", call. = FALSE)
tiles$local_bytes <- file.info(tiles[[tile_field]])$size
tiles$local_bytes[is.na(tiles$local_bytes)] <- 0
overlap <- suppressWarnings(st_intersection(tiles[, c(tile_field, "local_bytes")], holes))
overlap <- overlap[as.numeric(st_area(overlap)) > 0, ]
overlap$coverage_status <- "covered_by_local_laz"
overlap$overlap_area_m2 <- as.numeric(st_area(overlap))
st_write(overlap, opts[["output"]], layer = "local_tile_overlap", quiet = TRUE)

hole_area <- sum(as.numeric(st_area(holes)))
# PDAL tile indexes are non-overlapping source-tile footprints, so summing their
# intersections avoids an expensive national-scale dissolve. Input AOIs should
# likewise be non-overlapping for area percentages to be unioned totals.
covered_area <- sum(as.numeric(st_area(overlap)))
summary <- data.frame(
  local_tiles_intersecting = nrow(tiles), local_bytes_available = sum(tiles$local_bytes),
  local_tb_available = sum(tiles$local_bytes) / 1024^4,
  holes_area_km2 = hole_area / 1e6, locally_covered_hole_km2 = covered_area / 1e6,
  locally_covered_percent = 100 * covered_area / hole_area,
  still_missing_km2 = (hole_area - covered_area) / 1e6
)
summary_path <- paste0(tools::file_path_sans_ext(opts[["output"]]), "_summary.csv")
write.csv(summary, summary_path, row.names = FALSE)
message("Wrote local-LAZ accounting: ", opts[["output"]])
message("Wrote summary: ", summary_path)
