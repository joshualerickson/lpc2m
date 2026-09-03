#!/usr/bin/env Rscript

# Create a deliberately small, reproducible core AOI for EPT integration tests.
suppressPackageStartupMessages(library(sf))

input_path <- "data/lidar_need.gpkg"
input_layer <- "lidar_need"
output_path <- "data/test_block.gpkg"
output_layer <- "test_block"
side_m <- 1000

if (file.exists(output_path)) {
  stop("Refusing to overwrite existing test AOI: ", output_path, call. = FALSE)
}

aoi <- st_read(input_path, layer = input_layer, quiet = TRUE)
if (st_crs(aoi)$epsg != 5070) aoi <- st_transform(aoi, 5070)

# Use a point guaranteed to be in the first remaining forest, then preserve
# only the portion of its 1 km square that lies inside that forest.
anchor <- st_point_on_surface(aoi[1, ])
square <- st_as_sfc(st_bbox(anchor) + c(-side_m / 2, -side_m / 2, side_m / 2, side_m / 2))
core <- st_intersection(aoi[1, c("forestname")], square)
if (nrow(core) != 1L || st_is_empty(core)) stop("Could not construct a test block.")

core$block_id <- "dev_001"
core$core_area_m2 <- as.numeric(st_area(core))
st_write(core, output_path, layer = output_layer, quiet = TRUE)
message("Wrote ", output_path, " (", round(core$core_area_m2 / 1e6, 3), " km2)")
