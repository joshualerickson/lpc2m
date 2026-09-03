#!/usr/bin/env Rscript

# Make a 2 km core AOI inside a completed QL2 source, for direct comparison
# against the Northern Region production mosaics.
suppressPackageStartupMessages(library(sf))

output_path <- "data/repro_mt_p3_1_2km.gpkg"
output_layer <- "repro_mt_p3_1_2km"
side_m <- 2000
if (file.exists(output_path)) stop("Refusing to overwrite existing AOI: ", output_path, call. = FALSE)

# This interior location is in the MT_P3_1_B21 QL2 footprint and has valid
# cells in each of the existing 30 m Northern Region metric mosaics.
anchor_3857 <- st_as_sf(data.frame(block_id = "mt_p3_1_q2_2km", x = -12682389, y = 5986305),
                         coords = c("x", "y"), crs = 3857)
anchor <- st_transform(anchor_3857, 5070)
xy <- st_coordinates(anchor)
core <- st_as_sfc(st_bbox(c(xmin = xy[1] - side_m / 2, ymin = xy[2] - side_m / 2,
                            xmax = xy[1] + side_m / 2, ymax = xy[2] + side_m / 2), crs = st_crs(anchor)))
core <- st_as_sf(core)
core$block_id <- "mt_p3_1_q2_2km"
core$project_area <- "MT_P3_1_B21"
core$quality_level <- "QL2"
core$core_area_m2 <- as.numeric(st_area(core))
st_write(core, output_path, layer = output_layer, quiet = TRUE)
message("Wrote ", output_path, " (", side_m / 1000, " km x ", side_m / 1000, " km core)")
