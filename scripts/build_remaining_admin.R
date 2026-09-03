#!/usr/bin/env Rscript

# Create the portion of the Forest Service administrative layer that has not
# already received lidar metrics.  The source admin GeoPackage contains a
# geometry encoding that GDAL understands but sf cannot process directly, so
# it is first translated to valid EPSG:5070 multipolygons.

suppressPackageStartupMessages(library(sf))

admin_path <- "data/admin.gpkg"
admin_layer <- "admin"
completed_path <- "/mnt/mordor3/data/lidar_download/canopy_metrics/NR_lidar_polys_merged_epsg5070.gpkg"
completed_layer <- "NR_lidar_polys_merged_epsg5070"
output_path <- "data/admin_remaining_epsg5070.gpkg"
output_layer <- "admin_remaining"
requested_index <- commandArgs(trailingOnly = TRUE)

if (length(requested_index) > 1L ||
    (length(requested_index) == 1L && is.na(suppressWarnings(as.integer(requested_index)))) ||
    (length(requested_index) == 0L && file.exists(output_path))) {
  stop("Refusing to overwrite existing output: ", output_path, call. = FALSE)
}

sf_use_s2(FALSE)
translated_admin <- tempfile(fileext = ".gpkg")
on.exit(unlink(translated_admin), add = TRUE)

status <- system2(
  "ogr2ogr",
  c(
    "-f", "GPKG", translated_admin, admin_path, admin_layer,
    "-nln", admin_layer, "-t_srs", "EPSG:5070",
    "-makevalid", "-nlt", "PROMOTE_TO_MULTI"
  )
)
if (status != 0L) stop("GDAL could not translate the admin layer.", call. = FALSE)

admin <- st_read(translated_admin, layer = admin_layer, quiet = TRUE)
completed <- st_read(completed_path, layer = completed_layer, quiet = TRUE)
invalid_completed <- !st_is_valid(completed)
if (any(invalid_completed)) {
  st_geometry(completed)[invalid_completed] <- st_geometry(
    st_make_valid(completed[invalid_completed, ])
  )
}
intersections <- st_intersects(admin, completed)
indices <- if (length(requested_index)) as.integer(requested_index) else seq_len(nrow(admin))
if (any(indices < 1L | indices > nrow(admin))) {
  stop("Requested forest index is out of range.", call. = FALSE)
}

for (i in indices) {
  geometry <- st_geometry(admin[i, ])
  for (j in intersections[[i]]) {
    geometry <- st_difference(geometry, st_geometry(completed[j, ]))
    if (!length(geometry)) break
  }
  if (!length(geometry) || all(st_is_empty(geometry))) next

  # Restore one MULTIPOLYGON per source admin feature, retaining its fields.
  geometry <- st_cast(st_combine(geometry), "MULTIPOLYGON", warn = FALSE)
  remaining <- admin[i, ]
  st_geometry(remaining) <- geometry
  st_write(
    remaining, output_path, layer = output_layer,
    append = file.exists(output_path), quiet = TRUE
  )
  message(
    sprintf("%02d/%02d complete: %s", i, nrow(admin), admin$forestname[i])
  )
}

if (length(indices) == nrow(admin)) {
  remaining <- st_read(output_path, layer = output_layer, quiet = TRUE)
  message("Output: ", output_path)
  message("Features: ", nrow(remaining))
  message("Remaining acres: ", round(sum(st_area(remaining)) / 4046.8564224))
}
