#!/usr/bin/env Rscript

# Intersect an AOI with the published USGS Entwine resource catalog. This is an
# availability preflight only: it downloads catalog metadata, never lidar points.
suppressPackageStartupMessages({ library(sf); library(curl); library(jsonlite) })

usage <- paste(
  "Usage: Rscript scripts/make_ept_catalog_footprint.R --aoi AOI.gpkg --layer NAME",
  "--output EPT_COVERAGE.gpkg --output-layer NAME [--uncovered-output FILE]",
  "[--catalog-url URL] [--stac-base URL]",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opts <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
required <- c("aoi", "layer", "output", "output-layer")
if (!all(required %in% names(opts))) stop(usage, call. = FALSE)
if (file.exists(opts[["output"]])) stop("Refusing to overwrite: ", opts[["output"]], call. = FALSE)
opt_or <- function(name, default) if (name %in% names(opts)) opts[[name]] else default

aoi_features <- st_read(opts[["aoi"]], layer = opts[["layer"]], quiet = TRUE)
if (!nrow(aoi_features) || is.na(st_crs(aoi_features))) stop("AOI must contain at least one feature with a CRS.", call. = FALSE)
aoi_features <- st_make_valid(aoi_features)
aoi <- st_as_sf(st_union(aoi_features))
catalog_url <- opt_or("catalog-url", "https://raw.githubusercontent.com/hobuinc/usgs-lidar/master/boundaries/resources.geojson")
stac_base <- sub("/$", "", opt_or("stac-base", "https://s3-us-west-2.amazonaws.com/usgs-lidar-stac/ept"))
catalog_file <- tempfile(fileext = ".geojson")
on.exit(unlink(catalog_file), add = TRUE)
curl_download(catalog_url, catalog_file, quiet = TRUE)
catalog <- st_read(catalog_file, quiet = TRUE)
needed <- c("name", "url")
if (!all(needed %in% names(catalog))) stop("EPT catalog is missing expected fields: ", paste(needed, collapse = ", "), call. = FALSE)
catalog <- st_make_valid(st_transform(catalog[, intersect(c("name", "url", "count"), names(catalog))], st_crs(aoi)))

hits <- st_intersects(catalog, aoi, sparse = FALSE)[, 1]
catalog <- catalog[hits, ]
if (nrow(catalog)) {
  # The maintained resource GeoJSON provides a fast national boundary index.
  # Confirm only the intersecting records against the official USGS STAC items,
  # whose ept.json asset is the endpoint used downstream.
  stac_item_url <- paste0(stac_base, "/", utils::URLencode(catalog$name, reserved = TRUE), ".json")
  stac_info <- lapply(stac_item_url, function(item_url) {
    tryCatch({
      item <- fromJSON(rawToChar(curl_fetch_memory(item_url)$content), simplifyVector = FALSE)
      asset <- item$assets[["ept.json"]]$href
      if (is.null(asset) || !nzchar(asset)) stop("STAC item has no ept.json asset")
      list(available = TRUE, ept_url = as.character(asset), datetime = as.character(item$properties$datetime))
    }, error = function(e) list(available = FALSE, ept_url = NA_character_, datetime = NA_character_))
  })
  catalog$stac_item_url <- stac_item_url
  catalog$stac_available <- vapply(stac_info, `[[`, logical(1), "available")
  catalog$stac_datetime <- vapply(stac_info, `[[`, character(1), "datetime")
  catalog$ept_url <- vapply(stac_info, `[[`, character(1), "ept_url")
  coverage <- suppressWarnings(st_intersection(catalog, aoi))
  coverage <- coverage[as.numeric(st_area(coverage)) > 0, ]
  names(coverage)[names(coverage) == "name"] <- "ept_name"
  names(coverage)[names(coverage) == "url"] <- "catalog_ept_url"
  if ("count" %in% names(coverage)) names(coverage)[names(coverage) == "count"] <- "ept_points"
  coverage$coverage_status <- ifelse(coverage$stac_available, "ept", "ept_stac_unavailable")
  coverage$intersect_area_m2 <- as.numeric(st_area(coverage))
  st_write(coverage, opts[["output"]], layer = opts[["output-layer"]], quiet = TRUE)
  covered_geometry <- st_union(st_geometry(coverage[coverage$stac_available, ]))
} else {
  coverage <- NULL
  covered_geometry <- st_sfc(crs = st_crs(aoi))
}

remaining <- if (length(covered_geometry)) suppressWarnings(st_difference(st_geometry(aoi), covered_geometry)) else st_geometry(aoi)
remaining <- st_as_sf(remaining)
remaining <- remaining[as.numeric(st_area(remaining)) > 0, ]
uncovered_output <- opt_or("uncovered-output", paste0(tools::file_path_sans_ext(opts[["output"]]), "_not_ept.gpkg"))
if (file.exists(uncovered_output)) stop("Refusing to overwrite: ", uncovered_output, call. = FALSE)
if (nrow(remaining)) {
  # Preserve AOI attributes (for example forest/district name) on the holes so
  # the alternate-acquisition list is immediately actionable.
  not_ept <- suppressWarnings(st_intersection(aoi_features, remaining))
  not_ept <- not_ept[as.numeric(st_area(not_ept)) > 0, ]
  not_ept$coverage_status <- "not_in_ept_catalog"
  not_ept$area_m2 <- as.numeric(st_area(not_ept))
  st_write(not_ept, uncovered_output, layer = "not_ept", quiet = TRUE)
}

aoi_area <- sum(as.numeric(st_area(aoi)))
ept_area <- if (is.null(coverage)) 0 else sum(as.numeric(st_area(st_union(st_geometry(coverage)))))
not_ept_area <- sum(as.numeric(st_area(remaining)))
summary <- data.frame(
  aoi_area_km2 = aoi_area / 1e6,
  ept_area_km2 = ept_area / 1e6,
  ept_percent = 100 * ept_area / aoi_area,
  not_ept_area_km2 = not_ept_area / 1e6,
  not_ept_percent = 100 * not_ept_area / aoi_area,
  ept_resources = if (is.null(coverage)) 0L else sum(coverage$stac_available),
  stac_unavailable_resources = if (is.null(coverage)) 0L else sum(!coverage$stac_available)
)
summary_path <- paste0(tools::file_path_sans_ext(opts[["output"]]), "_summary.csv")
write.csv(summary, summary_path, row.names = FALSE)
message("Wrote ", if (is.null(coverage)) 0L else nrow(coverage), " EPT coverage pieces: ", opts[["output"]])
message("Wrote non-EPT footprint: ", uncovered_output)
message("Wrote EPT availability summary: ", summary_path)
