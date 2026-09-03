#!/usr/bin/env Rscript

# Combine STAC-confirmed EPT coverage with USGS 3DEP QL metadata. The result is
# a planning footprint: streamable EPT coverage that is also QL1 or QL2.
suppressPackageStartupMessages({ library(sf); library(curl); library(jsonlite) })

service <- "https://index.nationalmap.gov/arcgis/rest/services/3DEPElevationIndex/MapServer/8/query"
usage <- paste(
  "Usage: Rscript scripts/make_ept_ql12_ready_footprint.R --ept EPT.gpkg --layer NAME",
  "--aoi AOI.gpkg --aoi-layer NAME --output READY.gpkg [--not-ready-output FILE]",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opts <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
required <- c("ept", "layer", "aoi", "aoi-layer", "output")
if (!all(required %in% names(opts))) stop(usage, call. = FALSE)
if (file.exists(opts[["output"]])) stop("Refusing to overwrite: ", opts[["output"]], call. = FALSE)

ept <- st_read(opts[["ept"]], layer = opts[["layer"]], quiet = TRUE)
aoi <- st_read(opts[["aoi"]], layer = opts[["aoi-layer"]], quiet = TRUE)
if (!nrow(ept) || !nrow(aoi) || is.na(st_crs(ept)) || is.na(st_crs(aoi))) stop("EPT and AOI layers must be non-empty and have CRS.", call. = FALSE)
aoi <- st_as_sf(st_union(st_make_valid(st_transform(aoi, st_crs(ept)))))

fetch <- function(params) {
  query <- paste(paste(names(params), vapply(params, curl_escape, character(1)), sep = "="), collapse = "&")
  response <- curl_fetch_memory(paste0(service, "?", query))
  if (response$status_code != 200L) stop("USGS LPC index returned HTTP ", response$status_code, call. = FALSE)
  payload <- fromJSON(rawToChar(response$content), simplifyDataFrame = FALSE)
  if (!is.null(payload$error)) stop("USGS LPC index error: ", payload$error$message, call. = FALSE)
  payload
}
bbox <- st_bbox(st_transform(aoi, 4326))
envelope <- paste(unname(bbox[c("xmin", "ymin", "xmax", "ymax")]), collapse = ",")
fields <- c("workunit", "project", "ql", "collect_start", "collect_end")
ids <- fetch(list(f = "json", where = "1=1", geometry = envelope, geometryType = "esriGeometryEnvelope", inSR = "4326", spatialRel = "esriSpatialRelIntersects", returnIdsOnly = "true"))$objectIds
if (!length(ids)) stop("No National Map LPC work units intersect the AOI extent.", call. = FALSE)
features <- unlist(lapply(split(ids, ceiling(seq_along(ids) / 500L)), function(batch) {
  fetch(list(f = "json", objectIds = paste(batch, collapse = ","), outSR = "4326", returnGeometry = "true", outFields = paste(fields, collapse = ",")))$features
}), recursive = FALSE)
field <- function(x, name) if (!is.null(x[[name]])) x[[name]] else NA
attrs <- lapply(features, function(f) data.frame(
  workunit = as.character(field(f$attributes, "workunit")), project = as.character(field(f$attributes, "project")),
  quality_level = toupper(gsub("[^A-Za-z0-9]", "", as.character(field(f$attributes, "ql")))),
  collect_start_epoch_ms = as.numeric(field(f$attributes, "collect_start")), collect_end_epoch_ms = as.numeric(field(f$attributes, "collect_end")), stringsAsFactors = FALSE
))
geoms <- lapply(features, function(f) {
  rings <- f$geometry$rings
  ring_list <- if (is.matrix(rings)) list(rings) else rings
  st_multipolygon(lapply(ring_list, function(r) list(if (is.matrix(r)) r else matrix(unlist(r, use.names = FALSE), ncol = 2, byrow = TRUE))))
})
meta <- st_sf(do.call(rbind, attrs), geometry = st_sfc(geoms, crs = 4326))
meta <- st_make_valid(st_transform(meta, st_crs(ept)))
meta <- meta[meta$quality_level %in% c("QL1", "QL2"), ]
if (!nrow(meta)) stop("No QL1/QL2 National Map work units intersect the AOI extent.", call. = FALSE)
epoch_date <- function(x) ifelse(is.finite(x), format(as.POSIXct(x / 1000, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"), NA_character_)
meta$acquisition_start <- epoch_date(meta$collect_start_epoch_ms)
meta$acquisition_end <- epoch_date(meta$collect_end_epoch_ms)
meta$acquisition_end_year <- suppressWarnings(as.integer(substr(meta$acquisition_end, 1, 4)))

ready_sources <- suppressWarnings(st_intersection(ept, meta))
ready_sources <- ready_sources[as.numeric(st_area(ready_sources)) > 0, ]
if (!nrow(ready_sources)) stop("No STAC EPT coverage overlaps QL1/QL2 metadata.", call. = FALSE)
ready_sources$readiness_status <- "ready_ept_ql12"
ready_sources$source_area_m2 <- as.numeric(st_area(ready_sources))
st_write(ready_sources, opts[["output"]], layer = "ready_sources", quiet = TRUE)
ready_footprint <- st_as_sf(st_union(st_geometry(ready_sources)))
ready_footprint$readiness_status <- "ready_ept_ql12"
ready_footprint$area_m2 <- as.numeric(st_area(ready_footprint))
st_write(ready_footprint, opts[["output"]], layer = "ready_footprint", append = TRUE, quiet = TRUE)

remaining <- st_as_sf(suppressWarnings(st_difference(st_geometry(aoi), st_geometry(ready_footprint))))
remaining <- remaining[as.numeric(st_area(remaining)) > 0, ]
not_ready_path <- if ("not-ready-output" %in% names(opts)) opts[["not-ready-output"]] else paste0(tools::file_path_sans_ext(opts[["output"]]), "_not_ready.gpkg")
if (file.exists(not_ready_path)) stop("Refusing to overwrite: ", not_ready_path, call. = FALSE)
remaining$readiness_status <- "not_ready_ept_ql12"
remaining$area_m2 <- as.numeric(st_area(remaining))
st_write(remaining, not_ready_path, layer = "not_ready", quiet = TRUE)

aoi_area <- sum(as.numeric(st_area(aoi))); ready_area <- sum(as.numeric(st_area(ready_footprint)))
summary <- data.frame(aoi_area_km2 = aoi_area / 1e6, ready_area_km2 = ready_area / 1e6,
  ready_percent = 100 * ready_area / aoi_area, not_ready_area_km2 = (aoi_area - ready_area) / 1e6,
  not_ready_percent = 100 * (aoi_area - ready_area) / aoi_area, ready_source_pieces = nrow(ready_sources))
summary_path <- paste0(tools::file_path_sans_ext(opts[["output"]]), "_summary.csv")
write.csv(summary, summary_path, row.names = FALSE)
message("Wrote QL1/QL2 EPT-ready footprint: ", opts[["output"]])
message("Wrote non-ready footprint: ", not_ready_path)
message("Wrote readiness summary: ", summary_path)
