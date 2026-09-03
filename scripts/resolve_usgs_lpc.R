#!/usr/bin/env Rscript

# Query the USGS 3DEP LPC spatial index for a bounded AOI. This is a small
# REST response (not the multi-gigabyte national WESM GeoPackage). Multipart
# AOIs are accepted; candidates are selected using their combined bounding box.
suppressPackageStartupMessages({
  library(sf)
  library(curl)
  library(jsonlite)
})

service <- "https://index.nationalmap.gov/arcgis/rest/services/3DEPElevationIndex/MapServer/8/query"
usage <- "Usage: Rscript scripts/resolve_usgs_lpc.R --aoi FILE --layer NAME [--output FILE]"
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opts <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
if (!all(c("aoi", "layer") %in% names(opts))) stop(usage, call. = FALSE)

aoi <- st_read(opts[["aoi"]], layer = opts[["layer"]], quiet = TRUE)
if (!nrow(aoi) || is.na(st_crs(aoi))) {
  stop("Resolver requires a non-empty AOI layer with a CRS.", call. = FALSE)
}
bbox <- st_bbox(st_transform(aoi, 4326))
envelope <- paste(unname(bbox[c("xmin", "ymin", "xmax", "ymax")]), collapse = ",")
params <- list(
  f = "json", where = "1=1", geometry = envelope,
  geometryType = "esriGeometryEnvelope", inSR = "4326",
  spatialRel = "esriSpatialRelIntersects", returnGeometry = "false",
  outFields = paste(c(
    "workunit", "workunit_id", "project", "project_id", "ql", "collect_start",
    "collect_end", "lpc_link", "sourcedem_link", "metadata_link", "horiz_crs", "vert_crs"
  ), collapse = ",")
)
query <- paste(
  paste(names(params), vapply(params, curl_escape, character(1)), sep = "="),
  collapse = "&"
)
response <- curl_fetch_memory(paste0(service, "?", query))
if (response$status_code != 200L) stop("USGS LPC index returned HTTP ", response$status_code, call. = FALSE)
payload <- fromJSON(rawToChar(response$content), simplifyDataFrame = FALSE)
if (!is.null(payload$error)) stop("USGS LPC index error: ", payload$error$message, call. = FALSE)
if (!length(payload$features)) {
  message("No published LPC work unit intersects this AOI.")
  quit(status = 2)
}

result <- do.call(rbind, lapply(payload$features, function(feature) {
  as.data.frame(feature$attributes, stringsAsFactors = FALSE)
}))
result <- result[order(result$collect_end, decreasing = TRUE), , drop = FALSE]
if ("output" %in% names(opts)) {
  write.csv(result, opts[["output"]], row.names = FALSE)
  message("Wrote ", opts[["output"]])
} else {
  print(result, row.names = FALSE)
}
