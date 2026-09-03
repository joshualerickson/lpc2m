#!/usr/bin/env Rscript

# Resolve live EPT coverage for an AOI.  The maintained resource boundary
# index is used only to find candidate names; each candidate is confirmed
# against its official USGS STAC item, which supplies the exact footprint and
# ept.json endpoint.  QL and acquisition dates come from the USGS 3DEP index.
suppressPackageStartupMessages({ library(sf); library(curl); library(jsonlite) })
suppressMessages(sf_use_s2(FALSE))

resource_index <- "https://raw.githubusercontent.com/hobuinc/usgs-lidar/master/boundaries/resources.geojson"
stac_base <- "https://s3-us-west-2.amazonaws.com/usgs-lidar-stac/ept"
metadata_api <- "https://index.nationalmap.gov/arcgis/rest/services/3DEPElevationIndex/MapServer/24/query"
usage <- paste(
  "Usage: Rscript scripts/resolve_live_ept_sources.R --aoi AOI.gpkg --layer NAME",
  "--output SOURCES.gpkg [--output-layer ept_sources]",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opt <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
if (!all(c("aoi", "layer", "output") %in% names(opt))) stop(usage, call. = FALSE)
if (file.exists(opt[["output"]])) stop("Refusing to overwrite: ", opt[["output"]], call. = FALSE)
output_layer <- if ("output-layer" %in% names(opt)) opt[["output-layer"]] else "ept_sources"

repair <- function(x) {
  original <- st_crs(x)
  geographic <- st_is_longlat(x)
  if (geographic) {
    bbox <- st_bbox(x)
    local_crs <- paste0(
      "+proj=laea +lat_0=", mean(c(bbox[["ymin"]], bbox[["ymax"]])),
      " +lon_0=", mean(c(bbox[["xmin"]], bbox[["xmax"]])),
      " +datum=WGS84 +units=m +no_defs"
    )
    x <- st_transform(x, local_crs)
  }
  x <- st_set_geometry(x, st_buffer(st_geometry(st_make_valid(x)), 0))
  x <- x[!st_is_empty(x), ]
  if (geographic) x <- st_transform(x, original)
  x
}
fetch_json <- function(url, retries = 3L) {
  last_error <- NULL
  for (attempt in seq_len(retries)) {
    answer <- tryCatch({
      response <- curl_fetch_memory(url)
      if (response$status_code != 200L) stop("HTTP ", response$status_code)
      fromJSON(rawToChar(response$content), simplifyVector = FALSE)
    }, error = function(e) { last_error <<- e; NULL })
    if (!is.null(answer)) return(answer)
    if (attempt < retries) Sys.sleep(attempt)
  }
  stop(conditionMessage(last_error), call. = FALSE)
}
epoch_date <- function(x) {
  if (!is.finite(as.numeric(x))) return(NA_character_)
  format(as.POSIXct(as.numeric(x) / 1000, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d")
}
metadata_for <- function(workunit, bbox) {
  # The public service intermittently rejects exact string WHERE queries.
  # Its spatial-envelope query is reliable; select the exact STAC resource ID
  # from that response locally.
  params <- list(
    f = "json", where = "1=1",
    geometry = paste(unname(bbox[c("xmin", "ymin", "xmax", "ymax")]), collapse = ","),
    geometryType = "esriGeometryEnvelope", inSR = "4326", spatialRel = "esriSpatialRelIntersects",
    returnGeometry = "false", outFields = "workunit,ql,collect_start,collect_end"
  )
  query <- paste(paste(names(params), vapply(params, curl_escape, character(1)), sep = "="), collapse = "&")
  for (attempt in seq_len(5L)) {
    payload <- tryCatch(fetch_json(paste0(metadata_api, "?", query)), error = function(e) NULL)
    if (!is.null(payload) && is.null(payload$error) && length(payload$features)) {
      attributes <- lapply(payload$features, `[[`, "attributes")
      attributes <- Filter(function(x) identical(as.character(x$workunit), workunit), attributes)
      if (length(attributes)) {
        attributes <- attributes[[1]]
        quality <- toupper(gsub("[^A-Za-z0-9]", "", as.character(attributes$ql)))
        if (quality %in% c("QL1", "QL2")) return(list(
          quality_level = quality, acquisition_start = epoch_date(attributes$collect_start),
          acquisition_end = epoch_date(attributes$collect_end)
        ))
      }
    }
    if (attempt < 5L) Sys.sleep(attempt)
  }
  NULL
}

aoi <- repair(st_read(opt[["aoi"]], layer = opt[["layer"]], quiet = TRUE))
if (!nrow(aoi) || is.na(st_crs(aoi))) stop("AOI must be non-empty and have a CRS.", call. = FALSE)
index_file <- tempfile(fileext = ".geojson")
on.exit(unlink(index_file), add = TRUE)
curl_download(resource_index, index_file, quiet = TRUE)
index <- repair(st_read(index_file, quiet = TRUE))
if (!all(c("name", "url") %in% names(index))) stop("Live EPT resource index lacks name/url fields.", call. = FALSE)
index <- st_transform(index[, intersect(c("name", "url", "count"), names(index))], st_crs(aoi))
index <- index[lengths(st_intersects(index, aoi)) > 0, ]
if (!nrow(index)) quit(status = 2L)

rows <- lapply(seq_len(nrow(index)), function(i) {
  name <- as.character(index$name[i])
  item_url <- paste0(stac_base, "/", utils::URLencode(name, reserved = TRUE), ".json")
  item_file <- tempfile(fileext = ".geojson")
  on.exit(unlink(item_file), add = TRUE)
  ok <- tryCatch({ curl_download(item_url, item_file, quiet = TRUE); TRUE }, error = function(e) FALSE)
  if (!ok) return(NULL)
  item <- tryCatch(repair(st_read(item_file, quiet = TRUE)), error = function(e) NULL)
  if (is.null(item) || !nrow(item) || !length(st_intersects(item, st_transform(aoi, st_crs(item)))[[1]])) return(NULL)
  payload <- tryCatch(fetch_json(item_url), error = function(e) NULL)
  ept_url <- if (is.null(payload)) NA_character_ else as.character(payload$assets[["ept.json"]]$href)
  if (is.na(ept_url) || !nzchar(ept_url)) return(NULL)
  meta <- metadata_for(name, st_bbox(st_transform(item, 4326)))
  if (is.null(meta)) {
    message("USGS QL/date metadata unavailable or unsupported for live EPT source: ", name)
    return(NULL)
  }
  item <- st_transform(item, st_crs(aoi))
  item$ept_name <- name
  item$ept_url <- ept_url
  item$catalog_ept_url <- as.character(index$url[i])
  item$stac_item_url <- item_url
  item$quality_level <- meta$quality_level
  item$acquisition_start <- meta$acquisition_start
  item$acquisition_end <- meta$acquisition_end
  item$acquisition_year <- suppressWarnings(as.integer(substr(meta$acquisition_end, 1, 4)))
  item
})
sources <- Filter(Negate(is.null), rows)
if (!length(sources)) quit(status = 2L)
sources <- do.call(rbind, sources)
sources <- suppressWarnings(st_intersection(sources, st_union(aoi)))
sources <- sources[as.numeric(st_area(sources)) > 0, ]
if (!nrow(sources)) quit(status = 2L)
keep <- c("ept_name", "ept_url", "catalog_ept_url", "stac_item_url", "quality_level", "acquisition_start", "acquisition_end", "acquisition_year")
sources <- sources[, keep]
st_write(sources, opt[["output"]], layer = output_layer, quiet = TRUE)
message("Wrote ", nrow(sources), " live QL1/QL2 EPT source pieces: ", opt[["output"]])
