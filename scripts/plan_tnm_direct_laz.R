#!/usr/bin/env Rscript

# Plan non-EPT acquisition from the official TNMAccess product API. TNMAccess
# supplies exact LAZ URLs, byte counts, and tile bounds; the 3DEP query layer is
# used only for small metadata-by-workunit lookups (QL, dates, and native CRS).
suppressPackageStartupMessages({ library(sf); library(curl); library(jsonlite); library(parallel) })

# Source polygons occasionally contain duplicate vertices. Run all overlay work
# with GEOS in a projected CRS; s2 rejects those rings before st_make_valid()
# gets an opportunity to repair them.
suppressMessages(sf_use_s2(FALSE))

product_api <- "https://tnmaccess.nationalmap.gov/api/v1/products"
metadata_api <- "https://index.nationalmap.gov/arcgis/rest/services/3DEPElevationIndex/MapServer/24/query"
usage <- paste(
  "Usage: Rscript scripts/plan_tnm_direct_laz.R --holes HOLES.gpkg --layer NAME --output PLAN.csv",
  "--footprints-output TILES.gpkg [--local-index TILES.shp --local-layer NAME]",
  "[--metadata-cache WORKUNITS.csv] [--query-grid-deg 0.05] [--query-workers 8] [--buffer-m 60]",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opt <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
required <- c("holes", "layer", "output", "footprints-output")
if (!all(required %in% names(opt))) stop(usage, call. = FALSE)
if (file.exists(opt[["output"]]) || file.exists(opt[["footprints-output"]])) stop("Refusing to overwrite direct-LAZ planning output.", call. = FALSE)
opt_or <- function(name, default) if (name %in% names(opt)) opt[[name]] else default
grid_deg <- as.numeric(opt_or("query-grid-deg", "0.05"))
buffer_m <- as.numeric(opt_or("buffer-m", "60"))
query_workers <- as.integer(opt_or("query-workers", "8"))
if (!is.finite(grid_deg) || grid_deg <= 0 || !is.finite(buffer_m) || buffer_m < 0 || is.na(query_workers) || query_workers < 1L) stop("query-grid-deg and query-workers must be positive and buffer-m non-negative.", call. = FALSE)

holes <- st_read(opt[["holes"]], layer = opt[["layer"]], quiet = TRUE)
if (!nrow(holes) || is.na(st_crs(holes))) stop("Holes must be non-empty and have a CRS.", call. = FALSE)
holes <- st_transform(holes, 5070)
holes <- st_make_valid(holes)
# A zero-width GEOS buffer removes duplicated vertices that can survive
# st_make_valid() in nested/multipart AOIs.
holes <- st_buffer(holes, 0)
holes <- st_as_sf(st_union(holes))
holes_ll <- st_transform(holes, 4326)
query_area <- st_buffer(holes, buffer_m)
query_area_ll <- st_transform(query_area, 4326)
cells <- st_as_sf(st_make_grid(query_area_ll, cellsize = grid_deg, square = TRUE))
cells <- cells[lengths(suppressMessages(st_intersects(cells, query_area_ll))) > 0, ]
message("Querying TNMAccess in ", nrow(cells), " bounded geographic cell(s).")

fetch_json <- function(url, retries = 3L) {
  error <- NULL
  for (attempt in seq_len(retries)) {
    answer <- tryCatch({
      response <- curl_fetch_memory(url)
      if (response$status_code != 200L) stop("HTTP ", response$status_code)
      fromJSON(rawToChar(response$content), simplifyVector = FALSE)
    }, error = function(e) { error <<- e; NULL })
    if (!is.null(answer)) return(answer)
    if (attempt < retries) Sys.sleep(attempt)
  }
  stop(conditionMessage(error), call. = FALSE)
}

query_cell <- function(cell) {
  bbox <- st_bbox(cell)
  offset <- 0L; page_size <- 500L; found <- list()
  repeat {
    params <- list(datasets = "Lidar Point Cloud (LPC)", prodFormats = "LAS,LAZ",
      bbox = paste(unname(bbox[c("xmin", "ymin", "xmax", "ymax")]), collapse = ","),
      max = page_size, offset = offset)
    query <- paste(paste(names(params), vapply(params, curl_escape, character(1)), sep = "="), collapse = "&")
    payload <- fetch_json(paste0(product_api, "?", query))
    if (!is.null(payload$errorMessage)) stop(payload$errorMessage, call. = FALSE)
    items <- payload$items
    if (!length(items)) break
    found <- c(found, items)
    offset <- offset + length(items)
    if (offset >= as.integer(payload$total) || length(items) < page_size) break
  }
  found
}
cell_ids <- seq_len(nrow(cells))
item_pages <- if (.Platform$OS.type == "unix" && query_workers > 1L) {
  mclapply(cell_ids, function(i) query_cell(cells[i, ]), mc.cores = min(query_workers, length(cell_ids)))
} else lapply(cell_ids, function(i) query_cell(cells[i, ]))
items <- unlist(item_pages, recursive = FALSE)
if (!length(items)) {
  message("No published TNMAccess LAZ product intersects the non-EPT area.")
  quit(status = 2L)
}
value <- function(x, name, default = NA) if (is.null(x[[name]]) || !length(x[[name]])) default else x[[name]]
workunit_from_item <- function(item) {
  vendor <- as.character(value(item, "vendorMetaUrl", ""))
  if (nzchar(vendor)) {
    parts <- strsplit(sub("/+$", "", vendor), "/", fixed = TRUE)[[1]]
    if (length(parts) >= 2L) return(parts[length(parts) - 1L])
  }
  url <- as.character(value(item, "downloadURL", ""))
  parts <- strsplit(sub("/+$", "", url), "/", fixed = TRUE)[[1]]
  laz <- match("LAZ", parts)
  if (!is.na(laz) && laz > 1L) parts[laz - 1L] else NA_character_
}
rows <- lapply(items, function(item) {
  bbox <- item$boundingBox
  data.frame(
    source_id = as.character(value(item, "sourceId")), workunit = workunit_from_item(item),
    title = as.character(value(item, "title")), file_name = basename(as.character(value(item, "downloadURL"))),
    source_url = as.character(value(item, "downloadURL")), bytes = as.numeric(value(item, "sizeInBytes", NA_real_)),
    publication_date = as.character(value(item, "publicationDate")),
    xmin_ll = as.numeric(value(bbox, "minX", NA_real_)), ymin_ll = as.numeric(value(bbox, "minY", NA_real_)),
    xmax_ll = as.numeric(value(bbox, "maxX", NA_real_)), ymax_ll = as.numeric(value(bbox, "maxY", NA_real_)),
    stringsAsFactors = FALSE
  )
})
products <- do.call(rbind, rows)
products <- products[!duplicated(products$source_url) & nzchar(products$source_url) &
  is.finite(products$xmin_ll) & is.finite(products$ymin_ll) & is.finite(products$xmax_ll) & is.finite(products$ymax_ll), ]
if (!nrow(products)) stop("TNMAccess returned no usable LAZ product records.", call. = FALSE)
product_geom <- st_sfc(lapply(seq_len(nrow(products)), function(i) {
  xy <- matrix(c(products$xmin_ll[i], products$ymin_ll[i], products$xmax_ll[i], products$ymin_ll[i],
    products$xmax_ll[i], products$ymax_ll[i], products$xmin_ll[i], products$ymax_ll[i],
    products$xmin_ll[i], products$ymin_ll[i]), ncol = 2, byrow = TRUE)
  st_polygon(list(xy))
}), crs = 4326)
products <- st_sf(products, geometry = product_geom)
products <- st_transform(products, st_crs(holes))
products <- products[lengths(st_intersects(products, query_area)) > 0, ]

metadata_for <- function(workunit) {
  where <- paste0("workunit = '", gsub("'", "''", workunit, fixed = TRUE), "'")
  params <- list(f = "json", where = where, returnGeometry = "false",
    outFields = "workunit,project,ql,collect_start,collect_end,horiz_crs,vert_crs,lpc_link")
  query <- paste(paste(names(params), vapply(params, curl_escape, character(1)), sep = "="), collapse = "&")
  for (attempt in seq_len(5L)) {
    payload <- tryCatch(fetch_json(paste0(metadata_api, "?", query)), error = function(e) NULL)
    if (!is.null(payload) && is.null(payload$error) && length(payload$features)) {
      return(as.data.frame(payload$features[[1]]$attributes, stringsAsFactors = FALSE))
    }
    if (attempt < 5L) Sys.sleep(attempt)
  }
  message("USGS metadata lookup remained unavailable for ", workunit, ".")
  NULL
}
workunits <- unique(products$workunit[!is.na(products$workunit)])
metadata <- lapply(workunits, metadata_for)
metadata <- Filter(Negate(is.null), metadata)
metadata <- if (length(metadata)) do.call(rbind, metadata) else data.frame(workunit = character())
missing_workunits <- setdiff(workunits, metadata$workunit)
if (length(missing_workunits) && "metadata-cache" %in% names(opt) && file.exists(opt[["metadata-cache"]])) {
  cached <- read.csv(opt[["metadata-cache"]], stringsAsFactors = FALSE)
  cached <- cached[cached$workunit %in% missing_workunits, ]
  cached <- cached[order(-cached$collect_end), ]
  cached <- cached[!duplicated(cached$workunit), ]
  if (nrow(cached)) metadata <- if (nrow(metadata)) rbind(metadata, cached[, names(metadata), drop = FALSE]) else cached
}
required_metadata <- c("workunit", "project", "ql", "collect_start", "collect_end", "horiz_crs")
if (!nrow(metadata) || !all(required_metadata %in% names(metadata))) {
  stop("No usable QL/date/CRS metadata for TNMAccess work units: ", paste(unique(products$workunit), collapse = ", "), call. = FALSE)
}
products <- merge(products, metadata, by = "workunit", all.x = TRUE, sort = FALSE)
products$quality_level <- toupper(gsub("[^A-Za-z0-9]", "", products$ql))
products <- products[products$quality_level %in% c("QL1", "QL2") & !is.na(products$horiz_crs), ]
if (!nrow(products)) stop("TNMAccess products lack supported QL1/QL2 work-unit metadata.", call. = FALSE)
epoch_date <- function(x) {
  out <- rep(NA_character_, length(x)); ok <- is.finite(as.numeric(x))
  out[ok] <- format(as.POSIXct(as.numeric(x[ok]) / 1000, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d")
  out
}
products$acquisition_start <- epoch_date(products$collect_start)
products$acquisition_end <- epoch_date(products$collect_end)
products$acquisition_year <- suppressWarnings(as.integer(substr(products$acquisition_end, 1, 4)))

local_map <- setNames(character(), character())
if (all(c("local-index", "local-layer") %in% names(opt))) {
  local <- st_read(opt[["local-index"]], layer = opt[["local-layer"]], quiet = TRUE)
  path_field <- if ("location" %in% names(local)) "location" else names(st_drop_geometry(local))[1]
  local_paths <- as.character(local[[path_field]])
  keep <- !duplicated(basename(local_paths))
  local_map <- setNames(local_paths[keep], basename(local_paths)[keep])
}
products$local_path <- rep(NA_character_, nrow(products))
if (length(local_map)) products$local_path <- unname(local_map[products$file_name])

# Assign work-unit coverage by newest acquisition, with QL1 as tie-breaker.
source_dates <- aggregate(as.numeric(products$collect_end), list(workunit = products$workunit), max, na.rm = TRUE)
names(source_dates)[2] <- "collect_end"
source_ql <- aggregate(products$quality_level, list(workunit = products$workunit), function(x) sort(unique(x))[1])
names(source_ql)[2] <- "quality_level"
priority <- merge(source_dates, source_ql, by = "workunit")
priority <- priority[order(-priority$collect_end, priority$quality_level != "QL1"), ]
remaining <- st_geometry(holes)
coverage_rows <- list()
for (workunit in priority$workunit) {
  survey <- st_union(st_geometry(products[products$workunit == workunit, ]))
  piece <- suppressWarnings(st_intersection(remaining, survey))
  if (!length(piece) || sum(as.numeric(st_area(st_as_sf(piece)))) < 1) next
  first <- products[products$workunit == workunit, ][1, ]
  coverage_rows[[length(coverage_rows) + 1L]] <- st_sf(
    workunit = workunit, project = first$project, quality_level = first$quality_level,
    acquisition_start = first$acquisition_start, acquisition_end = first$acquisition_end,
    horiz_crs = as.character(first$horiz_crs), geometry = st_union(piece))
  remaining <- suppressWarnings(st_difference(remaining, st_union(piece)))
  if (!length(remaining)) break
}
if (!length(coverage_rows)) quit(status = 2L)
coverage <- do.call(rbind, coverage_rows)
# Persist only repaired coverage: union/difference can leave duplicate vertices
# at coincident tile boundaries even though the result is topologically valid.
coverage <- st_make_valid(coverage)
coverage <- st_set_geometry(coverage, st_buffer(st_geometry(coverage), 0))
coverage <- coverage[!st_is_empty(coverage), ]

# Retain complete tile footprints for any tile needed by the assigned coverage
# plus processing halo. Core assignment remains in direct_laz_coverage.
selected <- unique(unlist(lapply(seq_len(nrow(coverage)), function(i) {
  area <- st_buffer(coverage[i, ], buffer_m)
  which(products$workunit == coverage$workunit[i] & lengths(st_intersects(products, area)) > 0)
})))
tiles <- products[selected, ]
plan <- st_drop_geometry(tiles)
plan$estimated_gib <- plan$bytes / 1024^3
keep_columns <- c("source_id", "workunit", "project", "quality_level", "acquisition_start", "acquisition_end",
  "acquisition_year", "horiz_crs", "file_name", "source_url", "bytes", "estimated_gib", "local_path",
  "publication_date", "xmin_ll", "ymin_ll", "xmax_ll", "ymax_ll")
missing_columns <- setdiff(keep_columns, names(plan))
if (length(missing_columns)) stop("Internal direct-LAZ plan is missing columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
plan <- plan[, keep_columns]
write.csv(plan, opt[["output"]], row.names = FALSE)
tiles <- tiles[, intersect(keep_columns, names(tiles))]
st_write(tiles, opt[["footprints-output"]], layer = "direct_laz_tiles", quiet = TRUE)
st_write(coverage, opt[["footprints-output"]], layer = "direct_laz_coverage", quiet = TRUE, append = TRUE)
summary <- aggregate(cbind(bytes, estimated_gib) ~ workunit + project + quality_level + acquisition_year, plan, sum)
write.csv(summary, paste0(tools::file_path_sans_ext(opt[["output"]]), "_summary.csv"), row.names = FALSE)
message("Wrote ", nrow(plan), " required TNMAccess LAZ tiles (", round(sum(plan$estimated_gib), 2), " GiB): ", opt[["output"]])
message("Wrote tile and assigned-coverage layers: ", opt[["footprints-output"]])
