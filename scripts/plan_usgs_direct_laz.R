#!/usr/bin/env Rscript

# Build a direct-LAZ download plan for the portion of an AOI not available as
# USGS EPT.  It reads delivery inventories and only requests the first 375
# bytes of a candidate LAZ, which contains its extent and total byte size.
suppressPackageStartupMessages({ library(sf); library(curl); library(jsonlite) })

service <- "https://index.nationalmap.gov/arcgis/rest/services/3DEPElevationIndex/MapServer/8/query"
usage <- paste(
  "Usage: Rscript scripts/plan_usgs_direct_laz.R --holes HOLES.gpkg --layer NAME --output PLAN.csv",
  "[--local-index TILES.shp --local-layer NAME] [--header-workers 16] [--quality-levels QL1,QL2]",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opt <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
if (!all(c("holes", "layer", "output") %in% names(opt))) stop(usage, call. = FALSE)
if (file.exists(opt[["output"]])) stop("Refusing to overwrite: ", opt[["output"]], call. = FALSE)
workers <- as.integer(if ("header-workers" %in% names(opt)) opt[["header-workers"]] else 16L)
if (!is.finite(workers) || workers < 1L) stop("--header-workers must be positive.", call. = FALSE)
keep_ql <- toupper(gsub("[^A-Za-z0-9]", "", strsplit(if ("quality-levels" %in% names(opt)) opt[["quality-levels"]] else "QL1,QL2", ",")[[1]]))

holes <- st_read(opt[["holes"]], layer = opt[["layer"]], quiet = TRUE)
if (!nrow(holes) || is.na(st_crs(holes))) stop("Holes must be non-empty and have a CRS.", call. = FALSE)
holes <- st_as_sf(st_union(st_make_valid(holes)))

fetch_index <- function(params) {
  query <- paste(paste(names(params), vapply(params, curl_escape, character(1)), sep = "="), collapse = "&")
  response <- curl_fetch_memory(paste0(service, "?", query))
  if (response$status_code != 200L) stop("USGS index returned HTTP ", response$status_code, call. = FALSE)
  answer <- fromJSON(rawToChar(response$content), simplifyDataFrame = FALSE)
  if (!is.null(answer$error)) stop("USGS index error: ", answer$error$message, call. = FALSE)
  answer
}
bbox <- st_bbox(st_transform(holes, 4326))
envelope <- paste(unname(bbox[c("xmin", "ymin", "xmax", "ymax")]), collapse = ",")
ids <- fetch_index(list(f = "json", where = "1=1", geometry = envelope, geometryType = "esriGeometryEnvelope", inSR = "4326", spatialRel = "esriSpatialRelIntersects", returnIdsOnly = "true"))$objectIds
if (!length(ids)) stop("No USGS LPC work units intersect the hole extent.", call. = FALSE)
fields <- c("workunit", "workunit_id", "project", "ql", "collect_start", "collect_end", "lpc_link", "horiz_crs")
features <- unlist(lapply(split(ids, ceiling(seq_along(ids) / 500L)), function(batch) {
  fetch_index(list(f = "json", objectIds = paste(batch, collapse = ","), outSR = "4326", returnGeometry = "true", outFields = paste(fields, collapse = ",")))$features
}), recursive = FALSE)
field <- function(x, n) if (is.null(x[[n]])) NA else x[[n]]
as_poly <- function(feature) {
  rings <- feature$geometry$rings
  ring_list <- if (is.array(rings) && length(dim(rings)) == 3L) lapply(seq_len(dim(rings)[1]), function(i) matrix(rings[i, , ], ncol = 2L)) else if (is.matrix(rings)) list(rings) else rings
  st_multipolygon(lapply(ring_list, function(r) list(if (is.matrix(r)) r else matrix(unlist(r), ncol = 2L, byrow = TRUE))))
}
attrs <- lapply(features, function(f) data.frame(workunit = as.character(field(f$attributes, "workunit")), project = as.character(field(f$attributes, "project")), ql = as.character(field(f$attributes, "ql")), collect_end = as.numeric(field(f$attributes, "collect_end")), lpc_link = as.character(field(f$attributes, "lpc_link")), horiz_crs = as.character(field(f$attributes, "horiz_crs")), stringsAsFactors = FALSE))
wu <- st_sf(do.call(rbind, attrs), geometry = st_sfc(lapply(features, as_poly), crs = 4326))
wu <- st_make_valid(st_transform(wu, st_crs(holes)))
wu$quality_level <- toupper(gsub("[^A-Za-z0-9]", "", wu$ql))
wu <- wu[wu$quality_level %in% keep_ql & !is.na(wu$lpc_link) & nzchar(wu$lpc_link), ]
wu <- wu[lengths(st_intersects(wu, holes)) > 0, ]
if (!nrow(wu)) stop("No supported work units intersect the holes.", call. = FALSE)

# Work-unit delivery grids very rarely overlap. Keep each exact intersection
# instead of repeatedly subtracting complex statewide polygons; this makes the
# planning pass tractable. The final CSV retains workunit IDs for any later
# source-priority review.
pieces <- suppressWarnings(st_intersection(wu, holes))
pieces <- pieces[as.numeric(st_area(pieces)) > 0, ]
if (!nrow(pieces)) stop("No supported work-unit area covers the holes.", call. = FALSE)
message("Selected ", nrow(pieces), " work-unit coverage pieces for direct-LAZ planning.")

local_names <- character()
if (all(c("local-index", "local-layer") %in% names(opt))) {
  local <- st_read(opt[["local-index"]], layer = opt[["local-layer"]], quiet = TRUE)
  path_field <- if ("location" %in% names(local)) "location" else names(st_drop_geometry(local))[1]
  local_names <- basename(local[[path_field]])
}
read_inventory <- function(link) {
  u <- paste0(sub("/$", "", link), "/0_file_download_links.txt")
  tryCatch(strsplit(rawToChar(curl_fetch_memory(u)$content), "[\\r\\n]+")[[1]], error = function(e) character())
}
inventories <- lapply(pieces$lpc_link, read_inventory)
pieces$inventory_urls <- vapply(inventories, length, integer(1))
if (!any(pieces$inventory_urls)) stop("No readable USGS delivery inventories for selected work units.", call. = FALSE)
inventory_status_path <- paste0(tools::file_path_sans_ext(opt[["output"]]), "_delivery_inventory_status.csv")
write.csv(st_drop_geometry(pieces[, c("workunit", "project", "quality_level", "lpc_link", "inventory_urls")]), inventory_status_path, row.names = FALSE)
if (any(!pieces$inventory_urls)) {
  message("No readable delivery inventory for: ", paste(pieces$workunit[!pieces$inventory_urls], collapse = ", "))
}
job_rows <- lapply(seq_len(nrow(pieces)), function(i) {
  urls <- inventories[[i]]; urls <- urls[grepl("\\.la[sz]$", urls, ignore.case = TRUE)]
  if (!length(urls)) return(NULL)
  data.frame(source_id = i, workunit = pieces$workunit[i], project = pieces$project[i], quality_level = pieces$quality_level[i], horiz_crs = pieces$horiz_crs[i], url = urls, stringsAsFactors = FALSE)
})
job_rows <- Filter(Negate(is.null), job_rows)
if (!length(job_rows)) stop("Selected work units had no readable LAZ delivery links.", call. = FALSE)
jobs <- do.call(rbind, job_rows)
jobs$file_name <- basename(jobs$url)
jobs <- jobs[!jobs$file_name %in% local_names, ]
if (!nrow(jobs)) stop("All delivery tiles are already available locally.", call. = FALSE)
message("Reading bounded headers for ", nrow(jobs), " candidate remote tiles (", workers, " workers).")

las_header <- function(url) tryCatch({
  r <- curl_fetch_memory(url, handle = new_handle(range = "0-374", failonerror = TRUE))
  h <- rawToChar(r$headers); total <- sub(".*[Cc]ontent-[Rr]ange: bytes [0-9]+-[0-9]+/([0-9]+).*", "\\1", h)
  con <- rawConnection(r$content); on.exit(close(con)); get <- function(pos, what) { seek(con, pos); readBin(con, what, n = 1L, size = 8L, endian = "little") }
  c(bytes = as.numeric(total), xmin = get(187, "double"), xmax = get(179, "double"), ymin = get(203, "double"), ymax = get(195, "double"))
}, error = function(e) c(bytes = NA_real_, xmin = NA_real_, xmax = NA_real_, ymin = NA_real_, ymax = NA_real_))
headers <- if (.Platform$OS.type == "unix" && workers > 1L) parallel::mclapply(jobs$url, las_header, mc.cores = workers) else lapply(jobs$url, las_header)
headers <- as.data.frame(do.call(rbind, headers))
jobs <- cbind(jobs, headers)
jobs <- jobs[is.finite(jobs$xmin) & jobs$xmax > jobs$xmin & jobs$ymax > jobs$ymin, ]
message("Parsed ", nrow(jobs), " readable LAZ headers; intersecting assigned source pieces.")

# Header bounds are in each work unit's horizontal CRS. Retain only tiles that
# intersect that work unit's assigned, non-EPT piece.
selected <- lapply(split(seq_len(nrow(jobs)), jobs$source_id), function(ii) {
  source <- as.integer(jobs$source_id[ii[1]])
  crs <- suppressWarnings(st_crs(as.integer(jobs$horiz_crs[ii[1]])))
  if (is.na(crs)) return(integer())
  geom <- st_sfc(lapply(ii, function(k) {
    coords <- matrix(c(jobs$xmin[k], jobs$ymin[k], jobs$xmax[k], jobs$ymin[k],
      jobs$xmax[k], jobs$ymax[k], jobs$xmin[k], jobs$ymax[k],
      jobs$xmin[k], jobs$ymin[k]), ncol = 2, byrow = TRUE)
    st_polygon(list(coords))
  }), crs = crs)
  tiles <- st_sf(job_row = ii, geometry = geom)
  target <- st_transform(pieces[source, ], crs)
  ii[lengths(st_intersects(tiles, target)) > 0]
})
selected <- unlist(selected, use.names = FALSE)
plan <- jobs[selected, ]
plan$estimated_gib <- plan$bytes / 1024^3
plan$source_url <- plan$url
plan$url <- NULL
write.csv(plan, opt[["output"]], row.names = FALSE)
summary <- aggregate(cbind(bytes, estimated_gib) ~ workunit + project + quality_level, plan, sum)
summary_path <- paste0(tools::file_path_sans_ext(opt[["output"]]), "_summary.csv")
write.csv(summary, summary_path, row.names = FALSE)
message("Wrote ", nrow(plan), " required direct-LAZ tiles: ", opt[["output"]])
message("Estimated download: ", round(sum(plan$estimated_gib), 1), " GiB; summary: ", summary_path)
message("Wrote delivery-inventory status: ", inventory_status_path)
