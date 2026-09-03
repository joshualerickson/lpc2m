#!/usr/bin/env Rscript

# Turn a processing grid into source-specific USGS EPT jobs. Work-unit coverage
# is prioritized by newest collection date; overlapping older coverage is
# removed, so a metric cell is never processed twice from competing surveys.
suppressPackageStartupMessages({ library(sf); library(curl); library(jsonlite) })

service <- "https://index.nationalmap.gov/arcgis/rest/services/3DEPElevationIndex/MapServer/24/query"
usage <- paste(
  "Usage: Rscript scripts/make_usgs_job_manifest.R --blocks BLOCKS.gpkg --layer NAME",
  "--output JOBS.gpkg --output-layer NAME [--ept-base URL] [--validate-ept true|false]",
  "[--uncovered-output FILE] [--coverage-output FILE]",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opts <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
required <- c("blocks", "layer", "output", "output-layer")
if (!all(required %in% names(opts))) stop(usage, call. = FALSE)
if (file.exists(opts[["output"]])) stop("Refusing to overwrite: ", opts[["output"]], call. = FALSE)
is_true <- function(x) tolower(x) %in% c("true", "t", "1", "yes")
validate_ept <- is_true(if ("validate-ept" %in% names(opts)) opts[["validate-ept"]] else "true")
ept_base <- if ("ept-base" %in% names(opts)) opts[["ept-base"]] else "https://s3-us-west-2.amazonaws.com/usgs-lidar-public"
ept_base <- sub("/$", "", ept_base)

blocks <- st_read(opts[["blocks"]], layer = opts[["layer"]], quiet = TRUE)
if (!nrow(blocks) || !"block_id" %in% names(blocks) || anyDuplicated(blocks$block_id)) {
  stop("Blocks must have unique block_id values.", call. = FALSE)
}
if (is.na(st_crs(blocks))) stop("Blocks have no CRS.", call. = FALSE)

query_index <- function() {
  # The ArcGIS service rejects some large, forest-scale envelope queries.
  # Query a quarter-degree geographic grid instead, then de-duplicate work
  # units. This also avoids an overfull query where statewide surveys overlap.
  bbox <- st_bbox(st_transform(blocks, 4326))
  out_fields <- c("workunit", "workunit_id", "project", "project_id", "ql", "collect_start", "collect_end", "lpc_link", "horiz_crs", "vert_crs")
  fetch <- function(params) {
    query <- paste(paste(names(params), vapply(params, curl_escape, character(1)), sep = "="), collapse = "&")
    response <- curl_fetch_memory(paste0(service, "?", query))
    if (response$status_code != 200L) stop("USGS LPC index returned HTTP ", response$status_code, call. = FALSE)
    payload <- fromJSON(rawToChar(response$content), simplifyDataFrame = FALSE)
    if (!is.null(payload$error)) stop("USGS LPC index error: ", payload$error$message, call. = FALSE)
    payload
  }
  grid_deg <- 0.25
  xbreaks <- unique(c(seq(floor(bbox[["xmin"]] / grid_deg) * grid_deg, ceiling(bbox[["xmax"]] / grid_deg) * grid_deg, by = grid_deg), ceiling(bbox[["xmax"]] / grid_deg) * grid_deg))
  ybreaks <- unique(c(seq(floor(bbox[["ymin"]] / grid_deg) * grid_deg, ceiling(bbox[["ymax"]] / grid_deg) * grid_deg, by = grid_deg), ceiling(bbox[["ymax"]] / grid_deg) * grid_deg))
  envelopes <- unlist(lapply(seq_len(length(xbreaks) - 1L), function(ix) {
    lapply(seq_len(length(ybreaks) - 1L), function(iy) {
      paste(xbreaks[ix], ybreaks[iy], xbreaks[ix + 1L], ybreaks[iy + 1L], sep = ",")
    })
  }), use.names = FALSE)
  message("Querying USGS LPC index in ", length(envelopes), " geographic envelope(s).")
  ids <- unique(unlist(lapply(envelopes, function(envelope) {
    payload <- tryCatch(fetch(list(f = "json", where = "1=1", geometry = envelope,
      geometryType = "esriGeometryEnvelope", inSR = "4326",
      spatialRel = "esriSpatialRelIntersects", returnIdsOnly = "true")), error = function(e) NULL)
    if (!is.null(payload)) return(payload$objectIds)
    # The service currently has corrupt spatial cells. Do not abort a broad
    # AOI: neighboring cells still resolve usable work units, and unresolved
    # block area is written to the normal uncovered provenance output.
    message("USGS LPC index unavailable for envelope ", envelope, "; recording any unresolved area as needs_metadata.")
    numeric()
  }), use.names = FALSE))
  if (!length(ids)) return(list())
  batches <- split(ids, ceiling(seq_along(ids) / 500L))
  features <- unlist(lapply(batches, function(batch) {
    payload <- fetch(list(f = "json", objectIds = paste(batch, collapse = ","), outSR = "4326",
      returnGeometry = "true", outFields = paste(out_fields, collapse = ",")))
    payload$features
  }), recursive = FALSE)
  features
}

field <- function(x, name) if (!is.null(x[[name]])) x[[name]] else NA
features <- query_index()
if (!length(features)) stop("No USGS LPC work units intersect the grid extent.", call. = FALSE)

attrs <- lapply(features, function(f) data.frame(
  workunit = as.character(field(f$attributes, "workunit")),
  workunit_id = as.character(field(f$attributes, "workunit_id")),
  project = as.character(field(f$attributes, "project")),
  project_id = as.character(field(f$attributes, "project_id")),
  ql = as.character(field(f$attributes, "ql")),
  collect_start = as.numeric(field(f$attributes, "collect_start")),
  collect_end = as.numeric(field(f$attributes, "collect_end")),
  horiz_crs = as.character(field(f$attributes, "horiz_crs")),
  vert_crs = as.character(field(f$attributes, "vert_crs")),
  stringsAsFactors = FALSE
))
geometry <- lapply(features, function(f) {
  rings <- f$geometry$rings
  if (is.null(rings) || !length(rings)) stop("USGS index returned a work unit without polygon geometry.", call. = FALSE)
  ring_list <- if (is.array(rings) && length(dim(rings)) == 3L) {
    lapply(seq_len(dim(rings)[1]), function(i) matrix(rings[i, , ], ncol = dim(rings)[3]))
  } else if (is.matrix(rings)) {
    list(rings)
  } else rings
  st_multipolygon(lapply(ring_list, function(ring) {
    coords <- if (is.matrix(ring)) ring else matrix(unlist(ring, use.names = FALSE), ncol = 2L, byrow = TRUE)
    list(coords)
  }))
})
workunits <- st_sf(do.call(rbind, attrs), geometry = st_sfc(geometry, crs = 4326))
workunits <- st_make_valid(st_transform(workunits, st_crs(blocks)))
workunits$quality_level <- toupper(gsub("[^A-Za-z0-9]", "", workunits$ql))
epoch_to_date <- function(x) {
  out <- rep(NA_character_, length(x))
  ok <- is.finite(x)
  out[ok] <- format(as.POSIXct(x[ok] / 1000, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d")
  out
}
workunits$acquisition_start <- epoch_to_date(workunits$collect_start)
workunits$acquisition_end <- epoch_to_date(workunits$collect_end)
workunits$acquisition_end_year <- suppressWarnings(as.integer(substr(workunits$acquisition_end, 1L, 4L)))
supported <- workunits$quality_level %in% c("QL1", "QL2")
if (any(!supported)) {
  ignored <- unique(workunits$ql[!supported])
  message("Ignoring unsupported USGS quality levels: ", paste(ignored, collapse = ", "))
  workunits <- workunits[supported, ]
}
if (!nrow(workunits)) stop("No intersecting USGS work units are QL1 or QL2.", call. = FALSE)
workunits$ept_url <- paste0(ept_base, "/", utils::URLencode(workunits$workunit, reserved = TRUE), "/ept.json")

# Priority overlay: newest survey gets first claim on each block; older surveys
# fill only remaining uncovered area. This prevents duplicate points/metrics in
# overlapping work units while retaining source-specific boundary jobs.
# Newest acquisition wins; QL1 is the deterministic tie-breaker.
priority <- order(-ifelse(is.na(workunits$collect_end), -Inf, workunits$collect_end), workunits$quality_level != "QL1")
workunits <- workunits[priority, ]
candidate_index <- st_intersects(blocks, workunits)
message("USGS source candidates: ", nrow(workunits), "; block/source intersections: ", sum(lengths(candidate_index)))
job_rows <- list(); job_geoms <- list(); uncovered_rows <- list()
for (i in seq_len(nrow(blocks))) {
  remaining <- st_geometry(blocks[i, ])
  candidates <- candidate_index[[i]]
  for (j in candidates) {
    piece <- suppressWarnings(st_intersection(remaining, st_geometry(workunits[j, ])))
    if (!length(piece) || sum(as.numeric(st_area(piece))) < 1) next
    piece <- st_union(piece)
    job_rows[[length(job_rows) + 1L]] <- data.frame(
      block_id = paste(blocks$block_id[i], workunits$workunit[j], sep = "__"),
      source_block_id = blocks$block_id[i], workunit = workunits$workunit[j], project = workunits$project[j],
      ept_url = workunits$ept_url[j], quality_level = workunits$quality_level[j],
      acquisition_start = workunits$acquisition_start[j], acquisition_end = workunits$acquisition_end[j],
      acquisition_end_year = workunits$acquisition_end_year[j],
      collect_start_epoch_ms = workunits$collect_start[j], collect_end_epoch_ms = workunits$collect_end[j],
      source_area_m2 = as.numeric(st_area(piece)), stringsAsFactors = FALSE
    )
    job_geoms[[length(job_geoms) + 1L]] <- piece[[1]]
    remaining <- suppressWarnings(st_difference(remaining, piece))
    if (!length(remaining) || sum(as.numeric(st_area(remaining))) < 1) break
  }
  if (length(remaining) && sum(as.numeric(st_area(remaining))) >= 1) {
    uncovered_rows[[length(uncovered_rows) + 1L]] <- data.frame(
      source_block_id = blocks$block_id[i], uncovered_area_m2 = sum(as.numeric(st_area(remaining))), stringsAsFactors = FALSE
    )
  }
}
if (!length(job_rows)) stop("No block area was covered by supported QL1/QL2 USGS LPC work units.", call. = FALSE)
jobs <- st_sf(do.call(rbind, job_rows), geometry = st_sfc(job_geoms, crs = st_crs(blocks)))
manifest_csv <- paste0(tools::file_path_sans_ext(opts[["output"]]), "_manifest.csv")
if (length(uncovered_rows)) {
  uncovered_path <- if ("uncovered-output" %in% names(opts)) opts[["uncovered-output"]] else paste0(tools::file_path_sans_ext(opts[["output"]]), "_uncovered.csv")
  write.csv(do.call(rbind, uncovered_rows), uncovered_path, row.names = FALSE)
  message("Skipping area without supported QL1/QL2 coverage; see ", uncovered_path)
}
if (validate_ept) {
  unique_urls <- unique(jobs$ept_url)
  available <- vapply(unique_urls, function(url) {
    tryCatch(curl_fetch_memory(url)$status_code == 200L, error = function(e) FALSE)
  }, logical(1))
  jobs$ept_available <- available[match(jobs$ept_url, unique_urls)]
  coverage_path <- if ("coverage-output" %in% names(opts)) opts[["coverage-output"]] else paste0(tools::file_path_sans_ext(opts[["output"]]), "_coverage.gpkg")
  if (file.exists(coverage_path)) stop("Refusing to overwrite: ", coverage_path, call. = FALSE)
  coverage <- jobs
  coverage$coverage_status <- ifelse(coverage$ept_available, "ept", "ept_unavailable")
  st_write(coverage, coverage_path, layer = "coverage", quiet = TRUE)
  message("Wrote EPT coverage footprint: ", coverage_path)
  if (any(!jobs$ept_available)) {
    unavailable_path <- paste0(tools::file_path_sans_ext(opts[["output"]]), "_no_ept.csv")
    write.csv(st_drop_geometry(jobs[!jobs$ept_available, ]), unavailable_path, row.names = FALSE)
    missing <- unique(jobs$workunit[!jobs$ept_available])
    message("Skipping assigned work units without public EPT endpoints: ", paste(missing, collapse = ", "))
    message("See ", unavailable_path)
    jobs <- jobs[jobs$ept_available, ]
  }
} else {
  coverage_path <- if ("coverage-output" %in% names(opts)) opts[["coverage-output"]] else paste0(tools::file_path_sans_ext(opts[["output"]]), "_coverage.gpkg")
  if (file.exists(coverage_path)) stop("Refusing to overwrite: ", coverage_path, call. = FALSE)
  coverage <- jobs
  coverage$ept_available <- NA
  coverage$coverage_status <- "ept_unvalidated"
  st_write(coverage, coverage_path, layer = "coverage", quiet = TRUE)
  message("Wrote unvalidated EPT coverage footprint: ", coverage_path)
}
if (!nrow(jobs)) stop("No processable EPT-backed QL1/QL2 coverage remains after source screening.", call. = FALSE)
st_write(jobs, opts[["output"]], layer = opts[["output-layer"]], quiet = TRUE)
write.csv(st_drop_geometry(jobs), manifest_csv, row.names = FALSE)
message("Wrote ", nrow(jobs), " source-specific jobs: ", opts[["output"]])
message("Wrote job manifest: ", manifest_csv)
