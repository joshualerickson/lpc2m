.epoch_date <- function(x) {
  out <- as.Date(rep(NA_character_, length(x)))
  ok <- is.finite(x)
  out[ok] <- as.Date(as.POSIXct(x[ok] / 1000, origin = "1970-01-01", tz = "UTC"))
  out
}

.read_source_metadata <- function(project_dir) {
  path <- file.path(project_dir, "data", "lidar_need_not_ept_usgs_candidates.csv")
  if (!file.exists(path)) {
    return(data.frame(workunit = character(), quality_level = character(), collect_start = numeric(), collect_end = numeric()))
  }
  x <- utils::read.csv(path, stringsAsFactors = FALSE)
  x$quality_level <- toupper(gsub("[^A-Za-z0-9]", "", x$ql))
  x <- x[x$quality_level %in% c("QL1", "QL2"), ]
  x <- x[order(-x$collect_end, x$quality_level != "QL1"), ]
  x[!duplicated(x$workunit), ]
}

.write_sf_replace <- function(x, path, layer) {
  if (file.exists(path)) unlink(path)
  if (nrow(x)) sf::st_write(x, path, layer = layer, quiet = TRUE)
  invisible(path)
}

.empty_source_inventory <- function() {
  data.frame(
    source_type = character(), source_name = character(), source_url = character(),
    local_path = character(), quality_level = character(), acquisition_start = character(),
    acquisition_end = character(), acquisition_year = integer(), estimated_bytes = numeric(),
    availability = character(), stringsAsFactors = FALSE
  )
}

.run_direct_laz_planner <- function(holes, provenance_dir, name, project_dir,
                                    local_laz_index = NULL, local_laz_layer = NULL,
                                    header_workers = 16L, buffer_m = 60, refresh = FALSE) {
  holes_path <- file.path(provenance_dir, paste0(name, "_direct_laz_needed.gpkg"))
  plan_path <- file.path(provenance_dir, paste0(name, "_direct_laz_plan.csv"))
  tiles_path <- file.path(provenance_dir, paste0(name, "_direct_laz_tiles.gpkg"))
  .write_sf_replace(holes, holes_path, "direct_laz_needed")
  if (refresh) {
    for (path in c(plan_path, tiles_path,
                   paste0(tools::file_path_sans_ext(plan_path), "_summary.csv"),
                   paste0(tools::file_path_sans_ext(plan_path), "_delivery_inventory_status.csv"))) {
      if (file.exists(path)) unlink(path)
    }
  }
  if (!file.exists(plan_path) || !file.exists(tiles_path)) {
    script <- file.path(project_dir, "scripts", "plan_tnm_direct_laz.R")
    if (!file.exists(script)) stop("project_dir must contain scripts/plan_tnm_direct_laz.R.", call. = FALSE)
    args <- c(script, "--holes", holes_path, "--layer", "direct_laz_needed",
              "--output", plan_path, "--footprints-output", tiles_path,
              "--buffer-m", buffer_m, "--query-workers", as.integer(header_workers))
    metadata_cache <- file.path(project_dir, "data", "lidar_need_not_ept_usgs_candidates.csv")
    if (file.exists(metadata_cache)) args <- c(args, "--metadata-cache", metadata_cache)
    if (xor(is.null(local_laz_index), is.null(local_laz_layer))) stop("Supply local_laz_index and local_laz_layer together.", call. = FALSE)
    if (!is.null(local_laz_index)) args <- c(args, "--local-index", local_laz_index, "--local-layer", local_laz_layer)
    status <- system2("Rscript", shQuote(args))
    if (status == 2L) return(list(status = "no_published_lidar", holes = holes_path, plan = NULL, tiles = NULL))
    if (status != 0L) {
      return(list(status = "planning_failed", holes = holes_path, plan = NULL, tiles = NULL))
    }
  }
  list(status = "planned", holes = holes_path, plan = plan_path, tiles = tiles_path)
}

.resolve_live_ept_sources <- function(aoi, provenance_dir, name, project_dir) {
  aoi_path <- file.path(provenance_dir, paste0(name, "_live_ept_aoi.gpkg"))
  sources_path <- file.path(provenance_dir, paste0(name, "_live_ept_sources.gpkg"))
  .write_sf_replace(aoi, aoi_path, "aoi")
  if (file.exists(sources_path)) unlink(sources_path)
  script <- file.path(project_dir, "scripts", "resolve_live_ept_sources.R")
  if (!file.exists(script)) stop("project_dir must contain scripts/resolve_live_ept_sources.R.", call. = FALSE)
  status <- system2("Rscript", shQuote(c(script, "--aoi", aoi_path, "--layer", "aoi", "--output", sources_path)))
  if (status == 0L && file.exists(sources_path)) return(list(status = "resolved", path = sources_path))
  list(status = "unavailable", path = NULL)
}
