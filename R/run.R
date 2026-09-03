#' Run the AOI-to-metrics workflow interactively
#'
#' This is the stable R interface. During the migration it delegates execution
#' to the proven script engine in `project_dir`; its arguments are intentionally
#' the same concepts as the command-line flags.
#'
#' @param aoi Polygonal `sf` AOI.
#' @param name Unique run name.
#' @param delivery_template Raster defining the delivery grid.
#' @param normalized_dir,metrics_dir Output directories.
#' @param options A `lidar_options()` list.
#' @param project_dir Repository root containing `scripts/`.
#' @param ept_sources,ept_source_layer,ept_profiles Optional direct-EPT adapter
#'   inputs; supply all three to bypass the National Map index.
#' @param direct_laz_plan,direct_laz_tiles,direct_laz_cache Optional direct-LAZ
#'   acquisition inputs produced by `run_provenance()`; supply all three.
#' @param pdal_bin Optional PDAL executable path.
#' @return Invisibly, a list of run paths.
#' @export
run_lidar_metrics <- function(aoi, name, delivery_template, normalized_dir, metrics_dir,
                              options = lidar_options(), project_dir = getwd(),
                              ept_sources = NULL, ept_source_layer = NULL, ept_profiles = NULL,
                              direct_laz_plan = NULL, direct_laz_tiles = NULL, direct_laz_cache = NULL,
                              pdal_bin = Sys.getenv("PDAL_BIN", unset = Sys.which("pdal"))) {
  aoi <- validate_aoi(aoi)
  if (!is.list(options) || !all(c("block_m", "buffer_m", "families", "stream_workers", "normalize_workers", "metric_workers", "write_normalized", "resume") %in% names(options))) stop("options must come from lidar_options().", call. = FALSE)
  runner <- file.path(project_dir, "scripts", "run_aoi_production.R")
  if (!file.exists(runner)) stop("project_dir must contain scripts/run_aoi_production.R.", call. = FALSE)
  if (!nzchar(pdal_bin) || !file.exists(pdal_bin)) stop("pdal_bin must name an executable PDAL installation.", call. = FALSE)
  if (!dir.exists(metrics_dir)) dir.create(metrics_dir, recursive = TRUE)
  provenance <- file.path(metrics_dir, "provenance")
  dir.create(provenance, recursive = TRUE, showWarnings = FALSE)
  aoi_path <- file.path(provenance, paste0(name, "_input_aoi.gpkg"))
  if (!file.exists(aoi_path)) sf::st_write(aoi, aoi_path, layer = "aoi", quiet = TRUE)
  args <- c(runner, "--aoi", aoi_path, "--layer", "aoi", "--name", name,
    "--delivery-template", delivery_template, "--normalized-dir", normalized_dir, "--metrics-dir", metrics_dir,
    "--block-m", options$block_m, "--buffer-m", options$buffer_m,
    "--families", paste(options$families, collapse = ","),
    "--download-workers", if (!is.null(options$download_workers)) options$download_workers else options$stream_workers,
    "--stream-workers", options$stream_workers, "--normalize-workers", options$normalize_workers,
    "--metric-workers", options$metric_workers, "--write-normalized", tolower(options$write_normalized),
    "--resume", tolower(options$resume), "--pdal-bin", pdal_bin)
  direct <- !is.null(ept_sources) || !is.null(ept_source_layer) || !is.null(ept_profiles)
  if (direct && (is.null(ept_sources) || is.null(ept_source_layer) || is.null(ept_profiles))) stop("Supply ept_sources, ept_source_layer, and ept_profiles together.", call. = FALSE)
  if (direct) args <- c(args, "--ept-sources", ept_sources, "--ept-source-layer", ept_source_layer, "--ept-profiles", ept_profiles)
  direct_laz <- !is.null(direct_laz_plan) || !is.null(direct_laz_tiles) || !is.null(direct_laz_cache)
  if (direct_laz && (is.null(direct_laz_plan) || is.null(direct_laz_tiles) || is.null(direct_laz_cache))) stop("Supply direct_laz_plan, direct_laz_tiles, and direct_laz_cache together.", call. = FALSE)
  if (direct_laz) args <- c(args, "--direct-laz-plan", direct_laz_plan, "--direct-laz-tiles", direct_laz_tiles, "--direct-laz-cache-dir", direct_laz_cache)
  status <- system2("Rscript", shQuote(args))
  if (status != 0L) stop("LiDAR workflow failed; inspect the run provenance and work logs.", call. = FALSE)
  invisible(list(aoi = aoi_path, normalized_dir = normalized_dir, metrics_dir = metrics_dir, provenance_dir = provenance))
}

#' Resolve and process LiDAR metrics for an AOI
#'
#' High-level package entry point. Source discovery, QL selection, processing
#' blocks, resume behavior, and partial-coverage provenance are internal.
#'
#' @param aoi Polygonal `sf` AOI.
#' @param output_dir Root directory for metrics and provenance.
#' @param delivery_template Raster defining the requested delivery grid.
#' @param name Stable run name.
#' @param options A `lidar_options()` object.
#' @param normalized_dir Optional normalized-LAZ directory.
#' @param project_dir Repository root while the execution engine is migrated.
#' @param pdal_bin PDAL executable.
#' @param dry_run If true, resolve coverage and provenance without processing.
#' @param local_laz_index,local_laz_layer Optional PDAL tile index for source
#'   LAZ files already held locally.
#' @param direct_laz_cache Directory for downloaded USGS delivery tiles.
#' @param header_workers Concurrent bounded TNMAccess discovery queries during
#'   direct-LAZ planning.
#' @param refresh_sources Rebuild direct-LAZ planning products.
#' @return Invisibly, run and provenance paths.
#' @export
run_metrics <- function(aoi, output_dir, delivery_template,
                        name = "lidar_metrics", options = lidar_options(),
                        normalized_dir = file.path(output_dir, "normalized"),
                        project_dir = getwd(),
                        pdal_bin = Sys.getenv("PDAL_BIN", unset = Sys.which("pdal")),
                        dry_run = FALSE,
                        local_laz_index = NULL, local_laz_layer = NULL,
                        direct_laz_cache = file.path(output_dir, "source_laz"),
                        header_workers = 16L, refresh_sources = FALSE) {
  aoi <- validate_aoi(aoi)
  provenance <- run_provenance(
    aoi = aoi, output_dir = output_dir, name = name, project_dir = project_dir,
    include_direct_laz = TRUE, local_laz_index = local_laz_index,
    local_laz_layer = local_laz_layer, header_workers = header_workers,
    buffer_m = options$buffer_m, refresh = refresh_sources
  )
  if (isTRUE(dry_run)) return(invisible(provenance))
  selected <- provenance$candidates[provenance$candidates$selected, ]
  if (!nrow(selected)) stop("No EPT or direct-LAZ source is processable; inspect provenance$candidates.", call. = FALSE)
  # Provenance deliberately restores the caller's s2 setting.  The selected
  # source pieces can still share exact tile edges, so dissolve them through
  # GEOS before handing the AOI to the script engine.
  use_s2 <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(use_s2), add = TRUE)
  sf::sf_use_s2(FALSE)
  processing_aoi <- validate_aoi(sf::st_as_sf(sf::st_union(sf::st_geometry(selected))))
  profiles_path <- provenance$paths$profiles
  has_ept <- any(selected$source_type == "ept")
  has_direct <- identical(provenance$direct_laz_status, "planned") && any(selected$source_type == "direct_laz")

  result <- run_lidar_metrics(
    aoi = processing_aoi, name = name, delivery_template = delivery_template,
    normalized_dir = normalized_dir, metrics_dir = output_dir,
    options = options, project_dir = project_dir,
    ept_sources = if (has_ept) provenance$paths$ept_sources else NULL,
    ept_source_layer = if (has_ept) "ept_sources" else NULL,
    ept_profiles = if (has_ept) profiles_path else NULL,
    direct_laz_plan = if (has_direct) provenance$paths$direct_laz_plan else NULL,
    direct_laz_tiles = if (has_direct) provenance$paths$direct_laz_tiles else NULL,
    direct_laz_cache = if (has_direct) direct_laz_cache else NULL,
    pdal_bin = pdal_bin
  )
  if (has_direct && file.exists(provenance$paths$direct_laz_plan)) {
    acquired <- utils::read.csv(provenance$paths$direct_laz_plan, stringsAsFactors = FALSE, na.strings = c("", "NA"))
    direct_rows <- provenance$source_inventory$source_type == "direct_laz"
    matched <- match(provenance$source_inventory$source_url[direct_rows], acquired$source_url)
    provenance$source_inventory$local_path[direct_rows] <- acquired$local_path[matched]
    if ("download_status" %in% names(acquired)) provenance$source_inventory$availability[direct_rows] <- acquired$download_status[matched]
    utils::write.csv(provenance$source_inventory, provenance$paths$source_inventory, row.names = FALSE)
  }
  result$provenance <- provenance
  result$coverage_report <- provenance$paths$coverage
  result$source_profiles <- profiles_path
  result$source_inventory <- provenance$paths$source_inventory
  invisible(result)
}
