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
#' @param pdal_bin Optional PDAL executable path.
#' @return Invisibly, a list of run paths.
#' @export
run_lidar_metrics <- function(aoi, name, delivery_template, normalized_dir, metrics_dir,
                              options = lidar_options(), project_dir = getwd(),
                              ept_sources = NULL, ept_source_layer = NULL, ept_profiles = NULL,
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
    "--stream-workers", options$stream_workers, "--normalize-workers", options$normalize_workers,
    "--metric-workers", options$metric_workers, "--write-normalized", tolower(options$write_normalized),
    "--resume", tolower(options$resume), "--pdal-bin", pdal_bin)
  direct <- !is.null(ept_sources) || !is.null(ept_source_layer) || !is.null(ept_profiles)
  if (direct && (is.null(ept_sources) || is.null(ept_source_layer) || is.null(ept_profiles))) stop("Supply ept_sources, ept_source_layer, and ept_profiles together.", call. = FALSE)
  if (direct) args <- c(args, "--ept-sources", ept_sources, "--ept-source-layer", ept_source_layer, "--ept-profiles", ept_profiles)
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
#' @return Invisibly, run and provenance paths.
#' @export
run_metrics <- function(aoi, output_dir, delivery_template,
                        name = "lidar_metrics", options = lidar_options(),
                        normalized_dir = file.path(output_dir, "normalized"),
                        project_dir = getwd(),
                        pdal_bin = Sys.getenv("PDAL_BIN", unset = Sys.which("pdal")),
                        dry_run = FALSE) {
  aoi <- validate_aoi(aoi)
  source_path <- file.path(project_dir, "data", "lidar_need_ept.gpkg")
  metadata_path <- file.path(project_dir, "data", "lidar_need_not_ept_usgs_candidates.csv")
  if (!file.exists(source_path)) stop("Authoritative EPT source cache is missing: ", source_path, call. = FALSE)
  sources <- sf::st_read(source_path, layer = "ept_sources", quiet = TRUE)
  source_pieces <- preflight_ept_sources(aoi, sources)

  provenance <- file.path(output_dir, "provenance")
  dir.create(provenance, recursive = TRUE, showWarnings = FALSE)
  profiles_path <- file.path(provenance, paste0(name, "_resolved_source_profiles.csv"))
  report_path <- file.path(provenance, paste0(name, "_coverage_report.gpkg"))

  if (file.exists(metadata_path)) {
    metadata <- utils::read.csv(metadata_path, stringsAsFactors = FALSE)
  } else {
    metadata <- data.frame(workunit = character(), ql = character(), collect_start = numeric(), collect_end = numeric())
  }
  metadata$quality_level <- toupper(gsub("[^A-Za-z0-9]", "", metadata$ql))
  metadata <- metadata[metadata$quality_level %in% c("QL1", "QL2"), ]
  metadata <- metadata[order(-metadata$collect_end, metadata$quality_level != "QL1"), ]
  metadata <- metadata[!duplicated(metadata$workunit), ]
  selected <- match(source_pieces$ept_name, metadata$workunit)
  source_pieces$quality_level <- metadata$quality_level[selected]
  epoch_date <- function(x) {
    out <- rep(NA_character_, length(x)); ok <- is.finite(x)
    out[ok] <- format(as.POSIXct(x[ok] / 1000, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d")
    out
  }
  source_pieces$acquisition_start <- epoch_date(metadata$collect_start[selected])
  source_pieces$acquisition_end <- epoch_date(metadata$collect_end[selected])
  source_pieces$coverage_status <- ifelse(is.na(source_pieces$quality_level), "needs_metadata", "processable")

  resolved <- source_pieces[source_pieces$coverage_status == "processable", ]
  profiles <- unique(sf::st_drop_geometry(resolved)[, c("ept_name", "quality_level", "acquisition_start", "acquisition_end")])
  utils::write.csv(profiles, profiles_path, row.names = FALSE)

  aoi_union <- sf::st_union(aoi)
  ept_union <- if (nrow(source_pieces)) sf::st_union(source_pieces) else sf::st_sfc(crs = sf::st_crs(aoi))
  uncovered_geometry <- if (length(ept_union)) suppressWarnings(sf::st_difference(aoi_union, ept_union)) else aoi_union
  report <- source_pieces[, c("ept_name", "ept_url", "quality_level", "acquisition_start", "acquisition_end", "coverage_status")]
  if (length(uncovered_geometry) && sum(as.numeric(sf::st_area(uncovered_geometry))) > 0) {
    missing <- sf::st_sf(ept_name = NA_character_, ept_url = NA_character_, quality_level = NA_character_,
      acquisition_start = NA_character_, acquisition_end = NA_character_, coverage_status = "needs_direct_laz",
      geometry = uncovered_geometry)
    report <- rbind(report, missing)
  }
  if (file.exists(report_path)) unlink(report_path)
  sf::st_write(report, report_path, layer = "coverage_report", quiet = TRUE)
  counts <- table(report$coverage_status)
  message("Coverage preflight: ", paste(names(counts), as.integer(counts), sep = "=", collapse = ", "))
  if (any(report$coverage_status == "needs_metadata")) {
    message("Continuing without unresolved EPT sources: ", paste(report$ept_name[report$coverage_status == "needs_metadata"], collapse = ", "))
  }
  if (!nrow(resolved)) stop("No EPT-covered source has authoritative QL1/QL2 metadata; see ", report_path, call. = FALSE)

  if (isTRUE(dry_run)) return(invisible(list(coverage_report = report_path, source_profiles = profiles_path, report = report)))

  result <- run_lidar_metrics(
    aoi = aoi, name = name, delivery_template = delivery_template,
    normalized_dir = normalized_dir, metrics_dir = output_dir,
    options = options, project_dir = project_dir,
    ept_sources = source_path, ept_source_layer = "ept_sources", ept_profiles = profiles_path,
    pdal_bin = pdal_bin
  )
  result$coverage_report <- report_path
  result$source_profiles <- profiles_path
  invisible(result)
}
