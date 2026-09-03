#' Resolve LiDAR provenance and data-contract eligibility
#'
#' Resolves authoritative EPT sources, QL, and acquisition dates for each AOI
#' feature. When an event date or year is supplied, only LiDAR acquired before
#' the event is eligible.
#'
#' @param aoi Polygonal `sf` object.
#' @param output_dir Optional directory in which to write provenance products.
#' @param name Output stem.
#' @param id_col Optional unique AOI identifier column.
#' @param event_year_col Optional integer event-year column. Acquisition must
#'   end before January 1 of that year.
#' @param event_date_col Optional event-date column. Acquisition must end before
#'   that date. Supply at most one temporal column.
#' @param minimum_coverage Fraction from 0 to 1 required for an AOI feature to
#'   satisfy the contract. The default requires some eligible coverage.
#' @param project_dir Repository/package data root during development.
#' @param include_direct_laz Resolve USGS delivery LAZ for non-EPT portions.
#' @param local_laz_index,local_laz_layer Optional PDAL tile index for LAZ files
#'   already available locally. Supply both together.
#' @param header_workers Concurrent bounded TNMAccess discovery queries. The
#'   name is retained for compatibility with the earlier header-based planner.
#' @param buffer_m Processing halo used to include adjacent delivery tiles.
#' @param refresh Rebuild direct-LAZ planning products instead of reusing them.
#' @return An object of class `usgslpc_provenance` containing candidate pieces,
#'   eligible AOI geometry, feature summaries, and generated source profiles.
#' @export
run_provenance <- function(aoi, output_dir = NULL, name = "lidar_provenance",
                           id_col = NULL, event_year_col = NULL,
                           event_date_col = NULL, minimum_coverage = 0,
                           project_dir = getwd(), include_direct_laz = TRUE,
                           local_laz_index = NULL, local_laz_layer = NULL,
                           header_workers = 16L, buffer_m = 60, refresh = FALSE) {
  # Keep every overlay in this resolver in GEOS.  A few published source
  # footprints contain duplicate ring vertices; s2 errors before validity
  # repair can be applied, whereas GEOS plus the zero-buffer repair used by
  # validate_aoi() handles them safely.
  use_s2 <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(use_s2), add = TRUE)
  sf::sf_use_s2(FALSE)
  aoi <- validate_aoi(aoi)
  if (!is.null(event_year_col) && !is.null(event_date_col)) stop("Supply event_year_col or event_date_col, not both.", call. = FALSE)
  if (!is.numeric(minimum_coverage) || length(minimum_coverage) != 1L || minimum_coverage < 0 || minimum_coverage > 1) stop("minimum_coverage must be between 0 and 1.", call. = FALSE)
  if (is.null(id_col)) {
    id_col <- ".aoi_id"
    aoi[[id_col]] <- sprintf("aoi_%05d", seq_len(nrow(aoi)))
  }
  if (!id_col %in% names(aoi) || anyNA(aoi[[id_col]]) || anyDuplicated(aoi[[id_col]])) stop("id_col must identify every AOI feature uniquely.", call. = FALSE)
  if (!is.null(event_year_col) && !event_year_col %in% names(aoi)) stop("AOI has no event_year_col: ", event_year_col, call. = FALSE)
  if (!is.null(event_date_col) && !event_date_col %in% names(aoi)) stop("AOI has no event_date_col: ", event_date_col, call. = FALSE)

  provenance_dir <- if (is.null(output_dir)) tempfile(paste0(name, "_provenance_")) else file.path(output_dir, "provenance")
  dir.create(provenance_dir, recursive = TRUE, showWarnings = FALSE)
  # Live EPT discovery is the default. The repository footprint is retained
  # only as an offline fallback, never as the primary source of truth.
  live_ept <- .resolve_live_ept_sources(aoi, provenance_dir, name, project_dir)
  source_path <- live_ept$path
  if (is.null(source_path)) source_path <- file.path(project_dir, "data", "lidar_need_ept.gpkg")
  if (!file.exists(source_path)) stop("Live EPT discovery failed and no fallback EPT cache exists.", call. = FALSE)
  sources <- sf::st_read(source_path, layer = "ept_sources", quiet = TRUE)
  sources <- validate_aoi(sf::st_transform(sources, sf::st_crs(aoi)))
  # Retain live resolver metadata (QL and acquisition dates) alongside the
  # endpoint. The older static cache has only ept_name/ept_url and is enriched
  # below when those columns are absent.
  sources <- sources[lengths(sf::st_intersects(sources, aoi)) > 0, ]

  if (!all(c("quality_level", "acquisition_start", "acquisition_end") %in% names(sources))) {
    metadata <- .read_source_metadata(project_dir)
    source_meta <- data.frame(
      ept_name = sources$ept_name,
      quality_level = metadata$quality_level[match(sources$ept_name, metadata$workunit)],
      acquisition_start = .epoch_date(metadata$collect_start[match(sources$ept_name, metadata$workunit)]),
      acquisition_end = .epoch_date(metadata$collect_end[match(sources$ept_name, metadata$workunit)]),
      stringsAsFactors = FALSE
    )
    sources <- merge(sources, source_meta, by = "ept_name", all.x = TRUE, sort = FALSE)
  }
  sources$acquisition_start <- as.Date(sources$acquisition_start)
  sources$acquisition_end <- as.Date(sources$acquisition_end)
  candidates <- suppressWarnings(sf::st_intersection(aoi, sources))
  candidates <- candidates[as.numeric(sf::st_area(candidates)) > 0, ]
  candidates$source_type <- rep("ept", nrow(candidates))
  candidates$source_name <- candidates$ept_name
  candidates$source_url <- candidates$ept_url
  candidates$local_path <- rep(NA_character_, nrow(candidates))
  candidates$estimated_bytes <- rep(NA_real_, nrow(candidates))
  candidates$coverage_status <- ifelse(is.na(candidates$quality_level) | is.na(candidates$acquisition_end), "needs_metadata", "ready_ept")

  event_date <- as.Date(rep(NA_character_, nrow(candidates)))
  if (!is.null(event_year_col)) event_date <- as.Date(sprintf("%04d-01-01", as.integer(candidates[[event_year_col]])))
  if (!is.null(event_date_col)) event_date <- as.Date(candidates[[event_date_col]])
  temporal <- !is.na(event_date)
  candidates$event_date <- event_date
  candidates$coverage_status[temporal & !is.na(candidates$acquisition_end) & candidates$acquisition_end >= event_date] <- "not_pre_event"
  candidates$selected <- candidates$coverage_status == "ready_ept"

  aoi_union <- sf::st_union(aoi)
  ept_ready <- candidates[candidates$selected, ]
  ept_geometry <- if (nrow(ept_ready)) sf::st_union(sf::st_geometry(ept_ready)) else sf::st_sfc(crs = sf::st_crs(aoi))
  holes_geometry <- if (length(ept_geometry)) suppressWarnings(sf::st_difference(aoi_union, ept_geometry)) else aoi_union
  holes <- sf::st_as_sf(holes_geometry)
  holes <- holes[as.numeric(sf::st_area(holes)) > 0, ]
  direct <- list(status = "not_needed", plan = NULL, tiles = NULL, holes = NULL)
  direct_candidates <- candidates[0, ]
  direct_inventory <- .empty_source_inventory()
  if (isTRUE(include_direct_laz) && nrow(holes)) {
    direct <- .run_direct_laz_planner(holes, provenance_dir, name, project_dir,
      local_laz_index = local_laz_index, local_laz_layer = local_laz_layer,
      header_workers = header_workers, buffer_m = buffer_m, refresh = refresh)
    if (identical(direct$status, "planned")) {
      tile_sources <- sf::st_read(direct$tiles, layer = "direct_laz_tiles", quiet = TRUE)
      tile_sources <- validate_aoi(sf::st_transform(tile_sources, sf::st_crs(aoi)))
      source_layers <- sf::st_layers(direct$tiles)$name
      coverage_sources <- if ("direct_laz_coverage" %in% source_layers) {
        sf::st_read(direct$tiles, layer = "direct_laz_coverage", quiet = TRUE)
      } else tile_sources
      coverage_sources <- validate_aoi(sf::st_transform(coverage_sources, sf::st_crs(aoi)))
      direct_candidates <- suppressWarnings(sf::st_intersection(aoi, coverage_sources))
      direct_candidates <- direct_candidates[as.numeric(sf::st_area(direct_candidates)) > 0, ]
      direct_candidates$source_type <- "direct_laz"
      direct_candidates$source_name <- direct_candidates$workunit
      direct_candidates$source_url <- rep(NA_character_, nrow(direct_candidates))
      direct_candidates$local_path <- rep(NA_character_, nrow(direct_candidates))
      direct_candidates$acquisition_start <- as.Date(direct_candidates$acquisition_start)
      direct_candidates$acquisition_end <- as.Date(direct_candidates$acquisition_end)
      direct_candidates$estimated_bytes <- vapply(direct_candidates$workunit, function(x) sum(tile_sources$bytes[tile_sources$workunit == x], na.rm = TRUE), numeric(1))
      workunit_local <- vapply(direct_candidates$workunit, function(x) {
        paths <- tile_sources$local_path[tile_sources$workunit == x]
        length(paths) > 0 && all(!is.na(paths) & nzchar(paths))
      }, logical(1))
      direct_candidates$coverage_status <- ifelse(workunit_local, "ready_direct_laz_local", "ready_direct_laz_download")
      direct_event <- as.Date(rep(NA_character_, nrow(direct_candidates)))
      if (!is.null(event_year_col)) direct_event <- as.Date(sprintf("%04d-01-01", as.integer(direct_candidates[[event_year_col]])))
      if (!is.null(event_date_col)) direct_event <- as.Date(direct_candidates[[event_date_col]])
      direct_candidates$event_date <- direct_event
      temporal_direct <- !is.na(direct_event)
      direct_candidates$coverage_status[temporal_direct & !is.na(direct_candidates$acquisition_end) & direct_candidates$acquisition_end >= direct_event] <- "not_pre_event"
      direct_candidates$selected <- direct_candidates$coverage_status %in% c("ready_direct_laz_local", "ready_direct_laz_download")
      direct_inventory <- unique(data.frame(
        source_type = "direct_laz", source_name = tile_sources$workunit,
        source_url = tile_sources$source_url, local_path = tile_sources$local_path,
        quality_level = tile_sources$quality_level,
        acquisition_start = as.character(tile_sources$acquisition_start),
        acquisition_end = as.character(tile_sources$acquisition_end),
        acquisition_year = tile_sources$acquisition_year,
        estimated_bytes = tile_sources$bytes,
        availability = ifelse(!is.na(tile_sources$local_path) & nzchar(tile_sources$local_path), "local", "download_required"),
        stringsAsFactors = FALSE
      ))
    }
  }

  # Bind only the stable provenance columns because EPT and delivery-tile
  # records carry different source-specific metadata.
  stable <- c(id_col, "source_type", "source_name", "source_url", "local_path",
              "quality_level", "acquisition_start", "acquisition_end", "estimated_bytes",
              "event_date", "coverage_status", "selected")
  for (column in setdiff(stable, names(candidates))) candidates[[column]] <- NA
  for (column in setdiff(stable, names(direct_candidates))) direct_candidates[[column]] <- NA
  candidates <- rbind(candidates[, stable], direct_candidates[, stable])

  covered <- candidates[candidates$selected, ]
  covered_geometry <- if (nrow(covered)) sf::st_union(sf::st_geometry(covered)) else sf::st_sfc(crs = sf::st_crs(aoi))
  missing_geometry <- if (length(covered_geometry)) suppressWarnings(sf::st_difference(aoi_union, covered_geometry)) else aoi_union
  missing <- sf::st_as_sf(missing_geometry)
  # GEOS overlays can leave sub-square-metre numerical remnants along exactly
  # coincident source boundaries; these are not meaningful coverage gaps.
  missing <- missing[as.numeric(sf::st_area(missing)) >= 1, ]
  if (nrow(missing)) {
    missing <- suppressWarnings(sf::st_intersection(aoi, missing))
    for (column in stable) if (!column %in% names(missing)) missing[[column]] <- NA
    missing$source_type <- "none"
    missing$coverage_status <- if (identical(direct$status, "planning_failed")) "direct_laz_planning_failed" else if (isTRUE(include_direct_laz)) "no_published_lidar" else "needs_direct_laz"
    missing$selected <- FALSE
    candidates <- rbind(candidates, missing[, stable])
  }

  eligible_pieces <- candidates[candidates$selected, ]
  ids <- as.character(aoi[[id_col]])
  summary_rows <- lapply(seq_len(nrow(aoi)), function(i) {
    total <- as.numeric(sf::st_area(aoi[i, ]))
    pieces <- eligible_pieces[as.character(eligible_pieces[[id_col]]) == ids[i], ]
    eligible <- if (nrow(pieces)) as.numeric(sf::st_area(sf::st_union(pieces))) else 0
    data.frame(aoi_id = ids[i], aoi_area_m2 = total, eligible_area_m2 = eligible,
      eligible_fraction = eligible / total, meets_contract = eligible > 0 && eligible / total >= minimum_coverage)
  })
  summary <- do.call(rbind, summary_rows)
  eligible_ids <- summary$aoi_id[summary$meets_contract]
  eligible_aoi <- aoi[as.character(aoi[[id_col]]) %in% eligible_ids, ]
  ept_profiles <- ept_ready
  profiles <- unique(sf::st_drop_geometry(ept_profiles)[, c("ept_name", "quality_level", "acquisition_start", "acquisition_end")])
  ept_inventory <- if (nrow(ept_ready)) unique(data.frame(
      source_type = rep("ept", nrow(ept_ready)), source_name = ept_ready$ept_name, source_url = ept_ready$ept_url,
      local_path = rep(NA_character_, nrow(ept_ready)), quality_level = ept_ready$quality_level,
      acquisition_start = as.character(ept_ready$acquisition_start), acquisition_end = as.character(ept_ready$acquisition_end),
      acquisition_year = suppressWarnings(as.integer(format(ept_ready$acquisition_end, "%Y"))), estimated_bytes = rep(NA_real_, nrow(ept_ready)),
      availability = rep("stream", nrow(ept_ready)), stringsAsFactors = FALSE
    )) else .empty_source_inventory()
  source_inventory <- rbind(ept_inventory, direct_inventory)

  paths <- list(ept_sources = source_path, direct_laz_holes = direct$holes, direct_laz_plan = direct$plan, direct_laz_tiles = direct$tiles)
  if (!is.null(output_dir)) {
    paths$coverage <- file.path(provenance_dir, paste0(name, "_coverage_candidates.gpkg"))
    paths$summary <- file.path(provenance_dir, paste0(name, "_feature_summary.csv"))
    paths$profiles <- file.path(provenance_dir, paste0(name, "_source_profiles.csv"))
    paths$eligible_aoi <- file.path(provenance_dir, paste0(name, "_eligible_aoi.gpkg"))
    paths$source_inventory <- file.path(provenance_dir, paste0(name, "_source_inventory.csv"))
    if (file.exists(paths$coverage)) unlink(paths$coverage)
    if (nrow(candidates)) sf::st_write(candidates, paths$coverage, layer = "coverage_candidates", quiet = TRUE)
    utils::write.csv(summary, paths$summary, row.names = FALSE)
    utils::write.csv(profiles, paths$profiles, row.names = FALSE)
    utils::write.csv(source_inventory, paths$source_inventory, row.names = FALSE)
    if (nrow(eligible_aoi)) {
      if (file.exists(paths$eligible_aoi)) unlink(paths$eligible_aoi)
      sf::st_write(eligible_aoi, paths$eligible_aoi, layer = "eligible_aoi", quiet = TRUE)
    }
  }
  counts <- table(candidates$coverage_status)
  message("Provenance: ", paste(names(counts), as.integer(counts), sep = "=", collapse = ", "),
          "; eligible AOIs=", sum(summary$meets_contract), "/", nrow(summary))
  structure(list(candidates = candidates, eligible_aoi = eligible_aoi, summary = summary,
                 source_profiles = profiles, source_inventory = source_inventory,
                 direct_laz_status = direct$status, paths = paths, id_col = id_col), class = "usgslpc_provenance")
}
