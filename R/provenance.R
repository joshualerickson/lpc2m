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
#' @return An object of class `usgslpc_provenance` containing candidate pieces,
#'   eligible AOI geometry, feature summaries, and generated source profiles.
#' @export
run_provenance <- function(aoi, output_dir = NULL, name = "lidar_provenance",
                           id_col = NULL, event_year_col = NULL,
                           event_date_col = NULL, minimum_coverage = 0,
                           project_dir = getwd()) {
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

  source_path <- file.path(project_dir, "data", "lidar_need_ept.gpkg")
  metadata_path <- file.path(project_dir, "data", "lidar_need_not_ept_usgs_candidates.csv")
  if (!file.exists(source_path)) stop("Authoritative EPT source cache is missing: ", source_path, call. = FALSE)
  sources <- sf::st_read(source_path, layer = "ept_sources", quiet = TRUE)
  sources <- sf::st_make_valid(sf::st_transform(sources, sf::st_crs(aoi)))
  sources <- sources[lengths(sf::st_intersects(sources, aoi)) > 0, c("ept_name", "ept_url")]

  if (file.exists(metadata_path)) {
    metadata <- utils::read.csv(metadata_path, stringsAsFactors = FALSE)
    metadata$quality_level <- toupper(gsub("[^A-Za-z0-9]", "", metadata$ql))
    metadata <- metadata[metadata$quality_level %in% c("QL1", "QL2"), ]
    metadata <- metadata[order(-metadata$collect_end, metadata$quality_level != "QL1"), ]
    metadata <- metadata[!duplicated(metadata$workunit), ]
  } else metadata <- data.frame(workunit = character(), quality_level = character(), collect_start = numeric(), collect_end = numeric())
  epoch_date <- function(x) {
    out <- as.Date(rep(NA_character_, length(x))); ok <- is.finite(x)
    out[ok] <- as.Date(as.POSIXct(x[ok] / 1000, origin = "1970-01-01", tz = "UTC"))
    out
  }
  source_meta <- data.frame(
    ept_name = sources$ept_name,
    quality_level = metadata$quality_level[match(sources$ept_name, metadata$workunit)],
    acquisition_start = epoch_date(metadata$collect_start[match(sources$ept_name, metadata$workunit)]),
    acquisition_end = epoch_date(metadata$collect_end[match(sources$ept_name, metadata$workunit)]),
    stringsAsFactors = FALSE
  )
  sources <- merge(sources, source_meta, by = "ept_name", all.x = TRUE, sort = FALSE)
  candidates <- suppressWarnings(sf::st_intersection(aoi, sources))
  candidates <- candidates[as.numeric(sf::st_area(candidates)) > 0, ]
  candidates$coverage_status <- ifelse(is.na(candidates$quality_level) | is.na(candidates$acquisition_end), "needs_metadata", "processable")

  event_date <- as.Date(rep(NA_character_, nrow(candidates)))
  if (!is.null(event_year_col)) event_date <- as.Date(sprintf("%04d-01-01", as.integer(candidates[[event_year_col]])))
  if (!is.null(event_date_col)) event_date <- as.Date(candidates[[event_date_col]])
  temporal <- !is.na(event_date)
  candidates$event_date <- event_date
  candidates$coverage_status[temporal & !is.na(candidates$acquisition_end) & candidates$acquisition_end >= event_date] <- "not_pre_event"
  candidates$selected <- candidates$coverage_status == "processable"

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
  profiles <- unique(sf::st_drop_geometry(eligible_pieces)[, c("ept_name", "quality_level", "acquisition_start", "acquisition_end")])

  paths <- list()
  if (!is.null(output_dir)) {
    provenance_dir <- file.path(output_dir, "provenance"); dir.create(provenance_dir, recursive = TRUE, showWarnings = FALSE)
    paths$coverage <- file.path(provenance_dir, paste0(name, "_coverage_candidates.gpkg"))
    paths$summary <- file.path(provenance_dir, paste0(name, "_feature_summary.csv"))
    paths$profiles <- file.path(provenance_dir, paste0(name, "_source_profiles.csv"))
    paths$eligible_aoi <- file.path(provenance_dir, paste0(name, "_eligible_aoi.gpkg"))
    if (file.exists(paths$coverage)) unlink(paths$coverage)
    if (nrow(candidates)) sf::st_write(candidates, paths$coverage, layer = "coverage_candidates", quiet = TRUE)
    utils::write.csv(summary, paths$summary, row.names = FALSE)
    utils::write.csv(profiles, paths$profiles, row.names = FALSE)
    if (nrow(eligible_aoi)) {
      if (file.exists(paths$eligible_aoi)) unlink(paths$eligible_aoi)
      sf::st_write(eligible_aoi, paths$eligible_aoi, layer = "eligible_aoi", quiet = TRUE)
    }
  }
  counts <- table(candidates$coverage_status)
  message("Provenance: ", paste(names(counts), as.integer(counts), sep = "=", collapse = ", "),
          "; eligible AOIs=", sum(summary$meets_contract), "/", nrow(summary))
  structure(list(candidates = candidates, eligible_aoi = eligible_aoi, summary = summary,
                 source_profiles = profiles, paths = paths, id_col = id_col), class = "usgslpc_provenance")
}
