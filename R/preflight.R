#' Make stable processing blocks from an AOI
#'
#' @param aoi Valid polygonal `sf` AOI.
#' @param block_m Core block width in metres.
#' @return An `sf` layer with `block_id` and `core_area_m2`.
#' @export
make_processing_grid <- function(aoi, block_m = 500) {
  aoi <- validate_aoi(aoi)
  if (!is.numeric(block_m) || length(block_m) != 1L || !is.finite(block_m) || block_m <= 0) stop("block_m must be one positive number.", call. = FALSE)
  core <- sf::st_union(aoi)
  grid <- sf::st_make_grid(core, cellsize = block_m, square = TRUE, what = "polygons")
  blocks <- suppressWarnings(sf::st_intersection(sf::st_as_sf(data.frame(block_id = seq_along(grid)), geometry = grid), core))
  blocks <- blocks[as.numeric(sf::st_area(blocks)) > 0, ]
  blocks$block_id <- sprintf("block_%05d", seq_len(nrow(blocks)))
  blocks$core_area_m2 <- as.numeric(sf::st_area(blocks))
  blocks
}

#' Preflight authoritative EPT source coverage
#'
#' @param aoi Valid polygonal `sf` AOI.
#' @param ept_sources An `sf` source layer with `ept_name` and `ept_url`.
#' @return Source-specific AOI pieces, including exact EPT URLs.
#' @export
preflight_ept_sources <- function(aoi, ept_sources) {
  aoi <- validate_aoi(aoi)
  if (!inherits(ept_sources, "sf") || !all(c("ept_name", "ept_url") %in% names(ept_sources))) {
    stop("ept_sources must be an sf object with ept_name and ept_url.", call. = FALSE)
  }
  sources <- sf::st_make_valid(sf::st_transform(ept_sources, sf::st_crs(aoi)))
  hits <- sources[lengths(sf::st_intersects(sources, aoi)) > 0, ]
  if (!nrow(hits)) return(hits)
  out <- suppressWarnings(sf::st_intersection(hits, sf::st_union(aoi)))
  out <- out[as.numeric(sf::st_area(out)) > 0, ]
  out$source_area_m2 <- as.numeric(sf::st_area(out))
  out
}
