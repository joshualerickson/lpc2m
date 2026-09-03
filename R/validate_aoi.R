#' Validate a polygonal area of interest
#'
#' @param aoi An `sf` object containing polygon or multipolygon geometries.
#' @return A valid `sf` AOI with empty features removed.
#' @export
validate_aoi <- function(aoi) {
  if (!inherits(aoi, "sf")) stop("aoi must be an sf object.", call. = FALSE)
  if (!nrow(aoi) || is.na(sf::st_crs(aoi))) {
    stop("aoi must contain at least one feature with a CRS.", call. = FALSE)
  }
  # USGS and user-provided boundaries occasionally carry repeated vertices.
  # s2 rejects those rings before GEOS can repair them, so normalize through
  # GEOS here at the public boundary.  A zero-width buffer removes duplicate
  # vertices that can survive st_make_valid() in multipart polygons.
  use_s2 <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(use_s2), add = TRUE)
  sf::sf_use_s2(FALSE)
  aoi <- sf::st_make_valid(aoi)
  aoi <- sf::st_set_geometry(aoi, sf::st_buffer(sf::st_geometry(aoi), 0))
  aoi <- aoi[!sf::st_is_empty(aoi), ]
  types <- as.character(sf::st_geometry_type(aoi, by_geometry = TRUE))
  if (!length(types) || any(!types %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("aoi must contain only POLYGON or MULTIPOLYGON geometries.", call. = FALSE)
  }
  aoi
}
