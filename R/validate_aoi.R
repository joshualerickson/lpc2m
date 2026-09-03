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
  # GEOS here at the public boundary. A zero-width buffer removes duplicate
  # vertices that can survive st_make_valid() in multipart polygons. For a
  # longitude/latitude AOI, carry out that repair in a local metric projection:
  # GEOS buffering in degrees is both noisy and geometrically inappropriate.
  use_s2 <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(use_s2), add = TRUE)
  sf::sf_use_s2(FALSE)
  original_crs <- sf::st_crs(aoi)
  is_geographic <- sf::st_is_longlat(aoi)
  if (is_geographic) {
    bbox <- sf::st_bbox(aoi)
    local_crs <- paste0(
      "+proj=laea +lat_0=", mean(c(bbox[["ymin"]], bbox[["ymax"]])),
      " +lon_0=", mean(c(bbox[["xmin"]], bbox[["xmax"]])),
      " +datum=WGS84 +units=m +no_defs"
    )
    aoi <- sf::st_transform(aoi, local_crs)
  }
  aoi <- sf::st_make_valid(aoi)
  aoi <- sf::st_set_geometry(aoi, sf::st_buffer(sf::st_geometry(aoi), 0))
  aoi <- aoi[!sf::st_is_empty(aoi), ]
  if (is_geographic) aoi <- sf::st_transform(aoi, original_crs)
  types <- as.character(sf::st_geometry_type(aoi, by_geometry = TRUE))
  if (!length(types) || any(!types %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("aoi must contain only POLYGON or MULTIPOLYGON geometries.", call. = FALSE)
  }
  aoi
}
