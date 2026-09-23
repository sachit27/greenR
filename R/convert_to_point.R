#' Convert Geometries to Points and Reproject to WGS84
#'
#' This function converts geometries (points, lines, polygons) to their centroid points and reprojects them to WGS84.
#'
#' @param data An sf object containing geometries.
#' @param target_crs The target coordinate reference system (default is WGS84, EPSG:4326).
#' @return An sf object with point geometries reprojected to the target CRS.
#' @importFrom sf st_transform st_centroid st_coordinates st_drop_geometry st_geometry_type st_sfc st_read st_point st_crs st_as_sf
#' @examples
#' \donttest{
#'   library(sf)
#'   library(dplyr)
#'
#'   # Create example data with a CRS
#'   lines <- st_sf(
#'     id = 1:5,
#'     geometry = st_sfc(
#'       st_linestring(matrix(c(0,0, 1,1), ncol=2, byrow=TRUE)),
#'       st_linestring(matrix(c(1,1, 2,2), ncol=2, byrow=TRUE)),
#'       st_linestring(matrix(c(2,2, 3,3), ncol=2, byrow=TRUE)),
#'       st_linestring(matrix(c(3,3, 4,4), ncol=2, byrow=TRUE)),
#'       st_linestring(matrix(c(4,4, 5,5), ncol=2, byrow=TRUE))
#'     ),
#'     crs = 4326 # Assign WGS84 CRS
#'   )
#'
#'   # Convert geometries to points
#'   points <- convert_to_point(lines)
#' }
#' @export
convert_to_point <- function(data, target_crs = 4326) {
  if (!inherits(data, "sf")) {
    if (is.character(data) && file.exists(data)) {
      data <- sf::st_read(data, quiet = TRUE)
    } else {
      stop("Input data must be an sf object or a valid spatial file path.")
    }
  }
  if (is.na(sf::st_crs(data))) stop("Input data must have a CRS defined.")
  if (is.na(sf::st_crs(target_crs))) stop("target_crs must be valid.")
  if (!nrow(data)) {
    data$lon <- numeric(0)
    data$lat <- numeric(0)
    return(sf::st_transform(data, target_crs))
  }
  types <- as.character(sf::st_geometry_type(data))
  if (!all(types %in% c("POINT", "LINESTRING", "MULTILINESTRING",
                        "POLYGON", "MULTIPOLYGON")))
    stop("Unsupported geometry type.", call. = FALSE)
  bounds <- sf::st_bbox(sf::st_transform(data, 4326))
  lon <- mean(bounds[c("xmin", "xmax")])
  lat <- mean(bounds[c("ymin", "ymax")])
  if (abs(lat) > 84)
    stop("Use a local projected CRS for polar locations.", call. = FALSE)
  zone <- max(1L, min(60L, floor((lon + 180) / 6) + 1L))
  metric_crs <- if (lat >= 0) 32600L + zone else 32700L + zone
  projected <- sf::st_transform(data, metric_crs)
  points <- lapply(seq_len(nrow(projected)), function(i) {
    geom <- sf::st_geometry(projected[i, ])[[1]]
    if (types[i] == "POINT") return(geom)
    if (types[i] %in% c("POLYGON", "MULTIPOLYGON"))
      return(sf::st_centroid(geom))
    sample <- sf::st_line_sample(sf::st_sfc(geom, crs = metric_crs),
                                 sample = 0.5)
    sf::st_cast(sample, "POINT")[[1]]
  })
  result <- sf::st_sf(sf::st_drop_geometry(data),
                      geometry = sf::st_sfc(points, crs = metric_crs))
  result <- sf::st_transform(result, target_crs)
  coords <- sf::st_coordinates(sf::st_transform(result, 4326))
  result$lon <- coords[, 1]
  result$lat <- coords[, 2]
  result
}
