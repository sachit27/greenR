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
  # Check if data is already an sf object
  if (!inherits(data, "sf")) {
    # Try to convert the data to an sf object
    if (is.character(data) && file.exists(data)) {
      # Assuming the data is a file path
      data <- st_read(data)
    } else {
      stop("Input data must be an sf object or a valid file path to a GeoJSON file")
    }
  }

  # Ensure the data has a CRS set
  if (is.na(st_crs(data))) {
    stop("Input data must have a CRS defined")
  }

  # Function to calculate the midpoint of a linestring
  midpoint <- function(linestring) {
    coords <- st_coordinates(linestring)
    mid_index <- floor(nrow(coords) / 2)
    mid_point <- st_point(coords[mid_index, ])
    return(mid_point)
  }

  # Convert geometries to points
  data_points <- st_sf(
    st_drop_geometry(data),  # Drop the original geometry columns
    geometry = st_sfc(lapply(st_geometry(data), function(geom) {
      geom_type <- st_geometry_type(geom)
      if (geom_type == "POLYGON" || geom_type == "MULTIPOLYGON") {
        return(st_centroid(geom))
      } else if (geom_type == "LINESTRING" || geom_type == "MULTILINESTRING") {
        return(midpoint(geom))
      } else if (geom_type == "POINT") {
        return(geom)
      } else {
        stop("Unsupported geometry type")
      }
    }), crs = st_crs(data))
  )

  # Reproject to target CRS (WGS84 by default)
  data_points <- st_transform(data_points, crs = target_crs)

  # Extract lon and lat from the geometry column
  data_points <- data_points %>%
    mutate(
      lon = st_coordinates(geometry)[, 1],
      lat = st_coordinates(geometry)[, 2]
    )

  return(data_points)
}
