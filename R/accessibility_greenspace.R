#' Generate Accessibility Map for Green Spaces
#'
#' This function generates a map that shows green spaces accessible within a specified
#' walking time from a given location. The location is specified by its latitude and longitude.
#'
#' @param green_area_data A list containing green area data, usually obtained from the \code{get_osm_data} function.
#' @param location_lat Numeric latitude of the specified location.
#' @param location_lon Numeric longitude of the specified location.
#' @param max_walk_time Maximum walking time in minutes. Default is 15.
#' @param green_color Color for the green areas on the map. Default is "black".
#' @param location_color Color for the specified location on the map. Default is "blue".
#' @param isochrone_color Color palette for the isochrone lines. Default is "viridis".
#'
#' @return A tmap object representing the accessibility map.
#'
#' @importFrom sf st_as_sf st_transform st_intersection st_make_valid
#' @importFrom tibble tibble
#' @importFrom osrm osrmIsochrone
#' @importFrom tmap tm_shape tm_polygons tm_dots
#' @importFrom dplyr rename
#' @importFrom magrittr %>%
#' @export

accessibility_greenspace <- function(green_area_data, location_lat, location_lon,
                                     max_walk_time = 5, green_color = "green",
                                     location_color = "blue", isochrone_color = "viridis") {

  # Set tmap mode to view for interactive leaflet map
  tmap::tmap_mode("view")

  # Error Handling: Check if latitude and longitude are numeric and within valid range
  if (!is.numeric(location_lat) || location_lat < -90 || location_lat > 90) {
    stop("Invalid latitude provided. Latitude should be a numeric value between -90 and 90.")
  }

  if (!is.numeric(location_lon) || location_lon < -180 || location_lon > 180) {
    stop("Invalid longitude provided. Longitude should be a numeric value between -180 and 180.")
  }

  # Prepare the green area data
  osm_sf <- green_area_data$osm_polygons

  # Prepare the specified location as an sf object
  specified_location <-
    tibble::tibble(
      case_id = c(1),
      lat = c(location_lat),
      lon = c(location_lon)
    ) %>%
    sf::st_as_sf(., coords = c("lon", "lat"), crs = 4326, agr = "constant") %>%
    sf::st_transform(., crs = 4326)

  # Create the isochrone map using the specified location and walking time
  iso_map <- osrm::osrmIsochrone(loc = specified_location,
                                 breaks = seq(from = 0, to = max_walk_time, by = 5),
                                 osrm.profile = "foot")

  # Make the isochrone map valid for plotting
  iso_map_valid <- sf::st_make_valid(iso_map)

  # Find green spaces within the isochrone
  green_within_isochrone <- sf::st_intersection(osm_sf, iso_map_valid)

  # Generate and return the map
  return(
    tmap::tm_shape(iso_map_valid %>% dplyr::rename(walk_time = isomax)) +
      tmap::tm_polygons("walk_time", palette = isochrone_color, alpha = 0.6) +
      tmap::tm_shape(green_within_isochrone) +
      tmap::tm_dots(size = 0.1, col = green_color) +
      tmap::tm_shape(specified_location) +
      tmap::tm_dots(size = 0.2, col = location_color)
  )
}
