# Declare global variables to avoid R CMD check warnings
utils::globalVariables(c("landuse", "leisure"))

#' Calculate and Visualize the Shortest Walking Path to Specified Type of Nearest Green Space with Estimated Walking Time
#'
#' Determines the nearest specified type of green space from a given location and calculates the
#' shortest walking route using the road network optimized for walking. The result is visualized on a Leaflet map
#' displaying the path, the starting location, and the destination green space, with details on distance and estimated walking time.
#'
#' @param highway_data List containing road network data, typically obtained from OpenStreetMap.
#' @param green_areas_data List containing green areas data, obtained from `get_osm_data`.
#' @param location_lat Numeric, latitude of the starting location.
#' @param location_lon Numeric, longitude of the starting location.
#' @param green_space_types Vector of strings specifying types of green spaces to consider.
#' @param walking_speed_kmh Numeric, walking speed in kilometers per hour, default is 4.5.
#' @param osrm_server URL of the OSRM routing server with foot routing support, default is "https://router.project-osrm.org/".
#' @return A Leaflet map object showing the route, start point, and nearest green space with popup annotations.
#' @importFrom sf st_as_sf st_transform st_coordinates st_centroid st_sfc st_point st_crs st_distance
#' @importFrom leaflet leaflet addTiles addPolylines addMarkers addLegend makeAwesomeIcon
#' @importFrom osrm osrmRoute
#' @importFrom dplyr filter
#' @examples
#' \donttest{
#'   data <- get_osm_data("Fulham, London, United Kingdom")
#'   highway_data <- data$highways
#'   green_areas_data <- data$green_areas
#'   map <- nearest_greenspace(highway_data, green_areas_data, 51.4761, -0.2008, c("park", "forest"))
#'   print(map) # Display the map
#' }
#' @export
nearest_greenspace <- function(highway_data, green_areas_data, location_lat, location_lon, green_space_types = NULL, walking_speed_kmh = 4.5, osrm_server = "https://router.project-osrm.org/") {
  # Set OSRM server and profile
  options(osrm.server = osrm_server)

  # Validate inputs
  if (!is.numeric(location_lat) || location_lat < -90 || location_lat > 90) {
    stop("Invalid latitude provided. Latitude should be a numeric value between -90 and 90.")
  }

  if (!is.numeric(location_lon) || location_lon < -180 || location_lon > 180) {
    stop("Invalid longitude provided. Longitude should be a numeric value between -180 and 180.")
  }

  # Filter green spaces if specific types are specified
  if (!is.null(green_space_types) && length(green_space_types) > 0) {
    green_areas_data$osm_polygons <- green_areas_data$osm_polygons %>%
      dplyr::filter(landuse %in% green_space_types | leisure %in% green_space_types)
  }

  if (nrow(green_areas_data$osm_polygons) == 0) {
    stop("No green spaces found for the specified types.")
  }

  # Calculate centroids of green spaces
  green_points <- sf::st_centroid(green_areas_data$osm_polygons)

  # Find the nearest green space
  start_point <- sf::st_sfc(sf::st_point(c(location_lon, location_lat)), crs = sf::st_crs(green_points))
  distances <- sf::st_distance(green_points, start_point)
  nearest_green <- green_points[which.min(distances), ]
  nearest_coords <- sf::st_coordinates(nearest_green)

  # Calculate the shortest route using OSRM with the walking profile
  route <- osrm::osrmRoute(src = c(location_lon, location_lat), dst = nearest_coords[1, c("X", "Y")],
                           overview = "full", osrm.profile = "foot")

  # Calculate walking time in minutes (speed = user-defined walking speed)
  distance_m <- min(distances)  # Distance in meters
  distance_km <- distance_m / 1000  # Convert to kilometers
  walking_time_minutes <- (distance_km / walking_speed_kmh) * 60  # Convert hours to minutes

  # Print route details
  destination_name <- ifelse(is.na(nearest_green$name[1]), "Unknown", nearest_green$name[1])
  start_info <- sprintf("Start Position: (%f, %f)", location_lat, location_lon)
  destination_info <- sprintf("Nearest Green Space: (%f, %f)\nName: %s\nDistance: %.2f meters\nWalking Time: ~%.2f minutes",
                              nearest_coords[1, "Y"], nearest_coords[1, "X"],
                              destination_name, distance_m, walking_time_minutes)
  message("Route Details:\n", start_info, "\n", destination_info, "\n")

  # Create Leaflet map
  map <- leaflet() %>%
    addTiles() %>%
    addPolylines(data = route$geometry, color = "red", weight = 4, opacity = 0.75, label = "Walking Route") %>%
    addMarkers(lng = location_lon, lat = location_lat, popup = "Start Point", label = "Start Point", icon = makeAwesomeIcon(icon = "home", markerColor = "blue")) %>%
    addMarkers(lng = nearest_coords[1, "X"], lat = nearest_coords[1, "Y"],
               popup = sprintf("Nearest Green Space\nName: %s\nDistance: %.2f meters\nWalking Time: ~%.2f minutes",
                               destination_name, distance_m, walking_time_minutes),
               label = "Nearest Green Space", icon = makeAwesomeIcon(icon = "tree", markerColor = "green")) %>%
    addLegend("bottomright", colors = c("blue", "green", "red"),
              labels = c("Start Point", "Nearest Green Space", "Walking Route"),
              title = "Legend", opacity = 0.7)

  return(map)
}
