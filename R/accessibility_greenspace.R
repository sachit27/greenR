#' Generate Accessibility Map for Green Spaces
#'
#' This function generates a leaflet map that shows green spaces accessible within
#' a specified walking time from a given location. The location is specified by its
#' latitude and longitude coordinates.
#'
#' @param green_area_data A list containing green area data, usually obtained from the \code{get_osm_data} function.
#' @param location_lat Numeric latitude of the specified location.
#' @param location_lon Numeric longitude of the specified location.
#' @param max_walk_time Maximum walking time in minutes. Default is 15.
#' @param green_color Color for the green areas on the map. Default is "green".
#' @param location_color Color for the specified location on the map. Default is "blue".
#' @param isochrone_color Color palette for the isochrone lines. Default is "viridis".
#'
#' @return A leaflet map object.
#' @importFrom sf st_as_sf st_transform st_make_valid st_coordinates
#' @importFrom leaflet leaflet addProviderTiles addPolygons addCircles addLegend addLayersControl
#' @importFrom osrm osrmIsochrone
#' @importFrom tibble tibble
#' @importFrom viridisLite viridis
#' @examples
#' \dontrun{
#'   green_area_data <- data$green_areas
#'   accessibility_greenspace(data, 47.56, 7.59)
#' }
#' @export
accessibility_greenspace <- function(green_area_data, location_lat, location_lon,
                                     max_walk_time = 15, green_color = "green",
                                     location_color = "blue", isochrone_color = "viridis") {

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
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant") %>%
    sf::st_transform(crs = 4326)

  # Create the isochrone map using the specified location and walking time
  iso_map <- osrm::osrmIsochrone(loc = specified_location,
                                 breaks = seq(from = 0, to = max_walk_time, by = 5),
                                 osrm.profile = "foot")

  # Make the isochrone map valid for plotting
  iso_map_valid <- sf::st_make_valid(iso_map)

  # Create a label for each isochrone layer
  iso_map_valid$label <- paste(iso_map_valid$isomax, "minutes")

  # Generate leaflet map
  map <- leaflet::leaflet() %>%
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>%
    leaflet::addPolygons(
      data = osm_sf,
      fillColor = green_color,
      fillOpacity = 1,
      color = "black",
      weight = 1,
      group = "Green Spaces"
    ) %>%
    leaflet::addPolygons(
      data = iso_map_valid,
      fillColor = ~leaflet::colorNumeric(isochrone_color, iso_map_valid$isomax)(iso_map_valid$isomax),
      fillOpacity = 0.5,
      group = "Isochrones",
      label = ~label,  # Add this line to include labels
      layerId = ~iso_map_valid$isomax  # Add this line to assign an id to each layer
    ) %>%
    leaflet::addCircles(
      data = sf::st_coordinates(specified_location),
      radius = 10,
      color = location_color,
      fillOpacity = 1,
      group = "Specified Location"
    ) %>%
    leaflet::addLayersControl(
      overlayGroups = c("Green Spaces", "Isochrones", "Specified Location"),
      options = leaflet::layersControlOptions(collapsed = TRUE)
    )

  # Return the map
  return(map)
}
