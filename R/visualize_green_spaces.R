#' Visualize Green Spaces on a Leaflet Map
#'
#' This function visualizes green spaces on a Leaflet map using the green_areas_data obtained from the get_osm_data function.
#' Green spaces are labeled based on their tags and have different colors in the legend. Users can switch the green spaces layer on and off.
#'
#' @param green_areas_data List containing green areas data (obtained from get_osm_data function).
#' @return A Leaflet map displaying green spaces with labels and a legend, with a layer control for toggling the green spaces layer.
#' @importFrom leaflet addTiles addPolygons addLegend addLayersControl
#' @examples
#' \dontrun{
#'   # Assuming you have already obtained green_areas_data using get_osm_data
#'   visualize_green_spaces(green_areas_data)
#' }
#' @export
visualize_green_spaces <- function(green_areas_data) {
  # Check if green_areas_data contains the required components
  if (!is.list(green_areas_data) || !all(c("osm_polygons") %in% names(green_areas_data))) {
    stop("Invalid green_areas_data format. It should be a list containing 'osm_polygons'.")
  }

  # Create a Leaflet map
  map <- leaflet::leaflet() %>%
    leaflet::addTiles() # You can customize the basemap using other leaflet functions

  # Add green areas polygons to the map as a separate layer
  if (!is.null(green_areas_data$osm_polygons)) {
    map <- map %>%
      leaflet::addPolygons(
        data = green_areas_data$osm_polygons,
        fillColor = "green",
        fillOpacity = 0.7,
        color = "black",
        weight = 1,
        group = "Green Spaces"
      )
  }

  # Add a legend
  map <- map %>%
    leaflet::addLegend(
      "bottomright",
      colors = "green", # Legend color
      labels = "Green Space", # Legend label
      opacity = 0.7
    )

  # Add layer control to allow toggling the green spaces layer on and off
  map <- map %>%
    leaflet::addLayersControl(
      overlayGroups = "Green Spaces", # Group name for the green spaces layer
      options = leaflet::layersControlOptions(collapsed = TRUE) # Collapsed layer control
    )

  # Return the Leaflet map
  return(map)
}
