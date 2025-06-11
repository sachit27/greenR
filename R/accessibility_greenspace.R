#' Generate Accessibility Map for Green Spaces and Export Data
#'
#' This function generates a leaflet map that shows green spaces accessible within
#' a specified walking time from a given location. It also exports the spatial data
#' as a geopackage file for use in GIS software like QGIS.
#'
#' Note: This function requires an OSRM server for isochrone computation. By default,
#' it uses the public OSRM API, which requires internet access. During CRAN checks
#' and non-interactive sessions, the function will halt to prevent unintended web requests.
#'
#' @param green_area_data A list containing green area data, usually obtained from the \code{get_osm_data} function.
#' @param location_lat Numeric latitude of the specified location.
#' @param location_lon Numeric longitude of the specified location.
#' @param max_walk_time Maximum walking time in minutes. Default is 15.
#' @param green_color Color for the green areas on the map. Default is "green".
#' @param location_color Color for the specified location on the map. Default is "blue".
#' @param isochrone_color Color palette for the isochrone lines. Default is "viridis".
#' @param output_file Path and filename for the output geopackage. If NULL (default), no file is exported.
#'
#' @return A list containing a leaflet map object and the spatial data (sf objects).
#' @importFrom sf st_as_sf st_transform st_make_valid st_coordinates st_write
#' @importFrom leaflet leaflet addProviderTiles addPolygons addCircles addLegend addLayersControl
#' @importFrom osrm osrmIsochrone
#' @importFrom tibble tibble
#' @importFrom viridisLite viridis
#' @examples
#' \dontrun{
#'   # First, get OSM data (this requires internet connection)
#'   osm_data <- get_osm_data("Lausanne, Switzerland")
#'
#'   # Now use the green areas data in the accessibility function
#'   result <- accessibility_greenspace(
#'     green_area_data = osm_data$green_areas,
#'     location_lat = 46.5196,
#'     location_lon = 6.6322,
#'     output_file = tempfile(fileext = ".gpkg")
#'   )
#'
#'   # View the leaflet map
#'   result$map
#'
#'   # Check the structure of the returned data
#'   str(result, max.level = 1)
#' }
#' @export
accessibility_greenspace <- function(green_area_data, location_lat, location_lon,
                                     max_walk_time = 15, green_color = "green",
                                     location_color = "blue", isochrone_color = "viridis",
                                     output_file = NULL) {

  # Prevent unintentional internet use in CRAN checks or non-interactive sessions
  if (!interactive() && is.null(getOption("osrm.server"))) {
    stop("accessibility_greenspace() requires an OSRM server. By default, it uses a public OSRM API, which is not allowed during CRAN checks or in non-interactive sessions. Please run interactively or set up a local OSRM server via options(osrm.server = 'http://localhost:5000/').")
  }

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
  iso_map <- osrm::osrmIsochrone(
    loc = specified_location,
    breaks = seq(from = 0, to = max_walk_time, by = 5),
    osrm.profile = "foot"
  )

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
      label = ~label,
      layerId = ~iso_map_valid$isomax
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

  # Export data as geopackage if output_file is provided
  if (!is.null(output_file)) {
    sf::st_write(osm_sf, output_file, layer = "green_spaces", append = FALSE)
    sf::st_write(iso_map_valid, output_file, layer = "isochrones", append = TRUE)
    sf::st_write(specified_location, output_file, layer = "location", append = TRUE)
    message(paste("Data exported to", output_file))
  }

  # Return both the map and the spatial data
  return(list(
    map = map,
    green_spaces = osm_sf,
    isochrones = iso_map_valid,
    location = specified_location
  ))
}
