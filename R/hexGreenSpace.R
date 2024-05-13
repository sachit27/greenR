#' Visualize Green Space Concentration with Hexagonal Bins Using Centroids
#'
#' Creates a hexagonal binning map to visualize the concentration of green spaces within a specified area based on centroids.
#' Users can customize the hexagon size, color palette, and other map features.
#'
#' @param green_areas_data List containing green areas data (obtained from the `get_osm_data` function).
#' @param hex_size Numeric, size of the hexagons in meters, default is 500.
#' @param color_palette Character, name of the color palette to use, default is "viridis".
#' @param save_path Character, file path to save the map as an HTML file, default is NULL (do not save).
#' @return A Leaflet map displaying the concentration of green spaces.
#' @importFrom sf st_transform st_centroid st_bbox st_make_grid st_sf st_intersects
#' @importFrom leaflet leaflet addTiles addPolygons addLegend addLayersControl
#' @importFrom htmlwidgets saveWidget
#' @importFrom dplyr filter
#' @examples
#' \dontrun{
#'   data <- get_osm_data("Fulham, London, United Kingdom")
#'   green_areas_data <- data$green_areas
#'   hex_map <- hexGreenSpace(green_areas_data, hex_size = 300)
#'   print(hex_map) # Display the map
#' }
#' @export
hexGreenSpace <- function(green_areas_data, hex_size = 500, color_palette = "viridis", save_path = NULL) {
  # Ensure data is in the correct format
  if (!inherits(green_areas_data$osm_polygons, "sf")) {
    stop("green_areas_data$osm_polygons should be an 'sf' object.")
  }

  # Transform to Web Mercator for visualization
  green_areas_data <- st_transform(green_areas_data$osm_polygons, 3857)

  # Calculate centroids of green areas
  centroids <- st_centroid(green_areas_data)

  # Create hexagonal grid
  bbox <- st_bbox(centroids)
  hex_grid <- st_make_grid(centroids, cellsize = hex_size, square = FALSE)
  hex_grid <- st_sf(geometry = hex_grid)

  # Count number of centroids in each hexagon
  hex_counts <- st_intersects(hex_grid, centroids)
  hex_grid$counts <- lengths(hex_counts)

  # Transform hex grid back to WGS 84 for Leaflet
  hex_grid <- st_transform(hex_grid, 4326)

  # Calculate summary statistics
  total_green_spaces <- sum(hex_grid$counts)
  avg_green_spaces_per_hex <- mean(hex_grid$counts)

  # Create Leaflet map
  map <- leaflet() %>%
    addTiles() %>%
    addPolygons(
      data = hex_grid,
      fillColor = ~colorNumeric(color_palette, hex_grid$counts)(hex_grid$counts),
      fillOpacity = 0.7,
      color = "black",
      weight = 1,
      group = "Hex Bins",
      popup = ~paste("Count: ", hex_grid$counts)
    ) %>%
    addLegend(
      "bottomright",
      pal = colorNumeric(color_palette, hex_grid$counts),
      values = hex_grid$counts,
      title = "Number of Green Spaces",
      opacity = 0.7
    ) %>%
    addLayersControl(
      overlayGroups = c("Hex Bins"),
      options = layersControlOptions(collapsed = TRUE)
    )

  # Print summary statistics
  cat("Total number of green spaces:", total_green_spaces, "\n")
  cat("Average number of green spaces per hexagon:", avg_green_spaces_per_hex, "\n")

  # Save map as HTML if save_path is provided
  if (!is.null(save_path)) {
    library(htmlwidgets)
    saveWidget(map, file = save_path)
    cat("Map saved to:", save_path, "\n")
  }

  return(map)
}
