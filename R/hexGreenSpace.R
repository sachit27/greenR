#' Visualize Green Space Concentration with Hexagonal Bins Using Centroids
#'
#' Creates a hexagonal binning map to visualize the concentration of green spaces within a specified area based on centroids.
#' Users can customize the hexagon size, color palette, and other map features. It also generates a violin plot displaying
#' the distribution of green spaces per hexagon.
#'
#' @param green_areas_data List containing green areas data (obtained from the `get_osm_data` function).
#' @param hex_size Numeric, size of the hexagons in meters, default is 1000.
#' @param color_palette Character, name of the color palette to use, default is "viridis".
#' @param save_path Character, file path to save the map as an HTML file, default is NULL (do not save).
#' @return A list containing:
#'   - map: A Leaflet map displaying the concentration of green spaces.
#'   - violin: A ggplot2 object for the violin plot of green spaces distribution per hexagon.
#' @importFrom sf st_transform st_centroid st_bbox st_make_grid st_sf st_intersects
#' @importFrom leaflet leaflet addTiles addPolygons addLegend addLayersControl
#' @importFrom htmlwidgets saveWidget
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot geom_violin geom_boxplot aes labs theme stat_summary
#' @importFrom stats mean sd
#' @examples
#' \dontrun{
#'   data <- get_osm_data("Fulham, London, United Kingdom")
#'   green_areas_data <- data$green_areas
#'   hex_map <- hexGreenSpace(green_areas_data, hex_size = 300)
#'   print(hex_map$map) # Display the map
#'   print(hex_map$violin) # Display the violin plot
#' }
#' @export
hexGreenSpace <- function(green_areas_data, hex_size = 1000, color_palette = "viridis", save_path = NULL) {
  if (!inherits(green_areas_data$osm_polygons, "sf")) {
    stop("green_areas_data$osm_polygons should be an 'sf' object.")
  }

  green_areas_data <- sf::st_transform(green_areas_data$osm_polygons, 3857)
  centroids <- sf::st_centroid(green_areas_data)
  bbox <- sf::st_bbox(centroids)
  hex_grid <- sf::st_make_grid(centroids, cellsize = hex_size, square = FALSE)
  hex_grid <- sf::st_sf(geometry = hex_grid)
  hex_counts <- sf::st_intersects(hex_grid, centroids)
  hex_grid$counts <- lengths(hex_counts)
  hex_grid <- sf::st_transform(hex_grid, 4326)

  # Calculate summary statistics
  total_green_spaces <- sum(hex_grid$counts)
  avg_green_spaces_per_hex <- mean(hex_grid$counts)

  # Create Leaflet map
  map <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(
      data = hex_grid,
      fillColor = ~leaflet::colorNumeric(color_palette, hex_grid$counts)(hex_grid$counts),
      fillOpacity = 0.5,
      color = "black",
      weight = 1,
      group = "Hex Bins",
      popup = ~paste("Count: ", hex_grid$counts)
    ) %>%
    leaflet::addLegend(
      "bottomright",
      pal = leaflet::colorNumeric(color_palette, hex_grid$counts),
      values = hex_grid$counts,
      title = "Number of Green Spaces",
      opacity = 0.5
    ) %>%
    leaflet::addLayersControl(
      overlayGroups = c("Hex Bins"),
      options = leaflet::layersControlOptions(collapsed = TRUE)
    )

  if (!is.null(save_path)) {
    htmlwidgets::saveWidget(map, file = save_path)
  }

  # Create violin plot
  violin_plot <- ggplot2::ggplot(hex_grid, ggplot2::aes(x = factor(1), y = counts)) +
    ggplot2::geom_violin(fill = "skyblue", alpha = 0.7) +
    ggplot2::geom_boxplot(width = 0.1, fill = "white", color = "black") +
    ggplot2::labs(title = "Distribution of Green Spaces per Hexagon",
                  x = "Hexagons",
                  y = "Number of Green Spaces") +
    ggplot2::theme_minimal() +
    ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "blue")

  cat("Total number of green spaces:", total_green_spaces, "\n")
  cat("Average number of green spaces per hexagon:", avg_green_spaces_per_hex, "\n")

  return(list(map = map, violin = violin_plot))
}
