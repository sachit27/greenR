# Declare global variables to avoid R CMD check warnings
utils::globalVariables(c("geometry", "intersection_area", "coverage_pct"))

#' Visualize Green Space Coverage with Hexagonal Bins
#'
#' Creates a hexagonal binning map to visualize the percentage of green space coverage within a specified area.
#' Users can customize the hexagon size, color palette, and other map features.
#'
#' @param green_areas_data List containing green areas data (obtained from the `get_osm_data` function), default is NULL.
#' @param tree_data List containing tree data (obtained from the `get_osm_data` function), default is NULL.
#' @param hex_size Numeric, size of the hexagons in meters, default is 500.
#' @param color_palette Character, name of the color palette to use, default is "viridis".
#' @param save_path Character, file path to save the map as an HTML file, default is NULL (do not save).
#' @return A list containing a Leaflet map displaying the percentage of green space coverage, and a ggplot2 violin plot.
#' @importFrom sf st_transform st_bbox st_make_grid st_sf st_intersects st_union st_area st_intersection st_join st_buffer st_make_valid
#' @importFrom leaflet leaflet addTiles addPolygons addLegend addLayersControl colorNumeric addProviderTiles
#' @importFrom ggplot2 ggplot aes geom_violin theme_minimal labs annotate
#' @importFrom htmlwidgets saveWidget
#' @importFrom dplyr filter group_by summarise coalesce
#' @importFrom units set_units
#' @importFrom stats median sd
#' @examples
#' \donttest{
#'   data <- get_osm_data("City of London, United Kingdom")
#'   green_areas_data <- data$green_areas
#'   tree_data <- data$trees
#'   hex_map <- hexGreenSpace(green_areas_data, tree_data, hex_size = 300)
#'   print(hex_map$map) # Display the hex bin map
#'   print(hex_map$violin) # Display the violin plot
#' }
#' @export
hexGreenSpace <- function(green_areas_data = NULL, tree_data = NULL, hex_size = 500, color_palette = "viridis", save_path = NULL) {
  if (is.null(green_areas_data) & is.null(tree_data)) {
    stop("At least one of green_areas_data or tree_data must be provided.")
  }

  hex_size <- units::set_units(hex_size, "meters")

  hex_grid <- NULL

  # Process green areas data
  if (!is.null(green_areas_data)) {
    if (!inherits(green_areas_data$osm_polygons, "sf")) {
      stop("green_areas_data$osm_polygons should be an 'sf' object.")
    }

    green_areas <- green_areas_data$osm_polygons
    green_areas <- sf::st_transform(green_areas, 3857)
    green_areas <- sf::st_make_valid(green_areas)

    if (is.null(hex_grid)) {
      hex_grid <- sf::st_make_grid(green_areas, cellsize = hex_size, square = FALSE)
      hex_grid <- sf::st_sf(geometry = hex_grid)
      hex_grid$area <- as.numeric(sf::st_area(hex_grid))
    }

    green_intersections <- sf::st_intersection(hex_grid, green_areas)
    green_intersections$intersection_area <- as.numeric(sf::st_area(green_intersections))

    green_coverage_df <- green_intersections %>%
      dplyr::group_by(geometry) %>%
      dplyr::summarise(total_green_area = sum(intersection_area, na.rm = TRUE))

    hex_grid <- sf::st_join(hex_grid, green_coverage_df, join = sf::st_intersects)
  }

  # Process tree data
  if (!is.null(tree_data)) {
    if (!inherits(tree_data$osm_points, "sf")) {
      stop("tree_data$osm_points should be an 'sf' object.")
    }

    trees <- tree_data$osm_points
    trees <- sf::st_transform(trees, 3857)
    trees <- sf::st_make_valid(trees)

    if (is.null(hex_grid)) {
      hex_grid <- sf::st_make_grid(trees, cellsize = hex_size, square = FALSE)
      hex_grid <- sf::st_sf(geometry = hex_grid)
      hex_grid$area <- as.numeric(sf::st_area(hex_grid))
    }

    tree_buffers <- sf::st_buffer(trees, dist = 5) # Assuming 5 meters radius for tree canopy
    tree_intersections <- sf::st_intersection(hex_grid, tree_buffers)
    tree_intersections$intersection_area <- as.numeric(sf::st_area(tree_intersections))

    tree_coverage_df <- tree_intersections %>%
      dplyr::group_by(geometry) %>%
      dplyr::summarise(total_tree_area = sum(intersection_area, na.rm = TRUE))

    hex_grid <- sf::st_join(hex_grid, tree_coverage_df, join = sf::st_intersects)
  }

  hex_grid$total_intersection_area <- dplyr::coalesce(hex_grid$total_green_area, 0) + dplyr::coalesce(hex_grid$total_tree_area, 0)
  hex_grid$coverage_pct <- (hex_grid$total_intersection_area / hex_grid$area) * 100
  hex_grid$coverage_pct[is.na(hex_grid$coverage_pct)] <- 0

  hex_grid <- sf::st_transform(hex_grid, 4326)

  # Create Leaflet map
  map <- leaflet::leaflet() %>%
    leaflet::addTiles(group = "OSM") %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Positron") %>%
    leaflet::addPolygons(
      data = hex_grid,
      fillColor = ~leaflet::colorNumeric(color_palette, hex_grid$coverage_pct)(hex_grid$coverage_pct),
      fillOpacity = 0.8,
      color = "black",
      weight = 1,
      group = "Hex Bins",
      popup = ~paste("Coverage: ", round(hex_grid$coverage_pct, 2), "%")
    ) %>%
    leaflet::addLegend(
      "bottomright",
      pal = leaflet::colorNumeric(color_palette, hex_grid$coverage_pct),
      values = hex_grid$coverage_pct,
      title = "Percentage of Green Space Coverage",
      opacity = 0.7
    ) %>%
    leaflet::addLayersControl(
      baseGroups = c("OSM", "Positron"),
      overlayGroups = c("Hex Bins"),
      options = leaflet::layersControlOptions(collapsed = TRUE)
    )

  # Create violin plot
  coverage_df <- as.data.frame(hex_grid$coverage_pct)
  colnames(coverage_df) <- c("coverage_pct")

  violin_plot <- ggplot2::ggplot(coverage_df, ggplot2::aes(x = factor(1), y = coverage_pct)) +
    ggplot2::geom_violin(fill = "lightblue", color = "black") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Percentage of Green Space Coverage per Hexagon", x = "Hexagon", y = "Coverage (%)") +
    ggplot2::annotate("text", x = 1, y = max(coverage_df$coverage_pct), label = sprintf("Mean: %.2f%%\nMedian: %.2f%%\nStd Dev: %.2f%%",
                                                                                        mean(coverage_df$coverage_pct),
                                                                                        stats::median(coverage_df$coverage_pct),
                                                                                        stats::sd(coverage_df$coverage_pct)),
                      hjust = 1.1, vjust = 1.1, size = 3, color = "blue")

  # Print summary statistics
  message("Average percentage of green space coverage per hexagon: ", mean(coverage_df$coverage_pct), "%")

  # Save map as HTML if save_path is provided
  if (!is.null(save_path)) {
    htmlwidgets::saveWidget(map, file = save_path)
    message("Map saved to: ", save_path)
  }

  return(list(map = map, violin = violin_plot))
}
