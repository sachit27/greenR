#' Create Green Space Accessibility Visualizations
#'
#' Generates static and interactive visualizations for green space accessibility,
#' including distance maps, coverage plots (spatial and population-weighted), and a radar plot
#' with inside y-axis tick labels. Provides an interactive leaflet map with base and overlay controls.
#'
#' @param accessibility_analysis Output from \code{analyze_green_accessibility()}.
#' @param green_areas An `sf` object of green areas (e.g., OSM polygons).
#' @param mode Character. Mode to plot (for multi-mode results).
#'
#' @return A list with:
#' \describe{
#'   \item{distance_map}{ggplot map of grid distance to green space.}
#'   \item{coverage_plot}{Barplot of spatial and/or population-weighted coverage.}
#'   \item{directional_plot}{Radar plot for directional coverage (with y-axis/radius labels inside at N).}
#'   \item{combined_plot}{Patchwork combination of all static plots.}
#'   \item{leaflet_map}{Interactive leaflet map with overlays.}
#'   \item{summary}{Character summary of statistics.}
#'   \item{directional_table}{Table of directional mean coverage values.}
#'   \item{data}{Underlying data used for plotting.}
#' }
#' @import ggplot2
#' @importFrom sf st_geometry<- st_geometry st_transform st_coordinates st_union st_centroid
#' @importFrom patchwork plot_layout
#' @importFrom leaflet leaflet addProviderTiles addPolygons addLayersControl addLegend setView colorNumeric
#' @examples
#' \dontrun{
#' result <- analyze_green_accessibility(
#'   network_data = data$highways$osm_lines,
#'   green_areas = data$green_areas$osm_polygons,
#'   mode = "walking",
#'   grid_size = 300,
#'   population_raster = pop_raster_raw
#' )
#' viz <- create_accessibility_visualizations(result, data$green_areas$osm_polygons, mode = "walking")
#' print(viz$distance_map)
#' print(viz$coverage_plot)
#' print(viz$directional_plot)
#' print(viz$combined_plot)
#' viz$leaflet_map  # View in RStudio Viewer
#' cat(viz$summary)
#' print(viz$directional_table)
#' }
#' @export
create_accessibility_visualizations <- function(
    accessibility_analysis,
    green_areas,
    mode = "walking"
) {
  # --- 1. Handle single-mode and multi-mode results ---
  results <- if (!is.null(accessibility_analysis[[mode]])) {
    accessibility_analysis[[mode]]
  } else {
    accessibility_analysis
  }
  if (is.null(results$grid)) stop("Grid data missing in accessibility results.")
  grid <- results$grid
  if (!inherits(grid, "sf")) stop("Grid must be an sf object.")
  if (!"distance" %in% names(grid)) stop("Missing 'distance' column in grid.")
  sf::st_geometry(grid) <- "x"
  summary_stats <- results$summary

  # --- 2. Distance-based accessibility map (static) ---
  distance_map <- ggplot() +
    geom_sf(data = grid, aes(fill = distance), color = NA) +
    geom_sf(data = green_areas, fill = "darkgreen", alpha = 0.2, color = "forestgreen") +
    scale_fill_viridis_c(
      name = "Distance\nto Nearest\nGreen Space (m)",
      option = "magma", direction = -1
    ) +
    theme_minimal(base_size = 13) +
    labs(
      title = paste("Green Space Accessibility -", tools::toTitleCase(mode))
    )

  # --- 3. Coverage plot (spatial and population-weighted if available) ---
  pop_cov <- (!is.null(summary_stats$pop_coverage_400m) && !is.null(summary_stats$pop_coverage_800m))
  if (pop_cov) {
    coverage_data <- data.frame(
      Threshold = rep(c("400m", "800m"), 2),
      Coverage = c(summary_stats$coverage_400m, summary_stats$coverage_800m,
                   summary_stats$pop_coverage_400m, summary_stats$pop_coverage_800m),
      Type = rep(c("Spatial", "Population-weighted"), each = 2)
    )
    coverage_plot <- ggplot(coverage_data, aes(x = Threshold, y = Coverage, fill = Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = sprintf("%.1f%%", Coverage)), vjust = -0.5, position = position_dodge(.9)) +
      scale_fill_manual(values = c("Spatial" = "steelblue", "Population-weighted" = "firebrick")) +
      ylim(0, 100) +
      theme_minimal(base_size = 13) +
      labs(
        title = "Green Space Coverage by Distance",
        subtitle = "Spatial (cells) and population-weighted",
        x = "Distance Threshold", y = "Coverage (%)"
      )
  } else {
    coverage_data <- data.frame(
      Threshold = c("400m", "800m"),
      Coverage = c(summary_stats$coverage_400m, summary_stats$coverage_800m)
    )
    coverage_plot <- ggplot(coverage_data, aes(x = Threshold, y = Coverage)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = sprintf("%.1f%%", Coverage)), vjust = -0.5) +
      ylim(0, 100) +
      theme_minimal(base_size = 13) +
      labs(
        title = "Green Space Coverage by Distance (Spatial)",
        x = "Distance Threshold", y = "Coverage (%)"
      )
  }

  # --- 4. Directional radar plot (dynamic scaling, radius labels inside at N, no direction value labels) ---
  directional_stats <- summary_stats$directional_stats[[1]]
  compass_order <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  directional_stats_closed <- rbind(directional_stats, directional_stats[1,])
  directional_stats_closed$direction <- factor(
    directional_stats_closed$direction, levels = compass_order
  )

  # Dynamic y-axis scaling with margin
  min_y <- min(directional_stats$mean_coverage, na.rm = TRUE)
  max_y <- max(directional_stats$mean_coverage, na.rm = TRUE)
  if (abs(max_y - min_y) < 0.15) {
    y_lim <- c(floor(min_y*10)/10, ceiling(max_y*10)/10)
    if (y_lim[1] == y_lim[2]) y_lim <- c(min_y - 0.05, max_y + 0.05)
  } else {
    y_margin <- (max_y - min_y) * 0.1
    y_lim <- c(max(0, min_y - y_margin), min(1, max_y + y_margin))
  }
  breaks <- pretty(y_lim, n = 4)

  # Add y-axis (radius) tick labels at top center ("N") using annotate()
  directional_plot <- ggplot(directional_stats_closed, aes(x = direction, y = mean_coverage, group = 1)) +
    geom_polygon(fill = "blue", alpha = 0.15) +
    geom_line(color = "navy", linewidth = 1.2) +
    geom_point(size = 2, color = "navy") +
    coord_polar() +
    scale_y_continuous(
      limits = y_lim,
      breaks = breaks,
      labels = NULL  # Remove default labels
    ) +
    scale_x_discrete(labels = compass_order) +
    # Manual y-axis (radius) tick labels at 'N'
    annotate(
      "text",
      x = rep("N", length(breaks)),
      y = breaks,
      label = sprintf("%.2f", breaks),
      size = 4, color = "grey30",
      hjust = 0.5, vjust = 1.3  # vjust controls how far inside the circle the label is
    ) +
    theme_minimal(base_size = 14) +
    labs(
      title = "Directional Green Space Coverage",
      subtitle = "Higher = better (closer to 1 means more green coverage)",
      y = NULL, x = NULL,
      caption = "Note: Values shown in table. Higher is better."
    ) +
    theme(
      legend.position = "none",
      panel.grid.major = element_line(linetype = "dashed", color = "grey80"),
      axis.text.x = element_text(size = 14, face = "bold", color = "darkgreen"),
      axis.text.y = element_blank(),  # Remove default axis text
      plot.caption = element_text(size = 10, face = "italic", hjust = 0)
    )

  # --- 5. Interactive leaflet map: add population, access, and green overlays ---
  grid_ll <- sf::st_transform(grid, 4326)
  green_areas_ll <- sf::st_transform(green_areas, 4326)
  has_pop <- "population" %in% names(grid_ll) && any(!is.na(grid_ll$population))

  leaflet_map <- leaflet::leaflet(grid_ll) %>%
    leaflet::addProviderTiles("CartoDB.Positron", group = "Positron") %>%
    leaflet::addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetMap") %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    # Distance (green access) layer
    leaflet::addPolygons(
      color = ~leaflet::colorNumeric("magma", grid_ll$distance, reverse = TRUE)(grid_ll$distance),
      fillOpacity = 0.6, weight = 0.3, smoothFactor = 0.5,
      popup = ~paste0("<b>Distance:</b> ", round(distance, 1), " m",
                      if (has_pop) paste0("<br><b>Population:</b> ", round(population)) else ""),
      group = "Green Access"
    ) %>%
    # Population layer
    { if (has_pop) leaflet::addPolygons(
      .,
      fillColor = ~leaflet::colorNumeric("YlOrRd", grid_ll$population, na.color = "#00000000")(grid_ll$population),
      color = "#444444", weight = 0.2, fillOpacity = 0.7,
      popup = ~paste0("<b>Population:</b> ", round(population)),
      group = "Population"
    ) else . } %>%
    # Green area polygons
    leaflet::addPolygons(
      data = green_areas_ll, color = "darkgreen", fillOpacity = 0.4,
      weight = 1, group = "Green Areas",
      popup = "Green Space"
    ) %>%
    leaflet::addLayersControl(
      baseGroups = c("Positron", "OpenStreetMap", "Satellite"),
      overlayGroups = c("Green Access", "Population", "Green Areas"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) %>%
    leaflet::addLegend(
      pal = leaflet::colorNumeric("magma", grid_ll$distance, reverse = TRUE),
      values = grid_ll$distance,
      title = "Distance to Nearest Green (m)",
      position = "bottomright",
      group = "Green Access"
    ) %>%
    # Population legend only if present
    { if (has_pop) leaflet::addLegend(
      ., pal = leaflet::colorNumeric("YlOrRd", grid_ll$population, na.color = "#00000000"),
      values = grid_ll$population,
      title = "Population (per cell)",
      position = "bottomleft",
      group = "Population"
    ) else . } %>%
    leaflet::setView(
      lng = mean(sf::st_coordinates(sf::st_centroid(sf::st_union(grid_ll)))[,1]),
      lat = mean(sf::st_coordinates(sf::st_centroid(sf::st_union(grid_ll)))[,2]),
      zoom = 13
    )

  # --- 6. Patchwork all plots, but return individually as well ---
  combined_plot <- (distance_map + coverage_plot) / directional_plot +
    patchwork::plot_layout(heights = c(2, 1))

  # --- 7. Summary text and directional coverage table ---
  pop_text <- if (pop_cov) {
    sprintf(
      "- Pop-weighted mean distance: %.1f m\n- Pop-weighted coverage within 400m: %.1f%%\n- Pop-weighted coverage within 800m: %.1f%%\n",
      summary_stats$pop_weighted_mean_distance,
      summary_stats$pop_coverage_400m,
      summary_stats$pop_coverage_800m
    )
  } else ""
  summary_text <- sprintf(
    "Summary Statistics for %s:\n
    - Mean distance to green space: %.1f meters
    - Median distance: %.1f meters
    - Coverage within 400m: %.1f%%
    - Coverage within 800m: %.1f%%
    %s
    - Total cells analyzed: %d
    - Cells with access: %d (%.1f%%)",
    mode,
    summary_stats$mean_distance,
    summary_stats$median_distance,
    summary_stats$coverage_400m,
    summary_stats$coverage_800m,
    pop_text,
    summary_stats$total_cells,
    summary_stats$cells_with_access,
    100 * summary_stats$cells_with_access / summary_stats$total_cells
  )

  # Provide clean directional table as well
  dir_table <- data.frame(Direction = compass_order,
                          Mean_Coverage = round(directional_stats$mean_coverage, 3))

  return(list(
    distance_map = distance_map,
    coverage_plot = coverage_plot,
    directional_plot = directional_plot,
    combined_plot = combined_plot,
    leaflet_map = leaflet_map,
    summary = summary_text,
    directional_table = dir_table,
    data = results
  ))
}
