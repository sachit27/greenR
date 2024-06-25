# Declare global variables to avoid R CMD check warnings
utils::globalVariables(c("green_index", "coverage_pct", "geometry", "green_index_category",
                         "green_index_green_area", "green_index_tree", "intersection_area",
                         "landuse", "leisure", "median", "n", "osm_id", "sd"))

#' Plot the green index
#'
#' This function plots the green index for the highway network with extensive customization options.
#' Users can set various parameters like text size, color palette, resolution, base map, line width, line type, and more.
#'
#' @param green_index_data A data frame containing the calculated green index values for each edge.
#' @param base_map Character, base map to use. Default is "CartoDB.DarkMatter".
#' Other options include "Stamen.Toner", "CartoDB.Positron", "Esri.NatGeoWorldMap",
#' "MtbMap", "Stamen.TonerLines", and "Stamen.TonerLabels".
#' @param colors Character vector, colors for the gradient. Default is c("#F0BB62", "#BFDB38", "#367E18").
#' @param text_size Numeric, size of the text in the plot. Default is 12.
#' @param resolution Numeric, resolution of the plot. Default is 350.
#' @param title Character, title for the plot. Default is NULL.
#' @param xlab Character, x-axis label for the plot. Default is NULL.
#' @param ylab Character, y-axis label for the plot. Default is NULL.
#' @param legend_title Character, legend title for the plot. Default is "Green_Index".
#' @param legend_position Character, legend position for the plot. Default is "right".
#' @param theme ggplot theme object, theme for the plot. Default is ggplot2::theme_minimal().
#' @param line_width Numeric, width of the line for the edges. Default is 0.8.
#' @param line_type Character or numeric, type of the line for the edges. Default is "solid".
#' @param interactive Logical, whether to return an interactive plot using leaflet. Default is FALSE.
#' @param filename Character, filename to save the plot. Supported formats include HTML. Default is NULL (no file saved).
#'
#' @return If `interactive = TRUE`, returns a Leaflet map object. If `interactive = FALSE`, returns a ggplot object.
#' If a filename is provided, saves the plot to the specified file.
#'
#' @importFrom leaflet leaflet addProviderTiles addPolylines addLayersControl colorBin layersControlOptions
#' @importFrom sf st_as_sf st_transform
#' @importFrom htmlwidgets saveWidget
#' @export
plot_green_index <- function(green_index_data,
                             base_map = "CartoDB.DarkMatter",
                             colors = c("#F0BB62", "#BFDB38", "#367E18"),
                             text_size = 12,
                             resolution = 350,
                             title = NULL,
                             xlab = NULL,
                             ylab = NULL,
                             legend_title = "Green_Index",
                             legend_position = "right",
                             theme = ggplot2::theme_minimal(),
                             line_width = 0.8,
                             line_type = "solid",
                             interactive = FALSE,
                             filename = NULL) {

  # Convert data to sf object and transform to long-lat
  edges_sf <- sf::st_as_sf(green_index_data)
  edges_sf <- sf::st_transform(edges_sf, 4326)  # Transform to WGS 84 (long-lat)

  # Create a color palette function
  color_palette <- colorBin(palette = colors, domain = green_index_data$green_index)

  if (interactive) {
    plot <- leaflet(data = edges_sf) %>%
      addProviderTiles("CartoDB.DarkMatter", group = "Dark Matter") %>%
      addProviderTiles("CartoDB.Positron", group = "Positron") %>%
      addPolylines(color = ~color_palette(green_index_data$green_index), weight = line_width) %>%
      addLayersControl(baseGroups = c("Dark Matter", "Positron"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(pal = color_palette, values = ~green_index_data$green_index,
                title = legend_title, position = "bottomright")

    # Save the interactive map to a file if a filename is provided
    if (!is.null(filename)) {
      htmlwidgets::saveWidget(plot, file = filename)
    }

    return(plot)

  } else {
    # Create a ggplot for static plot
    plot <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = edges_sf, ggplot2::aes(color = green_index), lwd = line_width, linetype = line_type) +
      ggplot2::scale_color_gradientn(colors = colors) +
      theme +
      ggplot2::theme(text = ggplot2::element_text(size = text_size)) +
      ggplot2::labs(title = title, x = xlab, y = ylab, color = legend_title) +
      ggplot2::theme(legend.position = legend_position, axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    # Print the static plot
    print(plot)

    # Save the static plot if filename is provided
    if (!is.null(filename)) {
      ggplot2::ggsave(filename, plot, dpi = resolution)
    }
  }

  invisible()
}
