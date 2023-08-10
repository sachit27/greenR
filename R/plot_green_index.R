#' Plot the green index
#'
#' This function plots the green index for the highway network with extensive customization options.
#' Users can set various parameters like text size, color palette, resolution, theme, line width, line type, and more.
#'
#' @param green_index A data frame containing the calculated green index values for each edge.
#' @param colors Character vector, colors for the gradient. Default is c("#F0BB62", "#BFDB38", "#367E18").
#' @param text_size Numeric, size of the text in the plot. Default is 14.
#' @param resolution Numeric, resolution of the plot. Default is 350.
#' @param title Character, title for the plot. Default is NULL.
#' @param xlab Character, x-axis label for the plot. Default is NULL.
#' @param ylab Character, y-axis label for the plot. Default is NULL.
#' @param legend_title Character, legend title for the plot. Default is "Green_Index".
#' @param legend_position Character, legend position for the plot. Default is "right".
#' @param theme ggplot theme object, theme for the plot. Default is ggplot2::theme_minimal().
#' @param line_width Numeric, width of the line for the edges. Default is 1.
#' @param line_type Character or numeric, type of the line for the edges. Default is "solid".
#' @param interactive Logical, whether to return an interactive plot using plotly. Default is FALSE.
#' @param filename Character, filename to save the plot. Supported formats include png, pdf, jpeg, etc. Default is NULL (no file saved).
#'
#' @importFrom ggplot2 ggplot geom_sf scale_color_gradientn theme_minimal theme element_text aes labs
#' @importFrom sf st_as_sf
#' @importFrom plotly ggplotly
#'
#' @return NULL
#' @examples
#' \dontrun{
#' # Generate a sample green_index data frame
#' green_index <- data.frame(
#'   green_index = runif(1000),
#'   geometry = rep(sf::st_sfc(sf::st_point(c(0, 0))), 1000)
#' )
#' plot_green_index(green_index)
#' }
#' @export
plot_green_index <- function(green_index,
                             colors = c("#F0BB62", "#BFDB38", "#367E18"),
                             text_size = 14,
                             resolution = 350,
                             title = NULL,
                             xlab = NULL,
                             ylab = NULL,
                             legend_title = "Green_Index",
                             legend_position = "right",
                             theme = ggplot2::theme_minimal(),
                             line_width = 0.5,
                             line_type = "solid",
                             interactive = FALSE,
                             filename = NULL) {

  edges_sf <- sf::st_as_sf(green_index)

  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = edges_sf, ggplot2::aes(color = green_index), lwd = line_width, linetype = line_type) +
    ggplot2::scale_color_gradientn(colors = colors) +
    theme +
    ggplot2::theme(text = ggplot2::element_text(size = text_size)) +
    ggplot2::labs(title = title, x = xlab, y = ylab, color = legend_title) +
    ggplot2::theme(legend.position = legend_position)

  # Render interactive plot if requested
  if (interactive) {
    plot <- plotly::ggplotly(plot)
  }

  # Print the plot
  print(plot)

  # Save the plot if filename is provided
  if (!is.null(filename)) {
    ggplot2::ggsave(filename, plot, dpi = resolution)
  }

  invisible()
}
