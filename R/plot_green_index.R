#' Plot the green index
#'
#' This function plots the green index for the highway network.
#'
#' @param green_index A data frame containing the calculated green index values for each edge.
#' @return NULL
#' @importFrom ggplot2 ggplot geom_sf scale_color_gradientn theme_minimal theme element_text aes
#' @importFrom sf st_as_sf
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
plot_green_index <- function(green_index) {

  # Define the color range for the gradient
  colors <- c("#F0BB62", "#BFDB38", "#367E18")
  edges_sf <- sf::st_as_sf(green_index)
  text_size <- 14
  resolution <- 350

  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = edges_sf, ggplot2::aes(color = green_index), size = 0.5) +
    ggplot2::scale_color_gradientn(colors = colors) +
    ggplot2::theme_minimal() +
    ggplot2::theme(text = ggplot2::element_text(size = text_size))

  print(plot)
  invisible()
}
