#' Plot the green index
#'
#' This function plots the green index for the highway network.
#'
#' @param green_index A data frame containing the calculated green index values for each edge.
#' @return NULL
#' @examples
#' plot_green_index(green_index)
plot_green_index <- function(green_index) {
  library(ggplot2)

  # Define the color range for the gradient
  colors <- c("#F0BB62", "#BFDB38", "#367E18")
  edges_sf <- sf::st_as_sf(green_index)
  text_size <- 14
  resolution <- 350

  plot <- ggplot() +
    geom_sf(data = edges_sf, aes(color = green_index), size = 0.5) +
    scale_color_gradientn(colors = colors) +
    theme_minimal() +
    theme(text = element_text(size = text_size))

  print(plot)
  invisible()
}

