plot_green_index <- function(green_index) {
  library(ggplot2)
  library(sf)  # Add this line

  # Debugging prints
  print(head(green_index))  # Print the first few rows of green_index

  # Define the color range for the gradient
  colors <- c("#F0BB62", "#BFDB38", "#367E18")
  edges_sf <- sf::st_as_sf(green_index)

  # Debugging prints
  print(class(edges_sf))  # Print the class of edges_sf
  print(names(edges_sf))  # Print the column names of edges_sf

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
