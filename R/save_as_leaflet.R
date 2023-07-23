#' Save the green index data as a Leaflet map in an HTML file
#'
#' This function saves the green index data for the edges as a Leaflet map in an HTML file.
#'
#' @param green_index A data frame containing the calculated green index values for each edge.
#' @param file_path The file path where the HTML file will be saved.
#' @return NULL
#' @examples
#' save_as_leaflet(green_index, "green_index_map.html")
save_as_leaflet <- function(green_index, file_path) {
  library(leaflet)
  library(sf)
  library(htmlwidgets)
  library(leaflet.extras)

  # Convert to an sf object
  green_index <- st_as_sf(green_index)

  # Transform the data to the WGS84 CRS
  edges_sf <- sf::st_transform(green_index, crs = 4326)

  # Create a Leaflet map
  map <- leaflet(data = edges_sf) %>%
    leaflet::addProviderTiles(providers$OpenStreetMap)

  # Add layers of polylines for each level of the green index
  color_palette <- c("#F0BB62", "#BFDB38", "#367E18")
  for (i in 1:length(color_palette)) {
    map <- map %>%
      leaflet::addPolylines(data = edges_sf[edges_sf$green_index == i, ],
                            color = color_palette[i],
                            weight = 1,
                            opacity = 1,
                            label = ~ paste("Green Index:", green_index))
  }

  # Add scale bar
  map <- map %>% leaflet::addScaleBar(position = "bottomleft")

  # Save the map as an HTML file
  htmlwidgets::saveWidget(map, file = file_path)   
  invisible()
}
