#' Save the green index data as a Leaflet map in an HTML file
#'
#' This function saves the green index data as a Leaflet map in an HTML file.
#'
#' @param edges A data frame containing the calculated green index values for each edge.
#' @param file_path The file path where the HTML file will be saved.
#' @return No return value, called for side effects
#' @importFrom leaflet addTiles addPolylines addLegend leaflet colorNumeric highlightOptions
#' @importFrom sf st_as_sf st_transform
#' @importFrom htmlwidgets saveWidget
#' @examples
#' \dontrun{
#' # Assuming you have already obtained green index data
#' save_as_leaflet(green_index, "green_index_map.html")
#' }
#' @export
save_as_leaflet <- function(edges, file_path) {
  # Convert to an sf object and transform the data to the WGS84 CRS
  edges_sf <- sf::st_as_sf(edges) %>%
    sf::st_transform(crs = 4326)

  # Create a continuous color palette
  color_palette <- leaflet::colorNumeric(palette = c("#F0BB62", "#BFDB38", "#367E18"),
                                         domain = edges_sf$green_index)

  # Create labels
  labels <- sprintf(
    "<strong>Green index:</strong><br/>%.2f",
    edges_sf$green_index
  ) %>% lapply(htmltools::HTML)

  # Define highlighting options
  highlight <- leaflet::highlightOptions(
    weight = 5,
    color = "#666",
    fillOpacity = 0.7,
    bringToFront = TRUE
  )

  # Create a Leaflet map
  map <- leaflet::leaflet(edges_sf) %>%
    leaflet::addTiles() %>%
    leaflet::addPolylines(color = ~color_palette(green_index),
                          weight = 1,
                          opacity = 1,
                          label = ~labels,
                          highlight = highlight,
                          group = "edges") %>%
    leaflet::addLegend("bottomright",
                       pal = color_palette,
                       values = ~green_index,
                       title = "Green Index",
                       opacity = 1)

  # Save the map as an HTML file
  htmlwidgets::saveWidget(map, file = file_path)
  invisible()
}
