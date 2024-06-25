#' Save the green index data as a GeoJSON file
#'
#' This function saves the green index data for all the edges as a GeoJSON file.
#'
#' @param green_index A data frame containing the calculated green index values for each edge.
#' @param file_path The file path where the GeoJSON file will be saved.
#' @return No return value, called for side effects
#' @importFrom sf st_write
#' @examples
#' \dontrun{
#' # Generate a sample green_index data frame
#' green_index <- data.frame(
#'   green_index = runif(1000),
#'   geometry = rep(sf::st_sfc(sf::st_point(c(0, 0))), 1000)
#' )
#' save_json(green_index, "green_index_data.geojson")
#' }
#' @export
save_json <- function(green_index, file_path) {
  # Ensure the data frame is of class 'sf'
  if(!inherits(green_index, "sf")) {
    green_index <- sf::st_as_sf(green_index)
  }
  # Save as GeoJSON
  sf::st_write(green_index, file_path, driver = "GeoJSON")
  invisible()
}
