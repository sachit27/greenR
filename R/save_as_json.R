#' Save the green index data as a JSON file
#'
#' This function saves the green index data for all the edges as a JSON file.
#'
#' @param green_index A data frame containing the calculated green index values for each edge.
#' @param file_path The file path where the JSON file will be saved.
#' @return NULL
#' @importFrom jsonlite write_json
#' @examples
#' \dontrun{
#' # Generate a sample green_index data frame
#' green_index <- data.frame(
#'   green_index = runif(1000),
#'   geometry = rep(sf::st_sfc(sf::st_point(c(0, 0))), 1000)
#' )
#' save_json(green_index, "green_index_data.json")
#' }
#' @export
save_json <- function(green_index, file_path) {
  jsonlite::write_json(green_index, file_path)
  invisible()
}
