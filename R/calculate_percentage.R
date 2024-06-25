# Declare global variables to avoid R CMD check warnings
utils::globalVariables(c("green_index", "green_index_category", "n"))

#' Calculate the percentage of edges with their respective green index category
#'
#' This function calculates the percentage of edges within each green index category.
#'
#' @param green_index_data A data frame containing the calculated green index values for each edge.
#' @return A data frame with the percentage of each green index category.
#' @importFrom dplyr mutate group_by summarise n
#' @examples
#' \dontrun{
#' # Generate a sample green_index data frame
#' green_index_data <- data.frame(
#'   green_index = runif(1000)
#' )
#' calculate_percentage(green_index_data)
#' }
#' @export
calculate_percentage <- function(green_index_data) {

  # Error Handling: Check if green_index_data is a data frame
  if (!is.data.frame(green_index_data)) {
    stop("Invalid input: green_index_data must be a data frame.")
  }

  # Remove geometry before processing
  green_index_data <- as.data.frame(sf::st_drop_geometry(green_index_data))

  green_index_data <- green_index_data %>%
    dplyr::mutate(
      green_index_category = cut(green_index,
                                 breaks = c(-Inf, 0.4, 0.7, Inf),
                                 labels = c("<0.4", "0.4-0.7", ">0.7"))
    )

  percentage <- green_index_data %>%
    dplyr::group_by(green_index_category) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(percentage = n / sum(n) * 100)

  return(percentage)
}
