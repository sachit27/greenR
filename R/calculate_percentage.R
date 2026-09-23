# Declare global variables to avoid R CMD check warnings
utils::globalVariables(c("green_index", "green_index_category", "n"))

#' Calculate the percentage of edges with their respective green index category
#'
#' This function calculates the percentage of edges within each green index category.
#'
#' @param green_index_data A data frame containing the calculated green index values for each edge.
#' @return A data frame with counts and percentages for all three categories.
#'   Missing or non-finite index values are excluded from the denominator.
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
  if (!is.data.frame(green_index_data) ||
      !"green_index" %in% names(green_index_data) ||
      !is.numeric(green_index_data$green_index))
    stop("green_index_data must contain a numeric green_index column.",
         call. = FALSE)
  values <- green_index_data$green_index
  values <- values[is.finite(values)]
  labels <- c("<0.4", "0.4-0.7", ">0.7")
  category <- cut(values, breaks = c(-Inf, 0.4, 0.7, Inf),
                  labels = labels)
  counts <- as.integer(table(category))
  data.frame(green_index_category = factor(labels, levels = labels),
             n = counts,
             percentage = if (length(values)) counts / length(values) * 100
                          else rep(0, length(labels)))
}
