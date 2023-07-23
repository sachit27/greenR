#' Calculate the percentage of edges with their respective green index category
#'
#' This function calculates the percentage of edges within each green index category.
#'
#' @param green_index A data frame containing the calculated green index values for each edge.
#' @return NULL
#' @examples
#' calculate_percentage(green_index)
calculate_percentage <- function(green_index) {
  library(dplyr)

  green_index <- green_index %>%
    mutate(
      green_index_category = cut(green_index,
                                 breaks = c(-Inf, 0.4, 0.7, Inf),
                                 labels = c("<0.4", "0.4-0.7", ">0.7"))
    )

  percentage <- green_index %>%
    group_by(green_index_category) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(percentage = n / sum(n) * 100)

  print(percentage)
  invisible()
}
