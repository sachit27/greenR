# Declare global variables to avoid R CMD check warnings
utils::globalVariables(c("sd"))

#' Green Space Similarity Index (GSSI)
#'
#' This function calculates the Green Space Similarity Index (GSSI) for a list of cities,
#' based on the variability of green space sizes and their connectivity.
#' The function uses the `spatstat` package to calculate proximity measures and combines these
#' with area-based metrics to form the GSSI. The index is useful for comparing urban green
#' spaces across different cities.
#'
#' @param green_spaces_list A list of 'sf' objects, each representing the green spaces in a city.
#' @param equal_area_crs A character string representing an equal-area CRS for accurate area measurement.
#'                         Default is "ESRI:54009".
#' @return A numeric vector of normalized GSSI values for each city.
#' @importFrom sf st_as_sf st_transform st_area st_bbox st_coordinates
#' @importFrom spatstat.geom as.ppp nndist owin
#' @importFrom stats sd
#' @examples
#' \donttest{
#' d1 <- get_osm_data("New Delhi, India")
#' dsf <- d1$green_areas$osm_polygons
#' d2 <- get_osm_data("Basel, Switzerland")
#' bsf <- d2$green_areas$osm_polygons
#' d3 <- get_osm_data("Medellin, Colombia")
#' msf <- d3$green_areas$osm_polygons
#' cities_data <- list(dsf, bsf, msf)
#' gssi_values <- gssi(cities_data)
#' }
#' @export
gssi <- function(green_spaces_list, equal_area_crs = "ESRI:54009") {
  # Validate inputs
  if (!is.list(green_spaces_list)) {
    stop("green_spaces_list must be a list.", call. = FALSE)
  }

  if (!all(sapply(green_spaces_list, inherits, "sf"))) {
    stop("All items in green_spaces_list must be sf objects.", call. = FALSE)
  }

  if (!is.character(equal_area_crs)) {
    stop("equal_area_crs must be a character string representing a CRS.", call. = FALSE)
  }

  ggssi_values <- numeric(length(green_spaces_list))

  for (i in seq_along(green_spaces_list)) {
    green_areas <- sf::st_as_sf(green_spaces_list[[i]])

    # Check for empty or single-element green areas
    if (nrow(green_areas) < 2) {
      message("Skipping city ", i, " - not enough green areas (less than 2)")
      ggssi_values[i] <- NA
      next
    }

    # Transform and calculate area
    green_areas_transformed <- sf::st_transform(green_areas, crs = equal_area_crs)
    green_areas$area <- as.numeric(sf::st_area(green_areas_transformed))

    # Area-based metrics
    mean_area <- mean(green_areas$area)
    sd_area <- sd(green_areas$area)
    coefficient_of_variation_area <- sd_area / mean_area

    # Proximity calculation using spatstat
    coords <- sf::st_coordinates(green_areas_transformed)
    bbox <- sf::st_bbox(green_areas_transformed)
    window <- spatstat.geom::owin(xrange = bbox[c(1, 3)], yrange = bbox[c(2, 4)])
    ppp_object <- spatstat.geom::as.ppp(coords, W = window)
    nn_distances <- spatstat.geom::nndist(ppp_object)
    avg_nn_distance <- mean(nn_distances)

    # Combine area and proximity metrics
    combined_metric <- (1 / coefficient_of_variation_area) * (1 / avg_nn_distance)
    ggssi_values[i] <- combined_metric
  }

  # Normalize GGSSI values
  max_ggssi <- max(ggssi_values, na.rm = TRUE)
  if (max_ggssi == 0 || is.infinite(max_ggssi)) {
    message("No valid GGSSI values calculated")
    return(NA)
  }

  normalized_ggssi <- ggssi_values / max_ggssi

  return(normalized_ggssi)
}
