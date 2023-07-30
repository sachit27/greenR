#' Calculate green index for each edge
#'
#' This function calculates the green index for each edge in the road network.
#'
#' @param osm_data The OpenStreetMap data. This should be a list with three components: 
#'                 highways, green_areas, and trees. Each component should be a spatial data frame. 
#'                 You can use the osmdata package to get the required data.
#' @param crs_code The EPSG code for the Coordinate Reference System (CRS).
#' @param D The decay parameter in the decay function, default is 100.
#' @return A data frame with the green index for each edge.
#' @export
#' @examples
#' \dontrun{
#' # osm_data should be a list with three components: highways, green_areas, and trees
#' # You can use osmdata package to get this data.
#' calculate_green_index(osm_data, 2056, D = 100)
#' }
calculate_green_index <- function(osm_data, crs_code, D = 100) {

  # Extract data from the list
  highways_data <- osm_data$highways
  green_areas_data <- osm_data$green_areas
  trees_data <- osm_data$trees

  edges <- highways_data$osm_lines %>%
    sf::st_transform(crs = crs_code) %>%
    tibble::as_tibble() %>%
    dplyr::select(osm_id, geometry)

  green_areas <- green_areas_data$osm_polygons %>%
    sf::st_transform(crs = crs_code) %>%
    sf::st_union()

  trees <- trees_data$osm_points %>%
    sf::st_transform(crs = crs_code)

  sf::st_crs(green_areas) <- sf::st_crs(edges)
  sf::st_crs(trees) <- sf::st_crs(edges)

  # Define the distance decay functions
  distance_decay_green_area <- function(edge, green_area) {

    distance_to_green_area <- sf::st_distance(edge, green_area)

    decay_function <- exp(-(distance_to_green_area/D))

    decay_function <- pmax(pmin(decay_function, 1), 0)

    return(decay_function)
  }

  distance_decay_tree <- function(edge, trees) {

    distances_to_trees <- sf::st_distance(edge, trees)
    min_distance_to_tree <- min(distances_to_trees)

    decay_function <- exp(-(min_distance_to_tree/D))

    decay_function <- pmax(pmin(decay_function, 1), 0)

    return(decay_function)
  }

  future::plan(future::multisession)

  edges <- edges %>%
    dplyr::mutate(
      green_index_green_area = purrr::map_dbl(sf::st_geometry(geometry), function(x) sum(purrr::map_dbl(green_areas, ~distance_decay_green_area(x, .x), na.rm = TRUE))),
      green_index_tree = purrr::map_dbl(sf::st_geometry(geometry), function(x) distance_decay_tree(x, trees))
    )

  # Compute normalized green index
  edges <- edges %>%
    dplyr::mutate(
      green_index = (green_index_green_area + green_index_tree) / 2,
      green_index = (green_index - min(green_index)) / (max(green_index) - min(green_index))
    )

  return(edges)
}
