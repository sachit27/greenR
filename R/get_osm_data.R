#' Download OSM Data
#'
#' This function downloads OpenStreetMap (OSM) data for a specified location or bounding box.
#' The OSM data includes data about highways, green areas, and trees in the specified location.
#' It requires an internet connection.
#'
#' @param bbox A string representing the bounding box area or the location (e.g., "Lausanne, Switzerland").
#'
#' @return A list containing:
#'   * highways: an sf object with the OSM data about highways in the specified location.
#'   * green_areas: an sf object with the OSM data about green areas, such as parks, forests, gardens,
#'     and nature reserves, in the specified location.
#'   * trees: an sf object with the OSM data about trees in the specified location.
#'
#' @export
#' @examples
#' \dontrun{
#'   osm_data <- get_osm_data("Lausanne, Switzerland")
#' }
get_osm_data <- function(bbox) {
  library(osmdata)
  library(sf)
  library(dplyr)

  # Define a query to get the data from OpenStreetMap
  query <- opq(bbox = bbox)

  # Download highways data
  highways_data <- query %>%
    add_osm_feature(key = "highway") %>%
    osmdata_sf()

  # Download green areas data
  green_areas_data <- query %>%
    add_osm_feature(key = "landuse", value = c("park", "forest", "recreation_ground", "allotments",
                                                "meadow", "grass", "garden", "farmland", "nature_reserve")) %>%
    osmdata_sf()

  # Download trees data
  trees_data <- query %>%
    add_osm_feature(key = "natural", value = "tree") %>%
    osmdata_sf()

  return(list(highways = highways_data, green_areas = green_areas_data, trees = trees_data))
}
