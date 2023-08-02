#' Download OSM Data
#'
#' This function downloads OpenStreetMap (OSM) data for a specified location.
#' The OSM data includes data about highways, green areas, and trees in the specified location.
#' It requires an internet connection.
#'
#' @param location A string representing the location (e.g., "Genève, Schweiz/Suisse/Svizzera/Svizra").
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
#'   osm_data <- get_osm_data("Genève, Schweiz/Suisse/Svizzera/Svizra")
#' }
get_osm_data <- function(location) {
  
  # Retrieve bounding box from Nominatim
  response <- httr::GET(
    "https://nominatim.openstreetmap.org/search",
    query = list(
      q = location,
      format = "json",
      featuretype = "settlement"
    )
  )
  content <- httr::content(response, "parsed")
  bbox <- as.numeric(content[[1]]$boundingbox) # Convert to numeric
  
  # Define the bounding box for the location using retrieved values
  location_bbox <- c(left = bbox[3], bottom = bbox[1], right = bbox[4], top = bbox[2])

  # Define a query to get the data from OpenStreetMap
  query <- osmdata::opq(bbox = location_bbox)

  # Download highways data
  highways_data <- query %>%
    osmdata::add_osm_feature(key = "highway") %>%
    osmdata::osmdata_sf()

  # Download green areas data
  green_areas_data <- query %>%
    osmdata::add_osm_feature(key = "landuse", value = c("park", "forest", "recreation_ground", "allotments",
                                                "meadow", "grass", "garden", "farmland", "nature_reserve")) %>%
    osmdata::osmdata_sf()

  # Download trees data
  trees_data <- query %>%
    osmdata::add_osm_feature(key = "natural", value = "tree") %>%
    osmdata::osmdata_sf()

  return(list(highways = highways_data, green_areas = green_areas_data, trees = trees_data))
}
