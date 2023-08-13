#' Download OSM Data
#'
#' This function downloads OpenStreetMap (OSM) data for a specified location or bounding box.
#' The OSM data includes data about highways, green areas, and trees in the specified location.
#' It requires an internet connection. If using RStudio Cloud, or if you need to use a private
#' Nominatim server, you can specify an alternative server URL and credentials.
#'
#' @param bbox A string representing the bounding box area or the location (e.g., "Lausanne, Switzerland").
#' @param server_url Optional string representing an alternative Nominatim server URL.
#' @param username Optional string for username if authentication is required for the server.
#' @param password Optional string for password if authentication is required for the server.
#'
#' @return A list containing:
#'   * highways: an sf object with the OSM data about highways in the specified location.
#'   * green_areas: an sf object with the OSM data about green areas, such as parks, forests, gardens,
#'     and nature reserves, in the specified location.
#'   * trees: an sf object with the OSM data about trees in the specified location.
#'
#' @importFrom magrittr %>%
#' @importFrom httr GET content authenticate
#' @importFrom osmdata opq add_osm_feature osmdata_sf
#' @importFrom sf st_transform st_union
#' @importFrom dplyr select as_tibble
#' @export
#' @examples
#' \dontrun{
#'   osm_data <- get_osm_data("Lausanne, Switzerland")
#' }
get_osm_data <- function(bbox, server_url = "https://nominatim.openstreetmap.org/search", username = NULL, password = NULL) {
  # Prepare for authentication if username and password are provided
  auth <- if (!is.null(username) && !is.null(password)) httr::authenticate(username, password) else NULL

  # Query Nominatim for the bounding box
  response <- httr::GET(
    server_url,
    query = list(
      q = bbox,
      format = "json",
      featuretype = "settlement"
    ),
    auth
  )

  # Check if the request was successful
  if (response$status_code != 200) {
    stop(paste("Failed to retrieve bounding box. Status code:", response$status_code,
               "If using RStudio Cloud you may need to specify an alternative server URL and credentials."))
  }

  content <- httr::content(response, "parsed")
  if (length(content) == 0 || is.null(content[[1]]$boundingbox)) {
    stop("No bounding box information found for the provided location.")
  }
  bbox <- as.numeric(content[[1]]$boundingbox)

  # Define the bounding box using retrieved values
  bbox_query <- c(left = bbox[3], bottom = bbox[1], right = bbox[4], top = bbox[2])

  # Define a query to get the data from OpenStreetMap
  query <- osmdata::opq(bbox = bbox_query)

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
