#' Download OSM Data
#'
#' This function downloads OpenStreetMap (OSM) data for a specified location or bounding box.
#' The OSM data includes information about highways, green areas, and trees in the specified location.
#' It requires an internet connection. If using RStudio Cloud, or if you need to use a private
#' Nominatim server, you can specify an alternative server URL and credentials (username and password).
#'
#' @param bbox A string representing the bounding box area or the location (e.g., "Lausanne, Switzerland").
#' @param server_url Optional string representing an alternative Nominatim server URL.
#' @param username Optional string for username if authentication is required for the server.
#' @param password Optional string for password if authentication is required for the server.
#' @return A list containing:
#'   \item{highways}{An sf object with the OSM data about highways in the specified location.}
#'   \item{green_areas}{An sf object with the OSM data about green areas, such as parks, forests, gardens, and nature reserves, in the specified location.}
#'   \item{trees}{An sf object with the OSM data about trees in the specified location.}
#' @importFrom magrittr %>%
#' @importFrom httr GET content authenticate
#' @importFrom osmdata opq add_osm_feature osmdata_sf
#' @importFrom sf st_transform st_union
#' @importFrom dplyr select as_tibble
#' @export
#' @examples
#' \donttest{
#'   osm_data <- get_osm_data("Lausanne, Switzerland")
#' }
get_osm_data <- function(bbox, server_url = "https://nominatim.openstreetmap.org/search", username = NULL, password = NULL) {

  # Function to align columns of two sf data frames
  align_columns <- function(df1, df2) {
    missing_cols_df1 <- setdiff(names(df2), names(df1))
    missing_cols_df2 <- setdiff(names(df1), names(df2))

    for (col in missing_cols_df1) {
      df1[[col]] <- NA
    }

    for (col in missing_cols_df2) {
      df2[[col]] <- NA
    }

    # Reorder columns to match
    df1 <- df1[, names(df2)]

    return(list(df1 = df1, df2 = df2))
  }

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

  # Download landuse data
  green_areas_data_landuse <- query %>%
    osmdata::add_osm_feature(key = "landuse", value = c("forest", "vineyard", "plant_nursery", "orchard", "greenfield", "recreation_ground", "allotments",
                                                        "meadow", "village_green", "flowerbed", "grass", "farmland")) %>%
    osmdata::osmdata_sf()

  # Download leisure data
  green_areas_data_leisure <- query %>%
    osmdata::add_osm_feature(key = "leisure", value = c("garden", "dog_park", "nature_reserve", "park")) %>%
    osmdata::osmdata_sf()

  # Initialize an empty list to hold the combined data
  green_areas_data <- list()

  # Align and combine 'sf' objects
  aligned_polygons <- align_columns(green_areas_data_landuse$osm_polygons, green_areas_data_leisure$osm_polygons)
  green_areas_data$osm_polygons <- rbind(aligned_polygons$df1, aligned_polygons$df2)

  # Download highways data
  highways_data <- query %>%
    osmdata::add_osm_feature(key = "highway") %>%
    osmdata::osmdata_sf()

  # Download trees data
  trees_data <- query %>%
    osmdata::add_osm_feature(key = "natural", value = "tree") %>%
    osmdata::osmdata_sf()

  return(list(highways = highways_data, green_areas = green_areas_data, trees = trees_data))
}
