#' Download OSM Data
#'
#' This function downloads OpenStreetMap (OSM) data for a specified location or bounding box.
#' The OSM data includes information about highways, green areas, and trees in the specified location.
#'
#' @param bbox Either a string representing the location (e.g., "Lausanne, Switzerland") or
#'             a numeric vector of length 4 representing the bounding box coordinates
#'             in the order: c(left, bottom, right, top).
#' @param server_url Optional string representing an alternative Nominatim server URL.
#' @param username Optional string for username if authentication is required for the server.
#' @param password Optional string for password if authentication is required for the server.
#' @return A list containing:
#'   \item{highways}{An sf object with the OSM data about highways in the specified location.}
#'   \item{green_areas}{An sf object with the OSM data about green areas in the specified location.}
#'   \item{trees}{An sf object with the OSM data about trees in the specified location.}
#' @importFrom magrittr %>%
#' @importFrom httr GET content authenticate
#' @importFrom osmdata opq add_osm_feature osmdata_sf
#' @importFrom sf st_as_sf
#' @export
#' @examples
#' \dontrun{
#'   # Using a location name
#'   osm_data <- get_osm_data("Lausanne, Switzerland")
#'
#'   # Using coordinates for a bounding box
#'   bbox_coords <- c(6.6, 46.5, 6.7, 46.6)  # Example coordinates near Lausanne
#'   osm_data <- get_osm_data(bbox_coords)
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

  # Check if bbox is a vector of coordinates or a location name
  if (is.numeric(bbox) && length(bbox) == 4) {
    # If bbox is a vector of coordinates, use it directly
    bbox_query <- bbox
    names(bbox_query) <- c("left", "bottom", "right", "top")
  } else if (is.character(bbox) && length(bbox) == 1) {
    # If bbox is a location name, query Nominatim for the bounding box
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
      stop(paste("Failed to retrieve bounding box. Status code:", response$status_code))
    }

    content <- httr::content(response, "parsed")
    if (length(content) == 0 || is.null(content[[1]]$boundingbox)) {
      stop("No bounding box information found for the provided location.")
    }

    bbox <- as.numeric(content[[1]]$boundingbox)
    bbox_query <- c(left = bbox[3], bottom = bbox[1], right = bbox[4], top = bbox[2])
  } else {
    stop("Invalid bbox input. Please provide either a location name as a string or a numeric vector of coordinates (left, bottom, right, top).")
  }

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
