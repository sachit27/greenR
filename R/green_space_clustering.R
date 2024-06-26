#' Green Space Clustering with K-Means and Tile Layer Control in Leaflet
#'
#' This function performs K-means clustering on green spaces based on their area size and visualizes the results on a Leaflet map.
#' Users must specify the number of clusters. The function includes a layer control for switching between different basemap tiles.
#'
#' @param green_areas_data List containing green areas data (obtained from get_osm_data function or similar).
#' @param num_clusters Integer number of clusters to divide the green spaces into.
#' @return A Leaflet map object displaying clustered green spaces with layer control for basemap tiles.
#' @importFrom sf st_as_sf st_transform st_area st_crs
#' @importFrom leaflet addTiles addProviderTiles addPolygons addLayersControl addLegend leaflet colorFactor
#' @importFrom stats kmeans
#' @importFrom RColorBrewer brewer.pal
#' @examples
#' \donttest{
#'   # Create example green_areas_data
#'   library(sf)
#'   green_areas <- st_sf(
#'     id = 1:5,
#'     geometry = st_sfc(
#'       st_polygon(list(rbind(c(0, 0), c(0, 1), c(1, 1), c(1, 0), c(0, 0)))),
#'       st_polygon(list(rbind(c(1, 1), c(1, 2), c(2, 2), c(2, 1), c(1, 1)))),
#'       st_polygon(list(rbind(c(2, 2), c(2, 3), c(3, 3), c(3, 2), c(2, 2)))),
#'       st_polygon(list(rbind(c(3, 3), c(3, 4), c(4, 4), c(4, 3), c(3, 3)))),
#'       st_polygon(list(rbind(c(4, 4), c(4, 5), c(5, 5), c(5, 4), c(4, 4))))
#'     ),
#'     crs = 4326  # Assign a CRS (WGS 84)
#'   )
#'   green_areas_data <- list(osm_polygons = green_areas)
#'   # Run the clustering function
#'   map <- green_space_clustering(green_areas_data, num_clusters = 2)
#'   map # to display the map
#' }
#' @export
green_space_clustering <- function(green_areas_data, num_clusters) {
  if (!is.list(green_areas_data) || !all(c("osm_polygons") %in% names(green_areas_data))) {
    stop("Invalid green_areas_data format. It should be a list containing 'osm_polygons'.")
  }

  # Transform the green areas to an equal-area projection to calculate the area accurately
  green_areas <- sf::st_as_sf(green_areas_data$osm_polygons)
  green_areas_transformed <- sf::st_transform(green_areas, crs = sf::st_crs("ESRI:54009"))
  green_areas$area <- as.numeric(sf::st_area(green_areas_transformed))

  # Area data needs to be in a matrix or data frame
  area_data <- data.frame(area = green_areas$area)

  # Run kmeans with the number of clusters specified by the user
  kmeans_result <- stats::kmeans(area_data, centers = num_clusters, nstart = 25)
  green_areas$cluster <- kmeans_result$cluster

  # Define colors for clusters using a color palette
  pal <- leaflet::colorFactor(RColorBrewer::brewer.pal(ifelse(num_clusters <= 8, num_clusters, 8), "Dark2"), domain = green_areas$cluster)

  # Create a Leaflet map with base tiles
  map <- leaflet::leaflet(green_areas) %>%
    leaflet::addTiles(group = "OpenStreetMap") # Default OSM tiles

  # Add the clustered green areas to the map
  map <- map %>%
    leaflet::addPolygons(
      fillColor = ~pal(cluster),
      fillOpacity = 0.5,
      color = "#444444",
      weight = 1,
      smoothFactor = 0.5,
      popup = ~paste("Cluster:", cluster, "<br>Area:", formatC(area, format = "f", big.mark = ",", digits = 0), "m\u00B2"),
      group = "Clusters"
    )

  # Add different tile providers
  map <- map %>%
    leaflet::addProviderTiles("CartoDB.Positron", group = "Positron")

  # Add layer control
  map <- map %>%
    leaflet::addLayersControl(
      baseGroups = c("OpenStreetMap", "Positron"),
      overlayGroups = c("Clusters"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  # Add a legend for the clusters
  map <- map %>%
    leaflet::addLegend(
      position = "bottomright",
      pal = pal,
      values = ~cluster,
      title = "Cluster",
      opacity = 1
    )

  # Return the map
  return(map)
}
