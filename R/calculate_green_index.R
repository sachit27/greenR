# Declare global variables to avoid R CMD check warnings
utils::globalVariables(c("green_index", "green_index_green_area", "green_index_tree", ".", "osm_id", "geometry"))

#' Calculate Green Index
#'
#' This function calculates the green index for a given set of OpenStreetMap (OSM) data using DuckDB and Duckplyr.
#' The green index is calculated based on the proximity of highways to green areas and trees.
#'
#' @param osm_data List containing OSM data (highways, green_areas, trees).
#' @param crs_code Coordinate reference system code for transformations.
#' @param D Distance decay parameter (default = 100).
#' @param buffer_distance Buffer distance for spatial joins (default = 120).
#' @return A spatial data frame with calculated green index.
#' @importFrom sf st_transform st_union st_geometry st_as_text st_crs st_drop_geometry st_as_sf
#' @importFrom DBI dbConnect dbExecute dbWriteTable dbGetQuery dbDisconnect
#' @importFrom duckdb duckdb
#' @importFrom data.table as.data.table :=
#' @importFrom dplyr %>% select
#' @examples
#' \donttest{
#'   osm_data <- get_osm_data("Basel, Switzerland")
#'   green_index <- calculate_green_index(osm_data, 2056)
#' }
#' @export
calculate_green_index <- function(osm_data, crs_code, D = 100, buffer_distance = 120) {
  # Start time
  start_time <- Sys.time()

  # Establish DuckDB connection
  con <- DBI::dbConnect(duckdb::duckdb())

  # Check if spatial extension is installed
  spatial_installed <- try(DBI::dbExecute(con, "INSTALL spatial;"), silent = TRUE)
  if (inherits(spatial_installed, "try-error")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
    stop("Failed to install DuckDB spatial extension.")
  }

  # Load spatial extension
  spatial_loaded <- try(DBI::dbExecute(con, "LOAD spatial;"), silent = TRUE)
  if (inherits(spatial_loaded, "try-error")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
    stop("Failed to load DuckDB spatial extension.")
  }

  # Extract and transform data
  highways_data <- osm_data$highways
  green_areas_data <- osm_data$green_areas
  trees_data <- osm_data$trees

  edges <- sf::st_transform(highways_data$osm_lines, crs = crs_code)
  green_areas <- sf::st_transform(green_areas_data$osm_polygons, crs = crs_code) %>% sf::st_union()
  trees <- sf::st_transform(trees_data$osm_points, crs = crs_code)

  # Ensure CRS is correctly set
  sf::st_crs(edges) <- crs_code
  sf::st_crs(green_areas) <- crs_code
  sf::st_crs(trees) <- crs_code

  # Prepare data frames and add geometry as WKT
  edges_df <- sf::st_drop_geometry(edges)
  edges_df$geometry <- sf::st_as_text(sf::st_geometry(edges))
  edges_df$id <- seq_len(nrow(edges_df))
  green_areas_df <- data.frame(geometry = sf::st_as_text(sf::st_geometry(green_areas)))
  trees_df <- data.frame(geometry = sf::st_as_text(sf::st_geometry(trees)))

  # Check and rename duplicate columns
  check_duplicate_columns(edges_df)
  check_duplicate_columns(green_areas_df)
  check_duplicate_columns(trees_df)

  edges_df <- rename_duplicate_columns(edges_df)
  green_areas_df <- rename_duplicate_columns(green_areas_df)
  trees_df <- rename_duplicate_columns(trees_df)

  # Write data frames to DuckDB
  DBI::dbWriteTable(con, "edges", edges_df, overwrite = TRUE)
  DBI::dbWriteTable(con, "green_areas", green_areas_df, overwrite = TRUE)
  DBI::dbWriteTable(con, "trees", trees_df, overwrite = TRUE)

  # Add and populate the geometry columns in DuckDB
  DBI::dbExecute(con, "ALTER TABLE edges ADD COLUMN geom GEOMETRY;")
  DBI::dbExecute(con, "UPDATE edges SET geom = ST_GeomFromText(geometry);")
  DBI::dbExecute(con, "ALTER TABLE green_areas ADD COLUMN geom GEOMETRY;")
  DBI::dbExecute(con, "UPDATE green_areas SET geom = ST_GeomFromText(geometry);")
  DBI::dbExecute(con, "ALTER TABLE trees ADD COLUMN geom GEOMETRY;")
  DBI::dbExecute(con, "UPDATE trees SET geom = ST_GeomFromText(geometry);")

  # SQL query for green index calculation using ST_DWithin
  query <- "
    WITH green_area_distances AS (
      SELECT
        e.id,
        MIN(ST_Distance(e.geom, g.geom)) AS distance
      FROM
        edges e
      JOIN green_areas g ON ST_DWithin(e.geom, g.geom, ?)
      GROUP BY e.id
    ),
    tree_distances AS (
      SELECT
        e.id,
        MIN(ST_Distance(e.geom, t.geom)) AS distance
      FROM
        edges e
      JOIN trees t ON ST_DWithin(e.geom, t.geom, ?)
      GROUP BY e.id
    )
    SELECT
      e.id,
      e.osm_id,
      e.geometry,
      COALESCE(exp(-gad.distance / ?), 0) AS green_index_green_area,
      COALESCE(exp(-td.distance / ?), 0) AS green_index_tree
    FROM
      edges e
      LEFT JOIN green_area_distances gad ON e.id = gad.id
      LEFT JOIN tree_distances td ON e.id = td.id;
  "

  edges_df <- DBI::dbGetQuery(con, query, params = list(buffer_distance, buffer_distance, D, D))

  # Convert result to data.table and calculate the green index
  edges_dt <- data.table::as.data.table(edges_df)
  edges_dt[, green_index := (green_index_green_area + green_index_tree) / 2]

  min_green_index <- min(edges_dt$green_index, na.rm = TRUE)
  max_green_index <- max(edges_dt$green_index, na.rm = TRUE)

  edges_dt[, green_index := (green_index - min_green_index) / (max_green_index - min_green_index)]

  # Set minimum green index value to 0.05 if it is 0
  edges_dt[green_index == 0, green_index := 0.05]

  edges_dt <- edges_dt[, .(osm_id, geometry, green_index_green_area, green_index_tree, green_index)]

  edges <- sf::st_as_sf(edges_dt, wkt = "geometry", crs = crs_code)

  # Disconnect DuckDB
  DBI::dbDisconnect(con, shutdown = TRUE)

  # End time
  end_time <- Sys.time()

  # Calculate and print processing time
  processing_time <- end_time - start_time
  message(paste("Processing time:", processing_time))

  return(edges)
}

#' Helper function to rename duplicate columns
#' @param df Data frame with potential duplicate columns
#' @return Data frame with unique column names
rename_duplicate_columns <- function(df) {
  colnames(df) <- make.unique(tolower(colnames(df)))
  return(df)
}

#' Function to check for duplicate columns and print them
#' @param df Data frame to check for duplicate columns
#' @return Vector of duplicate column names
check_duplicate_columns <- function(df) {
  dup_cols <- colnames(df)[duplicated(tolower(colnames(df)))]
  if (length(dup_cols) > 0) {
    message("Duplicate columns found:", paste(dup_cols, collapse = ", "))
  } else {
    message("No duplicate columns found.")
  }
  return(dup_cols)
}
