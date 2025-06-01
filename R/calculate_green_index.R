# Declare global variables to avoid R CMD check warnings
utils::globalVariables(c("green_index", "green_index_green_area", "green_index_tree", ".", "osm_id", "geometry"))

#' Calculate Green Index (Optimized + Robust + Progress Bar)
#'
#' Calculates the green index for a given set of OpenStreetMap (OSM) data using DuckDB.
#'
#' @param osm_data List containing OSM data (highways, green_areas, trees).
#' @param crs_code Coordinate reference system code for transformations.
#' @param D Distance decay parameter (default = 100).
#' @param buffer_distance Buffer distance for spatial joins (default = 120).
#' @param show_time Logical, whether to print processing time (default TRUE).
#' @return A spatial data frame with calculated green index.
#' @importFrom sf st_transform st_geometry st_as_text st_crs st_as_sf
#' @importFrom DBI dbConnect dbExecute dbWriteTable dbGetQuery dbDisconnect
#' @importFrom duckdb duckdb
#' @importFrom data.table as.data.table :=
#' @importFrom progress progress_bar
#' @examples
#' \donttest{
#'   osm_data <- get_osm_data("Basel, Switzerland")
#'   green_index <- calculate_green_index(osm_data, 2056)
#' }
#' @export
calculate_green_index <- function(osm_data, crs_code, D = 100, buffer_distance = 120, show_time = TRUE) {

  if (show_time) start_time <- Sys.time()

  # Set up DuckDB connection
  con <- DBI::dbConnect(duckdb::duckdb())

  # Load spatial extension safely
  tryCatch({
    DBI::dbExecute(con, "LOAD spatial;")
  }, error = function(e) {
    DBI::dbExecute(con, "INSTALL spatial;")
    DBI::dbExecute(con, "LOAD spatial;")
  })

  # Progress bar setup
  pb <- progress::progress_bar$new(
    format = "  Processing [:bar] :percent in :elapsed",
    total = 5, clear = FALSE, width = 60
  )

  # Smart extraction: Handle both cases (sf object vs nested list)
  highways_data <- osm_data$highways
  green_areas_data <- osm_data$green_areas
  trees_data <- osm_data$trees

  edges <- if (inherits(highways_data, "sf")) highways_data else highways_data$osm_lines
  green_areas <- if (is.list(green_areas_data)) green_areas_data$osm_polygons else green_areas_data
  trees <- if (inherits(trees_data, "sf")) trees_data else trees_data$osm_points

  # Transform to CRS
  edges <- sf::st_transform(edges, crs = crs_code)
  green_areas <- sf::st_transform(green_areas, crs = crs_code)
  trees <- sf::st_transform(trees, crs = crs_code)

  pb$tick()

  # Prepare data frames and add WKT columns
  edges_df <- sf::st_drop_geometry(edges)
  edges_df$geometry <- sf::st_as_text(sf::st_geometry(edges))
  edges_df$id <- seq_len(nrow(edges_df))

  green_areas_df <- data.frame(geometry = sf::st_as_text(sf::st_geometry(green_areas)))
  trees_df <- data.frame(geometry = sf::st_as_text(sf::st_geometry(trees)))

  # Handle duplicates only if needed
  if (check_duplicate_columns(edges_df)) edges_df <- rename_duplicate_columns(edges_df)
  if (check_duplicate_columns(green_areas_df)) green_areas_df <- rename_duplicate_columns(green_areas_df)
  if (check_duplicate_columns(trees_df)) trees_df <- rename_duplicate_columns(trees_df)

  # Duplicate geometry into 'geom' columns for DuckDB
  edges_df$geom <- edges_df$geometry
  green_areas_df$geom <- green_areas_df$geometry
  trees_df$geom <- trees_df$geometry

  pb$tick()

  # Write to DuckDB and set spatial types
  DBI::dbWriteTable(con, "edges", edges_df, overwrite = TRUE)
  DBI::dbExecute(con, "ALTER TABLE edges ALTER COLUMN geom SET DATA TYPE GEOMETRY;")

  DBI::dbWriteTable(con, "green_areas", green_areas_df, overwrite = TRUE)
  DBI::dbExecute(con, "ALTER TABLE green_areas ALTER COLUMN geom SET DATA TYPE GEOMETRY;")

  DBI::dbWriteTable(con, "trees", trees_df, overwrite = TRUE)
  DBI::dbExecute(con, "ALTER TABLE trees ALTER COLUMN geom SET DATA TYPE GEOMETRY;")

  # (No manual spatial indexes needed — automatic in DuckDB 2024+)

  pb$tick()

  # Query for green index calculation
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

  edges_result <- DBI::dbGetQuery(con, query, params = list(buffer_distance, buffer_distance, D, D))
  pb$tick()

  # Process results
  edges_dt <- data.table::as.data.table(edges_result)
  edges_dt[, green_index := (green_index_green_area + green_index_tree) / 2]

  # Normalize
  min_green <- min(edges_dt$green_index, na.rm = TRUE)
  max_green <- max(edges_dt$green_index, na.rm = TRUE)
  edges_dt[, green_index := (green_index - min_green) / (max_green - min_green)]
  edges_dt[green_index == 0, green_index := 0.05]

  edges_dt <- edges_dt[, .(osm_id, geometry, green_index_green_area, green_index_tree, green_index)]

  edges_sf <- sf::st_as_sf(edges_dt, wkt = "geometry", crs = crs_code)

  # Disconnect DuckDB
  DBI::dbDisconnect(con, shutdown = TRUE)

  if (show_time) {
    end_time <- Sys.time()
    message(paste("Processing time:", round(end_time - start_time, 2), "seconds"))
  }

  return(edges_sf)
}

#' Helper function to rename duplicate columns
#' @param df A data.frame. The input data frame to rename duplicate columns in.

rename_duplicate_columns <- function(df) {
  colnames(df) <- make.unique(tolower(colnames(df)))
  return(df)
}

#' Helper function to check for duplicate columns
#' @param df A data.frame. The input data frame to check for duplicate columns.

check_duplicate_columns <- function(df) {
  dup_cols <- colnames(df)[duplicated(tolower(colnames(df)))]
  if (length(dup_cols) > 0) {
    message("Duplicate columns found:", paste(dup_cols, collapse = ", "))
    return(TRUE)
  } else {
    return(FALSE)
  }
}
