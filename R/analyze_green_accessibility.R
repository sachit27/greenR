#' Analyze Green Space Accessibility Using Street Network
#'
#' Computes green space accessibility using network distances from grid centroids to the nearest green area.
#' Supports travel modes like walking, cycling, and driving by filtering appropriate road types and assigning travel speed.
#' Optionally supports population-weighted metrics if population raster data is provided (e.g., GHSL).
#'
#' @param network_data `sf` object or `osmdata` object with `osm_lines` representing street network.
#' @param green_areas `sf` object or `osmdata` object with `osm_polygons` representing green areas.
#' @param mode Character. One of `"walking"`, `"cycling"`, `"driving"`, or `"all"`. Defaults to `"all"`.
#' @param grid_size Numeric. Grid cell size in meters. Default is 500.
#' @param population_raster Optional. A `terra::SpatRaster` object with gridded population data (e.g., GHSL).
#'
#' @return A named list by mode. Each element contains:
#' \describe{
#'   \item{grid}{An `sf` grid with per-cell accessibility and population metrics.}
#'   \item{stats}{Data frame with spatial and population-weighted accessibility metrics.}
#'   \item{summary}{Named list of summary statistics for plotting or reporting.}
#' }
#'
#' @importFrom sf st_transform st_geometry_type st_cast st_make_valid st_centroid st_distance st_make_grid st_as_sfc st_bbox st_union st_buffer st_intersects st_crs
#' @importFrom dplyr filter mutate row_number
#' @importFrom units drop_units
#' @importFrom stats weighted.mean
#' @importFrom igraph distances as.igraph E
#' @importFrom sfnetworks as_sfnetwork activate
#' @importFrom purrr map
#' @importFrom terra extract project
#' @examples
#' \dontrun{
#' # Example 1: Green accessibility using OSM network and green polygons, no population
#' data <- get_osm_data("City of London, United Kingdom")
#' result_no_pop <- analyze_green_accessibility(
#'   network_data = data$highways$osm_lines,
#'   green_areas = data$green_areas$osm_polygons,
#'   mode = "walking",
#'   grid_size = 300
#' )
#' print(result_no_pop$stats)
#'
#' # Example 2: With GHSL population raster (if you have the raster file)
#' library(terra)
#' ghsl_path <- "GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R4_C19.tif"  # Update path as needed
#' pop_raster_raw <- terra::rast(ghsl_path)
#'
#' # Optionally, crop raster to the city area (recommended for speed)
#' # aoi <- sf::st_transform(st_as_sfc(st_bbox(data$highways$osm_lines)), terra::crs(pop_raster_raw))
#' # pop_raster_raw <- terra::crop(pop_raster_raw, aoi)
#'
#' result_with_pop <- analyze_green_accessibility(
#'   network_data = data$highways$osm_lines,
#'   green_areas = data$green_areas$osm_polygons,
#'   mode = "walking",
#'   grid_size = 300,
#'   population_raster = pop_raster_raw
#' )
#' print(result_with_pop$stats)
#' }
#' @export

analyze_green_accessibility <- function(network_data,
                                        green_areas,
                                        mode = "all",
                                        grid_size = 500,
                                        population_raster = NULL) {
  # ---- Input Preprocessing ----
  process_inputs <- function(network, green) {
    if (inherits(network, "osmdata") || inherits(network, "osmdata_sf")) {
      network <- network$osm_lines
    }
    if (!inherits(network, "sf")) stop("Network must be an sf object or osmdata with $osm_lines.")

    network <- network %>%
      sf::st_make_valid() %>%
      sf::st_transform(3857) %>%
      dplyr::filter(sf::st_geometry_type(.) %in% c("LINESTRING", "MULTILINESTRING")) %>%
      sf::st_cast("LINESTRING")

    if (inherits(green, "osmdata") || inherits(green, "osmdata_sf")) {
      green <- green$osm_polygons
    }
    if (!inherits(green, "sf")) stop("Green areas must be an sf object or osmdata with $osm_polygons.")

    green <- green %>%
      sf::st_make_valid() %>%
      sf::st_transform(3857) %>%
      sf::st_cast("POLYGON")

    list(network = network, green = green)
  }

  # ---- Mode Filtering ----
  configure_modes <- function(mode) {
    available <- c("walking", "cycling", "driving")
    if (mode == "all") return(available)
    if (!mode %in% available) stop("Invalid mode")
    return(mode)
  }

  get_mode_params <- function(mode) {
    switch(mode,
           walking = list(speed = 5, filters = c("footway", "path", "pedestrian")),
           cycling = list(speed = 15, filters = c("cycleway", "path", "living_street")),
           driving = list(speed = 40, filters = c("motorway", "trunk", "primary", "secondary")),
           stop("Invalid mode"))
  }

  filter_network <- function(network, mode_params) {
    network %>%
      dplyr::filter(highway %in% mode_params$filters) %>%
      dplyr::mutate(length = sf::st_length(.))
  }

  create_grid <- function(network, size) {
    grid <- sf::st_make_grid(sf::st_as_sfc(sf::st_bbox(network)), cellsize = size, square = TRUE)
    grid_sf <- sf::st_as_sf(grid, crs = sf::st_crs(network)) %>%
      dplyr::mutate(grid_id = dplyr::row_number())
    buffered <- sf::st_union(sf::st_buffer(network, size))
    grid_sf[sf::st_intersects(grid_sf, buffered, sparse = FALSE), ]
  }

  calculate_directional_stats <- function(centroids, green_polys) {
    directions <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
    angle_bounds <- seq(0, 360, by = 45)
    lower_bounds <- (angle_bounds - 22.5) %% 360
    upper_bounds <- (angle_bounds + 22.5) %% 360

    centroid_coords <- sf::st_coordinates(centroids)
    green_coords <- sf::st_coordinates(sf::st_centroid(green_polys))

    coverage_list <- vector("list", length(directions))

    for (i in seq_along(directions)) {
      dir <- directions[i]
      lower <- lower_bounds[i]
      upper <- upper_bounds[i]

      # For each centroid, count how many green areas fall into this direction
      coverage_per_cell <- sapply(1:nrow(centroid_coords), function(j) {
        dx <- green_coords[, 1] - centroid_coords[j, 1]
        dy <- green_coords[, 2] - centroid_coords[j, 2]
        angles_deg <- (atan2(dy, dx) * 180 / pi + 360) %% 360
        in_sector <- if (lower < upper) {
          angles_deg >= lower & angles_deg < upper
        } else {
          angles_deg >= lower | angles_deg < upper
        }
        mean(in_sector)
      })

      coverage_list[[i]] <- data.frame(direction = dir, mean_coverage = mean(coverage_per_cell, na.rm = TRUE))
    }

    do.call(rbind, coverage_list)
  }


  calculate_stats <- function(grid) {
    total_cells <- nrow(grid)
    cells_with_access <- sum(!is.na(grid$access))
    coverage_400m <- mean(grid$distance <= 400, na.rm = TRUE) * 100
    coverage_800m <- mean(grid$distance <= 800, na.rm = TRUE) * 100
    dir_stats <- calculate_directional_stats(grid$centroid, green)

    stats <- data.frame(
      mean_distance = mean(grid$distance, na.rm = TRUE),
      median_distance = median(grid$distance, na.rm = TRUE),
      coverage_400m = coverage_400m,
      coverage_800m = coverage_800m,
      total_cells = total_cells,
      cells_with_access = cells_with_access,
      directional_stats = I(list(dir_stats))
    )

    # ---- Population-weighted metrics (optional) ----
    if ("population" %in% names(grid)) {
      total_pop <- sum(grid$population, na.rm = TRUE)
      if (total_pop > 0) {
        stats$pop_weighted_mean_distance <- weighted.mean(grid$distance, grid$population, na.rm = TRUE)
        stats$pop_coverage_400m <- 100 * sum(grid$population[grid$distance <= 400], na.rm = TRUE) / total_pop
        stats$pop_coverage_800m <- 100 * sum(grid$population[grid$distance <= 800], na.rm = TRUE) / total_pop
      }
    }

    stats
  }

  # ---- Main Logic ----
  processed <- process_inputs(network_data, green_areas)
  network <- processed$network
  green <- processed$green
  modes <- configure_modes(mode)

  results <- purrr::map(modes, function(m) {
    mode_params <- get_mode_params(m)
    filtered_net <- filter_network(network, mode_params)
    grid <- create_grid(filtered_net, grid_size)

    # Optional: attach population
    if (!is.null(population_raster)) {
      if (!inherits(population_raster, "SpatRaster")) stop("Population data must be a terra::SpatRaster")
      pop_proj <- terra::project(population_raster, paste0("EPSG:", sf::st_crs(grid)$epsg))
      grid$population <- terra::extract(pop_proj, sf::st_centroid(grid))[, 2]
    }

    # Graph and travel time
    graph <- sfnetworks::as_sfnetwork(filtered_net, directed = FALSE) %>%
      sfnetworks::activate("edges") %>%
      dplyr::mutate(weight = as.numeric(length) / (mode_params$speed / 3.6))

    centroids <- sf::st_centroid(grid)
    green_pts <- sf::st_centroid(green)
    nodes <- sfnetworks::activate(graph, "nodes") %>% sf::st_as_sf()
    centroid_nodes <- sf::st_nearest_feature(centroids, nodes)
    green_nodes <- unique(sf::st_nearest_feature(green_pts, nodes))
    g <- igraph::as.igraph(graph)
    travel_times <- igraph::distances(g, v = centroid_nodes, to = green_nodes, weights = igraph::E(g)$weight)

    access_time <- apply(travel_times, 1, function(x) min(x, na.rm = TRUE))
    euclidean_dists <- sf::st_distance(centroids, green)
    nearest_euc <- apply(euclidean_dists, 1, min)

    grid$centroid <- sf::st_geometry(centroids)
    grid$distance <- if (inherits(nearest_euc, "units")) {
      units::drop_units(nearest_euc)
    } else {
      as.numeric(nearest_euc)
    }

    grid$access <- access_time

    stats <- calculate_stats(grid)

    list(
      grid = grid,
      stats = stats,
      summary = as.list(stats)
    )
  })

  if (length(results) > 1) {
    names(results) <- modes
    return(results)
  } else {
    return(results[[1]])
  }
}

