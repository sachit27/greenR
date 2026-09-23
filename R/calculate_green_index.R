#' Calculate a street-segment green index
#'
#' For each street, finds the nearest green area and tree within
#' `buffer_distance` metres. Each component is `exp(-distance / D)`; a missing
#' component contributes zero. The mean is scaled from 0 to 1 across streets.
#' Distances use a local metric CRS when `crs_code` is geographic,
#' uses non-metre units, or is Web Mercator.
#'
#' @param osm_data Output of [get_osm_data()] or a compatible list.
#' @param crs_code CRS for the output.
#' @param D Positive distance-decay parameter in metres.
#' @param buffer_distance Nonnegative search radius in metres.
#' @param show_time Print elapsed processing time.
#' @return An `sf` object with street attributes and index columns.
#' @export
calculate_green_index <- function(osm_data, crs_code, D = 100,
                                  buffer_distance = 120, show_time = TRUE) {
  started <- Sys.time()
  if (!is.list(osm_data)) stop("osm_data must be a list.", call. = FALSE)
  if (length(D) != 1L || !is.numeric(D) || !is.finite(D) || D <= 0)
    stop("D must be a positive, finite number of metres.", call. = FALSE)
  if (length(buffer_distance) != 1L || !is.numeric(buffer_distance) ||
      !is.finite(buffer_distance) || buffer_distance < 0)
    stop("buffer_distance must be a nonnegative, finite number of metres.", call. = FALSE)
  output_crs <- sf::st_crs(crs_code)
  if (is.na(output_crs)) stop("crs_code must identify a valid CRS.", call. = FALSE)

  layer <- function(x, slot) {
    if (inherits(x, "sf")) return(x)
    if (is.list(x)) return(x[[slot]])
    NULL
  }
  edges <- layer(osm_data$highways, "osm_lines")
  green <- layer(osm_data$green_areas, "osm_polygons")
  trees <- layer(osm_data$trees, "osm_points")
  if (!inherits(edges, "sf"))
    stop("osm_data must contain an sf highways layer.", call. = FALSE)
  if (is.na(sf::st_crs(edges))) stop("The highways layer needs a CRS.", call. = FALSE)
  for (x in list(green, trees)) {
    if (!is.null(x) && (!inherits(x, "sf") || is.na(sf::st_crs(x))))
      stop("Green areas and trees must be sf layers with a CRS when supplied.", call. = FALSE)
  }
  edges <- sf::st_transform(edges, output_crs)
  if (!nrow(edges)) {
    edges$green_index_green_area <- numeric(0)
    edges$green_index_tree <- numeric(0)
    edges$green_index <- numeric(0)
    return(edges)
  }

  unit <- output_crs$units_gdal
  metric <- !identical(output_crs$epsg, 3857L) &&
    !isTRUE(sf::st_is_longlat(output_crs)) &&
    !is.null(unit) && tolower(unit) %in% c("metre", "meter", "metres", "meters", "m")
  analysis_crs <- if (metric) output_crs else {
    bounds <- sf::st_bbox(sf::st_transform(edges, 4326))
    lon <- mean(bounds[c("xmin", "xmax")])
    lat <- mean(bounds[c("ymin", "ymax")])
    if (abs(lat) > 84)
      stop("Use a local projected CRS in metres for polar locations.", call. = FALSE)
    zone <- max(1L, min(60L, floor((lon + 180) / 6) + 1L))
    sf::st_crs(if (lat >= 0) 32600L + zone else 32700L + zone)
  }
  edges_m <- sf::st_transform(edges, analysis_crs)

  nearest_score <- function(features) {
    result <- numeric(nrow(edges_m))
    if (is.null(features) || !nrow(features)) return(result)
    features <- sf::st_transform(features, analysis_crs)
    features <- features[!sf::st_is_empty(features), , drop = FALSE]
    if (!nrow(features)) return(result)
    valid_edges <- which(!sf::st_is_empty(edges_m))
    if (!length(valid_edges)) return(result)
    nearest <- sf::st_nearest_feature(edges_m[valid_edges, , drop = FALSE],
                                      features)
    distances <- rep(Inf, nrow(edges_m))
    found <- !is.na(nearest)
    distances[valid_edges[found]] <- as.numeric(sf::st_distance(
      edges_m[valid_edges[found], , drop = FALSE],
      features[nearest[found], , drop = FALSE], by_element = TRUE))
    within <- is.finite(distances) & distances <= buffer_distance
    result[within] <- exp(-distances[within] / D)
    result
  }
  edges$green_index_green_area <- nearest_score(green)
  edges$green_index_tree <- nearest_score(trees)
  raw <- (edges$green_index_green_area + edges$green_index_tree) / 2
  span <- diff(range(raw))
  edges$green_index <- if (span > 0) (raw - min(raw)) / span else raw
  if (isTRUE(show_time))
    message("Processing time: ", round(as.numeric(difftime(Sys.time(), started,
                                                       units = "secs")), 2), " seconds")
  edges
}

#' Helper function to rename duplicate columns
#' @param df A data.frame.
rename_duplicate_columns <- function(df) {
  colnames(df) <- make.unique(tolower(colnames(df)))
  df
}

#' Helper function to check for duplicate columns
#' @param df A data.frame.
check_duplicate_columns <- function(df) {
  anyDuplicated(tolower(colnames(df))) > 0L
}
