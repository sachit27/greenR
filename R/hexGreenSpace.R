#' Visualize green-space coverage in hexagonal bins
#'
#' Computes the covered area of each hexagon from the union of green polygons
#' and five-metre tree-canopy buffers. Overlapping inputs are counted once.
#'
#' @param green_areas_data A list containing `osm_polygons`, as returned by
#'   [get_osm_data()]. May be `NULL`.
#' @param tree_data A list containing `osm_points`. May be `NULL`.
#' @param hex_size Positive hexagon cell size in metres.
#' @param color_palette A palette name accepted by [leaflet::colorNumeric()].
#' @param save_path Optional HTML output path.
#' @return A list with a Leaflet `map`, a ggplot `violin`, and the `hexes`
#'   spatial data including `coverage_pct`.
#' @export
hexGreenSpace <- function(green_areas_data = NULL, tree_data = NULL,
                          hex_size = 500, color_palette = "viridis",
                          save_path = NULL) {
  if (!is.numeric(hex_size) || length(hex_size) != 1L ||
      !is.finite(hex_size) || hex_size <= 0)
    stop("hex_size must be a positive number of metres.", call. = FALSE)
  green <- if (is.list(green_areas_data)) green_areas_data$osm_polygons else NULL
  trees <- if (is.list(tree_data)) tree_data$osm_points else NULL
  if (!is.null(green) && !inherits(green, "sf"))
    stop("green_areas_data$osm_polygons must be an sf object.", call. = FALSE)
  if (!is.null(trees) && !inherits(trees, "sf"))
    stop("tree_data$osm_points must be an sf object.", call. = FALSE)
  layers <- Filter(function(x) inherits(x, "sf") && nrow(x) > 0,
                   list(green, trees))
  if (!length(layers))
    stop("At least one nonempty green-area or tree layer is required.",
         call. = FALSE)
  if (any(vapply(layers, function(x) is.na(sf::st_crs(x)), logical(1))))
    stop("Input layers need a CRS.", call. = FALSE)

  bounds <- sf::st_bbox(sf::st_transform(layers[[1]], 4326))
  lon <- mean(bounds[c("xmin", "xmax")])
  lat <- mean(bounds[c("ymin", "ymax")])
  if (abs(lat) > 84)
    stop("Use a local projected CRS for polar locations.", call. = FALSE)
  zone <- max(1L, min(60L, floor((lon + 180) / 6) + 1L))
  metric_crs <- if (lat >= 0) 32600L + zone else 32700L + zone
  coverage <- list()
  if (!is.null(green) && nrow(green)) {
    green <- sf::st_make_valid(sf::st_transform(green, metric_crs))
    coverage[[length(coverage) + 1L]] <- sf::st_geometry(green)
  }
  if (!is.null(trees) && nrow(trees)) {
    trees <- sf::st_transform(trees, metric_crs)
    coverage[[length(coverage) + 1L]] <-
      sf::st_geometry(sf::st_buffer(trees, dist = 5))
  }
  all_shapes <- do.call(c, coverage)
  all_shapes <- all_shapes[!sf::st_is_empty(all_shapes)]
  if (!length(all_shapes))
    stop("The supplied layers contain only empty geometries.", call. = FALSE)
  cells <- sf::st_make_grid(all_shapes, cellsize = hex_size, square = FALSE)
  grid <- sf::st_sf(hex_id = seq_along(cells), geometry = cells)
  area <- as.numeric(sf::st_area(grid))
  grid$coverage_pct <- 0
  covered <- sf::st_sf(geometry = sf::st_union(all_shapes))
  intersections <- suppressWarnings(sf::st_intersection(grid, covered))
  if (nrow(intersections)) {
    summed <- tapply(as.numeric(sf::st_area(intersections)),
                     intersections$hex_id, sum)
    idx <- as.integer(names(summed))
    grid$coverage_pct[idx] <- pmin(100, 100 * summed / area[idx])
  }
  grid <- sf::st_transform(grid, 4326)
  palette <- leaflet::colorNumeric(color_palette, domain = grid$coverage_pct)
  map <- leaflet::leaflet(grid) |>
    leaflet::addTiles(group = "OSM") |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron,
                              group = "Positron") |>
    leaflet::addPolygons(fillColor = ~palette(coverage_pct),
                         fillOpacity = 0.8, color = "black", weight = 1,
                         group = "Hex Bins",
                         popup = ~sprintf("Coverage: %.2f%%", coverage_pct)) |>
    leaflet::addLegend("bottomright", pal = palette,
                       values = ~coverage_pct,
                       title = "Green-space coverage (%)") |>
    leaflet::addLayersControl(
      baseGroups = c("OSM", "Positron"), overlayGroups = "Hex Bins",
      options = leaflet::layersControlOptions(collapsed = TRUE))
  distribution <- data.frame(coverage_pct = grid$coverage_pct)
  violin <- ggplot2::ggplot(distribution,
                            ggplot2::aes(x = "Hexagon", y = coverage_pct)) +
    ggplot2::geom_violin(fill = "lightblue", color = "black") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Green-space coverage per hexagon",
                  x = NULL, y = "Coverage (%)")
  if (!is.null(save_path)) htmlwidgets::saveWidget(map, file = save_path)
  list(map = map, violin = violin, hexes = grid)
}
