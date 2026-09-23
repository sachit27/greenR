test_that("convert_to_point uses a length midpoint and geographic columns", {
  line <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 1, 0, 1, 3),
                             ncol = 2, byrow = TRUE)), crs = 4326))
  result <- convert_to_point(line, target_crs = 32631)
  expect_s3_class(result, "sf")
  expect_equal(sf::st_crs(result)$epsg, 32631L)
  expect_gt(result$lat, .5)
  expect_lt(result$lat, 2)
  expect_lt(result$lon, 2)
})

test_that("green-space similarity uses one representative point per polygon", {
  polygon <- function(x, width) sf::st_polygon(list(matrix(
    c(x, 0, x + width, 0, x + width, width, x, width, x, 0),
    ncol = 2, byrow = TRUE)))
  a <- sf::st_sf(geometry = sf::st_sfc(
    polygon(0, .01), polygon(.02, .02), polygon(.05, .03), crs = 4326))
  result <- gssi(list(a, a))
  expect_equal(result, c(1, 1))
  expect_true(all(is.finite(result)))
})

test_that("static green-index plot returns a ggplot", {
  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  road <- sf::st_sf(green_index = .5, geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2,
                             byrow = TRUE)), crs = 4326))
  expect_s3_class(suppressMessages(plot_green_index(road)), "ggplot")
})

test_that("hex coverage counts overlaps once and supports tree-only input", {
  polygon <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, .002, 0, .002, .002,
                                0, .002, 0, 0), ncol = 2, byrow = TRUE))),
    crs = 4326))
  tree <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_point(c(.001, .001)), crs = 4326))
  both <- hexGreenSpace(list(osm_polygons = polygon),
                        list(osm_points = tree), hex_size = 100)
  expect_s3_class(both$map, "leaflet")
  expect_true(all(both$hexes$coverage_pct >= 0 &
                  both$hexes$coverage_pct <= 100))
  only_trees <- hexGreenSpace(tree_data = list(osm_points = tree),
                             hex_size = 100)
  expect_gt(max(only_trees$hexes$coverage_pct), 0)
})

test_that("network accessibility handles a straight street", {
  road <- sf::st_sf(highway = c("footway", "footway"),
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, .001, 0), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(.001, 0, .002, 0), ncol = 2,
                                byrow = TRUE)), crs = 4326))
  green <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(matrix(
    c(.001, -.0001, .002, -.0001, .002, .0001, .001, .0001,
      .001, -.0001), ncol = 2, byrow = TRUE))), crs = 4326))
  result <- analyze_green_accessibility(road, green, mode = "walking",
                                        grid_size = 100)
  expect_s3_class(result$grid, "sf")
  expect_gt(nrow(result$grid), 0)
  expect_true(all(is.finite(result$grid$distance)))
  figures <- create_accessibility_visualizations(result, green)
  expect_s3_class(figures$distance_map, "ggplot")
  expect_s3_class(figures$leaflet_map, "leaflet")
})

test_that("green-space clustering accepts two clusters without palette warnings", {
  polygon <- function(x, width) sf::st_polygon(list(matrix(
    c(x, 0, x + width, 0, x + width, width, x, width, x, 0),
    ncol = 2, byrow = TRUE)))
  green <- sf::st_sf(geometry = sf::st_sfc(
    polygon(0, .01), polygon(.02, .02), polygon(.05, .03), crs = 4326))
  expect_s3_class(green_space_clustering(
    list(osm_polygons = green), num_clusters = 2), "leaflet")
  expect_error(green_space_clustering(
    list(osm_polygons = green), num_clusters = 4), "distinct")
})

test_that("density analysis handles a single feature", {
  polygon <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(matrix(
    c(0, 0, .01, 0, .01, .01, 0, .01, 0, 0),
    ncol = 2, byrow = TRUE))), crs = 4326))
  data <- list(green_areas = list(osm_polygons = polygon))
  result <- suppressWarnings(analyze_green_and_tree_count_density(
    data, mode = "green_area", h3_res = 8))
  expect_s3_class(result$map, "leaflet")
  expect_equal(result$analytics$total_count, 1)
})

test_that("SVF polygon clipping works without DuckDB spatial", {
  square <- function(x0, y0, x1, y1) sf::st_polygon(list(matrix(
    c(x0, y0, x1, y0, x1, y1, x0, y1, x0, y0),
    ncol = 2, byrow = TRUE)))
  feature <- sf::st_sf(id = 1, geometry = sf::st_sfc(
    square(0, 0, 2, 2), crs = 32631))
  area <- sf::st_sfc(square(1, 1, 3, 3), crs = 32631)
  clipped <- greenR:::.uh_svf_spatial_intersection(
    feature, area, crs_code = 32631)
  expect_s3_class(clipped, "sf")
  expect_equal(as.numeric(sf::st_area(clipped)), 1)
})

test_that("Gini bootstrap handles an all-zero sample", {
  result <- compute_gini_bootstrap(c(0, 0, 0), R = 20)
  expect_equal(unlist(result), c(gini = 0, ci_low = 0, ci_high = 0))
  expect_error(compute_gini_bootstrap(c(-1, 2)), "nonnegative")
})

test_that("green-index percentages omit missing values and include empty bins", {
  result <- calculate_percentage(data.frame(
    green_index = c(0.2, 0.5, 0.8, NA_real_)))
  expect_equal(result$n, c(1L, 1L, 1L))
  expect_equal(sum(result$percentage), 100)
  expect_equal(calculate_percentage(data.frame(
    green_index = NA_real_))$percentage, c(0, 0, 0))
})

test_that("sky-view plots accept valid local results", {
  points <- sf::st_sf(point_id = 1, svf = .5,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326))
  points$horizon_angles <- list(rep(pi / 6, 8))
  expect_s3_class(uh_svf_plot_skyline(points, 1), "ggplot")
  expect_s3_class(uh_svf_plot_distribution(
    data.frame(svf_mean = c(.2, .4, .6, .8))), "patchwork")
  expect_error(uh_svf_plot_distribution(
    data.frame(svf_mean = NA_real_)), "no finite")
})

test_that("Mapbox HTML exporters write files in scripted sessions", {
  road <- sf::st_sf(green_index = .5, geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, .001, 0), ncol = 2,
                             byrow = TRUE)), crs = 4326))
  polygon <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(matrix(
    c(0, 0, .001, 0, .001, .001, 0, .001, 0, 0),
    ncol = 2, byrow = TRUE))), crs = 4326))
  files <- vapply(seq_len(3), function(i) tempfile(fileext = ".html"),
                  character(1))
  on.exit(unlink(files), add = TRUE)
  suppressMessages(create_linestring_3D(
    road, "green_index", "dummy", output_file = files[1]))
  suppressMessages(create_hexmap_3D(
    road, "green_index", mapbox_token = "dummy",
    output_file = files[2]))
  suppressMessages(accessibility_mapbox(
    list(osm_polygons = polygon), "dummy", output_file = files[3]))
  expect_true(all(file.exists(files)))
})
