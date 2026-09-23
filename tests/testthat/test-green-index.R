test_that("green index uses metres with geographic output and missing layers", {
  line <- function(x) sf::st_linestring(matrix(
    c(x, 51.5, x + 0.001, 51.5), ncol = 2, byrow = TRUE))
  roads <- sf::st_sf(osm_id = c("a", "b"), geometry = sf::st_sfc(
    line(-0.01), line(-0.02), crs = 4326))
  tree <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_point(c(-0.0095, 51.5)), crs = 4326))
  data <- list(highways = list(osm_lines = roads),
               green_areas = list(osm_polygons = NULL),
               trees = list(osm_points = tree))

  result <- calculate_green_index(data, 4326, D = 100,
                                  buffer_distance = 120, show_time = FALSE)
  expect_s3_class(result, "sf")
  expect_equal(sf::st_crs(result)$epsg, 4326L)
  expect_equal(result$green_index_green_area, c(0, 0))
  expect_gt(result$green_index_tree[1], 0.99)
  expect_equal(result$green_index_tree[2], 0)
  expect_equal(result$green_index, c(1, 0))
  expect_error(calculate_green_index(data, 4326, D = 0),
               "positive")
})

test_that("green index handles constant scores and no optional features", {
  road <- sf::st_sf(osm_id = "a", geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 10, 0), ncol = 2,
                             byrow = TRUE)), crs = 32631))
  tree <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(5, 0)),
                                         crs = 32631))
  data <- list(highways = list(osm_lines = road), trees = list(osm_points = tree))
  result <- calculate_green_index(data, 32631, show_time = FALSE)
  expect_equal(result$green_index, 0.5)
  data$trees <- NULL
  expect_equal(calculate_green_index(data, 32631,
                                     show_time = FALSE)$green_index, 0)
  data$highways$osm_lines <- road[0, ]
  expect_equal(nrow(calculate_green_index(data, 32631,
                                          show_time = FALSE)), 0)
})

test_that("OSM polygon builders respect closed ways and relation holes", {
  as_points <- function(xy) lapply(seq_len(nrow(xy)), function(i)
    list(lon = xy[i, 1], lat = xy[i, 2]))
  outer <- as_points(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
                            ncol = 2, byrow = TRUE))
  inner <- as_points(matrix(c(.25, .25, .75, .25, .75, .75,
                              .25, .75, .25, .25), ncol = 2, byrow = TRUE))
  relation <- list(type = "relation", id = 123,
                   tags = list(leisure = "park"),
                   members = list(list(role = "outer", geometry = outer),
                                  list(role = "inner", geometry = inner)))
  polygon <- greenR:::.build_multipolygons_sf(list(relation))
  expect_s3_class(polygon, "sf")
  expect_equal(nrow(polygon), 1)
  expect_equal(as.numeric(sf::st_area(sf::st_transform(polygon, 3857))),
               as.numeric(sf::st_area(sf::st_transform(
                 greenR:::.build_polygons_sf(list(list(type = "way", id = 1,
                   geometry = outer))), 3857))) * .75,
               tolerance = 0.02)
  expect_null(greenR:::.build_polygons_sf(list(list(
    type = "way", id = 2, geometry = outer[-length(outer)]))))
})

test_that("invalid bounding boxes fail before making a request", {
  expect_error(get_osm_data(c(2, 1, 1, 2), verbose = FALSE), "bbox")
  expect_error(get_osm_data(c(1, 2), verbose = FALSE), "bbox")
})
