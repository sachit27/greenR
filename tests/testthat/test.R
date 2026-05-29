library(testthat)
library(greenR)

test_that("get_osm_data function works correctly", {
  # this will download real data from the internet
  # use a very small area or a known location to be fast
  osm_data <- get_osm_data("Oerlikon, Zurich, Switzerland")

  # check if the function returns a list
  expect_true(is.list(osm_data))

  # check if the list contains the elements highways, green_areas, and trees
  expect_true(all(c("highways", "green_areas", "trees") %in% names(osm_data)))

  # check if each element in the list contains an sf object in the expected slot
  expect_s3_class(osm_data$highways$osm_lines, "sf")
  expect_s3_class(osm_data$green_areas$osm_polygons, "sf")
  expect_s3_class(osm_data$trees$osm_points, "sf")
})

test_that("calculate_green_index works correctly", {
  osm_data <- get_osm_data("Oerlikon, Zurich, Switzerland")
  
  # Use CRS 2056 (Swiss LV95)
  gi <- calculate_green_index(osm_data, 2056)
  
  expect_s3_class(gi, "sf")
  expect_true("green_index" %in% names(gi))
  expect_true(all(gi$green_index >= 0 & gi$green_index <= 1))
})

test_that("accessibility_greenspace works correctly", {
  osm_data <- get_osm_data("Oerlikon, Zurich, Switzerland")
  
  # Lat/Lon for Oerlikon area
  res <- accessibility_greenspace(
    green_area_data = osm_data$green_areas,
    location_lat = 47.4115,
    location_lon = 8.5441,
    max_walk_time = 5
  )
  
  expect_true(is.list(res))
  expect_s3_class(res$map, "leaflet")
  expect_s3_class(res$isochrones, "sf")
})

test_that("calculate_percentage works correctly", {
  df <- data.frame(green_index = c(0.2, 0.5, 0.8))
  pct <- calculate_percentage(df)
  
  expect_true(is.data.frame(pct))
  expect_equal(sum(pct$percentage), 100)
  expect_true(all(c("<0.4", "0.4-0.7", ">0.7") %in% pct$green_index_category))
})

test_that("convert_to_point works correctly", {
  # Create a small polygon sf object
  poly <- sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol=2, byrow=TRUE)))
  poly_sf <- sf::st_sf(geometry = sf::st_sfc(poly, crs = 4326))
  
  pts <- convert_to_point(poly_sf)
  
  expect_s3_class(pts, "sf")
  expect_true(all(sf::st_geometry_type(pts) == "POINT"))
})
