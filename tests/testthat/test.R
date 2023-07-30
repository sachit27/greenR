library(testthat)
library(greenR)

test_that("get_osm_data function works correctly", {
  # this will download real data from the internet
  osm_data <- get_osm_data("Oerlikon, Zurich, Switzerland")

  # check if the function returns a list
  expect_true(is.list(osm_data))

  # check if the list contains the elements highways, green_areas, and trees
  expect_true(all(c("highways", "green_areas", "trees") %in% names(osm_data)))

  # check if each element in the list is of class osmdata_sf
  expect_s3_class(osm_data$highways, "osmdata_sf")
  expect_s3_class(osm_data$green_areas, "osmdata_sf")
  expect_s3_class(osm_data$trees, "osmdata_sf")
})
