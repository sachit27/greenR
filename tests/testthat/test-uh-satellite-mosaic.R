test_that("paged clear-pixel mosaic fills spatial gaps and records provenance", {
  boundary <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(
    c(xmin = 0, ymin = 0, xmax = 40, ymax = 10), crs = 32632)))
  # The STAC bbox is WGS84; the synthetic scenes intentionally use metric
  # coordinates so that the four 10 m cells have exact known values.
  a <- terra::rast(nrows = 1, ncols = 2, xmin = 0, xmax = 20,
                   ymin = 0, ymax = 10, crs = "EPSG:32632", vals = c(.2, .4))
  b <- terra::rast(nrows = 1, ncols = 2, xmin = 20, xmax = 40,
                   ymin = 0, ymax = 10, crs = "EPSG:32632", vals = c(.6, .8))
  item <- function(id, cloud) list(id = id,
    properties = list(datetime = "2025-08-01T10:00:00Z", "eo:cloud_cover" = cloud))
  page1 <- list(features = list(item("left", 1)),
                links = list(list(rel = "next", href = "unused")))
  page2 <- list(features = list(item("right", 2)), links = list())
  calls <- 0L
  local_mocked_bindings(
    .uh_satellite_page = function(collection, bbox, datetime, page = NULL) {
      calls <<- calls + 1L
      if (is.null(page)) page1 else page2
    },
    .uh_sign_satellite_page = identity,
    .uh_satellite_scene = function(item, boundary, kind) {
      if (item$id == "left") a else b
    },
    .package = "greenR")
  cache <- tempfile(); dir.create(cache)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  out <- greenR:::.uh_satellite_mosaic(boundary, "2025-08-01/2025-08-31",
                                      cache, "ndvi", min_coverage = 1)
  expect_equal(calls, 2L)
  expect_equal(as.vector(terra::values(out$raster)), c(.2, .4, .6, .8), tolerance = 1e-6)
  expect_equal(out$item_id, c("left", "right"))
  expect_equal(out$pages_read, 2L)
  expect_equal(out$coverage, 1)
  source_path <- list.files(file.path(cache, "ndvi"), pattern = "_source\\.tif$", full.names = TRUE)
  expect_length(source_path, 1)
  expect_equal(as.vector(terra::values(terra::rast(source_path))), c(1, 1, 2, 2))
  cached <- greenR:::.uh_satellite_mosaic(boundary, "2025-08-01/2025-08-31",
                                         cache, "ndvi", use_cache = TRUE,
                                         min_coverage = 1)
  expect_equal(calls, 2L)
  expect_equal(cached$item_id, out$item_id)
})

test_that("truncated pagination cannot yield a partial satellite score", {
  boundary <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(
    c(xmin = 0, ymin = 0, xmax = 40, ymax = 10), crs = 32632)))
  a <- terra::rast(nrows = 1, ncols = 2, xmin = 0, xmax = 20,
                   ymin = 0, ymax = 10, crs = "EPSG:32632", vals = c(.2, .4))
  one <- list(features = list(list(id = "left", properties = list(
    datetime = "2025-08-01T10:00:00Z", "eo:cloud_cover" = 1))),
    links = list(list(rel = "next", href = "unused")))
  local_mocked_bindings(
    .uh_satellite_page = function(...) one,
    .uh_sign_satellite_page = identity,
    .uh_satellite_scene = function(...) a,
    .package = "greenR")
  cache <- tempfile(); dir.create(cache)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  expect_error(greenR:::.uh_satellite_mosaic(boundary,
    "2025-08-01/2025-08-31", cache, "ndvi", max_pages = 1L),
    "page limit reached")
  expect_length(list.files(file.path(cache, "ndvi")), 0)
})
