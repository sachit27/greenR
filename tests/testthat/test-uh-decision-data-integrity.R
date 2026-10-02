test_that("Sentinel-2 processing baseline offset is applied before NDVI", {
  r <- terra::rast(nrows = 1, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 1)
  terra::values(r) <- c(3000, 0)
  n <- r; terra::values(n) <- c(7000, 0)
  item <- list(properties = list("s2:processing_baseline" = "05.11"),
               assets = list(B04 = list(), B08 = list()))
  red <- greenR:::.uh_s2_reflectance(r, item, "B04")
  nir <- greenR:::.uh_s2_reflectance(n, item, "B08")
  ndvi <- (nir - red) / (nir + red)
  expect_equal(unname(terra::values(ndvi)[1, 1]), 0.5, tolerance = 1e-12)
  expect_true(is.na(terra::values(ndvi)[2, 1]))

  item$properties[["s2:processing_baseline"]] <- "03.01"
  red_old <- greenR:::.uh_s2_reflectance(r, item, "B04")
  nir_old <- greenR:::.uh_s2_reflectance(n, item, "B08")
  expect_equal(unname(terra::values((nir_old - red_old) / (nir_old + red_old))[1, 1]), 0.4)

  item$assets$B04[["raster:bands"]] <- list(list(scale = 0.0001, offset = -0.1))
  expect_equal(unname(terra::values(greenR:::.uh_s2_reflectance(r, item, "B04"))[1, 1]), 0.2)
  physical <- r; terra::values(physical) <- c(0.2, NA_real_)
  expect_equal(unname(terra::values(greenR:::.uh_s2_reflectance(physical, item, "B04", already_scaled = TRUE))[1, 1]), 0.2)
})

test_that("Landsat ST scaling handles raw DNs and already scaled Kelvin", {
  st <- terra::rast(nrows = 1, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 1)
  terra::values(st) <- c(44000, 0)
  celsius <- greenR:::.uh_lst_celsius(st)
  expect_equal(unname(terra::values(celsius)[1, 1]), 44000 * 0.00341802 + 149 - 273.15)
  expect_true(is.na(terra::values(celsius)[2, 1]))
  k <- st; terra::values(k) <- c(300, NA_real_)
  expect_equal(unname(terra::values(greenR:::.uh_lst_celsius(k, already_scaled = TRUE))[1, 1]), 26.85)
})

test_that("Landsat QA_PIXEL rejects cloud/shadow and accepts clear water", {
  qa <- terra::rast(nrows = 1, ncols = 6, xmin = 0, xmax = 6, ymin = 0, ymax = 1)
  terra::values(qa) <- c(21824, 21952, 44032, 44096, 44288, 21824 + 16)
  mask <- greenR:::.uh_landsat_clear_mask(qa)
  expect_equal(as.vector(terra::values(mask)), c(1, 1, 0, 0, 0, 0))
})

test_that("missing observations fail before they can become priorities", {
  expect_error(greenR:::.uh_require_complete(c(0.3, NA_real_), "NDVI"), "lack valid observations")
  expect_silent(greenR:::.uh_require_complete(c(0, 0.3), "NDVI"))
})

test_that("scene cache preserves real acquisition metadata", {
  item <- list(id = "scene-42", properties = list(datetime = "2025-08-11T10:30:00Z"))
  r <- terra::rast(nrows = 1, ncols = 1, xmin = 0, xmax = 1, ymin = 0, ymax = 1, vals = 0.4)
  path <- tempfile(fileext = ".tif")
  greenR:::.uh_save_scene(path, r, item)
  expect_equal(greenR:::.uh_cached_scene(path), list(item_id = "scene-42", datetime = "2025-08-11T10:30:00Z"))
  expect_true(greenR:::.uh_scene_in_range(path, "2025-08-01/2025-08-31"))
  expect_false(greenR:::.uh_scene_in_range(path, "2025-07-01/2025-07-31"))
  unlink(c(path, paste0(path, ".rds")))
})

test_that("canyon orientation output is explicitly a proxy", {
  lines <- sf::st_sfc(sf::st_linestring(rbind(c(0, 0), c(0, 1))),
                      sf::st_linestring(rbind(c(1, 0), c(2, 0))), crs = 32632)
  x <- list(canyons = sf::st_sf(canyon_bearing = c(0, 90),
                                canopy_pct_chm = c(0, 50), geometry = lines))
  out <- greenR::screen_canyon_orientation(x, latitude = 45)
  expect_equal(out$canyons$orientation_exposure_proxy, c(1 - sin(pi / 4), 1))
  expect_warning(greenR::emulate_canyon_microclimate(x, latitude = 45), "deprecated")
})
