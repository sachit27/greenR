# Sky-view factor engine: analytic and geometric checks with known answers.

svf_grid <- function(n = 801, res = 1, z = 0) {
  terra::rast(nrows = n, ncols = n, xmin = 0, xmax = n * res, ymin = 0, ymax = n * res, crs = "EPSG:32632", vals = z)
}
svf_point <- function(x, y) sf::st_sf(point_id = 1, geometry = sf::st_sfc(sf::st_point(c(x, y)), crs = 32632))
svf_run <- function(obst, pts, ...) {
  terrain <- terra::rast(obst); terra::values(terrain) <- 0
  greenR:::.uh_svf_compute_points(pts, terrain, obst, observer_height_m = 0, ...)
}

test_that("open flat ground has SVF exactly 1", {
  r <- svf_grid(201)
  p <- svf_run(r, svf_point(100.5, 100.5), max_distance_m = 80)
  expect_equal(p$svf, 1)
  expect_equal(p$max_horizon_deg, 0)
  expect_equal(p$ray_truncated_share, 0)
})

test_that("street canyon matches the analytic sky-view factor", {
  # Walls parallel to north, height H above the observer, faces at +/- W/2.
  # Infinite canyon: SVF = cos(atan(2H/W)) (Oke 1987). With a finite search radius R the exact
  # expectation for the same azimuths is mean(cos^2(beta)), beta = atan(H / d), d = (W/2)/|sin(az)|,
  # and beta = 0 where d > R. The engine must reproduce that expectation.
  n <- 2000; res <- 0.5; R <- 450
  for (case in list(c(H = 10, W = 20), c(H = 20, W = 10), c(H = 15, W = 30))) {
    H <- case[["H"]]; W <- case[["W"]]
    r <- svf_grid(n, res = res)
    xc <- n * res / 2                                   # a cell edge
    cx <- (seq_len(n) - 0.5) * res
    m <- matrix(0, n, n); m[, cx < xc - W / 2 | cx > xc + W / 2] <- H
    terra::values(r) <- as.vector(t(m))
    nd <- 720
    p <- svf_run(r, svf_point(xc, xc), n_directions = nd, max_distance_m = R)
    az <- (seq_len(nd) - 1) * 2 * pi / nd
    d <- (W / 2) / abs(sin(az)); beta <- ifelse(d <= R, atan(H / d), 0)
    expect_equal(p$svf, mean(cos(beta)^2), tolerance = 1e-9)       # finite-radius exact value
    expect_equal(p$svf, cos(atan(2 * H / W)), tolerance = 0.03)     # infinite-canyon value
    expect_equal(p$ray_truncated_share, 0)
  }
})

test_that("azimuths are compass bearings: a wall to the north is at 0 deg, east at 90 deg", {
  n <- 201; r <- svf_grid(n)
  m <- matrix(0, n, n)
  m[1:60, ] <- 30                             # top rows = north side of the grid (y > 141)
  m[, 180:201] <- pmax(m[, 180:201], 5)       # low wall on the east side
  terra::values(r) <- as.vector(t(m))
  p <- svf_run(r, svf_point(100.5, 100.5), n_directions = 4, max_distance_m = 90, return_raw_angles = TRUE)
  h <- p$horizon_angles[[1]] * 180 / pi       # directions N, E, S, W
  expect_gt(h[1], 30)                         # atan(30 / ~41 m)
  expect_equal(h[1], atan(30 / (141 - 100.5)) * 180 / pi, tolerance = 0.03)
  expect_gt(h[2], 0); expect_lt(h[2], h[1])   # east wall lower
  expect_equal(h[3], 0); expect_equal(h[4], 0)
})

test_that("thin walls are never stepped over", {
  n <- 201; r <- svf_grid(n)
  m <- matrix(0, n, n); m[1:201 <= 0, ] <- 0
  m[(201 - 119):(201 - 115), ] <- 20          # 5 m thick wall, 15-19 m north of the observer
  terra::values(r) <- as.vector(t(m))
  p_default <- svf_run(r, svf_point(100.5, 100.5), max_distance_m = 60, return_raw_angles = TRUE, n_directions = 4)
  expect_gt(p_default$horizon_angles[[1]][1] * 180 / pi, 45)
  # a 1 m wide wall (one cell) is also caught
  m2 <- matrix(0, n, n); m2[201 - 117, ] <- 20; terra::values(r) <- as.vector(t(m2))
  p_thin <- svf_run(r, svf_point(100.5, 100.5), max_distance_m = 60, return_raw_angles = TRUE, n_directions = 4)
  expect_equal(p_thin$horizon_angles[[1]][1] * 180 / pi, atan(20 / (117 - 100.5)) * 180 / pi, tolerance = 1e-6)
  expect_message(svf_run(r, svf_point(100.5, 100.5), max_distance_m = 60, step_m = 10), "no longer used")
})

test_that("points inside buildings get NA, points under canopy are flagged", {
  n <- 101; r <- svf_grid(n)
  b <- svf_grid(n); terra::values(b) <- NA
  m <- matrix(NA_real_, n, n); m[40:60, 40:60] <- 12
  terra::values(b) <- as.vector(t(m))
  obst <- max(r, b, na.rm = TRUE)
  pts <- sf::st_sf(point_id = 1:2, geometry = sf::st_sfc(sf::st_point(c(50.5, 50.5)), sf::st_point(c(10.5, 10.5)), crs = 32632))
  terrain <- r
  p <- greenR:::.uh_svf_compute_points(pts, terrain, obst, observer_height_m = 1.5, max_distance_m = 60, building_raster = b)
  expect_true(p$inside_building[1]); expect_true(is.na(p$svf[1]))
  expect_false(p$inside_building[2]); expect_true(is.finite(p$svf[2])); expect_lt(p$svf[2], 1)
  # a canopy-only obstruction over the second point: flagged, not treated as a building
  can <- r; mm <- matrix(0, n, n); mm[88:94, 8:14] <- 10; terra::values(can) <- as.vector(t(mm))
  p2 <- greenR:::.uh_svf_compute_points(pts[2, ], terrain, max(r, can), observer_height_m = 1.5, max_distance_m = 40, building_raster = b)
  expect_true(p2$under_canopy); expect_false(p2$inside_building)
})

test_that("rays leaving the data are reported, not counted as sky silently", {
  r <- svf_grid(101)
  p <- svf_run(r, svf_point(5.5, 50.5), max_distance_m = 60, n_directions = 8)
  expect_gt(p$ray_truncated_share, 0)
  p2 <- svf_run(r, svf_point(50.5, 50.5), max_distance_m = 40, n_directions = 8)
  expect_equal(p2$ray_truncated_share, 0)
})

test_that("the data area extends the sample area by the full horizon radius", {
  b <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(8.50, 47.37), c(8.51, 47.37), c(8.51, 47.38), c(8.50, 47.38), c(8.50, 47.37)))), crs = 4326))
  a <- greenR:::.uh_svf_make_analysis_area(b, bbox = c(8.50, 47.37, 8.51, 47.38), radius_m = 300)
  sb <- sf::st_bbox(a$sample_area); ab <- sf::st_bbox(a$analysis_area)
  expect_true(all(c(sb[["xmin"]] - ab[["xmin"]], ab[["xmax"]] - sb[["xmax"]], sb[["ymin"]] - ab[["ymin"]], ab[["ymax"]] - sb[["ymax"]]) >= 300))
})

test_that("buildings narrower than a cell still enter the obstruction raster", {
  terrain <- svf_grid(20, res = 5)
  sq <- sf::st_polygon(list(rbind(c(41, 41), c(43, 41), c(43, 43), c(41, 43), c(41, 41))))   # 2 x 2 m inside one 5 m cell
  bld <- sf::st_sf(roof_z = 15, geometry = sf::st_sfc(sq, crs = 32632))
  o <- greenR:::.uh_svf_build_obstruction(terrain, bld)
  expect_equal(max(terra::values(o$obstruction), na.rm = TRUE), 15)
})

test_that("skyline plot labels follow the compass convention", {
  pts <- svf_point(0, 0); pts$svf <- 0.8
  pts$horizon_angles <- list(c(40, 0, 0, 0) * pi / 180)
  g <- uh_svf_plot_skyline(pts, 1)
  b <- ggplot2::ggplot_build(g)
  expect_s3_class(g, "ggplot")
  expect_true(any(grepl("^N", g$scales$get_scales("x")$labels)))
})
