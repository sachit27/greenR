# Run from the package root: Rscript scripts/benchmark-green-index.R
# Synthetic scaling benchmark; excludes data generation and network downloads.
library(sf)
source("R/calculate_green_index.R")
set.seed(20260923)
make_data <- function(n) {
  side <- sqrt(n / 10000) * 10000
  xy <- function(k) cbind(500000 + runif(k, 0, side),
                          5700000 + runif(k, 0, side))
  origins <- xy(n)
  edges <- st_sfc(lapply(seq_len(n), function(i) {
    p <- origins[i, ]
    st_linestring(rbind(p, p + c(30, 10), p + c(65, 5), p + c(100, 20)))
  }), crs = 32630)
  centres <- xy(n / 5)
  parks <- st_sfc(lapply(seq_len(nrow(centres)), function(i) {
    angles <- seq(0, 2 * pi, length.out = 33)
    ring <- sweep(cbind(cos(angles), sin(angles)) * runif(1, 15, 80),
                  2, centres[i, ], "+")
    ring[nrow(ring), ] <- ring[1, ]
    st_polygon(list(ring))
  }), crs = 32630)
  list(highways = st_sf(id = seq_len(n), geometry = edges),
       green_areas = st_sf(geometry = parks),
       trees = st_as_sf(as.data.frame(xy(n * 2)), coords = c("V1", "V2"),
                        crs = 32630))
}
results <- list()
for (n in c(10000L, 50000L, 100000L)) {
  dat <- make_data(n)
  for (crs in c(32630L, 4326L)) {
    elapsed <- replicate(3, {
      gc()
      timing <- system.time(out <- calculate_green_index(dat, crs,
                                                          show_time = FALSE))
      stopifnot(nrow(out) == n, all(is.finite(out$green_index)))
      unname(timing[["elapsed"]])
    })
    row <- data.frame(streets = n, parks = n / 5, trees = 2 * n,
                      output_crs = crs, median_seconds = median(elapsed),
                      min_seconds = min(elapsed), max_seconds = max(elapsed))
    print(row, row.names = FALSE)
    results[[length(results) + 1L]] <- row
  }
  # Independent brute-force reference for 25 streets against every feature.
  reference <- function(features) {
    d <- apply(as.matrix(st_distance(dat$highways[1:25, ], features)), 1, min)
    ifelse(d <= 120, exp(-d / 100), 0)
  }
  out <- calculate_green_index(dat, 32630, show_time = FALSE)
  stopifnot(isTRUE(all.equal(out$green_index_tree[1:25], reference(dat$trees))),
            isTRUE(all.equal(out$green_index_green_area[1:25],
                             reference(dat$green_areas))))
}
write.csv(do.call(rbind, results), "scripts/benchmark-green-index-results.csv",
          row.names = FALSE)
print(sessionInfo())
