# Run from the package root after installing development dependencies:
# Rscript scripts/live-smoke.R
# This deliberately makes live Nominatim and Overpass requests.

pkgload::load_all(".", quiet = TRUE)

city_data <- get_osm_data("City of London, UK", cache = TRUE, verbose = TRUE)
stopifnot(inherits(city_data$highways$osm_lines, "sf"))
stopifnot(nrow(city_data$highways$osm_lines) > 0)

green_network <- calculate_green_index(city_data, crs_code = 4326, D = 100)
stopifnot(inherits(green_network, "sf"))
stopifnot(nrow(green_network) == nrow(city_data$highways$osm_lines))
stopifnot(all(is.finite(green_network$green_index)))
stopifnot(all(green_network$green_index >= 0 &
              green_network$green_index <= 1))
stopifnot(sf::st_crs(green_network)$epsg == 4326)

cat("Live City of London analysis passed with ",
    nrow(green_network), " street segments.\n", sep = "")
