# Internal shared helpers for network mode filtering
# Used by analyze_green_accessibility().
# These are not exported — they are package-internal functions.

# Internal helper: get travel mode parameters (speed in km/h, highway tag filters)
.get_mode_params <- function(mode) {
  switch(mode,
         walking = list(speed = 5, filters = c(
           "footway", "path", "pedestrian", "living_street", "residential",
           "service", "tertiary", "unclassified", "track", "steps")),
         cycling = list(speed = 15, filters = c(
           "cycleway", "path", "living_street", "residential", "service",
           "tertiary", "unclassified", "track")),
         driving = list(speed = 40, filters = c(
           "motorway", "trunk", "primary", "secondary", "tertiary",
           "residential", "service", "living_street", "unclassified")),
         stop("Invalid mode: ", mode, call. = FALSE))
}

# Internal helper: filter street network by travel mode and compute edge lengths
.filter_network <- function(network, mode_params) {
  network %>%
    dplyr::filter(highway %in% mode_params$filters) %>%
    dplyr::mutate(length = sf::st_length(.))
}
