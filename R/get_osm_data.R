# Geometry builders. All produce sfg objects in EPSG:4326.
# ---------------------------------------------------------------------------

#' Convert OSM way geometry to matrix
#' @param geom_list List of geometry points.
#' @return A matrix of coordinates.
#' @export
.way_geom_to_matrix <- function(geom_list) {
  if (is.null(geom_list) || length(geom_list) == 0) return(NULL)
  coords <- tryCatch(do.call(rbind, lapply(geom_list, function(p) c(p$lon, p$lat))), error = function(e) NULL)
  if (is.null(coords) || !is.matrix(coords) || nrow(coords) < 2) return(NULL)
  coords
}

.is_closed_ring <- function(coords) {
  !is.null(coords) && nrow(coords) >= 4 &&
    isTRUE(all.equal(coords[1, ], coords[nrow(coords), ],
                     tolerance = 1e-9, check.attributes = FALSE))
}

.close_ring <- function(coords) {
  if (nrow(coords) < 3) return(NULL)
  if (!isTRUE(all.equal(coords[1, ], coords[nrow(coords), ], tolerance = 1e-9, check.attributes = FALSE))) coords <- rbind(coords, coords[1, ])
  if (nrow(coords) < 4) return(NULL)
  coords
}

.tags_to_df <- function(tag_lists, osm_ids) {
  tag_keys <- unique(unlist(lapply(tag_lists, names), use.names = FALSE))
  out <- list(osm_id = as.character(osm_ids))
  for (k in tag_keys) out[[k]] <- vapply(tag_lists, function(t) if (is.null(t[[k]])) NA_character_ else as.character(t[[k]]), character(1))
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}

.build_points_sf <- function(elements) {
  nodes <- Filter(function(e) identical(e$type, "node") && !is.null(e$lat) && !is.null(e$lon), elements)
  if (length(nodes) == 0) return(NULL)
  geoms <- lapply(nodes, function(n) sf::st_point(c(n$lon, n$lat)))
  df <- .tags_to_df(lapply(nodes, function(n) if (is.null(n$tags)) list() else n$tags), vapply(nodes, function(n) n$id, numeric(1)))
  sf::st_sf(df, geometry = sf::st_sfc(geoms, crs = 4326))
}

.build_lines_sf <- function(elements) {
  ways <- Filter(function(e) identical(e$type, "way") && !is.null(e$geometry), elements)
  if (length(ways) == 0) return(NULL)
  geoms <- list(); keep <- integer(0)
  for (i in seq_along(ways)) {
    mat <- .way_geom_to_matrix(ways[[i]]$geometry)
    if (!is.null(mat)) { geoms[[length(geoms)+1]] <- sf::st_linestring(mat); keep <- c(keep, i) }
  }
  if (length(geoms) == 0) return(NULL)
  sf::st_sf(.tags_to_df(lapply(ways[keep], function(w) if (is.null(w$tags)) list() else w$tags), vapply(ways[keep], function(w) w$id, numeric(1))), geometry = sf::st_sfc(geoms, crs = 4326))
}

.build_polygons_sf <- function(elements) {
  ways <- Filter(function(e) identical(e$type, "way") && !is.null(e$geometry), elements)
  if (length(ways) == 0) return(NULL)
  geoms <- list(); keep <- integer(0)
  for (i in seq_along(ways)) {
    mat <- .way_geom_to_matrix(ways[[i]]$geometry)
    if (!.is_closed_ring(mat)) next
    if (!is.null(mat)) {
      poly <- tryCatch(sf::st_polygon(list(mat)), error = function(e) NULL)
      if (!is.null(poly)) { geoms[[length(geoms)+1]] <- poly; keep <- c(keep, i) }
    }
  }
  if (length(geoms) == 0) return(NULL)
  sf::st_sf(.tags_to_df(lapply(ways[keep], function(w) if (is.null(w$tags)) list() else w$tags), vapply(ways[keep], function(w) w$id, numeric(1))), geometry = sf::st_sfc(geoms, crs = 4326))
}

.build_multipolygons_sf <- function(elements) {
  rels <- Filter(function(e) identical(e$type, "relation") && !is.null(e$members), elements)
  if (length(rels) == 0) return(NULL)
  geoms <- list(); keep <- integer(0)
  polygonize_members <- function(members) {
    lines <- list()
    for (member in members) {
      mat <- .way_geom_to_matrix(member$geometry)
      if (!is.null(mat)) lines[[length(lines) + 1L]] <- sf::st_linestring(mat)
    }
    if (!length(lines)) return(NULL)
    tryCatch({
      polygonized <- sf::st_polygonize(sf::st_union(sf::st_sfc(lines, crs = 4326)))
      polys <- sf::st_collection_extract(polygonized, "POLYGON")
      if (!length(polys) || all(sf::st_is_empty(polys))) NULL else sf::st_union(polys)
    }, error = function(e) NULL)
  }
  for (i in seq_along(rels)) {
    members <- rels[[i]]$members
    outer <- polygonize_members(Filter(function(m) is.null(m$role) ||
                                       m$role != "inner", members))
    if (is.null(outer)) next
    inner <- polygonize_members(Filter(function(m) identical(m$role, "inner"),
                                       members))
    if (!is.null(inner))
      outer <- tryCatch(sf::st_difference(outer, inner), error = function(e) outer)
    mp <- tryCatch(sf::st_cast(outer, "MULTIPOLYGON")[[1]],
                   error = function(e) NULL)
    if (!is.null(mp) && !sf::st_is_empty(mp)) {
      geoms[[length(geoms) + 1L]] <- mp
      keep <- c(keep, i)
    }
  }
  if (length(geoms) == 0) return(NULL)
  sf::st_sf(.tags_to_df(lapply(rels[keep], function(r) if (is.null(r$tags)) list() else r$tags), vapply(rels[keep], function(r) r$id, numeric(1))), geometry = sf::st_sfc(geoms, crs = 4326))
}

#' Download OSM data
#'
#' Downloads OpenStreetMap (OSM) data for a specified location or bounding box.
#' Includes highways, green areas, trees, and water bodies for the specified
#' location.
#'
#' @param bbox Either a string representing the location (e.g., "Lausanne, Switzerland") or
#'   a numeric vector of length 4 representing the bounding box coordinates
#'   in the order: c(left, bottom, right, top).
#' @param server_url Nominatim base URL. Default: \code{"https://nominatim.openstreetmap.org"}.
#' @param username Ignored.
#' @param password Ignored.
#' @param cache Logical. If TRUE, cache results on disk and reuse them for the same input. Defaults to \code{FALSE} to ensure fresh data is always fetched. Set to \code{TRUE} to enable caching during rapid development or testing cycles to avoid Overpass API rate-limiting blocks.
#' @param cache_dir Character. Directory used for persistent disk cache.
#' @param timeout Numeric. Overpass query timeout in seconds.
#' @param overpass_servers Character vector of global Overpass API endpoints
#'   tried in order. Override this when using a private instance.
#' @param include_highways Logical. If TRUE, fetch highway features.
#' @param include_green_areas Logical. If TRUE, fetch green area polygons.
#' @param include_trees Logical. If TRUE, fetch tree points.
#' @param include_water Logical. If TRUE, fetch water bodies.
#' @param include_buildings Logical. If TRUE, fetch building footprints.
#' @param verbose Logical. Print progress messages. Default TRUE.
#'
#' @return A list containing sf objects.
#' @export
#' @examples
#' \dontrun{
#'   # Using a location name
#'   osm_data <- get_osm_data("Lausanne, Switzerland")
#'
#'   # Using coordinates for a bounding box
#'   bbox_coords <- c(6.6, 46.5, 6.7, 46.6)  # Example coordinates near Lausanne
#'   osm_data <- get_osm_data(bbox_coords)
#' }
get_osm_data <- function(
  bbox,
  server_url  = "https://nominatim.openstreetmap.org",
  username    = NULL, password = NULL, cache = FALSE,
  cache_dir   = tools::R_user_dir("greenR", which = "cache"),
  timeout     = 180,
  overpass_servers = c("https://overpass-api.de/api/interpreter",
                       "https://overpass.private.coffee/api/interpreter"),
  include_highways = TRUE, include_green_areas = TRUE, include_trees = TRUE,
  include_water = TRUE, include_buildings = FALSE,
  verbose = TRUE
) {
  vlog <- function(...) if (isTRUE(verbose)) message(...)

  if (!is.numeric(timeout) || length(timeout) != 1L || !is.finite(timeout) ||
      timeout < 1) stop("timeout must be a positive number of seconds.", call. = FALSE)
  if (!is.character(overpass_servers) || !length(overpass_servers) ||
      anyNA(overpass_servers) || any(!nzchar(overpass_servers)))
    stop("overpass_servers must contain at least one endpoint.", call. = FALSE)
  flags <- list(include_highways, include_green_areas, include_trees,
                include_water, include_buildings)
  if (!all(vapply(flags, function(x) is.logical(x) && length(x) == 1L &&
                  !is.na(x), logical(1))))
    stop("Each include_* argument must be TRUE or FALSE.", call. = FALSE)
  USER_AGENT <- "greenR R package (github.com/sachit27/greenR)"

  .geocode_nominatim <- function(place, base_url = "https://nominatim.openstreetmap.org") {
    url <- paste0(sub("/+$", "", base_url), "/search")
    resp <- tryCatch(httr::GET(url, query = list(q = place, format = "json", limit = "1"), httr::add_headers(`User-Agent` = USER_AGENT), httr::timeout(15)), error = function(e) NULL)
    if (is.null(resp) || httr::http_error(resp)) return(NULL)
    parsed <- httr::content(resp, as = "parsed")
    if (length(parsed) == 0) return(NULL)
    bb <- as.numeric(unlist(parsed[[1]]$boundingbox))
    c(left = bb[3], bottom = bb[1], right = bb[4], top = bb[2])
  }

  .resolve_bbox <- function(x) {
    if (is.numeric(x)) {
      if (length(x) != 4L || any(!is.finite(x)) ||
          x[1] >= x[3] || x[2] >= x[4] ||
          x[1] < -180 || x[3] > 180 || x[2] < -90 || x[4] > 90)
        stop("bbox must be c(left, bottom, right, top) in longitude/latitude.", call. = FALSE)
      names(x) <- c("left", "bottom", "right", "top")
      return(x)
    }
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x))
      stop("bbox must be a place name or four coordinates.", call. = FALSE)
    bq <- .geocode_nominatim(x, server_url)
    if (is.null(bq)) stop("Geocoding failed.")
    bq
  }

  .fetch_overpass <- function(ql, http_timeout = timeout + 15) {
    for (srv in overpass_servers) {
      vlog("  [overpass] trying: ", srv)
      resp <- tryCatch(httr::POST(srv, body = list(data = ql), encode = "form", httr::timeout(http_timeout), httr::add_headers(`User-Agent` = USER_AGENT)), error = function(e) NULL)
      if (!is.null(resp) && !httr::http_error(resp)) {
        txt <- httr::content(resp, as = "text", encoding = "UTF-8")
        parsed <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
        if (!is.null(parsed) && !is.null(parsed$elements)) {
          vlog(sprintf("  [overpass] success (%d elements)", length(parsed$elements)))
          return(parsed)
        }
      }
    }
    stop("All Overpass servers failed; retry later or set overpass_servers.",
         call. = FALSE)
  }

  bq <- .resolve_bbox(bbox)
  if (isTRUE(cache)) {
    dir.create(cache_dir, FALSE, TRUE)
    cache_key <- paste(c(bq, include_highways, include_green_areas,
                         include_trees, include_water, include_buildings),
                       collapse = "_")
    cfile <- file.path(cache_dir, paste0("osm_v3_", cache_key, ".rds"))
    if (file.exists(cfile)) { vlog("[get_osm_data] Using cache."); return(readRDS(cfile)) }
  }

  # Request all selected features together: one Overpass round trip and one
  # failover sequence instead of a separate request for every feature class.
  bb <- paste(bq[c("bottom", "left", "top", "right")], collapse = ",")
  clauses <- character()
  add <- function(types, filter) {
    for (type in types)
      clauses <<- c(clauses, sprintf("%s[%s](%s);", type, filter, bb))
  }
  green_landuse <- '"landuse"~"^(forest|vineyard|plant_nursery|orchard|greenfield|recreation_ground|allotments|meadow|village_green|flowerbed|grass|farmland)$"'
  green_leisure <- '"leisure"~"^(garden|dog_park|nature_reserve|park)$"'
  if (include_highways) add("way", '"highway"')
  if (include_green_areas) {
    add(c("way", "rel"), green_landuse)
    add(c("way", "rel"), green_leisure)
  }
  if (include_trees) add("node", '"natural"="tree"')
  if (include_water) {
    add(c("way", "rel"), '"natural"="water"')
    add(c("way", "rel"), '"waterway"="riverbank"')
  }
  if (include_buildings) add(c("way", "rel"), '"building"')
  elements <- if (length(clauses)) {
    ql <- sprintf("[out:json][timeout:%d];(%s);out geom;",
                  as.integer(timeout), paste(clauses, collapse = ""))
    .fetch_overpass(ql)$elements
  } else list()
  has_tag <- function(e, key, values = NULL) {
    tag <- e$tags[[key]]
    !is.null(tag) && (is.null(values) || tag %in% values)
  }
  subset_elements <- function(predicate)
    Filter(predicate, elements)
  polygon_layer <- function(x) .safe_rbind_sf(.build_polygons_sf(x),
                                              .build_multipolygons_sf(x))
  res <- list()
  if (include_highways)
    res$highways <- list(osm_lines = .build_lines_sf(subset_elements(
      function(e) has_tag(e, "highway"))))
  if (include_green_areas)
    res$green_areas <- list(osm_polygons = polygon_layer(subset_elements(
      function(e) has_tag(e, "landuse", c("forest", "vineyard", "plant_nursery",
        "orchard", "greenfield", "recreation_ground", "allotments", "meadow",
        "village_green", "flowerbed", "grass", "farmland")) ||
        has_tag(e, "leisure", c("garden", "dog_park", "nature_reserve", "park")))))
  if (include_trees)
    res$trees <- list(osm_points = .build_points_sf(subset_elements(
      function(e) has_tag(e, "natural", "tree"))))
  if (include_water)
    res$water <- list(osm_polygons = polygon_layer(subset_elements(
      function(e) has_tag(e, "natural", "water") ||
        (has_tag(e, "waterway", "riverbank") &&
         (identical(e$type, "relation") ||
          .is_closed_ring(.way_geom_to_matrix(e$geometry)))))))
  if (include_buildings)
    res$buildings <- list(osm_polygons = polygon_layer(subset_elements(
      function(e) has_tag(e, "building"))))

  res_final <- list(
    highways = res$highways,
    green_areas = res$green_areas,
    trees = res$trees,
    water = res$water,
    buildings = res$buildings
  )
  if (isTRUE(cache)) saveRDS(res_final, cfile)
  vlog("[get_osm_data] Done.")
  return(res_final)
}

.safe_rbind_sf <- function(x, y) {
  if (is.null(x) || nrow(x) == 0) return(y)
  if (is.null(y) || nrow(y) == 0) return(x)

  # Pad missing columns with NA
  cols_x <- names(x)
  cols_y <- names(y)
  all_cols <- union(cols_x, cols_y)
  all_cols <- setdiff(all_cols, "geometry")

  for (col in setdiff(all_cols, cols_x)) x[[col]] <- NA
  for (col in setdiff(all_cols, cols_y)) y[[col]] <- NA

  res <- rbind(x[, c(all_cols, "geometry")], y[, c(all_cols, "geometry")])
  return(res)
}
