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
  nrow(coords) >= 4 && isTRUE(all.equal(coords[1, ], coords[nrow(coords), ], tolerance = 1e-9, check.attributes = FALSE))
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
    mat <- .close_ring(.way_geom_to_matrix(ways[[i]]$geometry))
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
  for (i in seq_along(rels)) {
    lines <- list()
    for (m in rels[[i]]$members) {
      mat <- .way_geom_to_matrix(m$geometry)
      if (!is.null(mat)) lines[[length(lines)+1]] <- sf::st_linestring(mat)
    }
    if (length(lines) > 0) {
      combined <- tryCatch(sf::st_polygonize(sf::st_union(sf::st_sfc(lines, crs = 4326))), error = function(e) NULL)
      if (!is.null(combined) && length(combined) > 0) {
         # Handle GEOMETRYCOLLECTION
         gc <- combined[[1]]
         polys <- if (inherits(gc, "GEOMETRYCOLLECTION")) Filter(function(g) inherits(g, "POLYGON"), unclass(gc)) else if (inherits(gc, "POLYGON")) list(gc) else list()
         if (length(polys) > 0) {
           mp <- tryCatch(sf::st_multipolygon(lapply(polys, function(p) unclass(p))), error = function(e) NULL)
           if (!is.null(mp)) { geoms[[length(geoms)+1]] <- mp; keep <- c(keep, i) }
         }
      }
    }
  }
  if (length(geoms) == 0) return(NULL)
  sf::st_sf(.tags_to_df(lapply(rels[keep], function(r) if (is.null(r$tags)) list() else r$tags), vapply(rels[keep], function(r) r$id, numeric(1))), geometry = sf::st_sfc(geoms, crs = 4326))
}

#' Download OSM Data (Interactive Use Only)
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
#' @param cache_dir Character. Directory used for disk cache.
#' @param timeout Numeric. Overpass query timeout in seconds.
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
  cache_dir   = file.path(tempdir(), "greenR_osm_cache"),
  timeout     = 180,
  include_highways = TRUE, include_green_areas = TRUE, include_trees = TRUE,
  include_water = TRUE, include_buildings = FALSE,
  verbose = TRUE
) {
  if (!interactive() && !isTRUE(getOption("greenR.allow_non_interactive", FALSE)) && !isTRUE(getOption("example.ask", FALSE))) {
    # For CRAN or automated tests, we might want to skip or just warn
    # but for now let's just allow it if the user wants.
  }
  vlog <- function(...) if (isTRUE(verbose)) message(...)

  OVERPASS_SERVERS <- c(
    "https://overpass-api.de/api/interpreter",
    "https://overpass.osm.ch/api/interpreter",
    "https://api.openstreetmap.fr/oapi/interpreter",
    "https://overpass.kumi.systems/api/interpreter"
  )
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
    if (is.numeric(x)) { bq <- x; names(bq) <- c("left", "bottom", "right", "top"); return(bq) }
    bq <- .geocode_nominatim(x, server_url)
    if (is.null(bq)) stop("Geocoding failed.")
    bq
  }

  .fetch_overpass <- function(ql, http_timeout = timeout + 15) {
    last_empty_parsed <- NULL
    for (srv in OVERPASS_SERVERS) {
      vlog("  [overpass] trying: ", srv)
      resp <- tryCatch(httr::POST(srv, body = list(data = ql), encode = "form", httr::timeout(http_timeout), httr::add_headers(`User-Agent` = USER_AGENT)), error = function(e) NULL)
      if (!is.null(resp) && !httr::http_error(resp)) {
        txt <- httr::content(resp, as = "text", encoding = "UTF-8")
        parsed <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
        if (!is.null(parsed) && !is.null(parsed$elements)) {
          if (length(parsed$elements) > 0) {
            vlog(sprintf("  [overpass] success (%d elements)", length(parsed$elements)))
            return(parsed)
          } else {
            vlog("  [overpass] returned 0 elements, keeping it as fallback but trying other servers...")
            last_empty_parsed <- parsed
          }
        }
      }
    }
    if (!is.null(last_empty_parsed)) {
      vlog("  [overpass] returning fallback empty response.")
      return(last_empty_parsed)
    }
    stop("All servers failed.")
  }

  bq <- .resolve_bbox(bbox)
  if (isTRUE(cache)) {
    dir.create(cache_dir, FALSE, TRUE)
    cfile <- file.path(cache_dir, paste0("osm_", paste(bq, collapse="_"), ".rds"))
    if (file.exists(cfile)) { vlog("[get_osm_data] Using cache."); return(readRDS(cfile)) }
  }

  res <- list()
  if (include_highways) {
    ql <- sprintf("[out:json][timeout:%d];(way[\"highway\"](%f,%f,%f,%f););out geom;", timeout, bq[2], bq[1], bq[4], bq[3])
    p <- .fetch_overpass(ql); res$highways <- list(osm_lines = .build_lines_sf(p$elements))
  }
  if (include_green_areas) {
    ql <- sprintf(
      "[out:json][timeout:%d];(way[\"landuse\"~\"forest|vineyard|plant_nursery|orchard|greenfield|recreation_ground|allotments|meadow|village_green|flowerbed|grass|farmland\"](%f,%f,%f,%f);rel[\"landuse\"~\"forest|vineyard|plant_nursery|orchard|greenfield|recreation_ground|allotments|meadow|village_green|flowerbed|grass|farmland\"](%f,%f,%f,%f);way[\"leisure\"~\"garden|dog_park|nature_reserve|park\"](%f,%f,%f,%f);rel[\"leisure\"~\"garden|dog_park|nature_reserve|park\"](%f,%f,%f,%f););out geom;",
      timeout,
      bq[2], bq[1], bq[4], bq[3],
      bq[2], bq[1], bq[4], bq[3],
      bq[2], bq[1], bq[4], bq[3],
      bq[2], bq[1], bq[4], bq[3]
    )
    p <- .fetch_overpass(ql); polys <- .build_polygons_sf(p$elements); mpolys <- .build_multipolygons_sf(p$elements)
    res$green_areas <- list(osm_polygons = .safe_rbind_sf(polys, mpolys))
  }
  if (include_trees) {
    ql <- sprintf("[out:json][timeout:%d];(node[\"natural\"=\"tree\"](%f,%f,%f,%f););out geom;", timeout, bq[2], bq[1], bq[4], bq[3])
    p <- .fetch_overpass(ql); res$trees <- list(osm_points = .build_points_sf(p$elements))
  }
  if (include_water) {
    ql <- sprintf("[out:json][timeout:%d];(way[\"natural\"=\"water\"](%f,%f,%f,%f);way[\"waterway\"](%f,%f,%f,%f);relation[\"natural\"=\"water\"](%f,%f,%f,%f););out geom;", timeout, bq[2], bq[1], bq[4], bq[3], bq[2], bq[1], bq[4], bq[3], bq[2], bq[1], bq[4], bq[3])
    p <- .fetch_overpass(ql); polys <- .build_polygons_sf(p$elements); mpolys <- .build_multipolygons_sf(p$elements)
    res$water <- list(osm_polygons = .safe_rbind_sf(polys, mpolys))
  }
  if (include_buildings) {
    ql <- sprintf("[out:json][timeout:%d];(way[\"building\"](%f,%f,%f,%f);rel[\"building\"](%f,%f,%f,%f););out geom;", timeout, bq[2], bq[1], bq[4], bq[3], bq[2], bq[1], bq[4], bq[3])
    p <- .fetch_overpass(ql); polys <- .build_polygons_sf(p$elements); mpolys <- .build_multipolygons_sf(p$elements)
    res$buildings <- list(osm_polygons = .safe_rbind_sf(polys, mpolys))
  }

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
