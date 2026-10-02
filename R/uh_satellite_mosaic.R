# Satellite observations are assembled from every required STAC page.  A clear
# pixel is never replaced by a modelled or constant value.
.uh_satellite_page <- function(collection, bbox, datetime, page = NULL) {
  if (!is.null(page)) return(rstac::items_next(page))
  rstac::stac(.uh_pc_stac, force_version = "1.0.0") |>
    rstac::stac_search(collections = collection, bbox = bbox,
                       datetime = datetime, limit = 50) |>
    rstac::post_request()
}

.uh_has_next_page <- function(items) {
  any(vapply(items$links %||% list(), function(x) identical(x$rel, "next"), logical(1)))
}

.uh_sign_satellite_page <- function(items) rstac::items_sign_planetary_computer(items)

.uh_satellite_template <- function(boundary, scene, resolution) {
  projected <- sf::st_transform(boundary, terra::crs(scene))
  b <- sf::st_bbox(projected)
  x0 <- floor(b[["xmin"]] / resolution) * resolution
  x1 <- ceiling(b[["xmax"]] / resolution) * resolution
  y0 <- floor(b[["ymin"]] / resolution) * resolution
  y1 <- ceiling(b[["ymax"]] / resolution) * resolution
  target <- terra::rast(xmin = x0, xmax = x1, ymin = y0, ymax = y1,
                        resolution = resolution, crs = terra::crs(scene), vals = 1)
  if (terra::ncell(target) > 2e7)
    stop("Satellite AOI exceeds 20 million output cells; divide the area or supply a local raster.", call. = FALSE)
  terra::mask(target, terra::vect(projected), updatevalue = NA)
}

.uh_satellite_scene <- function(item, boundary, kind) {
  assets <- if (kind == "ndvi") c("B04", "B08", "SCL") else c("lwir11", "qa_pixel")
  if (!all(assets %in% names(item$assets))) stop("Required STAC assets are missing.")
  read_asset <- function(name) {
    href <- item$assets[[name]]$href
    if (!grepl("^https://", href)) stop("STAC asset has no HTTPS URL.")
    src <- terra::rast(paste0("/vsicurl/", href))
    projected <- sf::st_transform(boundary, terra::crs(src))
    # Preserve source cells immediately outside the AOI. Their footprints
    # can overlap target cells at the boundary after reprojection/resampling.
    buffered <- terra::vect(sf::st_buffer(projected, dist = 2 * max(terra::res(src))))
    terra::crop(src, buffered, snap = "out")
  }
  if (kind == "ndvi") {
    red <- read_asset("B04")
    nir <- read_asset("B08")
    scl <- read_asset("SCL")
    red_scaled <- any(abs(terra::scoff(red)[1, ] - c(1, 0)) > 1e-12, na.rm = TRUE)
    nir_scaled <- any(abs(terra::scoff(nir)[1, ] - c(1, 0)) > 1e-12, na.rm = TRUE)
    if (!terra::compareGeom(red, nir, stopOnError = FALSE))
      nir <- terra::project(nir, red, method = "near")
    if (!terra::compareGeom(red, scl, stopOnError = FALSE))
      scl <- terra::project(scl, red, method = "near")
    r <- .uh_s2_reflectance(red, item, "B04", already_scaled = red_scaled)
    n <- .uh_s2_reflectance(nir, item, "B08", already_scaled = nir_scaled)
    out <- terra::ifel((n + r) > 0, (n - r) / (n + r), NA)
    # SCL 4/5/6: vegetation, bare/urban, water.  Clouds,
    # shadows, snow and defective pixels remain missing.
    valid <- scl == 4 | scl == 5 | scl == 6
    out <- terra::mask(out, valid, maskvalues = 0, updatevalue = NA)
    names(out) <- "ndvi"
  } else {
    st <- read_asset("lwir11")
    qa <- read_asset("qa_pixel")
    scaled <- any(abs(terra::scoff(st)[1, ] - c(1, 0)) > 1e-12, na.rm = TRUE)
    if (!terra::compareGeom(st, qa, stopOnError = FALSE))
      qa <- terra::project(qa, st, method = "near")
    out <- .uh_lst_celsius(st, already_scaled = scaled)
    out <- terra::mask(out, .uh_landsat_clear_mask(qa), maskvalues = 0, updatevalue = NA)
    names(out) <- "lst_c"
  }
  out
}

.uh_satellite_coverage <- function(r, template) {
  v <- terra::values(r, mat = FALSE)
  inside <- is.finite(terra::values(template, mat = FALSE))
  if (!any(inside)) return(0)
  mean(is.finite(v[inside]))
}

.uh_satellite_mosaic <- function(boundary, datetime, cache_dir, kind,
                                 use_cache = FALSE, max_pages = 12L,
                                 min_coverage = 0.995) {
  if (!kind %in% c("ndvi", "lst") || length(max_pages) != 1L ||
      !is.finite(max_pages) || max_pages < 1 || max_pages != as.integer(max_pages) ||
      length(min_coverage) != 1L || !is.finite(min_coverage) ||
      min_coverage <= 0 || min_coverage > 1)
    stop("kind, max_pages, or min_coverage is invalid.", call. = FALSE)
  bb <- sf::st_bbox(sf::st_transform(boundary, 4326))
  bbox <- unname(as.numeric(bb))
  geometry_file <- tempfile(fileext = ".rds")
  on.exit(unlink(geometry_file), add = TRUE)
  saveRDS(sf::st_geometry(sf::st_transform(boundary, 4326)), geometry_file)
  geometry_hash <- unname(tools::md5sum(geometry_file))
  key <- gsub("[^A-Za-z0-9_-]", "_", paste(c("mosaic_v2", kind,
    format(round(bbox, 5), nsmall = 5), geometry_hash, datetime), collapse = "_"))
  path <- file.path(cache_dir, kind, paste0(key, ".tif"))
  if (use_cache && file.exists(path) && file.exists(paste0(path, ".rds"))) {
    meta <- readRDS(paste0(path, ".rds"))
    if (identical(meta$algorithm, "first-clear-paged-v2") &&
        identical(meta$datetime_query, datetime) &&
        identical(meta$bbox, bbox) && meta$coverage >= min_coverage)
      return(c(list(raster = terra::rast(path)), meta))
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  collection <- if (kind == "ndvi") "sentinel-2-l2a" else "landsat-c2-l2"
  resolution <- if (kind == "ndvi") 10 else 30
  mosaic <- NULL
  source_index <- NULL
  used <- list()
  seen <- character()
  page <- NULL
  exhausted <- FALSE
  for (page_number in seq_len(max_pages)) {
    items <- .uh_satellite_page(collection, bbox, datetime, page)
    if (!length(items$features)) { exhausted <- TRUE; break }
    signed <- .uh_sign_satellite_page(items)
    clouds <- vapply(signed$features, function(x) {
      z <- suppressWarnings(as.numeric(x$properties[["eo:cloud_cover"]] %||% Inf))
      if (length(z) != 1L || !is.finite(z)) Inf else z
    }, numeric(1))
    for (item in signed$features[order(clouds)]) {
      if (item$id %in% seen) next
      seen <- c(seen, item$id)
      scene <- tryCatch(.uh_satellite_scene(item, boundary, kind),
                        error = function(e) {
                          warning(sprintf("[%s] Scene %s skipped: %s", kind, item$id,
                                          conditionMessage(e)), call. = FALSE)
                          NULL
                        })
      if (is.null(scene)) next
      if (is.null(mosaic)) {
        template <- .uh_satellite_template(boundary, scene, resolution)
        mosaic <- terra::ifel(!is.na(template), NA_real_, NA_real_)
        source_index <- mosaic
      }
      aligned <- terra::project(scene, mosaic, method = "near")
      # Projecting to a full rectangular grid can introduce cells outside AOI.
      aligned <- terra::mask(aligned, !is.na(template), maskvalues = 0, updatevalue = NA)
      new_cells <- is.na(mosaic) & !is.na(aligned)
      added <- terra::global(new_cells, "sum", na.rm = TRUE)[1, 1]
      if (!is.finite(added) || added == 0) next
      used[[length(used) + 1L]] <- list(id = item$id,
        datetime = item$properties[["datetime"]] %||% NA_character_,
        cloud_cover = clouds[match(item$id, vapply(signed$features, `[[`, "", "id"))],
        added_pixels = as.integer(added))
      mosaic <- terra::ifel(new_cells, aligned, mosaic)
      source_index <- terra::ifel(new_cells, length(used), source_index)
      if (.uh_satellite_coverage(mosaic, template) >= min_coverage) break
    }
    if (!is.null(mosaic) && .uh_satellite_coverage(mosaic, template) >= min_coverage) break
    if (!.uh_has_next_page(items)) { exhausted <- TRUE; break }
    page <- items
  }
  coverage <- if (is.null(mosaic)) 0 else .uh_satellite_coverage(mosaic, template)
  if (coverage < min_coverage) stop(sprintf(
    "[%s] Paged STAC mosaic covers %.1f%% of the requested area (< %.1f%%); %s. No scores were produced. Try a longer date window or supply a covering local raster.",
    kind, 100 * coverage, 100 * min_coverage,
    if (exhausted) "catalog pages exhausted" else "page limit reached"), call. = FALSE)
  meta <- list(item_id = vapply(used, `[[`, "", "id"),
    datetime = vapply(used, `[[`, "", "datetime"),
    cloud_cover = vapply(used, `[[`, 0.0, "cloud_cover"),
    added_pixels = vapply(used, `[[`, 0L, "added_pixels"),
    algorithm = "first-clear-paged-v2", datetime_query = datetime,
    bbox = bbox, coverage = coverage, pages_read = page_number,
    source_index_path = sub("\\.tif$", "_source.tif", path))
  terra::writeRaster(mosaic, path, overwrite = TRUE,
                     gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
  terra::writeRaster(source_index, meta$source_index_path,
                     overwrite = TRUE, gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
  saveRDS(meta, paste0(path, ".rds"))
  c(list(raster = mosaic), meta)
}
