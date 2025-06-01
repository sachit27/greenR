#' Robust ALS GEDI CHM Analysis and Visualization
#'
#' End-to-end or single-raster Canopy Height Model (CHM) analysis using
#' Meta & WRI's 1m ALS GEDI v6 global dataset. Downloads, mosaics,
#' crops, analyzes, and visualizes CHM data for any AOI, or analyzes a
#' user-supplied .tif directly. Outputs both publication-quality (tmap)
#' and interactive (mapview) maps, with progress/status reporting.
#'
#' @description
#' - **Data Source:** ALS GEDI v6 global canopy height model, Meta & WRI (2024).
#' - **Interactive map is downsampled for browser performance**; see `max_cells_mapview`.
#'
#' @param location Character. Place name for AOI (e.g. "Basel, Switzerland").
#' @param bbox Numeric. Bounding box (xmin, ymin, xmax, ymax, EPSG:4326).
#' @param aoi_geojson Path to GeoJSON file defining AOI (overrides location/bbox).
#' @param chm_tif Path to a local CHM raster (.tif). If supplied, skips tile download/mosaic.
#' @param output_dir Output directory for all files.
#' @param apply_mask Mask raster to AOI (default TRUE).
#' @param crop_result Crop raster to AOI (default TRUE).
#' @param create_plots Export static tmap and histogram (default TRUE).
#' @param height_threshold Height (m) for tree coverage (default 2).
#' @param max_tiles Max CHM tiles to use (default 100).
#' @param mapview_html Output HTML file for interactive map.
#' @param tmap_png Output PNG for publication-quality map.
#' @param max_cells_mapview Maximum number of raster cells in the interactive map (default 1e5).
#' @param n_cores Parallel cores for download (default: parallel::detectCores() - 1).
#' @param chunk_size Tiles per parallel chunk (default 5).
#' @param cache_tiles Cache tiles/AOI (default TRUE).
#' @param compression Compression for output raster ("LZW", etc.).
#' @param user_agent_string HTTP user agent string (for Nominatim, etc).
#' @param request_timeout HTTP timeout in seconds (default 300).
#'
#' @return List with: raster, stats, static_map, mapview_file, plot_hist_file, tmap_file
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: AOI from bounding box (Zurich, Switzerland)
#' res_bbox <- chm_analysis(
#'   bbox = c(8.51, 47.36, 8.56, 47.40),
#'   output_dir = tempdir(), max_tiles = 2, create_plots = TRUE
#' )
#' print(res_bbox$stats)
#'
#' # Example 2: AOI from location string (Parc La Grange, Geneva)
#' res_loc <- chm_analysis(
#'   location = "Parc La Grange, Geneva, Switzerland",
#'   output_dir = tempdir(), max_tiles = 2
#' )
#' print(res_loc$stats)
#'
#' # Example 3: Analyze a user-supplied CHM raster
#' # Assume you have a file "my_canopy.tif" (projected or WGS84)
#' res_tif <- chm_analysis(
#'   chm_tif = "my_canopy.tif",
#'   output_dir = tempdir(),
#'   create_plots = TRUE,
#'   height_threshold = 3
#' )
#' print(res_tif$stats)
#' }
chm_analysis <- function(
    location = NULL,
    bbox = NULL,
    aoi_geojson = NULL,
    chm_tif = NULL,
    output_dir = "chm_output",
    apply_mask = TRUE,
    crop_result = TRUE,
    create_plots = TRUE,
    height_threshold = 2,
    max_tiles = 100,
    mapview_html = "chm_mapview.html",
    tmap_png = "chm_tmap.png",
    max_cells_mapview = 2e5,
    n_cores = parallel::detectCores() - 1,
    chunk_size = 5,
    cache_tiles = TRUE,
    compression = "LZW",
    user_agent_string = "R/chm_analysis_script (your_email_or_project_url)",
    request_timeout = 300
) {
  pkgs <- c("sf", "terra", "httr", "jsonlite", "glue", "curl", "ggplot2", "viridis",
            "tmap", "mapview", "htmlwidgets", "parallel")
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop(sprintf("Package '%s' required!", pkg), call. = FALSE)
  }
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  message("Starting CHM Analysis: ", Sys.time())

  # AOI (required even if chm_tif provided, for masking/stats/maps)
  aoi <- NULL; aoi_cache_file <- file.path(output_dir, "aoi_cache.rds")
  if (cache_tiles && file.exists(aoi_cache_file)) {
    message("Loaded AOI from cache.")
    aoi <- readRDS(aoi_cache_file)
  } else if (!is.null(aoi_geojson)) {
    message("Using AOI from GeoJSON.")
    aoi <- sf::st_read(aoi_geojson, quiet = TRUE) |> sf::st_transform(3857)
    if (cache_tiles) saveRDS(aoi, aoi_cache_file)
  } else if (!is.null(bbox)) {
    message("Using AOI from bbox.")
    coords <- matrix(c(bbox[1], bbox[2], bbox[3], bbox[2], bbox[3], bbox[4],
                       bbox[1], bbox[4], bbox[1], bbox[2]), ncol = 2, byrow = TRUE)
    aoi <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326)) |> sf::st_transform(3857)
    if (cache_tiles) saveRDS(aoi, aoi_cache_file)
  } else if (!is.null(location)) {
    message("Geocoding AOI with Nominatim: ", location)
    nom_url <- "https://nominatim.openstreetmap.org/search"
    resp <- httr::GET(
      nom_url, query = list(q = location, format = "geojson", polygon_geojson = 1, limit = 1),
      httr::user_agent(user_agent_string), httr::timeout(request_timeout)
    )
    stopifnot(httr::status_code(resp) == 200)
    tf <- tempfile(fileext = ".geojson")
    writeLines(httr::content(resp, as = "text", encoding = "UTF-8"), tf)
    aoi <- sf::st_read(tf, quiet = TRUE)[1, ] |> sf::st_transform(3857)
    unlink(tf)
    if (cache_tiles) saveRDS(aoi, aoi_cache_file)
  } else if (is.null(chm_tif)) {
    stop("Must provide at least one of: location, bbox, aoi_geojson, or chm_tif.")
  }
  aoi_wgs <- if (!is.null(aoi)) sf::st_transform(aoi, 4326) else NULL

  # If chm_tif is supplied, read and use as-is (skip download/mosaic)
  if (!is.null(chm_tif)) {
    message("Using user-supplied CHM raster: ", chm_tif)
    mosaic_safe <- terra::rast(chm_tif)
    # Project raster to AOI CRS if needed
    if (!is.null(aoi) && terra::crs(mosaic_safe) != terra::crs(aoi)) {
      mosaic_safe <- terra::project(mosaic_safe, terra::crs(aoi))
    }
  } else {
    # Download tiles as before
    idx_url  <- "https://dataforgood-fb-data.s3.amazonaws.com/forests/v1/alsgedi_global_v6_float/tiles.geojson"
    idx_file <- file.path(output_dir, "tiles.geojson")
    if (!file.exists(idx_file)) curl::curl_download(idx_url, idx_file)
    tiles <- sf::st_read(idx_file, quiet = TRUE) |> sf::st_transform(sf::st_crs(aoi))
    tgt <- tiles[sf::st_intersects(tiles, aoi, sparse = FALSE), ]
    if (!nrow(tgt)) stop("AOI not covered by CHM tiles.")
    if (nrow(tgt) > max_tiles) {
      warning(sprintf("Limiting to first %d tiles", max_tiles))
      tgt <- tgt[seq_len(max_tiles), ]
    }
    base_tile_url <- "https://dataforgood-fb-data.s3.amazonaws.com/forests/v1/alsgedi_global_v6_float/chm"
    tile_cache_dir <- file.path(output_dir, "tile_cache")
    if (!dir.exists(tile_cache_dir)) dir.create(tile_cache_dir)
    validate_raster <- function(p) {
      if (!file.exists(p) || file.info(p)$size < 1000) return(FALSE)
      tryCatch({
        r <- terra::rast(p)
        ext_valid <- !is.na(terra::ext(r)) && terra::ncell(r) > 0
        if (!ext_valid) return(FALSE)
        vals <- terra::spatSample(r, size = min(100, terra::ncell(r)), method = "random", na.rm = TRUE)
        any(is.finite(vals[,1]))
      }, error = function(e) FALSE)
    }
    download_tile_batch <- function(tile_ids, target_dir) {
      lapply(tile_ids, function(tile_id) {
        chm_file <- file.path(target_dir, paste0(tile_id, ".tif"))
        url <- sprintf("%s/%s.tif", base_tile_url, tile_id)
        if (!file.exists(chm_file) || !validate_raster(chm_file)) {
          try(unlink(chm_file), silent = TRUE)
          curl::curl_download(url, chm_file, quiet = TRUE)
        }
        list(file = chm_file, tile_id = tile_id, valid = validate_raster(chm_file))
      })
    }
    tile_ids <- tgt$tile
    actual_n_cores <- min(n_cores, length(tile_ids))
    tile_chunks <- split(tile_ids, ceiling(seq_along(tile_ids) / chunk_size))
    message("Downloading CHM tiles...")
    all_results <- if (actual_n_cores > 1) {
      cl <- parallel::makeCluster(actual_n_cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      parallel::clusterExport(cl, c("download_tile_batch", "validate_raster", "base_tile_url", "tile_cache_dir"), envir = environment())
      res <- parallel::parLapply(cl, tile_chunks, function(chunk) download_tile_batch(chunk, tile_cache_dir))
      do.call(c, res)
    } else {
      do.call(c, lapply(tile_chunks, function(chunk) download_tile_batch(chunk, tile_cache_dir)))
    }
    valid_files <- vapply(all_results, function(x) if (isTRUE(x$valid)) x$file else NA_character_, character(1))
    valid_files <- valid_files[!is.na(valid_files)]
    if (!length(valid_files)) stop("No valid rasters downloaded.")
    message(sprintf("Successfully obtained %d CHM tiles.", length(valid_files)))
    # Mosaic tiles as before
    if (length(valid_files) == 1) {
      mosaic_safe <- terra::rast(valid_files)
    } else {
      max_direct_mosaic <- 8
      if (length(valid_files) <= max_direct_mosaic) {
        mosaic_safe <- terra::mosaic(terra::sprc(lapply(valid_files, terra::rast)), fun = "max", na.rm = TRUE)
      } else {
        chunk_size_mosaic <- min(8, ceiling(length(valid_files) / (actual_n_cores * 1.5)))
        file_chunks <- split(valid_files, ceiling(seq_along(valid_files) / chunk_size_mosaic))
        temp_mosaics <- lapply(file_chunks, function(chunk) terra::mosaic(terra::sprc(lapply(chunk, terra::rast)), fun = "max", na.rm = TRUE))
        mosaic_safe <- terra::mosaic(terra::sprc(temp_mosaics), fun = "max", na.rm = TRUE)
      }
    }
  }
  aoi_vect <- if (!is.null(aoi)) terra::vect(aoi) else NULL
  if (!is.null(aoi_vect)) {
    if (crop_result) mosaic_safe <- terra::crop(mosaic_safe, aoi_vect, snap = "out")
    if (apply_mask) mosaic_safe <- terra::mask(mosaic_safe, aoi_vect)
  }
  mosaic_safe[mosaic_safe > 200 | mosaic_safe < 0] <- NA
  out_rast_path <- file.path(output_dir, "chm_final_processed.tif")
  gdal_options <- c("TILED=YES", "BIGTIFF=IF_SAFER")
  if (compression != "NONE") gdal_options <- c(paste0("COMPRESS=", compression), gdal_options)
  terra::writeRaster(mosaic_safe, out_rast_path, overwrite = TRUE, gdal = gdal_options)
  message("Raster saved: ", out_rast_path)

  # --- Statistics ---
  message("Calculating summary statistics...")
  stats <- terra::global(mosaic_safe, fun = c("min", "max", "mean", "sd"), na.rm = TRUE)
  total_px <- terra::global(mosaic_safe, "notNA", na.rm = TRUE)$notNA
  tree_mask <- mosaic_safe >= height_threshold
  tree_px <- terra::global(tree_mask, "sum", na.rm = TRUE)$sum
  percent_tree_cover <- (tree_px / total_px) * 100
  aoi_equal_area <- if (!is.null(aoi)) sf::st_transform(aoi, "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs") else NULL
  aoi_area_km2 <- if (!is.null(aoi_equal_area)) as.numeric(sf::st_area(aoi_equal_area)) / 1e6 else NA_real_

  stats_summary <- list(
    min_height_m = stats$min, max_height_m = stats$max, mean_height_m = stats$mean, sd_height_m = stats$sd,
    total_valid_pixels = total_px, tree_pixels_gte_threshold = tree_px, percent_tree_cover = percent_tree_cover,
    aoi_geometric_sqkm = aoi_area_km2,
    height_threshold_for_tree_stats_m = height_threshold,
    final_raster_crs_proj4 = terra::crs(mosaic_safe, proj = TRUE)
  )

  # --- Publication quality map (tmap, v3/v4 robust) ---
  tmap_file <- file.path(output_dir, tmap_png)
  static_map <- NULL
  if (create_plots) {
    tmap::tmap_mode("plot")
    proj_r <- terra::project(mosaic_safe, "EPSG:4326")
    tmap_v <- as.integer(utils::packageVersion("tmap")$major)
    if (tmap_v >= 4) {
      static_map <- tmap::tm_shape(proj_r) +
        tmap::tm_raster(
          col.scale = tmap::tm_scale_continuous(values = "viridis"),
          col_alpha = 0.8,
          col.legend = tmap::tm_legend(title = "Canopy Height (m)")
        ) +
        tmap::tm_shape(aoi_wgs) + tmap::tm_borders(col = "red", lwd = 2) +
        tmap::tm_basemap("OpenStreetMap") +
        tmap::tm_layout(legend.outside = TRUE)
    } else {
      static_map <- tmap::tm_shape(proj_r) +
        tmap::tm_raster(style = "cont", palette = "viridis",
                        alpha = 0.8, title = "Canopy Height (m)") +
        tmap::tm_shape(aoi_wgs) + tmap::tm_borders(col = "red", lwd = 2) +
        tmap::tm_basemap("OpenStreetMap") +
        tmap::tm_layout(legend.outside = TRUE, frame = FALSE, legend.frame = FALSE)
    }
    tmap::tmap_save(static_map, filename = tmap_file, width = 2800, height = 2000, dpi = 300)
    message(sprintf("Saved static tmap to: %s", tmap_file))
  }

  # --- Histogram as fraction of AOI ---
  plot_hist_file <- NULL
  chm_df <- terra::as.data.frame(mosaic_safe, xy = FALSE, na.rm = TRUE)
  colnames(chm_df) <- "height"
  if (nrow(chm_df) > 0) {
    hist_plot <- ggplot2::ggplot(subset(chm_df, height > 0), ggplot2::aes(x = height)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ..count../sum(..count..)), binwidth = 1,
                              fill = "forestgreen", color = "black") +
      ggplot2::labs(
        title = "Fraction of Area vs. Canopy Height",
        x = "Height (m)", y = "Fraction of AOI"
      ) +
      ggplot2::theme_minimal()
    plot_hist_file <- file.path(output_dir, "chm_hist_fraction.png")
    ggplot2::ggsave(plot_hist_file, hist_plot, width = 8, height = 6, dpi = 300)
    message(sprintf("Saved histogram to: %s", plot_hist_file))
  }

  # --- Interactive map (mapview) ---
  mapview_file <- file.path(output_dir, mapview_html)
  # Downsample raster for mapview
  agg_factor <- ceiling(sqrt(terra::ncell(mosaic_safe) / max_cells_mapview))
  web_r <- if (terra::ncell(mosaic_safe) > max_cells_mapview) {
    terra::aggregate(mosaic_safe, fact = agg_factor, fun = "mean", na.rm = TRUE)
  } else {
    mosaic_safe
  }
  web_r_proj <- terra::project(web_r, "EPSG:4326")
  raster_layer <- mapview::mapview(web_r_proj, col.regions = viridis::viridis(99),
                                   legend = TRUE, layer.name = "Canopy Height (m)", maxpixels = max_cells_mapview)
  aoi_layer <- if (!is.null(aoi_wgs)) mapview::mapview(aoi_wgs, color = "red", lwd = 2, alpha.regions = 0, layer.name = "AOI Outline") else NULL
  if (!is.null(aoi_layer)) {
    mv <- raster_layer + aoi_layer
    leaf <- mv@map
    leaf <- leaflet::hideGroup(leaf, "AOI Outline")
  } else {
    mv <- raster_layer
    leaf <- mv@map
  }
  htmlwidgets::saveWidget(leaf, file = mapview_file, selfcontained = TRUE)
  message(sprintf("Saved interactive mapview HTML to: %s", mapview_file))

  message("CHM Analysis complete.")

  return(list(
    raster = mosaic_safe,
    stats = stats_summary,
    output_file = out_rast_path,
    tiles_used = if (exists("valid_files")) basename(valid_files) else NA,
    static_map = static_map,
    mapview_file = mapview_file,
    plot_hist_file = plot_hist_file,
    tmap_file = tmap_file
  ))
}
