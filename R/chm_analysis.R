# ==============================================================================
# chm_analysis.R
# Meta/WRI DINOv3 global CHM analysis and visualization
# ==============================================================================

utils::globalVariables(c(
  "height", "height_capped", "mean_chm", "max_chm", "tree_cover_pct",
  "tall_canopy_pct", "gap_pct", "canopy_volume_proxy", "geometry", "grid_id",
  "threshold_m", "cover_pct", "area_km2", "canopy_type"
))

`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0) a else b
}

# ------------------------------------------------------------------------------
# Package and utility helpers
# ------------------------------------------------------------------------------

.chm_check_packages <- function() {
  required <- c("sf", "terra", "httr", "curl", "ggplot2", "viridisLite")
  miss <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss) > 0) {
    stop(
      "Please install missing packages:\n  install.packages(c(\"",
      paste(miss, collapse = "\", \""),
      "\"))",
      call. = FALSE
    )
  }

  list(
    exactextractr = requireNamespace("exactextractr", quietly = TRUE),
    leaflet = requireNamespace("leaflet", quietly = TRUE),
    htmlwidgets = requireNamespace("htmlwidgets", quietly = TRUE)
  )
}

.chm_set_gdal_env <- function() {
  old <- Sys.getenv(c(
    "GDAL_DISABLE_READDIR_ON_OPEN",
    "CPL_VSIL_CURL_ALLOWED_EXTENSIONS",
    "VSI_CACHE",
    "VSI_CACHE_SIZE",
    "GDAL_HTTP_MULTIRANGE",
    "GDAL_HTTP_VERSION"
  ), unset = NA)

  Sys.setenv(
    GDAL_DISABLE_READDIR_ON_OPEN = "YES",
    CPL_VSIL_CURL_ALLOWED_EXTENSIONS = ".tif,.TIF,.tiff,.TIFF,.vrt,.VRT",
    VSI_CACHE = "TRUE",
    VSI_CACHE_SIZE = "500000000",
    GDAL_HTTP_MULTIRANGE = "YES",
    GDAL_HTTP_VERSION = "2"
  )

  invisible(old)
}

.chm_msg <- function(..., verbose = TRUE) {
  if (isTRUE(verbose)) message(...)
}

.chm_safe_make_valid <- function(x) {
  if (is.null(x) || !inherits(x, "sf") || nrow(x) == 0) return(x)
  x <- tryCatch(sf::st_make_valid(x), error = function(e) x)
  x <- tryCatch(suppressWarnings(sf::st_collection_extract(x, "POLYGON")), error = function(e) x)
  x
}

.chm_safe_rbind_sf <- function(x, y) {
  if (is.null(x) || !inherits(x, "sf") || nrow(x) == 0) return(y)
  if (is.null(y) || !inherits(y, "sf") || nrow(y) == 0) return(x)

  x <- sf::st_transform(x, 4326)
  y <- sf::st_transform(y, 4326)

  x_geom <- sf::st_geometry(x)
  y_geom <- sf::st_geometry(y)
  x_df <- sf::st_drop_geometry(x)
  y_df <- sf::st_drop_geometry(y)

  cols <- union(names(x_df), names(y_df))
  for (nm in setdiff(cols, names(x_df))) x_df[[nm]] <- NA
  for (nm in setdiff(cols, names(y_df))) y_df[[nm]] <- NA

  out <- rbind(x_df[, cols, drop = FALSE], y_df[, cols, drop = FALSE])
  out$geometry <- c(x_geom, y_geom)
  sf::st_as_sf(out, sf_column_name = "geometry", crs = 4326)
}

.chm_resolve_aoi <- function(
    location = NULL,
    bbox = NULL,
    aoi_geojson = NULL,
    aoi = NULL,
    user_agent_string = "R/chm_analysis (research-use)",
    request_timeout = 60,
    verbose = TRUE
) {
  if (inherits(aoi, "sf")) {
    .chm_msg("  Using user-supplied sf AOI.", verbose = verbose)
    out <- sf::st_transform(aoi, 4326)
    out <- .chm_safe_make_valid(out)
    out <- sf::st_as_sf(sf::st_union(out))
    sf::st_crs(out) <- 4326
    return(list(aoi_wgs = out, label = "user-supplied sf AOI"))
  }

  if (!is.null(aoi_geojson) && file.exists(aoi_geojson)) {
    .chm_msg("  Using AOI file: ", aoi_geojson, verbose = verbose)
    out <- sf::st_read(aoi_geojson, quiet = TRUE)
    out <- sf::st_transform(out, 4326)
    out <- .chm_safe_make_valid(out)
    out <- sf::st_as_sf(sf::st_union(out))
    sf::st_crs(out) <- 4326
    return(list(aoi_wgs = out, label = basename(aoi_geojson)))
  }

  if (!is.null(bbox)) {
    if (!is.numeric(bbox) || length(bbox) != 4) {
      stop("bbox must be numeric c(xmin, ymin, xmax, ymax) in EPSG:4326.", call. = FALSE)
    }
    .chm_msg("  Using bbox AOI.", verbose = verbose)
    bb <- sf::st_bbox(
      c(xmin = bbox[1], ymin = bbox[2], xmax = bbox[3], ymax = bbox[4]),
      crs = sf::st_crs(4326)
    )
    out <- sf::st_as_sf(sf::st_as_sfc(bb))
    return(list(aoi_wgs = out, label = sprintf("bbox_%.4f_%.4f_%.4f_%.4f", bbox[1], bbox[2], bbox[3], bbox[4])))
  }

  if (!is.null(location)) {
    if (!is.character(location) || length(location) != 1) {
      stop("location must be a single character string.", call. = FALSE)
    }

    .chm_msg("  Geocoding location with Nominatim: ", location, verbose = verbose)
    resp <- httr::GET(
      "https://nominatim.openstreetmap.org/search",
      query = list(q = location, format = "geojson", polygon_geojson = 1, limit = 1),
      httr::user_agent(user_agent_string),
      httr::timeout(request_timeout)
    )

    if (httr::status_code(resp) >= 300) {
      stop("Nominatim geocoding failed for: ", location, call. = FALSE)
    }

    tf <- tempfile(fileext = ".geojson")
    writeLines(httr::content(resp, as = "text", encoding = "UTF-8"), tf)
    out <- sf::st_read(tf, quiet = TRUE)
    unlink(tf)

    if (nrow(out) == 0) stop("No AOI returned for location: ", location, call. = FALSE)
    out <- out[1, ]
    out <- sf::st_transform(out, 4326)
    out <- .chm_safe_make_valid(out)
    out <- sf::st_as_sf(sf::st_union(out))
    sf::st_crs(out) <- 4326
    return(list(aoi_wgs = out, label = location))
  }

  NULL
}

.chm_get_osm_context <- function(aoi_wgs, verbose = TRUE) {
  if (is.null(aoi_wgs)) return(NULL)

  bb <- sf::st_bbox(aoi_wgs)
  bbox_vec <- c(
    left = unname(bb["xmin"]),
    bottom = unname(bb["ymin"]),
    right = unname(bb["xmax"]),
    top = unname(bb["ymax"])
  )

  if (exists("get_osm_data", mode = "function")) {
    return(tryCatch(
      get_osm_data(
        bbox_vec,
        include_highways = TRUE,
        include_green_areas = TRUE,
        include_trees = TRUE,
        include_water = TRUE,
        cache = TRUE,
        verbose = FALSE
      ),
      error = function(e) {
        .chm_msg("  [i] get_osm_data() context skipped: ", e$message, verbose = verbose)
        NULL
      }
    ))
  }

  .chm_msg("  [i] get_osm_data() not found; context layers skipped.", verbose = verbose)
  NULL
}

.chm_prepare_context_layers <- function(osm_context) {
  prep <- function(x) {
    if (is.null(x) || !inherits(x, "sf") || nrow(x) == 0) return(NULL)
    x <- tryCatch(sf::st_make_valid(x), error = function(e) x)
    tryCatch(sf::st_transform(x, 4326), error = function(e) NULL)
  }

  if (is.null(osm_context)) {
    return(list(roads = NULL, green = NULL, water = NULL, trees = NULL))
  }

  roads <- prep(osm_context$highways$osm_lines %||% NULL)
  green <- prep(osm_context$green_areas$osm_polygons %||% NULL)
  trees <- prep(osm_context$trees$osm_points %||% NULL)

  water_poly <- osm_context$water$osm_polygons %||% NULL
  water_mpoly <- osm_context$water$osm_multipolygons %||% NULL
  water <- NULL
  if (!is.null(water_poly) && !is.null(water_mpoly)) {
    water_mpoly <- suppressWarnings(sf::st_cast(water_mpoly, "POLYGON", warn = FALSE))
    water <- .chm_safe_rbind_sf(water_poly, water_mpoly)
  } else {
    water <- water_poly %||% water_mpoly
  }
  water <- prep(water)

  list(roads = roads, green = green, water = water, trees = trees)
}

.chm_validate_raster <- function(path_or_vsi) {
  tryCatch({
    r <- terra::rast(path_or_vsi)
    terra::ncell(r) > 0
  }, error = function(e) FALSE)
}

.chm_downsample_raster <- function(r, max_cells = 2e5, fun = "mean") {
  if (terra::ncell(r) <= max_cells) return(r)
  fact <- ceiling(sqrt(terra::ncell(r) / max_cells))
  terra::aggregate(r, fact = fact, fun = fun, na.rm = TRUE)
}

.chm_extract_mean <- function(r, p, exactextract_available = FALSE) {
  if (isTRUE(exactextract_available)) {
    exactextractr::exact_extract(r, p, fun = "mean", progress = FALSE)
  } else {
    terra::extract(r, terra::vect(p), fun = mean, na.rm = TRUE)[, 2]
  }
}

.chm_extract_max <- function(r, p, exactextract_available = FALSE) {
  if (isTRUE(exactextract_available)) {
    exactextractr::exact_extract(r, p, fun = "max", progress = FALSE)
  } else {
    terra::extract(r, terra::vect(p), fun = max, na.rm = TRUE)[, 2]
  }
}

.chm_make_hex_summary <- function(
    chm,
    aoi_wgs,
    hex_cellsize_m = 100,
    height_threshold = 2,
    tall_canopy_threshold = 10,
    gap_threshold = 1,
    exactextract_available = FALSE
) {
  if (is.null(aoi_wgs)) return(NULL)

  aoi_r <- sf::st_transform(aoi_wgs, terra::crs(chm))
  grid <- sf::st_make_grid(aoi_r, cellsize = hex_cellsize_m, square = FALSE)
  grid <- sf::st_as_sf(data.frame(grid_id = seq_along(grid), geometry = grid))
  sf::st_crs(grid) <- sf::st_crs(aoi_r)
  suppressWarnings(grid <- sf::st_intersection(grid, aoi_r))
  grid <- .chm_safe_make_valid(grid)
  grid$grid_id <- seq_len(nrow(grid))
  grid$area_km2 <- as.numeric(sf::st_area(grid)) / 1e6

  grid$mean_chm <- .chm_extract_mean(chm, grid, exactextract_available)
  grid$max_chm <- .chm_extract_max(chm, grid, exactextract_available)

  tree_mask <- chm >= height_threshold
  tall_mask <- chm >= tall_canopy_threshold
  gap_mask <- chm <= gap_threshold
  chm_excess <- chm - height_threshold
  volume_proxy <- terra::ifel(chm_excess < 0, 0, chm_excess)

  grid$tree_cover_pct <- .chm_extract_mean(tree_mask, grid, exactextract_available) * 100
  grid$tall_canopy_pct <- .chm_extract_mean(tall_mask, grid, exactextract_available) * 100
  grid$gap_pct <- .chm_extract_mean(gap_mask, grid, exactextract_available) * 100
  grid$canopy_volume_proxy <- .chm_extract_mean(volume_proxy, grid, exactextract_available)

  sf::st_transform(grid, 4326)
}

.chm_canopy_classes <- function(chm, gap_threshold = 1, height_threshold = 2, tall_canopy_threshold = 10) {
  terra::app(chm, function(x) {
    out <- rep(NA_real_, length(x))
    out[x >= 0 & x < gap_threshold] <- 0
    out[x >= gap_threshold & x < height_threshold] <- 1
    out[x >= height_threshold & x < tall_canopy_threshold] <- 2
    out[x >= tall_canopy_threshold & x < 20] <- 3
    out[x >= 20] <- 4
    out
  })
}

# ------------------------------------------------------------------------------
# Plot helpers
# ------------------------------------------------------------------------------

.chm_map_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 10, color = "#9A9A9A"),
      axis.ticks = ggplot2::element_line(color = "#B8B8B8", linewidth = 0.25),
      panel.border = ggplot2::element_rect(color = "#D8D8D8", fill = NA, linewidth = 0.4),
      legend.position = "right",
      plot.title = ggplot2::element_text(face = "bold", size = 16, margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(size = 10, color = "#555555", margin = ggplot2::margin(b = 8)),
      plot.caption = ggplot2::element_text(size = 8, color = "#666666", hjust = 0)
    )
}

.chm_plot_cartographic_map <- function(chm, aoi_wgs, context, location_label, max_cells_plot = 2e5) {
  if (is.null(aoi_wgs)) return(NULL)
  r <- .chm_downsample_raster(chm, max_cells = max_cells_plot)
  r_wgs <- terra::project(r, "EPSG:4326")
  df <- terra::as.data.frame(r_wgs, xy = TRUE, na.rm = TRUE)
  names(df) <- c("x", "y", "height")
  if (nrow(df) == 0) return(NULL)

  height_cap <- as.numeric(stats::quantile(df$height, 0.98, na.rm = TRUE))
  df$height_capped <- pmin(df$height, height_cap)

  p <- ggplot2::ggplot() + .chm_map_theme()

  if (!is.null(context$green)) {
    p <- p + ggplot2::geom_sf(data = context$green, fill = "#E8F1E4", color = NA, alpha = 0.70)
  }
  if (!is.null(context$water)) {
    p <- p + ggplot2::geom_sf(data = context$water, fill = "#DCEAF2", color = NA, alpha = 0.90)
  }
  if (!is.null(context$roads)) {
    p <- p + ggplot2::geom_sf(data = context$roads, color = "#CFCFCF", linewidth = 0.13, alpha = 0.65)
  }

  p +
    ggplot2::geom_raster(data = df, ggplot2::aes(x = x, y = y, fill = height_capped), alpha = 0.82) +
    ggplot2::scale_fill_gradientn(
      colours = c("#F7FCF5", "#C7E9C0", "#74C476", "#238B45", "#00441B"),
      limits = c(0, height_cap),
      name = "Canopy\nheight (m)",
      na.value = "transparent"
    ) +
    ggplot2::geom_sf(data = aoi_wgs, fill = NA, color = "#222222", linewidth = 0.35) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(
      title = "Canopy Height Structure",
      subtitle = location_label %||% "Selected study area",
      caption = "Data: Meta/WRI DINOv3 global CHM. Contextual features from OpenStreetMap when available. Heights capped at 98th percentile for readability."
    )
}

.chm_plot_raster_map <- function(chm, location_label, max_cells_plot = 2e5) {
  r <- .chm_downsample_raster(chm, max_cells = max_cells_plot)
  r_wgs <- terra::project(r, "EPSG:4326")
  df <- terra::as.data.frame(r_wgs, xy = TRUE, na.rm = TRUE)
  names(df) <- c("x", "y", "height")
  if (nrow(df) == 0) return(NULL)

  height_cap <- as.numeric(stats::quantile(df$height, 0.98, na.rm = TRUE))
  df$height_capped <- pmin(df$height, height_cap)

  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = height_capped)) +
    ggplot2::geom_raster() +
    ggplot2::coord_equal() +
    ggplot2::scale_fill_gradientn(
      colours = c("#F7FCF5", "#C7E9C0", "#74C476", "#238B45", "#00441B"),
      limits = c(0, height_cap),
      name = "Height (m)",
      na.value = "transparent"
    ) +
    ggplot2::labs(
      title = "CHM Analytical Raster",
      subtitle = location_label %||% "Downsampled for plotting only",
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )
}

.chm_plot_histogram <- function(values, height_threshold, tall_canopy_threshold) {
  d <- data.frame(height = values[is.finite(values)])
  if (nrow(d) == 0) return(NULL)

  ggplot2::ggplot(d, ggplot2::aes(x = height)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(count / sum(count))),
      binwidth = 1,
      fill = "#74C476",
      color = "white"
    ) +
    ggplot2::geom_vline(xintercept = height_threshold, linetype = "dashed", linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = tall_canopy_threshold, linetype = "dotted", linewidth = 0.8) +
    ggplot2::labs(
      title = "Canopy Height Distribution",
      subtitle = paste0("Dashed = tree threshold (", height_threshold, " m); dotted = tall canopy (", tall_canopy_threshold, " m)"),
      x = "Canopy height (m)",
      y = "Fraction of valid CHM pixels"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

.chm_plot_threshold_curve <- function(threshold_stats) {
  if (is.null(threshold_stats) || nrow(threshold_stats) == 0) return(NULL)

  ggplot2::ggplot(threshold_stats, ggplot2::aes(x = threshold_m, y = cover_pct)) +
    ggplot2::geom_area(alpha = 0.18, fill = "#74C476") +
    ggplot2::geom_line(linewidth = 1.1, color = "#238B45") +
    ggplot2::geom_point(size = 2.2, color = "#00441B") +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(x), "%")) +
    ggplot2::labs(
      title = "Canopy Cover Sensitivity to Height Threshold",
      subtitle = "Different thresholds answer different ecological and planning questions.",
      x = "Height threshold (m)",
      y = "Area above threshold"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

.chm_plot_hex_map <- function(hex_summary, value_col, title, subtitle, legend_title, context = NULL, aoi_wgs = NULL) {
  if (is.null(hex_summary) || !value_col %in% names(hex_summary)) return(NULL)

  hex_3857 <- sf::st_transform(hex_summary, 3857)
  aoi_3857 <- if (!is.null(aoi_wgs)) sf::st_transform(aoi_wgs, 3857) else NULL

  p <- ggplot2::ggplot() + .chm_map_theme()
  if (!is.null(aoi_3857) && requireNamespace("ggspatial", quietly = TRUE)) {
    p <- tryCatch(
      p + ggspatial::annotation_map_tile(
        type = "cartolight",
        zoom = NULL,
        data = aoi_3857,
        progress = "none",
        quiet = TRUE
      ),
      error = function(e) p
    )
  }

  outline_layer <- if (!is.null(aoi_3857)) {
    ggplot2::geom_sf(data = aoi_3857, fill = NA, color = "#26354A", linewidth = 0.85, lineend = "round")
  } else {
    NULL
  }

  p +
    ggplot2::geom_sf(data = hex_3857, ggplot2::aes(fill = .data[[value_col]]), color = NA, alpha = 0.92) +
    ggplot2::scale_fill_gradientn(
      colours = c("#F7FCF5", "#C7E9C0", "#74C476", "#238B45", "#00441B"),
      name = legend_title,
      na.value = "transparent"
    ) +
    outline_layer +
    ggspatial::annotation_scale(
      location = "bl",
      plot_unit = "m",
      width_hint = 0.18,
      line_width = 0.5,
      text_cex = 0.8,
      pad_x = grid::unit(0.6, "cm"),
      pad_y = grid::unit(0.6, "cm")
    ) +
    ggplot2::coord_sf(crs = 3857, expand = FALSE, datum = sf::st_crs(3857)) +
    ggplot2::labs(title = title, subtitle = subtitle)
}

.chm_plot_typology <- function(hex_summary) {
  if (is.null(hex_summary) || !all(c("tree_cover_pct", "mean_chm") %in% names(hex_summary))) return(NULL)

  d <- sf::st_drop_geometry(hex_summary)
  if (nrow(d) == 0) return(NULL)

  med_cover <- stats::median(d$tree_cover_pct, na.rm = TRUE)
  med_height <- stats::median(d$mean_chm, na.rm = TRUE)

  d$canopy_type <- dplyr::case_when(
    d$tree_cover_pct >= med_cover & d$mean_chm >= med_height ~ "Dense + tall canopy",
    d$tree_cover_pct >= med_cover & d$mean_chm < med_height ~ "Dense + low canopy",
    d$tree_cover_pct < med_cover & d$mean_chm >= med_height ~ "Sparse + tall patches",
    TRUE ~ "Sparse + low canopy"
  )

  ggplot2::ggplot(d, ggplot2::aes(x = tree_cover_pct, y = mean_chm, color = canopy_type)) +
    ggplot2::geom_vline(xintercept = med_cover, linetype = "dashed", color = "#777777") +
    ggplot2::geom_hline(yintercept = med_height, linetype = "dashed", color = "#777777") +
    ggplot2::geom_point(alpha = 0.70, size = 2) +
    ggplot2::labs(
      title = "Canopy Structure Typology",
      subtitle = "Each point is one hexagon; dashed lines show median values.",
      x = "Tree cover (%)",
      y = "Mean canopy height (m)",
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), legend.position = "bottom")
}

.chm_make_leaflet <- function(chm, aoi_wgs, hex_summary = NULL, output_file = NULL, max_cells_mapview = 2e5) {
  if (!requireNamespace("leaflet", quietly = TRUE) || !requireNamespace("htmlwidgets", quietly = TRUE)) return(NULL)
  if (is.null(aoi_wgs)) return(NULL)

  web_r <- .chm_downsample_raster(chm, max_cells = max_cells_mapview)
  web_r <- terra::project(web_r, "EPSG:4326")
  vals <- terra::values(web_r, mat = FALSE)
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return(NULL)

  pal <- leaflet::colorNumeric(
    palette = viridisLite::viridis(100),
    domain = vals,
    na.color = "transparent"
  )

  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Light") |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter, group = "Dark") |>
    leaflet::addRasterImage(web_r, colors = pal, opacity = 0.78, group = "Canopy height") |>
    leaflet::addLegend(pal = pal, values = vals, title = "Canopy height (m)", group = "Canopy height") |>
    leaflet::addPolygons(data = aoi_wgs, fill = FALSE, color = "black", weight = 2, group = "AOI")

  if (!is.null(hex_summary)) {
    hex_pal <- leaflet::colorNumeric(
      palette = viridisLite::viridis(100),
      domain = hex_summary$mean_chm,
      na.color = "transparent"
    )

    m <- m |>
      leaflet::addPolygons(
        data = hex_summary,
        fillColor = ~hex_pal(mean_chm),
        fillOpacity = 0.55,
        color = "#333333",
        weight = 0.25,
        group = "Hex mean CHM",
        popup = ~paste0(
          "<b>Grid ID:</b> ", grid_id, "<br>",
          "<b>Mean CHM:</b> ", round(mean_chm, 2), " m<br>",
          "<b>Max CHM:</b> ", round(max_chm, 2), " m<br>",
          "<b>Tree cover:</b> ", round(tree_cover_pct, 1), "%<br>",
          "<b>Tall canopy:</b> ", round(tall_canopy_pct, 1), "%<br>",
          "<b>Gap:</b> ", round(gap_pct, 1), "%<br>",
          "<b>Volume proxy:</b> ", round(canopy_volume_proxy, 2)
        )
      )
  }

  m <- m |>
    leaflet::addLayersControl(
      baseGroups = c("Light", "Dark"),
      overlayGroups = c("Canopy height", "Hex mean CHM", "AOI"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  if (!is.null(output_file)) {
    htmlwidgets::saveWidget(m, output_file, selfcontained = TRUE)
  }

  m
}

# ==============================================================================
# Main function
# ==============================================================================

#' Canopy Height Model Analysis and Visualization
#'
#' Downloads, crops, analyzes, and visualizes Meta/WRI DINOv3 global canopy
#' height data for an AOI, or analyzes a local CHM raster. In minimal mode,
#' the function only produces the processed CHM raster and metadata, which is
#' useful when called internally by urban_heat_decision_support().
#'
#' @param location Character string used to geocode an AOI with Nominatim.
#' @param bbox Numeric vector `c(xmin, ymin, xmax, ymax)` in EPSG:4326.
#' @param aoi_geojson Path to a GeoJSON file describing the AOI.
#' @param aoi An `sf` object describing the AOI.
#' @param chm_tif Optional local canopy height raster to analyze instead of downloading tiles.
#' @param output_dir Output directory for rasters, plots, and metadata.
#' @param minimal If `TRUE`, return only the processed raster and metadata.
#' @param reuse_downloads If `TRUE`, reuse previously processed raster outputs when present.
#' @param apply_mask If `TRUE`, mask the raster to the AOI after cropping.
#' @param crop_result If `TRUE`, crop the raster to the AOI extent.
#' @param create_plots If `TRUE`, create static plots.
#' @param create_hex_summary If `TRUE`, create the hexagonal summary layer.
#' @param hex_cellsize_m Hexagon cell size in meters.
#' @param create_interactive If `TRUE`, create an interactive leaflet map.
#' @param fetch_osm_context If `TRUE`, fetch optional OSM context layers for plotting.
#' @param height_threshold Height threshold in meters used for tree cover summaries.
#' @param canopy_thresholds Numeric vector of canopy height thresholds for sensitivity analysis.
#' @param tall_canopy_threshold Threshold in meters used for tall canopy summaries.
#' @param gap_threshold Threshold in meters used for gap summaries.
#' @param max_tiles Maximum number of CHM tiles to process.
#' @param stream_cog If `TRUE`, stream COG tiles instead of downloading them locally.
#' @param cache_tiles If `TRUE`, cache tile downloads in `output_dir`.
#' @param aggregate_for_heat If `TRUE`, also create an aggregated raster for downstream heat workflows.
#' @param aggregate_resolution_m Resolution in meters for the aggregated raster.
#' @param max_cells_plot Maximum number of raster cells used when drawing static plots.
#' @param max_cells_mapview Maximum number of raster cells used in the interactive map.
#' @param compression GDAL compression option passed to `terra::writeRaster()`.
#' @param request_timeout Timeout in seconds for geocoding requests.
#' @param user_agent_string User agent string used for Nominatim requests.
#' @param verbose If `TRUE`, print progress messages.
#'
#' @return A list containing the processed raster, derived statistics, optional hex summary, static plots, optional interactive map, output file paths, selected tiles, and metadata.
#'
#' @examples
#' \dontrun{
#' res <- chm_analysis(
#'   location = "Basel, Switzerland",
#'   output_dir = tempfile("chm_"),
#'   create_interactive = FALSE
#' )
#' names(res$plots)
#' }
#'
#' @export
chm_analysis <- function(
    location = NULL,
    bbox = NULL,
    aoi_geojson = NULL,
    aoi = NULL,
    chm_tif = NULL,
    output_dir = "chm_output",
    minimal = FALSE,
    reuse_downloads = TRUE,
    apply_mask = TRUE,
    crop_result = TRUE,
    create_plots = TRUE,
    create_hex_summary = TRUE,
    hex_cellsize_m = 100,
    create_interactive = TRUE,
    fetch_osm_context = TRUE,
    height_threshold = 2,
    canopy_thresholds = c(2, 5, 10, 15, 20),
    tall_canopy_threshold = 10,
    gap_threshold = 1,
    max_tiles = 100,
    stream_cog = FALSE,
    cache_tiles = TRUE,
    aggregate_for_heat = FALSE,
    aggregate_resolution_m = 10,
    max_cells_plot = 2e5,
    max_cells_mapview = 2e5,
    compression = "LZW",
    request_timeout = 300,
    user_agent_string = "R/chm_analysis (research-use)",
    verbose = TRUE
) {
  optional <- .chm_check_packages()
  .chm_set_gdal_env()
  start_time <- Sys.time()

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  tile_cache_dir <- file.path(output_dir, "tile_cache")
  if (isTRUE(cache_tiles)) dir.create(tile_cache_dir, showWarnings = FALSE, recursive = TRUE)

  .chm_msg("\n+========================================+", verbose = verbose)
  .chm_msg("|        CHM CANOPY ANALYSIS             |", verbose = verbose)
  .chm_msg("|   Meta/WRI DINOv3 global CHM           |", verbose = verbose)
  .chm_msg("+========================================+\n", verbose = verbose)

  dataset_root <- "https://dataforgood-fb-data.s3.amazonaws.com/forests/v2/global/dinov3_global_chm_v2_ml3"
  tiles_index_url <- paste0(dataset_root, "/tiles.geojson")
  chm_base_url <- paste0(dataset_root, "/chm")
  metadata_base_url <- paste0(dataset_root, "/metadata")

  processed_raster_path <- file.path(output_dir, "chm_final_processed.tif")
  aggregated_raster_path <- file.path(output_dir, paste0("chm_aggregated_", aggregate_resolution_m, "m.tif"))
  metadata_path <- file.path(output_dir, "chm_metadata.rds")

  # Reuse existing processed raster if requested.
  if (isTRUE(reuse_downloads) && file.exists(processed_raster_path) && file.info(processed_raster_path)$size > 1000) {
    .chm_msg("[reuse] Using existing processed CHM: ", processed_raster_path, verbose = verbose)
    chm <- terra::rast(processed_raster_path)
    names(chm) <- "canopy_height_m"
    selected_tiles <- NULL
    selected_tile_ids <- character(0)
    aoi_res <- .chm_resolve_aoi(location, bbox, aoi_geojson, aoi, user_agent_string, request_timeout, verbose = FALSE)
    aoi_wgs <- if (!is.null(aoi_res)) aoi_res$aoi_wgs else NULL
    location_label <- if (!is.null(aoi_res)) aoi_res$label else "reused CHM"
  } else {
    # ------------------------------------------------------------------------
    # 1. AOI
    # ------------------------------------------------------------------------
    .chm_msg("[1/8] Preparing AOI...", verbose = verbose)

    aoi_res <- .chm_resolve_aoi(
      location = location,
      bbox = bbox,
      aoi_geojson = aoi_geojson,
      aoi = aoi,
      user_agent_string = user_agent_string,
      request_timeout = request_timeout,
      verbose = verbose
    )

    aoi_wgs <- if (!is.null(aoi_res)) aoi_res$aoi_wgs else NULL
    location_label <- if (!is.null(aoi_res)) aoi_res$label else "local CHM"

    if (is.null(aoi_wgs) && is.null(chm_tif)) {
      stop("Provide one of location, bbox, aoi_geojson, aoi, or chm_tif.", call. = FALSE)
    }

    # ------------------------------------------------------------------------
    # 2. Resolve raster source
    # ------------------------------------------------------------------------
    .chm_msg("[2/8] Resolving CHM raster source...", verbose = verbose)

    selected_tiles <- NULL
    selected_tile_ids <- character(0)
    raster_sources <- character(0)

    if (!is.null(chm_tif)) {
      if (!file.exists(chm_tif)) stop("chm_tif does not exist: ", chm_tif, call. = FALSE)
      .chm_msg("  Using local CHM raster: ", chm_tif, verbose = verbose)
      raster_sources <- chm_tif
    } else {
      index_file <- file.path(output_dir, "chm_tiles.geojson")
      if (!file.exists(index_file) || !isTRUE(cache_tiles)) {
        .chm_msg("  Downloading CHM tile index...", verbose = verbose)
        curl::curl_download(tiles_index_url, index_file, quiet = TRUE)
      } else {
        .chm_msg("  Using cached CHM tile index.", verbose = verbose)
      }

      tiles <- sf::st_read(index_file, quiet = TRUE)
      tiles <- sf::st_transform(tiles, 4326)
      tiles <- .chm_safe_make_valid(tiles)

      id_candidates <- c("tile", "quadkey", "qk", "id", "name")
      id_col <- id_candidates[id_candidates %in% names(tiles)][1]
      if (is.na(id_col)) {
        stop("Could not identify tile id column in tiles.geojson. Columns: ", paste(names(tiles), collapse = ", "), call. = FALSE)
      }

      overlap <- suppressWarnings(sf::st_intersects(tiles, aoi_wgs, sparse = FALSE))
      selected_tiles <- tiles[apply(overlap, 1, any), ]

      if (nrow(selected_tiles) == 0) {
        stop("No CHM tiles intersect the AOI.", call. = FALSE)
      }

      if (nrow(selected_tiles) > max_tiles) {
        warning(
          sprintf("AOI intersects %d tiles; limiting to first %d. Use a smaller AOI or increase max_tiles.", nrow(selected_tiles), max_tiles),
          call. = FALSE
        )
        selected_tiles <- selected_tiles[seq_len(max_tiles), ]
      }

      selected_tile_ids <- as.character(selected_tiles[[id_col]])
      .chm_msg(sprintf("  Selected %d CHM tile(s).", length(selected_tile_ids)), verbose = verbose)

      tile_exists <- function(url) {
        ok <- FALSE
        try({
          h <- httr::HEAD(url, httr::timeout(30))
          ok <- httr::status_code(h) >= 200 && httr::status_code(h) < 400
        }, silent = TRUE)
        ok
      }

      for (tile_id in selected_tile_ids) {
        url_tif <- paste0(chm_base_url, "/", tile_id, ".tif")
        url_tiff <- paste0(chm_base_url, "/", tile_id, ".tiff")
        url <- if (tile_exists(url_tif)) url_tif else url_tiff
        ext <- tools::file_ext(url)
        if (!nzchar(ext)) ext <- "tif"
        local_file <- file.path(tile_cache_dir, paste0(tile_id, ".", ext))

        if (isTRUE(stream_cog)) {
          src <- paste0("/vsicurl/", url)
        } else {
          if (!file.exists(local_file) || file.info(local_file)$size < 1000) {
            .chm_msg("    Downloading tile ", tile_id, verbose = verbose)
            curl::curl_download(url, local_file, quiet = TRUE)
          } else {
            .chm_msg("    Reusing cached tile ", tile_id, verbose = verbose)
          }
          src <- local_file
        }

        if (.chm_validate_raster(src)) {
          raster_sources <- c(raster_sources, src)
        } else {
          warning("Tile could not be read and will be skipped: ", tile_id, call. = FALSE)
        }
      }
    }

    if (length(raster_sources) == 0) {
      stop("No valid CHM rasters available for analysis.", call. = FALSE)
    }

    # ------------------------------------------------------------------------
    # 3. Read, mosaic, crop, mask
    # ------------------------------------------------------------------------
    .chm_msg("[3/8] Reading, mosaicking, cropping, and masking CHM raster...", verbose = verbose)

    read_safe <- function(src) {
      r <- terra::rast(src)
      if (is.na(terra::crs(r))) {
        warning("Raster has no CRS; assigning EPSG:3857 because CHM tiles are web-tile based.", call. = FALSE)
        terra::crs(r) <- "EPSG:3857"
      }
      r
    }

    rasters <- lapply(raster_sources, read_safe)

    if (length(rasters) == 1) {
      chm <- rasters[[1]]
    } else {
      chunk_size <- 8
      chunks <- split(rasters, ceiling(seq_along(rasters) / chunk_size))
      temp_mosaics <- lapply(seq_along(chunks), function(i) {
        .chm_msg(sprintf("  Mosaic chunk %d/%d", i, length(chunks)), verbose = verbose)
        terra::mosaic(terra::sprc(chunks[[i]]), fun = "max", na.rm = TRUE)
      })
      chm <- terra::mosaic(terra::sprc(temp_mosaics), fun = "max", na.rm = TRUE)
    }

    if (!is.null(aoi_wgs)) {
      aoi_for_raster <- sf::st_transform(aoi_wgs, terra::crs(chm))
      aoi_vect <- terra::vect(aoi_for_raster)
      if (isTRUE(crop_result)) chm <- terra::crop(chm, aoi_vect, snap = "out")
      if (isTRUE(apply_mask)) chm <- terra::mask(chm, aoi_vect)
    }

    chm[chm < 0 | chm > 200] <- NA
    names(chm) <- "canopy_height_m"

    gdal_options <- c("TILED=YES", "BIGTIFF=IF_SAFER")
    if (!identical(compression, "NONE")) gdal_options <- c(paste0("COMPRESS=", compression), gdal_options)

    terra::writeRaster(chm, processed_raster_path, overwrite = TRUE, gdal = gdal_options)
    .chm_msg("  Saved processed CHM: ", processed_raster_path, verbose = verbose)
  }

  gdal_options <- c("TILED=YES", "BIGTIFF=IF_SAFER")
  if (!identical(compression, "NONE")) gdal_options <- c(paste0("COMPRESS=", compression), gdal_options)

  # Optional aggregation for downstream heat workflow.
  aggregated_file <- NULL
  if (isTRUE(aggregate_for_heat)) {
    if (terra::res(chm)[1] < aggregate_resolution_m || terra::res(chm)[2] < aggregate_resolution_m) {
      fact <- max(1, round(aggregate_resolution_m / min(abs(terra::res(chm)))))
      chm_agg <- terra::aggregate(chm, fact = fact, fun = mean, na.rm = TRUE)
    } else {
      chm_agg <- chm
    }
    names(chm_agg) <- "canopy_height_m"
    terra::writeRaster(chm_agg, aggregated_raster_path, overwrite = TRUE, gdal = gdal_options)
    aggregated_file <- aggregated_raster_path
  }

  # Minimal return for urban_heat_decision_support().
  if (isTRUE(minimal)) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    meta <- list(
      dataset = "Meta/WRI DINOv3 global CHM",
      dataset_root = dataset_root,
      chm_base_url = chm_base_url,
      metadata_base_url = metadata_base_url,
      location = location_label,
      bbox = if (!is.null(aoi_wgs)) as.numeric(sf::st_bbox(aoi_wgs)) else NULL,
      selected_tile_ids = selected_tile_ids,
      streamed_cogs = stream_cog,
      max_tiles = max_tiles,
      mode = "minimal",
      processed_raster = processed_raster_path,
      aggregated_raster = aggregated_file,
      processing_time_seconds = elapsed,
      created_at = Sys.time(),
      note = "Minimal mode returns only the processed CHM raster and metadata."
    )
    saveRDS(meta, metadata_path)

    .chm_msg("\n* chm_analysis() minimal mode complete.", verbose = verbose)
    .chm_msg("  Processed CHM: ", processed_raster_path, verbose = verbose)

    return(list(
      raster = chm,
      canopy_class_raster = NULL,
      stats = NULL,
      hex_summary = NULL,
      plots = list(),
      interactive_map = NULL,
      files = list(
        processed_raster = processed_raster_path,
        aggregated_raster = aggregated_file,
        canopy_class_raster = NULL,
        hex_summary = NULL,
        interactive_map = NULL,
        plots = list(),
        metadata = metadata_path
      ),
      selected_tiles = selected_tiles,
      selected_tile_ids = selected_tile_ids,
      metadata = meta
    ))
  }

  # --------------------------------------------------------------------------
  # Full mode only from here
  # --------------------------------------------------------------------------

  # Optional OSM context for plots.
  context <- list(roads = NULL, green = NULL, water = NULL, trees = NULL)
  if (isTRUE(fetch_osm_context) && !is.null(aoi_wgs)) {
    .chm_msg("[1b/8] Fetching optional OSM context for cartography...", verbose = verbose)
    osm_context <- .chm_get_osm_context(aoi_wgs, verbose = verbose)
    context <- .chm_prepare_context_layers(osm_context)
  }

  # --------------------------------------------------------------------------
  # 4. Statistics
  # --------------------------------------------------------------------------
  .chm_msg("[4/8] Calculating canopy statistics...", verbose = verbose)

  vals <- terra::values(chm, mat = FALSE)
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) stop("No valid CHM values after crop/mask.", call. = FALSE)

  pixel_area_m2 <- prod(abs(terra::res(chm)))
  valid_px <- length(vals)
  valid_area_km2 <- valid_px * pixel_area_m2 / 1e6

  threshold_stats <- do.call(rbind, lapply(canopy_thresholds, function(th) {
    px <- sum(vals >= th, na.rm = TRUE)
    data.frame(
      threshold_m = th,
      pixels = px,
      area_m2 = px * pixel_area_m2,
      area_km2 = px * pixel_area_m2 / 1e6,
      cover_pct = 100 * px / valid_px
    )
  }))

  aoi_area_km2 <- NA_real_
  if (!is.null(aoi_wgs)) {
    aoi_equal_area <- sf::st_transform(aoi_wgs, "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs")
    aoi_area_km2 <- as.numeric(sf::st_area(aoi_equal_area)) / 1e6
  }

  stats_aoi <- list(
    min_height_m = unname(min(vals, na.rm = TRUE)),
    max_height_m = unname(max(vals, na.rm = TRUE)),
    mean_height_m = unname(mean(vals, na.rm = TRUE)),
    median_height_m = unname(stats::median(vals, na.rm = TRUE)),
    sd_height_m = unname(stats::sd(vals, na.rm = TRUE)),
    p75_height_m = unname(stats::quantile(vals, 0.75, na.rm = TRUE)),
    p90_height_m = unname(stats::quantile(vals, 0.90, na.rm = TRUE)),
    p95_height_m = unname(stats::quantile(vals, 0.95, na.rm = TRUE)),
    tree_cover_pct = unname(100 * sum(vals >= height_threshold, na.rm = TRUE) / valid_px),
    tall_canopy_pct = unname(100 * sum(vals >= tall_canopy_threshold, na.rm = TRUE) / valid_px),
    gap_pct = unname(100 * sum(vals <= gap_threshold, na.rm = TRUE) / valid_px),
    canopy_volume_proxy_m3_per_m2 = unname(mean(pmax(vals - height_threshold, 0), na.rm = TRUE)),
    valid_pixel_count = valid_px,
    valid_area_km2 = valid_area_km2,
    aoi_geometric_area_km2 = aoi_area_km2,
    pixel_area_m2 = pixel_area_m2,
    height_threshold_for_tree_stats_m = height_threshold,
    tall_canopy_threshold_m = tall_canopy_threshold,
    gap_threshold_m = gap_threshold,
    raster_crs = terra::crs(chm, proj = TRUE)
  )

  # --------------------------------------------------------------------------
  # 5. Canopy class raster
  # --------------------------------------------------------------------------
  .chm_msg("[5/8] Creating canopy class raster...", verbose = verbose)

  canopy_class <- .chm_canopy_classes(
    chm,
    gap_threshold = gap_threshold,
    height_threshold = height_threshold,
    tall_canopy_threshold = tall_canopy_threshold
  )
  names(canopy_class) <- "canopy_class"
  class_raster_path <- file.path(output_dir, "chm_canopy_classes.tif")
  terra::writeRaster(canopy_class, class_raster_path, overwrite = TRUE, gdal = gdal_options)

  # --------------------------------------------------------------------------
  # 6. Hex summary
  # --------------------------------------------------------------------------
  .chm_msg("[6/8] Creating optional hex summary...", verbose = verbose)

  hex_summary <- NULL
  hex_path <- NULL

  if (isTRUE(create_hex_summary) && !is.null(aoi_wgs)) {
    hex_summary <- .chm_make_hex_summary(
      chm = chm,
      aoi_wgs = aoi_wgs,
      hex_cellsize_m = hex_cellsize_m,
      height_threshold = height_threshold,
      tall_canopy_threshold = tall_canopy_threshold,
      gap_threshold = gap_threshold,
      exactextract_available = optional$exactextractr
    )

    hex_path <- file.path(output_dir, "chm_hex_summary.gpkg")
    sf::st_write(hex_summary, hex_path, delete_dsn = TRUE, quiet = TRUE)
    .chm_msg("  Saved hex summary: ", hex_path, verbose = verbose)
  }

  # --------------------------------------------------------------------------
  # 7. Plots
  # --------------------------------------------------------------------------
  .chm_msg("[7/8] Creating plots...", verbose = verbose)

  plots <- list()
  plot_files <- list()

  if (isTRUE(create_plots)) {
    plots$histogram <- .chm_plot_histogram(vals, height_threshold, tall_canopy_threshold)
    plots$threshold_curve <- .chm_plot_threshold_curve(threshold_stats)

    if (!is.null(hex_summary)) {
      plots$hex_mean_height <- .chm_plot_hex_map(
        hex_summary,
        value_col = "mean_chm",
        title = "Neighbourhood-Scale Canopy Structure",
        subtitle = paste0("Hex grid: ", hex_cellsize_m, " m; mean canopy height"),
        legend_title = "Mean\nCHM (m)",
        context = context,
        aoi_wgs = aoi_wgs
      )

      plots$hex_tree_cover <- .chm_plot_hex_map(
        hex_summary,
        value_col = "tree_cover_pct",
        title = "Tree Canopy Cover",
        subtitle = paste0("Percentage of CHM pixels >= ", height_threshold, " m"),
        legend_title = "Tree\ncover (%)",
        context = context,
        aoi_wgs = aoi_wgs
      )

      plots$canopy_structure_typology <- .chm_plot_typology(hex_summary)
    }

    plots <- plots[!vapply(plots, is.null, logical(1))]

    for (nm in names(plots)) {
      file <- file.path(output_dir, paste0(nm, ".png"))
      bg <- if (grepl("map|hex", nm)) "#FAF9F6" else "white"
      ggplot2::ggsave(file, plots[[nm]], width = 9.5, height = 7.8, dpi = 300, bg = bg)
      plot_files[[nm]] <- file
    }
  }

  # --------------------------------------------------------------------------
  # 8. Interactive map
  # --------------------------------------------------------------------------
  .chm_msg("[8/8] Creating optional interactive map...", verbose = verbose)

  interactive_map <- NULL
  interactive_file <- NULL

  if (isTRUE(create_interactive)) {
    interactive_file <- file.path(output_dir, "chm_interactive_map.html")
    interactive_map <- .chm_make_leaflet(
      chm = chm,
      aoi_wgs = aoi_wgs,
      hex_summary = hex_summary,
      output_file = interactive_file,
      max_cells_mapview = max_cells_mapview
    )
    if (is.null(interactive_map)) interactive_file <- NULL
  }

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  meta <- list(
    dataset = "Meta/WRI DINOv3 global CHM",
    dataset_root = dataset_root,
    chm_base_url = chm_base_url,
    metadata_base_url = metadata_base_url,
    location = location_label,
    bbox = if (!is.null(aoi_wgs)) as.numeric(sf::st_bbox(aoi_wgs)) else NULL,
    selected_tile_ids = selected_tile_ids,
    streamed_cogs = stream_cog,
    max_tiles = max_tiles,
    mode = "full",
    processed_raster = processed_raster_path,
    aggregated_raster = aggregated_file,
    processing_time_seconds = elapsed,
    created_at = Sys.time(),
    note = "Interactive and static maps may be downsampled for performance. Exported raster preserves processed CHM resolution."
  )

  saveRDS(meta, metadata_path)

  .chm_msg("\n* chm_analysis() complete.", verbose = verbose)
  .chm_msg("  Mean height: ", round(stats_aoi$mean_height_m, 2), " m", verbose = verbose)
  .chm_msg("  Tree cover >= ", height_threshold, " m: ", round(stats_aoi$tree_cover_pct, 1), "%", verbose = verbose)
  .chm_msg("  Output dir: ", normalizePath(output_dir, mustWork = FALSE), verbose = verbose)

  list(
    raster = chm,
    canopy_class_raster = canopy_class,
    stats = list(
      aoi = stats_aoi,
      threshold_cover = threshold_stats
    ),
    hex_summary = hex_summary,
    plots = plots,
    interactive_map = interactive_map,
    files = list(
      processed_raster = processed_raster_path,
      aggregated_raster = aggregated_file,
      canopy_class_raster = class_raster_path,
      hex_summary = hex_path,
      interactive_map = interactive_file,
      plots = plot_files,
      metadata = metadata_path
    ),
    selected_tiles = selected_tiles,
    selected_tile_ids = selected_tile_ids,
    metadata = meta
  )
}
