#' Strict High-Resolution Sky-View Factor (SVF) Calculation Engine
#'
#' @description
#' Enforces explicit spatial quality tiers and handles hybrid data sources (local rasters/shapefiles,
#' Global Building Atlas Parquet files via S3, and AWS terrain tiles via elevatr).
#' Performs high-performance parallelized ray-casting to compute point-based sky-view factors.
#' Calculates SVF using the mathematically rigorous horizontal solid-angle projection formula
#' proposed by Johnson and Watson (1984) and Oke (1987): SVF = mean(cos^2(horizon_angle)),
#' representing the horizontal projection of visible sky accounting for the Lambert cosine law
#' of diffuse solar irradiance. Generates interactive Leaflet maps with dynamic layer-linked legends.
#'
#' @importFrom sf st_transform st_geometry st_centroid st_union st_coordinates st_bbox st_as_sfc st_sf st_sfc st_polygon st_multipolygon st_make_valid st_simplify st_read st_write st_intersection st_is_empty st_point_on_surface st_nearest_feature st_distance st_length st_line_sample st_cast st_crs
#' @importFrom terra rast project crop mask rasterize ext values app res resample writeRaster vect extract
#' @importFrom dplyr filter select collect bind_rows mutate transmute group_by summarise left_join n any_of .data
#' @importFrom httr GET POST http_error timeout content add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom arrow open_dataset
#' @importFrom leaflet leaflet leafletOptions addProviderTiles addPolylines addCircleMarkers addPolygons addLayersControl layersControlOptions colorNumeric addLegend hideGroup
#' @importFrom leaflet.extras addHeatmap
#' @importFrom htmlwidgets saveWidget
#' @importFrom ggplot2 ggplot geom_sf aes scale_color_gradientn coord_sf labs theme_minimal theme element_text element_blank ggsave
#' @importFrom ggspatial layer_spatial annotation_scale annotation_north_arrow north_arrow_minimal
#' @importFrom parallel detectCores mclapply
#'
#' @param city_name Optional city name used to fetch a real boundary from Nominatim.
#' @param boundary Optional sf/sfc/bbox boundary. Used directly if supplied.
#' @param bbox Optional analysis rectangle, accepted as bbox/sfc/sf or numeric c(xmin, ymin, xmax, ymax).
#' @param bbox_crs CRS for numeric bbox input.
#' @param boundary_simplify_m Simplification tolerance in metres for Osm boundary.
#' @param analysis_buffer_m Optional buffer around boundary for analysis.
#' @param analysis_scale Either "city_screening" (coarser) or "street_canyon_local" (high-res).
#' @param terrain_source Either "local" or "elevatr".
#' @param terrain_path Path to a local terrain raster or local terrain vector/TIN source when terrain_source="local".
#' @param terrain_layer Optional layer name for vector terrain sources.
#' @param terrain_resolution_m Rasterization resolution for local vector terrain sources.
#' @param elevatr_z Zoom level for elevatr fallback (12-14 recommended).
#' @param buildings_source Either "local" or "gba".
#' @param buildings_path Path to a local building file when buildings_source="local".
#' @param buildings_object Optional local building sf object.
#' @param canopy_path Optional local canopy-height raster path.
#' @param canopy_object Optional local canopy-height raster object.
#' @param sample_mode "street", "grid", or "both".
#' @param spacing_street_m Street sampling interval in metres.
#' @param spacing_grid_m Grid sampling interval in metres.
#' @param max_distance_m Horizon search radius in metres.
#' @param step_m Step length along each ray.
#' @param n_directions Number of azimuth directions.
#' @param observer_height_m Observer height above ground.
#' @param target_resolution_m Optional coarsening target for the obstruction raster.
#' @param return_raw_angles Whether to return raw horizon angles for skyline plotting.
#' @param include_gpkg Whether to save outputs as GeoPackage files.
#' @param include_static Whether to write a static street-level map when street output exists.
#' @param include_leaflet Whether to write a Leaflet map when street output exists.
#' @param include_3d Whether to generate an interactive 3D WebGL explorer.
#' @param output_dir Output directory for written files. If NULL, files are not written.
#' @param output_prefix Output file prefix.
#' @param street_width Street canyon line width in pixels for 3D WebGL map (default 4).
#' @param palette Color palette name for 3D map. One of "urban", "spectral", "magma", "viridis", "coolwarm", "plasma".
#' @param static_linewidth Line width for static PNG maps. If NULL, scales proportionally from street_width.
#' @param osm_timeout Numeric. Timeout in seconds for the Overpass API query fetching street networks. Default is 180.
#'
#' @return A list with point outputs, summaries, analytics, and method metadata.
#'
#' @examples
#' \dontrun{
#'   library(greenR)
#'
#'   # Example 1: Fast Citywide screening using online data
#'   # Defaults to only writing fast static maps (no heavy GIS or interactive outputs)
#'   result_svf <- uh_svf(
#'     city_name = "Basel, Switzerland",
#'     analysis_scale = "city_screening",
#'     terrain_source = "elevatr",
#'     elevatr_z = 13,
#'     buildings_source = "gba",
#'     sample_mode = "street",
#'     spacing_street_m = 30,
#'     output_dir = tempdir(),
#'     output_prefix = "basel_svf"
#'   )
#'   
#'   # View calculated street canyon SVF
#'   print(head(result_svf$street_summary))
#'
#'   # Example 2: Local street-canyon analysis using custom local datasets
#'   result_local <- uh_svf(
#'     city_name = "Custom Area",
#'     bbox = c(7.58, 47.55, 7.60, 47.57),
#'     analysis_scale = "street_canyon_local",
#'     terrain_source = "local",
#'     terrain_path = "path/to/local_dem.tif",       # Local DEM raster
#'     buildings_source = "local",
#'     buildings_path = "path/to/local_buildings.gpkg", # Local building shapes
#'     canopy_path = "path/to/local_canopy_chm.tif",  # Optional canopy raster
#'     sample_mode = "both",
#'     spacing_street_m = 15,
#'     spacing_grid_m = 30,
#'     n_directions = 72,                             # Rigorous ray casting
#'     output_dir = tempdir()
#'   )
#'
#'   # Example 3: Full-data rendering with complete output generation
#'   # Explicitly opts-in to generating GPKG files, Interactive Leaflet maps, and 3D WebGL explorers.
#'   # Note: Generating interactive maps for large cities can be very slow.
#'   result_full <- uh_svf(
#'     city_name = "Monaco",
#'     analysis_scale = "city_screening",
#'     terrain_source = "elevatr",
#'     buildings_source = "gba",
#'     sample_mode = "both",
#'     spacing_street_m = 20,
#'     spacing_grid_m = 40,
#'     include_gpkg = TRUE,
#'     include_static = TRUE,
#'     include_leaflet = TRUE,
#'     include_3d = TRUE,
#'     output_dir = tempdir(),
#'     output_prefix = "monaco_full"
#'   )
#' }
#' @export
uh_svf <- function(
  city_name = NULL,
  boundary = NULL,
  bbox = NULL,
  bbox_crs = 4326,
  boundary_simplify_m = 0,
  analysis_buffer_m = 0,
  analysis_scale = c("city_screening", "street_canyon_local"),
  terrain_source = c("local", "elevatr"),
  terrain_path = NULL,
  terrain_layer = NULL,
  terrain_resolution_m = 5,
  elevatr_z = 12,
  buildings_source = c("local", "gba"),
  buildings_path = NULL,
  buildings_object = NULL,
  canopy_path = NULL,
  canopy_object = NULL,
  sample_mode = c("street", "grid", "both"),
  spacing_street_m = 25,
  spacing_grid_m = 50,
  max_distance_m = 300,
  step_m = 10,
  n_directions = 72,
  observer_height_m = 1.5,
  target_resolution_m = NULL,
  return_raw_angles = FALSE,
  include_gpkg = FALSE,
  include_static = TRUE,
  include_leaflet = FALSE,
  include_3d = FALSE,
  output_dir = NULL,
  output_prefix = NULL,
  street_width = 4,
  palette = "urban",
  static_linewidth = NULL,
  osm_timeout = 180
) {
  analysis_scale <- match.arg(analysis_scale)
  terrain_source <- match.arg(terrain_source)
  buildings_source <- match.arg(buildings_source)
  sample_mode <- match.arg(sample_mode)

  if (analysis_scale == "street_canyon_local") {
    if (terrain_source != "local") {
      stop("street_canyon_local analysis requires terrain_source='local'.", call. = FALSE)
    }
    if (buildings_source != "local") {
      stop("street_canyon_local analysis requires buildings_source='local'.", call. = FALSE)
    }
  }

  boundary_use <- .uh_svf_coerce_boundary(city_name = city_name, boundary = boundary, simplify_m = boundary_simplify_m)
  area <- .uh_svf_make_analysis_area(
    boundary = boundary_use,
    bbox = bbox,
    bbox_crs = bbox_crs,
    buffer_m = analysis_buffer_m,
    radius_m = max_distance_m
  )

  if (is.null(output_prefix)) output_prefix <- .uh_svf_slug(city_name %||% "svf")

  can_parallel <- .Platform$OS.type != "windows" && parallel::detectCores() > 1
  
  if (can_parallel) {
    message("[svf] Starting parallel fetching of terrain, buildings, and canopy...")
    t_start <- proc.time()[[3]]
    
    # Spawn terrain job
    job_terrain <- parallel::mcparallel({
      meta <- .uh_svf_get_terrain(
        analysis_area = area$analysis_area,
        processing_crs = area$processing_crs,
        terrain_source = terrain_source,
        terrain_path = terrain_path,
        terrain_layer = terrain_layer,
        terrain_resolution_m = terrain_resolution_m,
        elevatr_z = elevatr_z
      )
      if (!is.null(meta$raster)) {
        meta$raster <- terra::wrap(meta$raster)
      }
      meta
    })
    
    # Spawn buildings job (with deferred elevation extraction)
    job_buildings <- parallel::mcparallel({
      .uh_svf_get_buildings(
        analysis_area = area$analysis_area,
        processing_crs = area$processing_crs,
        terrain_raster = NULL,
        buildings_source = buildings_source,
        buildings_path = buildings_path,
        buildings_object = buildings_object
      )
    })
    
    # Spawn canopy job (with deferred resampling)
    job_canopy <- parallel::mcparallel({
      meta <- .uh_svf_get_canopy(
        analysis_area = area$analysis_area,
        processing_crs = area$processing_crs,
        canopy_path = canopy_path,
        canopy_object = canopy_object,
        target_res = NULL
      )
      if (!is.null(meta$raster)) {
        meta$raster <- terra::wrap(meta$raster)
      }
      meta
    })
    
    # Collect all jobs
    jobs_collected <- parallel::mccollect(list(job_terrain, job_buildings, job_canopy))
    
    # Extract results by PID
    terrain_meta <- jobs_collected[[as.character(job_terrain$pid)]]
    building_meta <- jobs_collected[[as.character(job_buildings$pid)]]
    canopy_meta <- jobs_collected[[as.character(job_canopy$pid)]]
    
    # Check for failures and fall back to sequential if needed
    if (inherits(terrain_meta, "try-error") || is.null(terrain_meta)) {
      stop("[svf] Parallel terrain fetching failed.", call. = FALSE)
    }
    
    if (inherits(building_meta, "try-error")) {
      message("[svf] Parallel building fetching failed. Retrying sequentially...")
      building_meta <- .uh_svf_get_buildings(
        analysis_area = area$analysis_area,
        processing_crs = area$processing_crs,
        terrain_raster = NULL,
        buildings_source = buildings_source,
        buildings_path = buildings_path,
        buildings_object = buildings_object
      )
    }
    
    if (inherits(canopy_meta, "try-error")) {
      message("[svf] Parallel canopy fetching failed. Retrying sequentially...")
      canopy_meta <- .uh_svf_get_canopy(
        analysis_area = area$analysis_area,
        processing_crs = area$processing_crs,
        canopy_path = canopy_path,
        canopy_object = canopy_object,
        target_res = NULL
      )
    }
    
    # Unwrap SpatRaster objects
    if (!is.null(terrain_meta$raster)) {
      terrain_meta$raster <- terra::rast(terrain_meta$raster)
    }
    terrain_raster <- terrain_meta$raster
    
    if (!is.null(canopy_meta) && !is.null(canopy_meta$raster)) {
      canopy_meta$raster <- terra::rast(canopy_meta$raster)
    }
    
    # Post-process building heights sequentially
    if (!is.null(building_meta) && !is.null(building_meta$buildings)) {
      buildings <- .uh_svf_extract_building_elevation(building_meta$buildings, terrain_raster)
      building_meta$buildings <- buildings
    } else {
      buildings <- NULL
    }
    
    # Post-process canopy resampling sequentially
    if (!is.null(canopy_meta) && !is.null(canopy_meta$raster)) {
      target_res <- mean(terra::res(terrain_raster))
      canopy_raster <- canopy_meta$raster
      canopy_raster <- terra::project(canopy_raster, area$processing_crs, res = target_res, method = "bilinear")
      canopy_raster <- terra::crop(canopy_raster, terra::vect(area$analysis_area), snap = "out")
      canopy_raster <- terra::mask(canopy_raster, terra::vect(area$analysis_area))
      canopy_raster[canopy_raster < 0] <- NA
      names(canopy_raster) <- "canopy_height_m"
      canopy_meta$raster <- canopy_raster
    } else {
      canopy_raster <- NULL
    }
    
    message(sprintf("[svf] Parallel fetch and sequential post-processing complete in %.1f seconds.", proc.time()[[3]] - t_start))
    
  } else {
    # Sequential fallback (original logic)
    message("[svf] Step 1/7: Fetching terrain...")
    t0 <- proc.time()[[3]]
    terrain_meta <- .uh_svf_get_terrain(
      analysis_area = area$analysis_area,
      processing_crs = area$processing_crs,
      terrain_source = terrain_source,
      terrain_path = terrain_path,
      terrain_layer = terrain_layer,
      terrain_resolution_m = terrain_resolution_m,
      elevatr_z = elevatr_z
    )
    terrain_raster <- terrain_meta$raster
    message(sprintf("[svf] Step 1/7 complete (%.1fs). Terrain res: %.1f m", proc.time()[[3]] - t0, mean(terra::res(terrain_raster))))

    message("[svf] Step 2/7: Fetching buildings...")
    t0 <- proc.time()[[3]]
    building_meta <- .uh_svf_get_buildings(
      analysis_area = area$analysis_area,
      processing_crs = area$processing_crs,
      terrain_raster = terrain_raster,
      buildings_source = buildings_source,
      buildings_path = buildings_path,
      buildings_object = buildings_object
    )
    buildings <- building_meta$buildings
    message(sprintf("[svf] Step 2/7 complete (%.1fs). %d buildings found.", proc.time()[[3]] - t0, if (is.null(buildings)) 0 else nrow(buildings)))

    message("[svf] Step 3/7: Fetching canopy height model...")
    t0 <- proc.time()[[3]]
    canopy_meta <- .uh_svf_get_canopy(
      analysis_area = area$analysis_area,
      processing_crs = area$processing_crs,
      canopy_path = canopy_path,
      canopy_object = canopy_object,
      target_res = mean(terra::res(terrain_raster))
    )
    canopy_raster <- canopy_meta$raster
    message(sprintf("[svf] Step 3/7 complete (%.1fs). Canopy available: %s", proc.time()[[3]] - t0, !is.null(canopy_raster)))
  }

  message("[svf] Step 4/7: Building obstruction raster...")
  t0 <- proc.time()[[3]]
  obstruction <- .uh_svf_build_obstruction(
    terrain_raster = terrain_raster,
    buildings = buildings,
    canopy_raster = canopy_raster,
    target_resolution_m = target_resolution_m
  )

  roads <- NULL
  street_points <- NULL
  street_summary <- NULL
  message(sprintf("[svf] Step 4/7 complete (%.1fs).", proc.time()[[3]] - t0))

  if (sample_mode %in% c("street", "both")) {
    message("[svf] Step 5/7: Fetching street network and computing street SVF...")
    t0 <- proc.time()[[3]]
    roads <- .uh_svf_get_osm_roads(area$analysis_area, timeout = osm_timeout)
    street_points <- .uh_svf_make_street_points(roads, spacing_m = spacing_street_m)
    street_points <- .uh_svf_compute_points(
      sample_points = street_points,
      terrain_raster = obstruction$terrain,
      obstruction_raster = obstruction$obstruction,
      n_directions = n_directions,
      max_distance_m = max_distance_m,
      step_m = step_m,
      observer_height_m = observer_height_m,
      return_raw_angles = return_raw_angles
    )
    street_summary <- .uh_svf_summarise_streets(street_points, roads)
    street_points <- sf::st_filter(street_points, area$boundary_proj)
    street_points <- street_points[sf::st_geometry_type(street_points) %in% c("POINT", "MULTIPOINT"), ]
    street_summary <- sf::st_filter(street_summary, area$boundary_proj)
    street_summary <- street_summary[sf::st_geometry_type(street_summary) %in% c("LINESTRING", "MULTILINESTRING"), ]
    street_points <- street_points[!sf::st_is_empty(street_points), ]
    street_summary <- street_summary[!sf::st_is_empty(street_summary), ]
    message(sprintf("[svf] Step 5/7 complete (%.1fs). %d street points, %d segments.", proc.time()[[3]] - t0, nrow(street_points), nrow(street_summary)))
  }

  grid_points <- NULL
  building_svf <- NULL
  if (sample_mode %in% c("grid", "both")) {
    message("[svf] Step 6/7: Computing grid SVF and building-adjacent SVF...")
    t0 <- proc.time()[[3]]
    grid_points <- .uh_svf_make_grid_points(area$analysis_area, spacing_m = spacing_grid_m)
    grid_points <- .uh_svf_compute_points(
      sample_points = grid_points,
      terrain_raster = obstruction$terrain,
      obstruction_raster = obstruction$obstruction,
      n_directions = n_directions,
      max_distance_m = max_distance_m,
      step_m = step_m,
      observer_height_m = observer_height_m,
      return_raw_angles = return_raw_angles
    )
    building_svf <- .uh_svf_summarise_buildings(grid_points, buildings)
    building_svf <- sf::st_filter(building_svf, area$boundary_proj)
    building_svf <- building_svf[sf::st_geometry_type(building_svf) %in% c("POLYGON", "MULTIPOLYGON"), ]
    building_svf <- building_svf[!sf::st_is_empty(building_svf), ]
    message(sprintf("[svf] Step 6/7 complete (%.1fs). %d grid points, %d buildings.", proc.time()[[3]] - t0, nrow(grid_points), if (!is.null(building_svf)) nrow(building_svf) else 0))
  }

  quality_tier <- if (analysis_scale == "street_canyon_local") {
    "local_high_quality"
  } else if (terrain_meta$quality_tier == "local_high_quality" && building_meta$quality_tier == "local_high_quality") {
    "local_high_quality"
  } else {
    "global_screening"
  }

  analytics <- .uh_svf_analytics(
    street_points = street_points,
    street_summary = street_summary,
    building_svf = building_svf,
    canopy_raster = canopy_raster
  )
  boundary_source_val <- if ("boundary_source" %in% names(boundary_use)) boundary_use$boundary_source[[1]] else "user_supplied"

  method_meta <- data.frame(
    parameter = c(
      "analysis_scale", "quality_tier", "boundary_source", "processing_crs",
      "terrain_source", "terrain_native_resolution_m",
      "buildings_source", "canopy_source",
      "sample_mode", "spacing_street_m", "spacing_grid_m",
      "observer_height_m", "max_distance_m", "step_m", "n_directions",
      "svf_formula", "notes"
    ),
    value = c(
      analysis_scale,
      quality_tier,
      boundary_source_val,
      area$processing_crs,
      terrain_meta$source,
      round(terrain_meta$native_resolution_m, 2),
      building_meta$source,
      canopy_meta$source,
      sample_mode,
      spacing_street_m,
      spacing_grid_m,
      observer_height_m,
      max_distance_m,
      step_m,
      n_directions,
      "mean(cos(max_horizon_angle_by_azimuth)^2)",
      if (quality_tier == "global_screening") "Use for citywide screening, not for validated street-canyon microclimate claims." else "Suitable for local geometric street-form analysis if local inputs are valid."
    ),
    stringsAsFactors = FALSE
  )

  # Optional file writing and map creation
  if (!is.null(output_dir)) {
    message("[svf] Step 7/7: Writing outputs (GIS files, static maps, interactive maps, 3D explorer)...")
    t0 <- proc.time()[[3]]
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    out_dir <- file.path(output_dir, output_prefix)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    # ---- 7a. GIS files (fast, optional) ----
    if (include_gpkg) {
      if (!is.null(street_points) && nrow(street_points) > 0) {
        write_pts <- street_points
        if ("horizon_angles" %in% names(write_pts)) write_pts$horizon_angles <- NULL
        sf::st_write(sf::st_transform(write_pts, 4326), file.path(out_dir, paste0(output_prefix, "_street_svf_points.gpkg")), delete_dsn = TRUE, quiet = TRUE)
        sf::st_write(sf::st_transform(street_summary, 4326), file.path(out_dir, paste0(output_prefix, "_street_svf_summary.gpkg")), delete_dsn = TRUE, quiet = TRUE)
      }
      if (!is.null(building_svf) && nrow(building_svf) > 0) {
        sf::st_write(sf::st_transform(building_svf, 4326), file.path(out_dir, paste0(output_prefix, "_building_svf.gpkg")), delete_dsn = TRUE, quiet = TRUE)
      }
      if (!is.null(grid_points) && nrow(grid_points) > 0) {
        write_grid <- grid_points
        if ("horizon_angles" %in% names(write_grid)) write_grid$horizon_angles <- NULL
        sf::st_write(sf::st_transform(write_grid, 4326), file.path(out_dir, paste0(output_prefix, "_grid_svf_points.gpkg")), delete_dsn = TRUE, quiet = TRUE)
      }
      message(sprintf("[svf] Step 7a complete (%.1fs). GIS files written.", proc.time()[[3]] - t0))
    }
    utils::write.csv(analytics, file.path(out_dir, paste0(output_prefix, "_svf_analytics.csv")), row.names = FALSE)

    # ---- 7b. Static PNG maps (fast) ----
    if (include_static && !is.null(street_summary) && nrow(street_summary) > 0) {
      basemap <- tryCatch(.uh_svf_fetch_basemap(area$analysis_area, provider = "CartoDB.Positron", zoom = 15), error = function(e) NULL)
      plot_lw <- static_linewidth %||% (street_width * 0.22)
      p <- .uh_svf_plot_static(
        street_summary = street_summary,
        boundary = area$boundary,
        buildings = buildings,
        basemap = basemap,
        title = sprintf("%s street-level SVF", city_name %||% "SVF"),
        subtitle = sprintf("%s analysis * point-based horizon SVF at %.1f m observer height", gsub("_", " ", quality_tier), observer_height_m),
        linewidth = plot_lw
      )
      ggplot2::ggsave(file.path(out_dir, paste0(output_prefix, "_street_svf_static.png")), p, width = 12, height = 9, dpi = 220, bg = "white")
    } else if (include_static && !is.null(grid_points) && nrow(grid_points) > 0) {
      basemap <- tryCatch(.uh_svf_fetch_basemap(area$analysis_area, provider = "CartoDB.Positron", zoom = 15), error = function(e) NULL)
      p <- .uh_svf_plot_grid_static(
        grid_points = grid_points,
        boundary = area$boundary,
        buildings = buildings,
        basemap = basemap,
        title = sprintf("%s grid-based SVF", city_name %||% "SVF"),
        subtitle = sprintf("%s analysis * point-based horizon SVF at %.1f m observer height", gsub("_", " ", quality_tier), observer_height_m)
      )
      ggplot2::ggsave(file.path(out_dir, paste0(output_prefix, "_grid_svf_static.png")), p, width = 12, height = 9, dpi = 220, bg = "white")
    }

    # Distribution plot
    if (!is.null(street_summary) && nrow(street_summary) > 0) {
      tryCatch({
        dist_plot <- uh_svf_plot_distribution(street_summary, title = sprintf("%s: Urban Microclimate Exposure Assessment", city_name %||% "SVF"))
        ggplot2::ggsave(file.path(out_dir, paste0(output_prefix, "_svf_distribution.png")), dist_plot, width = 14, height = 6, dpi = 220, bg = "white")
      }, error = function(e) message(sprintf("[svf] Warning: Distribution plot skipped: %s", e$message)))
    }
    message(sprintf("[svf] Step 7b complete (%.1fs). Static maps written.", proc.time()[[3]] - t0))

    # ---- 7c. Interactive Leaflet map (full data, preferCanvas=TRUE) ----
    if (include_leaflet && !is.null(street_summary) && nrow(street_summary) > 0) {
      street_range <- range(street_summary$svf_mean, na.rm = TRUE)
      if (diff(street_range) < 0.01) street_range <- c(0, 1)
      pal_street <- leaflet::colorNumeric(.uh_svf_palette, domain = street_range, na.color = "transparent")

      building_range <- c(0, 1)
      if (!is.null(building_svf) && nrow(building_svf) > 0) {
        building_range <- range(building_svf$svf, na.rm = TRUE)
        if (diff(building_range) < 0.01) building_range <- c(0, 1)
      }
      pal_building <- leaflet::colorNumeric(.uh_svf_palette, domain = building_range, na.color = "transparent")

      street_points_4326 <- sf::st_transform(street_points, 4326)

      leaf <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Positron") |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter, group = "Dark") |>
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Imagery") |>
        leaflet::addPolylines(
          data = sf::st_transform(street_summary, 4326),
          group = "Street SVF",
          color = ~pal_street(svf_mean),
          weight = 4,
          opacity = 0.92,
          popup = ~sprintf("Street Mean SVF: %.2f", svf_mean),
          label = ~sprintf("Street Mean SVF: %.2f", svf_mean),
          highlightOptions = leaflet::highlightOptions(weight = 6, color = "#ffffff", bringToFront = TRUE)
        ) |>
        leaflet::addCircleMarkers(
          data = street_points_4326,
          group = "Street sample points",
          radius = 2.5,
          stroke = FALSE,
          fillOpacity = 0.85,
          fillColor = ~pal_street(svf),
          popup = ~sprintf("SVF: %.2f", svf)
        )

      # Building polygons
      building_svf_leaf <- NULL
      if (!is.null(building_svf) && nrow(building_svf) > 0) {
        building_svf_leaf <- sf::st_transform(building_svf, 4326)
        leaf <- leaf |>
          leaflet::addPolygons(
            data = building_svf_leaf,
            group = "Building-adjacent SVF",
            fillColor = ~pal_building(svf),
            fillOpacity = 0.48,
            color = "#4f463d",
            weight = 0.5,
            opacity = 0.65,
            popup = ~sprintf("Building-adjacent SVF: %.2f", svf)
          )
      }

      # Legends
      leaf <- leaf |>
        leaflet::addLegend(
          pal = pal_street,
          values = street_range,
          title = "Street SVF",
          group = "Street SVF",
          position = "bottomright"
        )
      if (!is.null(building_svf_leaf) && nrow(building_svf_leaf) > 0) {
        leaf <- leaf |>
          leaflet::addLegend(
            pal = pal_building,
            values = building_range,
            title = "Building-adjacent SVF",
            group = "Building-adjacent SVF",
            position = "bottomright"
          )
      }

      # Layer controls
      overlays <- c("Street SVF", "Street sample points")
      if (!is.null(building_svf_leaf) && nrow(building_svf_leaf) > 0) overlays <- c(overlays, "Building-adjacent SVF")
      leaf <- leaf |>
        leaflet::addLayersControl(
          baseGroups = c("Positron", "Dark", "Imagery"),
          overlayGroups = overlays,
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
      htmlwidgets::saveWidget(leaf, file.path(out_dir, paste0(output_prefix, "_svf_leaflet.html")), selfcontained = FALSE)

    } else if (include_leaflet && !is.null(grid_points) && nrow(grid_points) > 0) {
      grid_range <- range(grid_points$svf, na.rm = TRUE)
      if (diff(grid_range) < 0.01) grid_range <- c(0, 1)
      pal_grid <- leaflet::colorNumeric(.uh_svf_palette, domain = grid_range, na.color = "transparent")

      building_range <- c(0, 1)
      if (!is.null(building_svf) && nrow(building_svf) > 0) {
        building_range <- range(building_svf$svf, na.rm = TRUE)
        if (diff(building_range) < 0.01) building_range <- c(0, 1)
      }
      pal_building <- leaflet::colorNumeric(.uh_svf_palette, domain = building_range, na.color = "transparent")

      grid_points_4326 <- sf::st_transform(grid_points, 4326)

      leaf <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Positron") |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter, group = "Dark") |>
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Imagery") |>
        leaflet::addCircleMarkers(
          data = grid_points_4326,
          group = "Grid SVF",
          radius = 2.5,
          stroke = FALSE,
          fillOpacity = 0.9,
          fillColor = ~pal_grid(svf),
          popup = ~sprintf("SVF: %.2f", svf)
        ) |>
        leaflet::addLegend(
          pal = pal_grid,
          values = grid_range,
          title = "Grid SVF",
          group = "Grid SVF",
          position = "bottomright"
        )

      # Building polygons
      building_svf_leaf <- NULL
      if (!is.null(building_svf) && nrow(building_svf) > 0) {
        building_svf_leaf <- sf::st_transform(building_svf, 4326)
        leaf <- leaf |>
          leaflet::addPolygons(
            data = building_svf_leaf,
            group = "Building-adjacent SVF",
            fillColor = ~pal_building(svf),
            fillOpacity = 0.48,
            color = "#4f463d",
            weight = 0.5,
            opacity = 0.65,
            popup = ~sprintf("Building-adjacent SVF: %.2f", svf)
          ) |>
          leaflet::addLegend(
            pal = pal_building,
            values = building_range,
            title = "Building-adjacent SVF",
            group = "Building-adjacent SVF",
            position = "bottomright"
          )
      }

      overlays <- "Grid SVF"
      if (!is.null(building_svf_leaf) && nrow(building_svf_leaf) > 0) overlays <- c(overlays, "Building-adjacent SVF")
      leaf <- leaf |>
        leaflet::addLayersControl(
          baseGroups = c("Positron", "Dark", "Imagery"),
          overlayGroups = overlays,
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
      htmlwidgets::saveWidget(leaf, file.path(out_dir, paste0(output_prefix, "_svf_leaflet.html")), selfcontained = FALSE)
    }
    message(sprintf("[svf] Step 7c complete (%.1fs). Interactive Leaflet map written.", proc.time()[[3]] - t0))

    # ---- 7d. 3D WebGL Explorer (last, most expensive serialization, optional) ----
    if (include_3d) {
      tryCatch({
        result_for_3d <- list(building_svf = building_svf, street_summary = street_summary)
        if (!is.null(building_svf) && nrow(building_svf) > 0) {
          uh_svf_plot_3d(result_for_3d, file.path(out_dir, paste0(output_prefix, "_svf_3d_explorer.html")), street_width = street_width, palette = palette)
        }
      }, error = function(e) message(sprintf("[svf] Warning: 3D explorer skipped: %s", e$message)))
    }

    message(sprintf("[svf] Step 7/7 complete (%.1fs). All outputs written to: %s", proc.time()[[3]] - t0, out_dir))
  }

  list(
    street_points = street_points,
    street_summary = street_summary,
    grid_points = grid_points,
    building_svf = building_svf,
    buildings = buildings,
    analytics = analytics,
    method_metadata = method_meta
  )
}

#' Plot a Radial Horizon-Line Skyline Projection (Hemispherical Sky View Profile)
#'
#' @description
#' Generates a beautiful polar-coordinate representation of building and terrain obstructions
#' around a specific point, simulating a wide-angle hemispherical/fisheye sky lens.
#'
#' @param points_sf An sf object containing calculated points (returned from uh_svf with return_raw_angles=TRUE).
#' @param point_id The unique ID of the point to visualize.
#' @param title Optional plot title.
#'
#' @return A ggplot2 polar object.
#'
#' @examples
#' \dontrun{
#' svf <- uh_svf(
#'   city_name = "Basel, Switzerland",
#'   analysis_scale = "city_screening",
#'   terrain_source = "elevatr",
#'   buildings_source = "gba",
#'   sample_mode = "street",
#'   return_raw_angles = TRUE,
#'   include_static = FALSE,
#'   include_leaflet = FALSE
#' )
#' p <- uh_svf_plot_skyline(svf$street_points, svf$street_points$point_id[[1]])
#' print(p)
#' }
#' @export
uh_svf_plot_skyline <- function(points_sf, point_id, title = NULL) {
  if (!"horizon_angles" %in% names(points_sf)) {
    stop("Skyline plotting requires points calculated with return_raw_angles=TRUE.", call. = FALSE)
  }
  
  pt <- points_sf[points_sf$point_id == point_id, ]
  if (nrow(pt) == 0) {
    stop(sprintf("Point with ID %s not found.", point_id), call. = FALSE)
  }
  
  angles_rad <- pt$horizon_angles[[1]]
  n_dirs <- length(angles_rad)
  azimuth_deg <- seq(0, 360 - 360 / n_dirs, length.out = n_dirs)
  elevation_deg <- angles_rad * 180 / pi
  
  df <- data.frame(
    azimuth = azimuth_deg,
    elevation = elevation_deg,
    sky_mask = 90 - elevation_deg
  )
  
  # Ensure clean wrapping at 360
  df <- rbind(df, data.frame(azimuth = 360, elevation = df$elevation[1], sky_mask = df$sky_mask[1]))
  
  svf_val <- pt$svf[[1]]
  
  ggplot2::ggplot(df, ggplot2::aes(x = .data$azimuth)) +
    # Draw obstruction filling from the top of the building to the horizon (outer edge)
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$sky_mask, ymax = 90), fill = "#2c3e50", alpha = 0.90, color = "#ec8b3b", linewidth = 0.8) +
    ggplot2::scale_x_continuous(breaks = c(0, 90, 180, 270), labels = c("N (0 deg)", "E (90 deg)", "S (180 deg)", "W (270 deg)")) +
    ggplot2::scale_y_continuous(limits = c(0, 90), breaks = c(0, 30, 60, 90), labels = c("90 deg (Zenith)", "60 deg", "30 deg", "0 deg (Horizon)")) +
    ggplot2::coord_polar(start = 0) +
    ggplot2::labs(
      title = title %||% sprintf("Simulated Sky Obstruction Profile (Point ID: %s)", point_id),
      subtitle = sprintf("Calculated Sky-View Factor: %.3f (Sky Openness: %.1f%%)", svf_val, svf_val * 100),
      caption = "Dark fill represents solid urban/terrain obstructions. Center represents zenith (open sky)."
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "#e2e6eb", linetype = "dashed"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 14, color = "#1e242b", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 10, color = "#4d5966", hjust = 0.5),
      plot.caption = ggplot2::element_text(size = 8, color = "#6f7882", hjust = 0.5),
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(color = "#6f7882", size = 8)
    )
}

#' Generate an Urban Microclimate Exposure Assessment Panel
#'
#' @description
#' Produces a multi-panel composite infographic combining: (1) an empirical cumulative
#' distribution function (ECDF) with annotated heat-stress threshold zones, (2) a histogram
#' with urban climate classification bands showing what fraction of streets fall in each
#' exposure tier, and (3) key summary statistics. Far more actionable for urban planners than
#' a simple density curve.
#'
#' @param street_summary An sf object containing calculated street segments.
#' @param title Optional plot title.
#'
#' @return A patchwork-assembled ggplot2 composite.
#'
#' @examples
#' \dontrun{
#' svf <- uh_svf(
#'   city_name = "Basel, Switzerland",
#'   analysis_scale = "city_screening",
#'   terrain_source = "elevatr",
#'   buildings_source = "gba",
#'   sample_mode = "street",
#'   include_static = FALSE,
#'   include_leaflet = FALSE
#' )
#' p <- uh_svf_plot_distribution(svf$street_summary)
#' print(p)
#' }
#' @export
uh_svf_plot_distribution <- function(street_summary, title = NULL) {
  vals <- street_summary$svf_mean[is.finite(street_summary$svf_mean)]
  n <- length(vals)
  mean_val <- mean(vals)
  med_val <- stats::median(vals)
  p10 <- stats::quantile(vals, 0.10)
  p90 <- stats::quantile(vals, 0.90)

  # Define urban planning exposure tiers
  tier_breaks <- c(0, 0.25, 0.50, 0.75, 1.0)
  tier_labels <- c("Deep Canyon\n(SVF < 0.25)", "Moderate Shade\n(0.25-0.50)", "Partial Exposure\n(0.50-0.75)", "Fully Exposed\n(SVF > 0.75)")
  tier_colors <- c("#433d84", "#2e78a6", "#84c7d3", "#f4d06f")
  tier_pcts <- vapply(seq_along(tier_labels), function(i) mean(vals >= tier_breaks[i] & vals < tier_breaks[i + 1]), numeric(1)) * 100
  tier_pcts[length(tier_pcts)] <- mean(vals >= tier_breaks[length(tier_breaks) - 1]) * 100

  tier_df <- data.frame(
    tier = factor(tier_labels, levels = tier_labels),
    pct = tier_pcts,
    fill = tier_colors,
    stringsAsFactors = FALSE
  )

  # Panel 1: ECDF with threshold annotations
  ecdf_fn <- stats::ecdf(vals)
  x_seq <- seq(0, 1, length.out = 500)
  ecdf_df <- data.frame(svf = x_seq, cdf = ecdf_fn(x_seq))

  p1 <- ggplot2::ggplot(ecdf_df, ggplot2::aes(x = .data$svf, y = .data$cdf)) +
    ggplot2::annotate("rect", xmin = 0, xmax = 0.25, ymin = 0, ymax = 1, fill = "#433d84", alpha = 0.12) +
    ggplot2::annotate("rect", xmin = 0.25, xmax = 0.50, ymin = 0, ymax = 1, fill = "#2e78a6", alpha = 0.08) +
    ggplot2::annotate("rect", xmin = 0.50, xmax = 0.75, ymin = 0, ymax = 1, fill = "#84c7d3", alpha = 0.06) +
    ggplot2::annotate("rect", xmin = 0.75, xmax = 1.00, ymin = 0, ymax = 1, fill = "#f4d06f", alpha = 0.06) +
    ggplot2::geom_line(linewidth = 1.2, color = "#2e78a6") +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dotted", color = "#6f7882", linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = med_val, linetype = "dashed", color = "#ec8b3b", linewidth = 0.8) +
    ggplot2::annotate("text", x = med_val, y = 0.02, label = sprintf("Median: %.2f", med_val), color = "#ec8b3b", fontface = "bold", hjust = if (med_val > 0.5) 1.1 else -0.1, size = 3.2) +
    ggplot2::annotate("text", x = 0.125, y = 0.96, label = "Deep\nCanyon", color = "#433d84", fontface = "bold", size = 2.8, alpha = 0.7) +
    ggplot2::annotate("text", x = 0.875, y = 0.96, label = "Fully\nExposed", color = "#b8860b", fontface = "bold", size = 2.8, alpha = 0.7) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, 0.25)) +
    ggplot2::labs(x = "Street SVF", y = "Cumulative % of Streets", subtitle = "Cumulative Exposure Curve") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), plot.subtitle = ggplot2::element_text(face = "bold", size = 12, color = "#1e242b"))

  # Panel 2: Exposure tier bar chart
  p2 <- ggplot2::ggplot(tier_df, ggplot2::aes(x = .data$tier, y = .data$pct, fill = .data$tier)) +
    ggplot2::geom_col(width = 0.7, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", .data$pct)), vjust = -0.5, fontface = "bold", size = 3.5, color = "#1e242b") +
    ggplot2::scale_fill_manual(values = stats::setNames(tier_colors, tier_labels)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(x = NULL, y = "% of Streets", subtitle = "Street Network Exposure Classification") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_text(face = "bold", size = 12, color = "#1e242b"),
      axis.text.x = ggplot2::element_text(size = 8)
    )

  # Panel 3: Summary statistics table as a grob
  stats_text <- sprintf("N = %s segments | Mean = %.2f | Median = %.2f | P10 = %.2f | P90 = %.2f | IQR = %.2f", format(n, big.mark = ","), mean_val, med_val, p10, p90, stats::IQR(vals))
  p_caption <- grid::textGrob(stats_text, gp = grid::gpar(fontsize = 9, fontfamily = "sans", col = "#4d5966"))

  composite <- patchwork::wrap_plots(p1, p2, ncol = 2) +
    patchwork::plot_annotation(
      title = title %||% "Urban Microclimate Exposure Assessment",
      subtitle = "Actionable street-level shading diagnostics for heat mitigation planning",
      caption = stats_text,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 16, color = "#1e242b"),
        plot.subtitle = ggplot2::element_text(size = 11, color = "#4d5966"),
        plot.caption = ggplot2::element_text(size = 9, color = "#4d5966", hjust = 0.5)
      )
    )
  composite
}

uh_svf_plot_3d <- function(svf_results, output_html, street_width = 4, palette = "urban") {
  if (is.null(svf_results$building_svf) || nrow(svf_results$building_svf) == 0) {
    stop("3D visualization requires calculated building SVF results.", call. = FALSE)
  }

  tmp_b <- tempfile(fileext = ".geojson")
  tmp_s <- tempfile(fileext = ".geojson")
  on.exit(unlink(c(tmp_b, tmp_s)), add = TRUE)

  sf::st_write(sf::st_transform(svf_results$building_svf, 4326), tmp_b, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE)
  b_geojson <- paste(readLines(tmp_b, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  s_geojson <- '{"type": "FeatureCollection", "features": []}'
  if (!is.null(svf_results$street_summary) && nrow(svf_results$street_summary) > 0) {
    sf::st_write(sf::st_transform(svf_results$street_summary, 4326), tmp_s, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE)
    s_geojson <- paste(readLines(tmp_s, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }

  bb <- sf::st_bbox(sf::st_transform(svf_results$building_svf, 4326))
  clon <- as.character(as.numeric((bb[["xmin"]] + bb[["xmax"]]) / 2))
  clat <- as.character(as.numeric((bb[["ymin"]] + bb[["ymax"]]) / 2))

  # Define palette lists for R formatting and JS injection
  palettes <- list(
    urban = c(0, "#1f0f3b", 0.25, "#433d84", 0.5, "#2e78a6", 0.7, "#84c7d3", 0.85, "#dff0ef", 0.95, "#f4d06f", 1, "#ec8b3b"),
    spectral = c(0, "#d53f4f", 0.2, "#f46d43", 0.4, "#fdae61", 0.6, "#fee08b", 0.8, "#e6f598", 0.9, "#abdda4", 1.0, "#3288bd"),
    magma = c(0, "#000004", 0.2, "#3b0f70", 0.4, "#8c2981", 0.6, "#de4968", 0.8, "#fe9f6d", 1.0, "#fcfdbf"),
    viridis = c(0, "#440154", 0.2, "#414487", 0.4, "#2a788e", 0.6, "#22a884", 0.8, "#7ad151", 1.0, "#fde725"),
    coolwarm = c(0, "#3b4cc0", 0.25, "#86a5d9", 0.5, "#dddddd", 0.75, "#f2a685", 1.0, "#b40426"),
    plasma = c(0, "#0d0887", 0.2, "#46039f", 0.4, "#7201a8", 0.6, "#9c179e", 0.8, "#bd3786", 0.9, "#d8576b", 1.0, "#ed7953")
  )

  sel_pal_name <- if (palette %in% names(palettes)) palette else "urban"
  sel_pal_vec <- palettes[[sel_pal_name]]

  # Dynamic CSS color-bar background gradient string based on selected palette
  gradients <- list(
    urban = "linear-gradient(to right,#1f0f3b,#433d84,#2e78a6,#84c7d3,#dff0ef,#f4d06f,#ec8b3b)",
    spectral = "linear-gradient(to right,#d53f4f,#f46d43,#fdae61,#fee08b,#e6f598,#abdda4,#3288bd)",
    magma = "linear-gradient(to right,#000004,#3b0f70,#8c2981,#de4968,#fe9f6d,#fcfdbf)",
    viridis = "linear-gradient(to right,#440154,#414487,#2a788e,#22a884,#7ad151,#fde725)",
    coolwarm = "linear-gradient(to right,#3b4cc0,#86a5d9,#dddddd,#f2a685,#b40426)",
    plasma = "linear-gradient(to right,#0d0887,#46039f,#7201a8,#9c179e,#bd3786,#d8576b,#ed7953)"
  )
  sel_gradient <- gradients[[sel_pal_name]]

  # Helper function to compile MapLibre expression string
  .pal_to_js <- function(vec, prop_name) {
    elts <- c('"interpolate"', '["linear"]', sprintf('["get", "%s"]', prop_name))
    for (i in seq(1, length(vec), by = 2)) {
      elts <- c(elts, as.character(vec[i]), sprintf('"%s"', vec[i+1]))
    }
    paste0("[", paste(elts, collapse = ","), "]")
  }

  building_paint_expr <- .pal_to_js(sel_pal_vec, "svf")
  street_paint_expr <- .pal_to_js(sel_pal_vec, "svf_mean")

  # Generate select input option elements dynamically with R matching selected name
  options_html <- paste(vapply(c("urban", "spectral", "magma", "viridis", "coolwarm", "plasma"), function(nm) {
    sel <- if (nm == sel_pal_name) " selected" else ""
    lbl <- switch(nm,
      urban = "Urban (Default)",
      spectral = "Spectral",
      magma = "Magma",
      viridis = "Viridis",
      coolwarm = "Coolwarm",
      plasma = "Plasma"
    )
    sprintf('<option value="%s"%s>%s</option>', nm, sel, lbl)
  }, character(1)), collapse = "\n")

  # Build HTML in 3 pieces: pre-data, post-data-pre-coords, post-coords
  part1 <- paste0('<!DOCTYPE html>
<html><head><meta charset="utf-8"/>
<title>3D SVF Explorer</title>
<meta name="viewport" content="initial-scale=1,maximum-scale=1,user-scalable=no"/>
<script src="https://unpkg.com/maplibre-gl@3.6.2/dist/maplibre-gl.js"></script>
<link href="https://unpkg.com/maplibre-gl@3.6.2/dist/maplibre-gl.css" rel="stylesheet"/>
<link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;800&display=swap" rel="stylesheet">
<style>
body{margin:0;padding:0;font-family:"Inter",sans-serif;overflow:hidden}
#map{position:absolute;top:0;bottom:0;width:100%}
.panel{position:absolute;top:16px;left:16px;width:310px;backdrop-filter:blur(14px);border-radius:12px;padding:18px;z-index:2;transition:all .3s}
.dk{background:rgba(15,23,42,.92);border:1px solid rgba(255,255,255,.1);box-shadow:0 8px 28px rgba(0,0,0,.5);color:#f1f5f9}
.lt{background:rgba(255,255,255,.92);border:1px solid rgba(0,0,0,.08);box-shadow:0 8px 28px rgba(0,0,0,.1);color:#1e293b}
h1{margin:0 0 6px;font-size:18px;font-weight:800;letter-spacing:-.4px}
.sub{font-size:11px;opacity:.65;margin-bottom:14px;line-height:1.4}
.mc{border-radius:8px;padding:10px 12px;margin-bottom:12px;border-left:4px solid #3b82f6}
.dk .mc{background:rgba(255,255,255,.04)}.lt .mc{background:rgba(0,0,0,.03)}
.mt{font-size:9px;text-transform:uppercase;opacity:.55;letter-spacing:.5px}
.mv{font-size:20px;font-weight:600;color:#3b82f6;margin-top:3px}
.lb{height:10px;border-radius:5px;margin:8px 0 4px}
.ll{display:flex;justify-content:space-between;font-size:9px;opacity:.55}
.bt{border:none;border-radius:6px;padding:7px 10px;font-weight:600;font-size:11px;cursor:pointer;width:100%;margin-bottom:5px;background:#3b82f6;color:#fff;transition:background .2s}
.bt:hover{background:#2563eb}
.maplibregl-popup-content{border-radius:8px;padding:10px;box-shadow:0 4px 16px rgba(0,0,0,.15);font-family:"Inter",sans-serif;font-size:12px}

/* Advanced premium control panel styles */
.ctrl-row {
  margin-top: 14px;
  border-top: 1px solid rgba(255,255,255,0.08);
  padding-top: 12px;
}
.lt .ctrl-row {
  border-top: 1px solid rgba(0,0,0,0.06);
}
.ctrl-label {
  display: block;
  font-size: 10px;
  font-weight: 800;
  text-transform: uppercase;
  letter-spacing: .5px;
  margin-bottom: 6px;
  opacity: 0.7;
}
.flex-row {
  display: flex;
  justify-content: space-between;
  align-items: center;
  font-size: 11px;
  margin-bottom: 8px;
}
.checkbox-container {
  display: flex;
  align-items: center;
  cursor: pointer;
}
.checkbox-container input {
  margin-right: 8px;
  cursor: pointer;
}
.select-input {
  width: 100%;
  padding: 6px 8px;
  border-radius: 6px;
  background: rgba(15,23,42,0.8);
  border: 1px solid rgba(255,255,255,0.15);
  color: #f1f5f9;
  font-size: 11px;
  font-weight: 600;
  cursor: pointer;
  outline: none;
  font-family: inherit;
}
.lt .select-input {
  background: rgba(255,255,255,0.9);
  border: 1px solid rgba(0,0,0,0.15);
  color: #1e293b;
}
.slider-input {
  width: 100%;
  cursor: pointer;
  accent-color: #3b82f6;
}
</style></head>
<body style="background:#0f172a">
<div id="map"></div>
<div id="pnl" class="panel dk">
<h1>3D Street-Canyon Explorer</h1>
<div class="sub">Fly through urban canyons to identify shading hot-spots. Hover features for details.</div>
<div class="mc"><div class="mt">Building-Adjacent SVF</div><div class="mv">Active</div></div>

<!-- Layer Visibility Toggles -->
<div class="ctrl-row">
  <span class="ctrl-label">Layer Visibility</span>
  <label class="checkbox-container flex-row" style="margin-bottom: 6px;">
    <span>3D Buildings Layer</span>
    <input type="checkbox" id="chk-buildings" checked onchange="toggleLayer(\'buildings-3d\', this.checked)"/>
  </label>
  <label class="checkbox-container flex-row">
    <span>Street Canyon Lines</span>
    <input type="checkbox" id="chk-streets" checked onchange="toggleLayer(\'streets-layer\', this.checked)"/>
  </label>
</div>

<!-- Color Palette Selection -->
<div class="ctrl-row">
  <span class="ctrl-label">Visual Theme (Palette)</span>
  <select id="pal-select" class="select-input" onchange="updatePalette(this.value)">
    ', options_html, '
  </select>
</div>

<!-- SVF Scale Legend -->
<div class="ctrl-row">
  <div style="font-size:10px;font-weight:600;margin-bottom:3px">SVF Scale</div>
  <div id="color-bar" class="lb" style="background: ', sel_gradient, '"></div>
  <div class="ll"><span>0.0 Shaded</span><span>0.5</span><span>1.0 Exposed</span></div>
</div>

<!-- Interactive Street Width -->
<div class="ctrl-row">
  <div class="flex-row">
    <span class="ctrl-label" style="margin:0;">Street Canyon Width</span>
    <span id="width-val" style="font-size:10px; font-weight:800; color:#3b82f6;">', street_width, 'px</span>
  </div>
  <input type="range" id="width-slider" class="slider-input" min="1" max="12" step="0.5" value="', street_width, '" oninput="updateLineWidth(this.value)"/>
</div>

<div style="margin-top:14px">
<button class="bt" onclick="toggleTheme()">Toggle Light / Dark</button>
<button class="bt" onclick="toggleRotation()">Toggle Fly-Through</button>
</div></div>
<script>
var bGJ=')

  part2 <- '
var sGJ='

  part3 <- paste0('
var dkT=["https://a.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png","https://b.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png"];
var ltT=["https://a.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png","https://b.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png"];
var isDark=true;
function mkS(t){return{"version":8,"sources":{"rt":{"type":"raster","tiles":t,"tileSize":256}},"layers":[{"id":"base","type":"raster","source":"rt"}]}}
var map=new maplibregl.Map({container:"map",style:mkS(dkT),center:[', clon, ',', clat, '],zoom:15,pitch:58,bearing:-20,antialias:true});
map.addControl(new maplibregl.NavigationControl());

// Interactive palettes and configurations
var palettes = {
  urban: ["interpolate",["linear"],["get","X"],0,"#1f0f3b",0.25,"#433d84",0.5,"#2e78a6",0.7,"#84c7d3",0.85,"#dff0ef",0.95,"#f4d06f",1,"#ec8b3b"],
  spectral: ["interpolate",["linear"],["get","X"],0,"#d53f4f",0.2,"#f46d43",0.4,"#fdae61",0.6,"#fee08b",0.8,"#e6f598",0.9,"#abdda4",1.0,"#3288bd"],
  magma: ["interpolate",["linear"],["get","X"],0,"#000004",0.2,"#3b0f70",0.4,"#8c2981",0.6,"#de4968",0.8,"#fe9f6d",1.0,"#fcfdbf"],
  viridis: ["interpolate",["linear"],["get","X"],0,"#440154",0.2,"#414487",0.4,"#2a788e",0.6,"#22a884",0.8,"#7ad151",1.0,"#fde725"],
  coolwarm: ["interpolate",["linear"],["get","X"],0,"#3b4cc0",0.25,"#86a5d9",0.5,"#dddddd",0.75,"#f2a685",1.0,"#b40426"],
  plasma: ["interpolate",["linear"],["get","X"],0,"#0d0887",0.2,"#46039f",0.4,"#7201a8",0.6,"#9c179e",0.8,"#bd3786",0.9,"#d8576b",1.0,"#ed7953"]
};

function getPaletteExpression(name, prop) {
  var expr = JSON.parse(JSON.stringify(palettes[name] || palettes.urban));
  expr[2][1] = prop;
  return expr;
}

function updatePalette(name) {
  if (map.getLayer("buildings-3d")) {
    map.setPaintProperty("buildings-3d", "fill-extrusion-color", getPaletteExpression(name, "svf"));
  }
  if (map.getLayer("streets-layer")) {
    map.setPaintProperty("streets-layer", "line-color", getPaletteExpression(name, "svf_mean"));
  }
  var grads = {
    urban: "linear-gradient(to right,#1f0f3b,#433d84,#2e78a6,#84c7d3,#dff0ef,#f4d06f,#ec8b3b)",
    spectral: "linear-gradient(to right,#d53f4f,#f46d43,#fdae61,#fee08b,#e6f598,#abdda4,#3288bd)",
    magma: "linear-gradient(to right,#000004,#3b0f70,#8c2981,#de4968,#fe9f6d,#fcfdbf)",
    viridis: "linear-gradient(to right,#440154,#414487,#2a788e,#22a884,#7ad151,#fde725)",
    coolwarm: "linear-gradient(to right,#3b4cc0,#86a5d9,#dddddd,#f2a685,#b40426)",
    plasma: "linear-gradient(to right,#0d0887,#46039f,#7201a8,#9c179e,#bd3786,#d8576b,#ed7953)"
  };
  document.getElementById("color-bar").style.background = grads[name] || grads.urban;
}

function updateLineWidth(val) {
  document.getElementById("width-val").innerText = val + "px";
  if (map.getLayer("streets-layer")) {
    map.setPaintProperty("streets-layer", "line-width", parseFloat(val));
  }
}

function toggleLayer(id, visible) {
  var val = visible ? "visible" : "none";
  if (map.getLayer(id)) {
    map.setLayoutProperty(id, "visibility", val);
  }
  if (id === "streets-layer" && map.getLayer("streets-hit")) {
    map.setLayoutProperty("streets-hit", "visibility", val);
  }
}

function addLayers(){
  if(map.getSource("buildings")){try{map.removeLayer("buildings-3d")}catch(e){};try{map.removeSource("buildings")}catch(e){}}
  if(map.getSource("streets")){try{map.removeLayer("streets-hit")}catch(e){};try{map.removeLayer("streets-layer")}catch(e){};try{map.removeSource("streets")}catch(e){}}
  
  map.addSource("buildings",{type:"geojson",data:bGJ});
  map.addLayer({
    "id":"buildings-3d",
    "type":"fill-extrusion",
    "source":"buildings",
    "paint":{
      "fill-extrusion-color":', building_paint_expr, ',
      "fill-extrusion-height":["get","height_m"],
      "fill-extrusion-base":0,
      "fill-extrusion-opacity":0.88
    }
  });
  
  map.addSource("streets",{type:"geojson",data:sGJ});
  map.addLayer({
    "id":"streets-layer",
    "type":"line",
    "source":"streets",
    "layout":{"line-join":"round","line-cap":"round"},
    "paint":{
      "line-color":', street_paint_expr, ',
      "line-width":', street_width, ',
      "line-opacity":0.92
    }
  });
  
  map.addLayer({
    "id":"streets-hit",
    "type":"line",
    "source":"streets",
    "layout":{"line-join":"round","line-cap":"round"},
    "paint":{
      "line-color":"#000000",
      "line-width":14,
      "line-opacity":0.001
    }
  });
}

map.on("load",function(){
  addLayers();
  var popup=new maplibregl.Popup({closeButton:false,closeOnClick:false});
  
  map.on("mousemove","buildings-3d",function(e){
    map.getCanvas().style.cursor="pointer";
    var p=e.features[0].properties;
    var sv=parseFloat(p.svf||0).toFixed(3);
    var ht=parseFloat(p.height_m||0).toFixed(1);
    popup.setLngLat(e.lngLat).setHTML("<div style=\\"color:#1e293b\\"><b>SVF:</b> "+sv+"<br><b>Height:</b> "+ht+" m</div>").addTo(map);
  });
  map.on("mouseleave","buildings-3d",function(){map.getCanvas().style.cursor="";popup.remove()});
  
  map.on("mousemove","streets-hit",function(e){
    map.getCanvas().style.cursor="pointer";
    var p=e.features[0].properties;
    var sv=parseFloat(p.svf_mean||0).toFixed(3);
    var sid=(p.street_id===undefined||p.street_id===null)?"n/a":p.street_id;
    var npts=(p.point_n===undefined||p.point_n===null)?((p.n_points===undefined||p.n_points===null)?"n/a":p.n_points):p.point_n;
    popup.setLngLat(e.lngLat).setHTML("<div style=\\"color:#1e293b\\"><b>Street mean SVF:</b> "+sv+"<br><b>Street ID:</b> "+sid+"<br><b>Sample points:</b> "+npts+"</div>").addTo(map);
  });
  map.on("mouseleave","streets-hit",function(){map.getCanvas().style.cursor="";popup.remove()});
});

var rot=false;
function rotateCamera(){if(!rot)return;map.rotateTo((map.getBearing()+0.15)%360,{duration:0});requestAnimationFrame(rotateCamera)}
function toggleRotation(){rot=!rot;if(rot)rotateCamera()}
function toggleTheme(){
  isDark=!isDark;
  document.body.style.background=isDark?"#0f172a":"#f8fafc";
  document.getElementById("pnl").className="panel "+(isDark?"dk":"lt");
  if(map.getSource("rt")){
    map.getSource("rt").setTiles(isDark?dkT:ltT);
  }
}
</script></body></html>')

  html_content <- paste0(part1, b_geojson, part2, s_geojson, part3)
  writeLines(html_content, output_html, useBytes = TRUE)
  invisible(NULL)
}


# =========================================================================
# INTERNAL HELPER FUNCTIONS
# =========================================================================

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

.uh_svf_require <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(sprintf("Missing required packages: %s", paste(missing, collapse = ", ")), call. = FALSE)
  }
}

.uh_svf_slug <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

.uh_svf_safe_stat <- function(x, fun, default = NA_real_, ...) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(default)
  fun(x, ...)
}

.uh_svf_gini <- function(x) {
  x <- x[is.finite(x) & x >= 0]
  if (length(x) == 0) return(NA_real_)
  if (all(x == 0)) return(0)
  x <- sort(x)
  n <- length(x)
  sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
}

.uh_svf_utm_crs <- function(x) {
  centroid <- sf::st_centroid(sf::st_union(sf::st_geometry(sf::st_transform(x, 4326))))
  coords <- sf::st_coordinates(centroid)
  zone <- floor((coords[1, "X"] + 180) / 6) + 1
  epsg <- if (coords[1, "Y"] >= 0) 32600 + zone else 32700 + zone
  paste0("EPSG:", epsg)
}

.uh_svf_fetch_boundary <- function(city_name, simplify_m = 0, cache_dir = file.path("data_cache", "boundary")) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- file.path(cache_dir, sprintf("%s_simplify_%s.rds", .uh_svf_slug(city_name), gsub("\\.", "_", as.character(simplify_m))))
  if (file.exists(cache_file)) return(readRDS(cache_file))

  resp <- httr::GET(
    "https://nominatim.openstreetmap.org/search",
    query = list(q = city_name, format = "json", polygon_geojson = 1, limit = 5, addressdetails = 1),
    httr::add_headers(`User-Agent` = "greenR SVF strict workflow"),
    httr::timeout(60)
  )
  if (httr::http_error(resp)) stop(sprintf("Boundary request failed for %s.", city_name), call. = FALSE)
  parsed <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  polygon_items <- Filter(function(x) !is.null(x$geojson) && x$geojson$type %in% c("Polygon", "MultiPolygon"), parsed)
  if (length(polygon_items) == 0) stop(sprintf("No polygon boundary found for query: %s", city_name), call. = FALSE)
  item <- polygon_items[[1]]

  make_ring <- function(ring) {
    mat <- do.call(rbind, lapply(ring, function(x) c(as.numeric(x[[1]]), as.numeric(x[[2]]))))
    if (!isTRUE(all.equal(mat[1, ], mat[nrow(mat), ], tolerance = 1e-9, check.attributes = FALSE))) {
      mat <- rbind(mat, mat[1, ])
    }
    mat
  }

  geom <- if (identical(item$geojson$type, "Polygon")) {
    sf::st_sfc(sf::st_polygon(lapply(item$geojson$coordinates, make_ring)), crs = 4326)
  } else {
    sf::st_sfc(sf::st_multipolygon(lapply(item$geojson$coordinates, function(poly) lapply(poly, make_ring))), crs = 4326)
  }

  boundary <- sf::st_sf(
    display_name = item$display_name %||% city_name,
    boundary_source = "OpenStreetMap/Nominatim",
    geometry = sf::st_make_valid(geom)
  )

  if (simplify_m > 0) {
    target_crs <- .uh_svf_utm_crs(boundary)
    boundary <- boundary |>
      sf::st_transform(target_crs) |>
      sf::st_simplify(dTolerance = simplify_m, preserveTopology = TRUE) |>
      sf::st_transform(4326)
  }

  saveRDS(boundary, cache_file)
  boundary
}

.uh_svf_coerce_boundary <- function(city_name = NULL, boundary = NULL, simplify_m = 0) {
  if (!is.null(boundary)) {
    if (inherits(boundary, "bbox")) boundary <- sf::st_as_sfc(boundary)
    if (inherits(boundary, "sfc")) boundary <- sf::st_sf(geometry = boundary, crs = sf::st_crs(boundary))
    if (!inherits(boundary, "sf")) stop("boundary must be sf, sfc, or bbox.", call. = FALSE)
    return(boundary)
  }
  if (is.null(city_name)) stop("Provide either city_name or boundary.", call. = FALSE)
  .uh_svf_fetch_boundary(city_name, simplify_m = simplify_m)
}

.uh_svf_coerce_bbox <- function(bbox, bbox_crs = 4326) {
  if (is.null(bbox)) return(NULL)
  if (inherits(bbox, "bbox")) return(sf::st_as_sfc(bbox))
  if (inherits(bbox, "sfc")) return(bbox)
  if (inherits(bbox, "sf")) return(sf::st_as_sfc(sf::st_bbox(bbox)))
  if (is.numeric(bbox) && length(bbox) == 4) {
    return(sf::st_as_sfc(sf::st_bbox(c(xmin = bbox[[1]], ymin = bbox[[2]], xmax = bbox[[3]], ymax = bbox[[4]]), crs = sf::st_crs(bbox_crs))))
  }
  stop("bbox must be NULL, bbox, sfc/sf, or numeric c(xmin, ymin, xmax, ymax).", call. = FALSE)
}

.uh_svf_make_analysis_area <- function(boundary, bbox = NULL, bbox_crs = 4326, processing_crs = NULL, buffer_m = 0, radius_m = 300) {
  if (is.null(processing_crs)) processing_crs <- .uh_svf_utm_crs(boundary)
  boundary_proj <- sf::st_transform(boundary, processing_crs)
  if (is.null(bbox)) {
    buffer_use <- max(buffer_m, radius_m + 100)
    area <- sf::st_as_sfc(sf::st_bbox(sf::st_buffer(boundary_proj, buffer_use)))
  } else {
    area <- sf::st_transform(.uh_svf_coerce_bbox(bbox, bbox_crs = bbox_crs), processing_crs)
  }
  list(boundary = boundary, boundary_proj = boundary_proj, analysis_area = area, processing_crs = processing_crs)
}

.uh_svf_geom_mean_z <- function(g) {
  coords <- sf::st_coordinates(sf::st_sfc(g, crs = sf::st_crs(g)))
  if (!"Z" %in% colnames(coords)) return(NA_real_)
  mean(coords[, "Z"], na.rm = TRUE)
}

.uh_svf_rasterize_local_terrain_vector <- function(terrain_path, analysis_area, processing_crs, terrain_layer = NULL, terrain_resolution_m = 5, cache_dir = file.path("data_cache", "svf_strict")) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  bb <- sf::st_bbox(sf::st_transform(analysis_area, processing_crs))
  cache_key <- sprintf(
    "terrain_%s_%s_%s_%s_%s_%s.tif",
    .uh_svf_slug(basename(terrain_path)),
    round(bb[["xmin"]]), round(bb[["ymin"]]), round(bb[["xmax"]]), round(bb[["ymax"]]),
    terrain_resolution_m
  )
  cache_file <- file.path(cache_dir, cache_key)
  if (file.exists(cache_file)) return(terra::rast(cache_file))

  subset_file <- tempfile(fileext = ".gpkg")
  on.exit(unlink(subset_file), add = TRUE)
  if (is.null(terrain_layer)) terrain_layer <- sf::st_layers(terrain_path)$name[[1]]
  crs_str <- paste0("EPSG:", sf::st_crs(processing_crs)$epsg)
  args <- c(
    "-f", "GPKG",
    subset_file,
    terrain_path,
    terrain_layer,
    "-spat_srs", crs_str,
    "-spat", format(bb[["xmin"]], scientific = FALSE, trim = TRUE),
    format(bb[["ymin"]], scientific = FALSE, trim = TRUE),
    format(bb[["xmax"]], scientific = FALSE, trim = TRUE),
    format(bb[["ymax"]], scientific = FALSE, trim = TRUE),
    "-overwrite"
  )
  status <- system2("ogr2ogr", args = args, stdout = FALSE, stderr = FALSE)
  if (!identical(status, 0L) || !file.exists(subset_file)) {
    stop("Failed to subset local terrain vector with ogr2ogr.", call. = FALSE)
  }

  terrain_tin <- sf::st_read(subset_file, quiet = TRUE) |>
    sf::st_transform(processing_crs)
  terrain_tin$z_mean <- vapply(sf::st_geometry(terrain_tin), .uh_svf_geom_mean_z, numeric(1))
  terrain_tin <- suppressWarnings(sf::st_intersection(terrain_tin, sf::st_sf(geometry = analysis_area)))
  terrain_tin <- terrain_tin[!sf::st_is_empty(terrain_tin), ]
  if (nrow(terrain_tin) == 0) stop("No terrain features remain after subsetting.", call. = FALSE)
  terrain_tin <- terrain_tin[is.finite(terrain_tin$z_mean), ]
  if (nrow(terrain_tin) == 0) stop("Local terrain subset has no usable Z values.", call. = FALSE)

  template <- terra::rast(terra::ext(sf::st_bbox(analysis_area)), resolution = terrain_resolution_m, crs = sf::st_crs(processing_crs)$wkt)
  dem <- terra::rasterize(terra::vect(terrain_tin), template, field = "z_mean", fun = "mean", background = NA)
  dem <- terra::mask(dem, terra::vect(analysis_area))
  names(dem) <- "terrain_z"
  if (all(is.na(terra::values(dem)))) stop("Terrain rasterization produced only NA values.", call. = FALSE)
  terra::writeRaster(dem, cache_file, overwrite = TRUE, gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
  dem
}

.uh_svf_get_terrain <- function(analysis_area, processing_crs, terrain_source = c("local", "elevatr"), terrain_path = NULL, terrain_layer = NULL, terrain_resolution_m = 5, elevatr_z = 12) {
  terrain_source <- match.arg(terrain_source)

  if (terrain_source == "local") {
    if (is.null(terrain_path) || !file.exists(terrain_path)) {
      stop("terrain_source='local' requires a readable terrain_path.", call. = FALSE)
    }
    ext <- tolower(tools::file_ext(terrain_path))
    if (ext %in% c("tif", "tiff", "img")) {
      dem <- terra::rast(terrain_path)
      dem <- terra::project(dem, processing_crs, method = "bilinear")
      dem <- terra::crop(dem, terra::vect(analysis_area), snap = "out")
      dem <- terra::mask(dem, terra::vect(analysis_area))
    } else {
      dem <- .uh_svf_rasterize_local_terrain_vector(
        terrain_path = terrain_path,
        analysis_area = analysis_area,
        processing_crs = processing_crs,
        terrain_layer = terrain_layer,
        terrain_resolution_m = terrain_resolution_m
      )
    }
    names(dem) <- "terrain_z"
    return(list(
      raster = dem,
      source = if (ext %in% c("tif", "tiff", "img")) "local_raster" else "local_vector_terrain",
      native_resolution_m = mean(terra::res(dem)),
      quality_tier = "local_high_quality"
    ))
  }

  aoi_latlon <- sf::st_sf(geometry = sf::st_make_valid(sf::st_transform(analysis_area, 4326)))
  dem_latlon <- elevatr::get_elev_raster(aoi_latlon, z = elevatr_z, clip = "bbox")
  dem <- terra::rast(dem_latlon)
  dem <- terra::project(dem, processing_crs, method = "bilinear")
  dem <- terra::crop(dem, terra::vect(analysis_area), snap = "out")
  dem <- terra::mask(dem, terra::vect(analysis_area))
  names(dem) <- "terrain_z"
  list(
    raster = dem,
    source = sprintf("elevatr_z%s", elevatr_z),
    native_resolution_m = mean(terra::res(dem)),
    quality_tier = "global_screening"
  )
}

.uh_svf_boundary_bbox_wgs84 <- function(x) {
  bb <- sf::st_bbox(sf::st_transform(x, 4326))
  c(left = bb[["xmin"]], bottom = bb[["ymin"]], right = bb[["xmax"]], top = bb[["ymax"]])
}

.uh_svf_gba_tiles_for_bbox <- function(bbox) {
  fmt_lon <- function(x) sprintf("%s%03d", ifelse(x < 0, "w", "e"), abs(as.integer(x)))
  fmt_lat <- function(x) sprintf("%s%02d", ifelse(x < 0, "s", "n"), abs(as.integer(x)))
  lon_min <- floor(bbox[["left"]] / 5) * 5
  lon_max <- floor(bbox[["right"]] / 5) * 5
  lat_min <- floor(bbox[["bottom"]] / 5) * 5
  lat_max <- floor(bbox[["top"]] / 5) * 5
  grid <- expand.grid(lon_start = seq(lon_min, lon_max, by = 5), lat_start = seq(lat_min, lat_max, by = 5))
  paste0(
    fmt_lon(grid$lon_start), "_",
    fmt_lat(grid$lat_start + 5), "_",
    fmt_lon(grid$lon_start + 5), "_",
    fmt_lat(grid$lat_start), ".parquet"
  )
}

.uh_svf_wkb_to_sf <- function(tbl) {
  old_s2 <- sf::sf_use_s2(FALSE)
  geom <- tryCatch(
    sf::st_as_sfc(tbl$geometry, EWKB = FALSE, crs = 4326),
    error = function(e) sf::st_as_sfc(structure(tbl$geometry, class = "WKB"), EWKB = FALSE, crs = 4326)
  )
  sf::sf_use_s2(old_s2)
  tbl$geometry <- geom
  sf::st_as_sf(tbl)
}

.uh_svf_get_gba_buildings <- function(analysis_area, processing_crs, terrain_raster = NULL) {
  bbox <- .uh_svf_boundary_bbox_wgs84(sf::st_transform(analysis_area, 4326))
  tile_files <- .uh_svf_gba_tiles_for_bbox(bbox)
  chunks <- list()

  # Create an anonymous S3 connection to prevent IMDS credential probing freezes!
  bucket <- tryCatch(
    arrow::s3_bucket("us-west-2.opendata.source.coop/tge-labs/globalbuildingatlas-lod1", anonymous = TRUE, region = "us-west-2"),
    error = function(e) {
      message("[gba] Error: Unable to establish anonymous S3 connection: ", e$message)
      NULL
    }
  )
  
  if (is.null(bucket)) {
    message("[gba] AWS SDK anonymization failed. Attempting direct fallback connection...")
    Sys.setenv(AWS_NO_SIGN_REQUEST = "YES", AWS_REGION = "us-west-2")
  }

  for (tile in tile_files) {
    message(sprintf("[gba] Querying Global Building Atlas tile partition: %s", tile))
    ds <- tryCatch({
      if (!is.null(bucket)) {
        arrow::open_dataset(tile, format = "parquet", filesystem = bucket)
      } else {
        arrow::open_dataset(paste0("s3://us-west-2.opendata.source.coop/tge-labs/globalbuildingatlas-lod1/", tile), format = "parquet")
      }
    }, error = function(e) {
      message(sprintf("[gba] Warning: Failed to open dataset for tile %s: %s", tile, e$message))
      NULL
    })
    
    if (is.null(ds)) next
    
    chunk <- tryCatch(
      ds |>
        dplyr::filter(
          bbox$xmax >= !!bbox[["left"]],
          bbox$xmin <= !!bbox[["right"]],
          bbox$ymax >= !!bbox[["bottom"]],
          bbox$ymin <= !!bbox[["top"]]
        ) |>
        dplyr::select(dplyr::any_of(c("id", "source", "height", "geometry"))) |>
        dplyr::collect(),
      error = function(e) {
        message(sprintf("[gba] Warning: Query filter failed on tile %s: %s", tile, e$message))
        NULL
      }
    )
    if (!is.null(chunk) && nrow(chunk) > 0) chunks[[tile]] <- chunk
  }

  if (length(chunks) == 0) return(NULL)
  raw <- dplyr::bind_rows(chunks)
  buildings <- .uh_svf_wkb_to_sf(raw)
  buildings <- buildings |>
    sf::st_transform(processing_crs) |>
    sf::st_make_valid()
  buildings <- .uh_svf_duckdb_spatial_intersection(buildings, analysis_area, processing_crs)
  buildings <- buildings[sf::st_geometry_type(buildings) %in% c("POLYGON", "MULTIPOLYGON"), ]
  buildings <- buildings[!sf::st_is_empty(buildings), ]
  if (nrow(buildings) == 0) return(NULL)

  if (is.null(terrain_raster)) {
    height_m <- suppressWarnings(as.numeric(buildings$height))
    building_id <- if ("id" %in% names(buildings)) as.character(buildings$id) else as.character(seq_len(nrow(buildings)))
    source_val <- if ("source" %in% names(buildings)) as.character(buildings$source) else rep("global_building_atlas", nrow(buildings))
    
    return(
      buildings |>
        dplyr::transmute(
          building_id = building_id,
          source = source_val,
          ground_z = NA_real_,
          roof_z = NA_real_,
          height_m = height_m,
          geometry = geometry
        )
    )
  }

  pt <- suppressWarnings(sf::st_point_on_surface(buildings))
  ground_z <- terra::extract(terrain_raster, terra::vect(pt))[[names(terrain_raster)[1]]]
  height_m <- suppressWarnings(as.numeric(buildings$height))
  keep <- is.finite(height_m) & height_m > 0 & is.finite(ground_z)
  buildings <- buildings[keep, ]
  ground_z <- ground_z[keep]
  height_m <- height_m[keep]
  if (nrow(buildings) == 0) return(NULL)
  building_id <- if ("id" %in% names(buildings)) as.character(buildings$id) else as.character(seq_len(nrow(buildings)))
  source_val <- if ("source" %in% names(buildings)) as.character(buildings$source) else rep("global_building_atlas", nrow(buildings))

  buildings |>
    dplyr::transmute(
      building_id = building_id,
      source = source_val,
      ground_z = ground_z,
      roof_z = ground_z + height_m,
      height_m = height_m,
      geometry = geometry
    )
}

.uh_svf_standardise_local_buildings <- function(buildings, processing_crs, terrain_raster = NULL, analysis_area) {
  b <- buildings
  if (inherits(b, "bbox")) b <- sf::st_as_sfc(b)
  if (inherits(b, "sfc")) b <- sf::st_sf(geometry = b, crs = sf::st_crs(b))
  if (!inherits(b, "sf")) stop("Local buildings must be provided as sf/sfc or a readable file.", call. = FALSE)
  b <- b |>
    sf::st_transform(processing_crs) |>
    sf::st_make_valid()
  b <- .uh_svf_duckdb_spatial_intersection(b, analysis_area, processing_crs)
  b <- b[sf::st_geometry_type(b) %in% c("POLYGON", "MULTIPOLYGON"), ]
  b <- b[!sf::st_is_empty(b), ]
  if (nrow(b) == 0) stop("No local buildings remain after clipping.", call. = FALSE)

  nms <- names(b)
  if (is.null(terrain_raster) && !all(c("ground_z", "roof_z") %in% nms) && !all(c("h_boden", "h_first") %in% nms)) {
    if (!("height_m" %in% nms || "height" %in% nms)) {
      stop("Local buildings need either ground_z+roof_z, h_boden+h_first, or height/height_m.", call. = FALSE)
    }
    height_m <- suppressWarnings(as.numeric(b[[if ("height_m" %in% nms) "height_m" else "height"]]))
    
    return(
      b |>
        dplyr::transmute(
          building_id = if ("building_id" %in% names(b)) as.character(b$building_id) else as.character(seq_len(nrow(b))),
          source = "local_buildings",
          ground_z = NA_real_,
          roof_z = NA_real_,
          height_m = height_m,
          geometry = geometry
        )
    )
  }

  if (all(c("ground_z", "roof_z") %in% nms)) {
    ground_z <- as.numeric(b$ground_z)
    roof_z <- as.numeric(b$roof_z)
  } else if (all(c("h_boden", "h_first") %in% nms)) {
    ground_z <- as.numeric(b$h_boden)
    roof_z <- as.numeric(b$h_first)
  } else if ("height_m" %in% nms || "height" %in% nms) {
    height_raw <- as.numeric(b[[if ("height_m" %in% nms) "height_m" else "height"]])
    pt <- suppressWarnings(sf::st_point_on_surface(b))
    ground_z <- terra::extract(terrain_raster, terra::vect(pt))[[names(terrain_raster)[1]]]
    roof_z <- ground_z + height_raw
  } else {
    stop("Local buildings need either ground_z+roof_z, h_boden+h_first, or height/height_m.", call. = FALSE)
  }

  height_m <- roof_z - ground_z
  keep <- is.finite(ground_z) & is.finite(roof_z) & is.finite(height_m) & height_m > 0
  out <- b[keep, ]
  if (nrow(out) == 0) stop("No valid local building heights remain after filtering.", call. = FALSE)

  out |>
    dplyr::transmute(
      building_id = if ("building_id" %in% names(out)) as.character(.data$building_id) else as.character(seq_len(nrow(out))),
      source = "local_buildings",
      ground_z = ground_z[keep],
      roof_z = roof_z[keep],
      height_m = height_m[keep],
      geometry = geometry
    )
}

.uh_svf_get_buildings <- function(analysis_area, processing_crs, terrain_raster = NULL, buildings_source = c("local", "gba"), buildings_path = NULL, buildings_object = NULL) {
  buildings_source <- match.arg(buildings_source)
  if (buildings_source == "local") {
    if (!is.null(buildings_object)) {
      return(list(
        buildings = .uh_svf_standardise_local_buildings(buildings_object, processing_crs, terrain_raster, analysis_area),
        source = "local_buildings",
        quality_tier = "local_high_quality"
      ))
    }
    if (is.null(buildings_path) || !file.exists(buildings_path)) {
      stop("buildings_source='local' requires buildings_object or a readable buildings_path.", call. = FALSE)
    }
    return(list(
      buildings = .uh_svf_standardise_local_buildings(sf::st_read(buildings_path, quiet = TRUE), processing_crs, terrain_raster, analysis_area),
      source = "local_buildings",
      quality_tier = "local_high_quality"
    ))
  }

  gba <- .uh_svf_get_gba_buildings(analysis_area, processing_crs, terrain_raster)
  if (is.null(gba) || nrow(gba) == 0) {
    stop("Global Building Atlas returned no usable buildings for this analysis area.", call. = FALSE)
  }
  list(buildings = gba, source = "global_building_atlas", quality_tier = "global_screening")
}

.uh_svf_extract_building_elevation <- function(buildings, terrain_raster) {
  if (is.null(buildings) || nrow(buildings) == 0) return(NULL)
  
  pt <- suppressWarnings(sf::st_point_on_surface(buildings))
  ground_z <- as.numeric(terra::extract(terrain_raster, terra::vect(pt))[[names(terrain_raster)[1]]])
  height_m <- suppressWarnings(as.numeric(buildings$height_m))
  
  keep <- is.finite(height_m) & height_m > 0 & is.finite(ground_z)
  buildings <- buildings[keep, ]
  ground_z <- ground_z[keep]
  height_m <- height_m[keep]
  if (nrow(buildings) == 0) return(NULL)
  
  buildings$ground_z <- ground_z
  buildings$roof_z <- ground_z + height_m
  buildings$height_m <- height_m
  
  buildings
}

.uh_svf_duckdb_spatial_intersection <- function(sf_obj, analysis_area, crs_code) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) return(NULL)
  
  con <- tryCatch({
    DBI::dbConnect(duckdb::duckdb())
  }, error = function(e) NULL)
  
  if (is.null(con)) {
    return(suppressWarnings(sf::st_intersection(sf_obj, sf::st_sf(geometry = analysis_area))))
  }
  
  on.exit({
    try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
  })
  
  ext_ok <- tryCatch({
    DBI::dbExecute(con, "LOAD spatial;")
    TRUE
  }, error = function(e) {
    tryCatch({
      DBI::dbExecute(con, "INSTALL spatial;")
      DBI::dbExecute(con, "LOAD spatial;")
      TRUE
    }, error = function(e2) FALSE)
  })
  
  if (!ext_ok) {
    return(suppressWarnings(sf::st_intersection(sf_obj, sf::st_sf(geometry = analysis_area))))
  }
  
  df <- sf::st_drop_geometry(sf_obj)
  df$geom_wkt <- as.character(sf::st_as_text(sf::st_geometry(sf_obj)))
  df$row_id <- seq_len(nrow(df))
  
  area_wkt <- as.character(sf::st_as_text(analysis_area))
  
  write_ok <- tryCatch({
    DBI::dbWriteTable(con, "features", df, overwrite = TRUE)
    DBI::dbExecute(con, "ALTER TABLE features ADD COLUMN geom GEOMETRY;")
    DBI::dbExecute(con, "UPDATE features SET geom = ST_GeomFromText(geom_wkt);")
    TRUE
  }, error = function(e) FALSE)
  
  if (!write_ok) {
    return(suppressWarnings(sf::st_intersection(sf_obj, sf::st_sf(geometry = analysis_area))))
  }
  
  cols <- names(df)
  cols <- cols[cols != "geom_wkt" & cols != "row_id"]
  cols_select <- paste(cols, collapse = ", ")
  
  query <- sprintf("
    SELECT %s, 
           ST_AsText(ST_Intersection(ST_MakeValid(geom), ST_MakeValid(ST_GeomFromText('%s')))) as intersected_wkt
    FROM features
    WHERE ST_Intersects(ST_MakeValid(geom), ST_MakeValid(ST_GeomFromText('%s')))
  ", cols_select, area_wkt, area_wkt)
  
  res <- tryCatch({
    DBI::dbGetQuery(con, query)
  }, error = function(e) {
    message("[duckdb] Spatial query failed. Falling back to st_intersection: ", e$message)
    NULL
  })
  
  if (is.null(res)) {
    return(suppressWarnings(sf::st_intersection(sf_obj, sf::st_sf(geometry = analysis_area))))
  }
  
  if (nrow(res) == 0) {
    return(sf_obj[0, ])
  }
  
  res_sf <- tryCatch({
    sf::st_as_sf(res, wkt = "intersected_wkt", crs = crs_code)
  }, error = function(e) {
    suppressWarnings(sf::st_intersection(sf_obj, sf::st_sf(geometry = analysis_area)))
  })
  
  if ("intersected_wkt" %in% names(res_sf)) {
    sf::st_geometry(res_sf) <- "geometry"
  }
  
  res_sf
}

.uh_svf_get_canopy <- function(analysis_area, processing_crs, canopy_path = NULL, canopy_object = NULL, target_res = NULL) {
  if (!is.null(canopy_object)) {
    canopy <- canopy_object
    source_name <- "canopy_height_object"
  } else if (!is.null(canopy_path)) {
    canopy <- terra::rast(canopy_path)
    source_name <- "canopy_height_raster"
  } else {
    # Dynamically download Meta CHMv2 tile if no local file is supplied!
    message("[canopy] No local canopy height model supplied. Querying Meta CHMv2 dynamically...")
    cache_dir <- file.path("data_cache", "meta_chm")
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Calculate bounding box for search
    boundary_wgs <- sf::st_transform(analysis_area, 4326)
    bb <- sf::st_bbox(boundary_wgs)
    safe_bbox <- gsub("[^A-Za-z0-9_\\-]", "_", paste(format(round(unname(bb), 5), nsmall = 5), collapse = "_"))
    
    # Check cache first
    cached_files <- list.files(cache_dir, pattern = "\\.tif$", full.names = TRUE)
    cached_match <- cached_files[grepl(safe_bbox, basename(cached_files))]
    if (length(cached_match) > 0) {
      message(sprintf("[canopy] Using cached Meta CHMv2: %s", cached_match[1]))
      canopy <- terra::rast(cached_match[1])
      source_name <- "meta_chm_v2_cached"
    } else {
      # Fetch from Meta CHMv2 index via temporary local download (extremely fast and robust!)
      base_url <- "https://dataforgood-fb-data.s3.amazonaws.com/forests/v2/global/dinov3_global_chm_v2_ml3"
      Sys.setenv(AWS_NO_SIGN_REQUEST = "YES")
      
      temp_geojson <- file.path(cache_dir, "tiles.geojson")
      if (!file.exists(temp_geojson)) {
        message("[canopy] Downloading Meta CHMv2 global tile index (tiles.geojson)...")
        download_ok <- tryCatch({
          utils::download.file(
            url = paste0(base_url, "/tiles.geojson"),
            destfile = temp_geojson,
            method = "auto",
            quiet = TRUE,
            mode = "wb"
          )
          TRUE
        }, error = function(e) FALSE)
        if (!download_ok) unlink(temp_geojson, force = TRUE)
      }
      
      index <- NULL
      if (file.exists(temp_geojson)) {
        index <- tryCatch(sf::st_read(temp_geojson, quiet = TRUE), error = function(e) NULL)
      }
      
      if (is.null(index)) {
        message("[canopy] Unable to load Meta CHMv2 tile index. Proceeding without canopy data.")
        return(list(raster = NULL, source = "none", quality_tier = "none"))
      }
      
      match <- sf::st_filter(index, sf::st_as_sfc(bb))
      if (nrow(match) == 0) {
        message("[canopy] No intersecting Meta CHMv2 tile found. Proceeding without canopy data.")
        return(list(raster = NULL, source = "none", quality_tier = "none"))
      }
      
      tile_id <- match$tile[[1]]
      message(sprintf("[canopy] Intersecting Meta CHMv2 tile: %s. Downloading and cropping...", tile_id))
      uri <- sprintf("/vsicurl/%s/chm/%s.tif", base_url, tile_id)
      
      chm_raster <- tryCatch(terra::rast(uri), error = function(e) NULL)
      if (is.null(chm_raster)) {
        message("[canopy] Failed to read remote Meta CHMv2 tile. Proceeding without canopy data.")
        return(list(raster = NULL, source = "none", quality_tier = "none"))
      }
      
      # Crop and mask
      chm_crs <- sf::st_transform(analysis_area, terra::crs(chm_raster))
      canopy_crop <- terra::crop(chm_raster, terra::vect(chm_crs), snap = "out")
      canopy <- terra::mask(canopy_crop, terra::vect(chm_crs))
      
      # Save to cache
      cache_file <- file.path(cache_dir, paste0("chm_", tile_id, "_", safe_bbox, ".tif"))
      terra::writeRaster(canopy, cache_file, overwrite = TRUE, gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
      source_name <- "meta_chm_v2_download"
    }
  }
  
  if (!is.null(target_res)) {
    canopy <- terra::project(canopy, processing_crs, res = target_res, method = "bilinear")
  } else {
    canopy <- terra::project(canopy, processing_crs, method = "bilinear")
  }
  canopy <- terra::crop(canopy, terra::vect(analysis_area), snap = "out")
  canopy <- terra::mask(canopy, terra::vect(analysis_area))
  canopy[canopy < 0] <- NA
  names(canopy) <- "canopy_height_m"
  list(raster = canopy, source = source_name, quality_tier = "canopy_obstruction")
}

.uh_svf_build_obstruction <- function(terrain_raster, buildings, canopy_raster = NULL, target_resolution_m = NULL) {
  terrain_use <- terrain_raster
  terrain_res <- mean(terra::res(terrain_use))
  if (!is.null(target_resolution_m) && target_resolution_m > terrain_res) {
    template <- terra::rast(terra::ext(terrain_use), resolution = target_resolution_m, crs = terra::crs(terrain_use))
    terrain_use <- terra::resample(terrain_use, template, method = "bilinear")
  }

  template <- terra::rast(terra::ext(terrain_use), resolution = terra::res(terrain_use), crs = terra::crs(terrain_use))
  building_top <- terra::rasterize(terra::vect(buildings), template, field = "roof_z", fun = "max", background = NA)
  obstruction <- max(terrain_use, building_top, na.rm = TRUE)
  names(obstruction) <- "obstruction_z"

  if (!is.null(canopy_raster)) {
    canopy_use <- terra::resample(canopy_raster, terrain_use, method = "bilinear")
    canopy_top <- terrain_use + canopy_use
    obstruction <- max(obstruction, canopy_top, na.rm = TRUE)
    names(obstruction) <- "obstruction_z"
  }

  list(terrain = terrain_use, obstruction = obstruction)
}

.uh_svf_get_osm_roads <- function(analysis_area, timeout = 180, cache_dir = file.path("data_cache", "osm")) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  bbox <- .uh_svf_boundary_bbox_wgs84(sf::st_transform(analysis_area, 4326))
  cache_file <- file.path(cache_dir, paste0(.uh_svf_slug(paste("roads", round(unname(bbox), 5), collapse = "_")), ".rds"))
  if (file.exists(cache_file)) {
    cached <- readRDS(cache_file)
    if (inherits(cached, "sf")) return(cached)
    unlink(cache_file)
  }

  message(sprintf("[svf] Querying street network from Overpass API with timeout=%d s...", timeout))
  message(sprintf("[svf] Overpass BBOX: bottom=%f, left=%f, top=%f, right=%f", bbox[["bottom"]], bbox[["left"]], bbox[["top"]], bbox[["right"]]))
  query <- sprintf('[out:json][timeout:%d];(way["highway"](%f,%f,%f,%f););out geom;', timeout, bbox[["bottom"]], bbox[["left"]], bbox[["top"]], bbox[["right"]])
  servers <- c(
    "https://overpass-api.de/api/interpreter",
    "https://overpass.kumi.systems/api/interpreter",
    "https://overpass.osm.ch/api/interpreter",
    "https://api.openstreetmap.fr/oapi/interpreter"
  )

  parsed <- NULL
  success_server <- NULL
  for (server in servers) {
    message(sprintf("[svf] Contacting Overpass server: %s", server))
    resp <- tryCatch(
      httr::POST(server, body = list(data = query), encode = "form", httr::timeout(timeout + 30), httr::add_headers(`User-Agent` = "greenR SVF strict workflow")),
      error = function(e) e
    )
    if (!inherits(resp, "error") && !httr::http_error(resp)) {
      parsed_temp <- tryCatch(jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE), error = function(e) NULL)
      if (!is.null(parsed_temp) && !is.null(parsed_temp$elements)) {
        if (!is.null(parsed_temp$remark)) {
          message(sprintf("[svf] Overpass server returned remark/error: %s", parsed_temp$remark))
        }
        if (length(parsed_temp$elements) > 0 && is.null(parsed_temp$remark)) {
          parsed <- parsed_temp
          success_server <- server
          break
        } else {
          message(sprintf("[svf] Overpass server returned 0 elements or has a remark. Trying next mirror..."))
        }
      }
    } else {
      if (inherits(resp, "error")) {
        message(sprintf("[svf] Connection error: %s", resp$message))
      } else {
        message(sprintf("[svf] HTTP error: Status %d", httr::status_code(resp)))
      }
    }
  }

  if (is.null(parsed) || is.null(parsed$elements)) {
    message("[svf] Primary POST attempts did not succeed with non-empty results. Falling back to Curl...")
    tmp_json <- tempfile(fileext = ".json")
    on.exit(unlink(tmp_json), add = TRUE)
    for (server in servers) {
      message(sprintf("[svf] Contacting Overpass server via Curl: %s", server))
      args <- c("-sS", "--fail", "--max-time", as.character(timeout + 30), "-A", "greenR SVF strict workflow", "-d", paste0("data=", query), server, "-o", tmp_json)
      status <- tryCatch(system2("curl", args = args, stdout = FALSE, stderr = FALSE), error = function(e) 1L)
      if (!identical(status, 0L) || !file.exists(tmp_json) || !isTRUE(file.info(tmp_json)$size > 0)) {
        message("[svf] Curl failed or returned empty file.")
        next
      }
      parsed_temp <- tryCatch(jsonlite::fromJSON(paste(readLines(tmp_json, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), simplifyVector = FALSE), error = function(e) NULL)
      if (!is.null(parsed_temp) && !is.null(parsed_temp$elements)) {
        if (!is.null(parsed_temp$remark)) {
          message(sprintf("[svf] Curl Overpass server returned remark/error: %s", parsed_temp$remark))
        }
        if (length(parsed_temp$elements) > 0 && is.null(parsed_temp$remark)) {
          parsed <- parsed_temp
          success_server <- server
          break
        } else {
          message("[svf] Curl Overpass server returned 0 elements or has a remark.")
        }
      }
    }
  }

  if (is.null(parsed) || is.null(parsed$elements)) stop("OSM roads request failed across all mirrors.", call. = FALSE)

  message(sprintf("[svf] Successfully retrieved %d elements from Overpass mirror: %s", length(parsed$elements), success_server))

  lines <- lapply(parsed$elements, function(el) {
    if (!identical(el$type, "way") || is.null(el$geometry)) return(NULL)
    coords <- do.call(rbind, lapply(el$geometry, function(pt) c(pt$lon, pt$lat)))
    if (nrow(coords) < 2) return(NULL)
    sf::st_linestring(coords)
  })
  lines <- Filter(Negate(is.null), lines)
  
  if (length(lines) == 0) {
    message("[svf] Error: JSON parsed successfully but contained 0 ways with valid coordinates.")
    stop("OSM roads query returned no usable ways.", call. = FALSE)
  }

  roads <- sf::st_sf(geometry = sf::st_sfc(lines, crs = 4326)) |>
    sf::st_transform(sf::st_crs(analysis_area)) |>
    sf::st_make_valid()
  roads <- suppressWarnings(sf::st_intersection(roads, sf::st_sf(geometry = analysis_area)))
  roads <- roads[!sf::st_is_empty(roads), ]
  
  if (nrow(roads) == 0) {
    stop("OSM roads query successfully fetched streets, but none intersected the analysis boundary area.", call. = FALSE)
  }

  roads$street_id <- seq_len(nrow(roads))
  saveRDS(roads, cache_file)
  roads
}

.uh_svf_make_grid_points <- function(analysis_area, spacing_m = 50) {
  pts <- sf::st_make_grid(analysis_area, cellsize = spacing_m, what = "centers")
  pts <- sf::st_sf(point_id = seq_along(pts), geometry = pts, crs = sf::st_crs(analysis_area))
  pts <- suppressWarnings(sf::st_intersection(pts, sf::st_sf(geometry = sf::st_union(sf::st_geometry(analysis_area)))))
  pts$point_id <- seq_len(nrow(pts))
  pts
}

.uh_svf_make_street_points <- function(roads, spacing_m = 25) {
  streets <- suppressWarnings(sf::st_cast(roads, "LINESTRING"))
  if (!"street_id" %in% names(streets)) streets$street_id <- seq_len(nrow(streets))
  
  # Vectorized regular sampling along all lines at once
  pts_mult <- sf::st_line_sample(streets, density = 1 / spacing_m, type = "regular")
  
  pts_sf <- sf::st_sf(
    street_id = streets$street_id,
    geometry = pts_mult,
    crs = sf::st_crs(streets)
  )
  
  # Remove empty geometries first to bypass the sf::st_cast bug on empty elements
  pts_sf <- pts_sf[!sf::st_is_empty(pts_sf), ]
  if (nrow(pts_sf) == 0) stop("No street sample points were created.", call. = FALSE)
  
  # Expand MULTIPOINT to individual POINT features duplicating attributes
  pts <- suppressWarnings(sf::st_cast(pts_sf, "POINT"))
  
  # Final sanity filter to ensure no empty geometries are passed to ray casting
  pts <- pts[!sf::st_is_empty(pts), ]
  if (nrow(pts) == 0) stop("No street sample points were created.", call. = FALSE)
  
  # Compute point sequence index per street using base R ave (extremely fast)
  pts$point_seq <- ave(seq_len(nrow(pts)), pts$street_id, FUN = seq_along)
  pts$point_id <- seq_len(nrow(pts))
  
  pts
}

.uh_svf_compute_points <- function(sample_points, terrain_raster, obstruction_raster, n_directions = 72, max_distance_m = 300, step_m = 10, observer_height_m = 1.5, return_raw_angles = FALSE, n_cores = NULL) {
  coords <- sf::st_coordinates(sample_points)[, c("X", "Y"), drop = FALSE]
  base_z <- as.numeric(terra::extract(terrain_raster, coords)[[1]])
  observer_z <- base_z + observer_height_m
  directions_deg <- seq(0, 360 - 360 / n_directions, length.out = n_directions)
  distances <- seq(step_m, max_distance_m, by = step_m)

  obs_values <- as.vector(obstruction_raster)
  r_ext <- terra::ext(obstruction_raster)
  r_res <- terra::res(obstruction_raster)
  r_dim <- dim(obstruction_raster)
  xmin <- r_ext[1]
  ymax <- r_ext[4]
  xres <- r_res[1]
  yres <- r_res[2]
  ncol <- r_dim[2]
  nrow <- r_dim[1]

  # Call C++ implementation
  res <- svf_raycast_cpp(
    coords = coords,
    obs_values = obs_values,
    observer_z = observer_z,
    directions_deg = directions_deg,
    distances = distances,
    xmin = xmin,
    ymax = ymax,
    xres = xres,
    yres = yres,
    ncol = ncol,
    nrow = nrow,
    return_raw_angles = return_raw_angles
  )

  sample_points$ground_z <- base_z
  sample_points$observer_z <- observer_z
  sample_points$svf <- res$svf
  sample_points$sky_obstruction <- 1 - res$svf
  sample_points$mean_horizon_deg <- res$mean_horizon
  sample_points$max_horizon_deg <- res$max_horizon
  
  if (return_raw_angles) {
    sample_points$horizon_angles <- split(res$horizon_mat, row(res$horizon_mat))
  }
  
  sample_points
}

.uh_svf_summarise_streets <- function(street_points, roads) {
  stats <- street_points |>
    sf::st_drop_geometry() |>
    dplyr::group_by(street_id) |>
    dplyr::summarise(
      point_n = dplyr::n(),
      svf_mean = mean(svf, na.rm = TRUE),
      svf_min = min(svf, na.rm = TRUE),
      svf_p10 = stats::quantile(svf, 0.10, na.rm = TRUE),
      mean_horizon_deg = mean(mean_horizon_deg, na.rm = TRUE),
      .groups = "drop"
    )
  dplyr::left_join(roads, stats, by = "street_id")
}

.uh_svf_summarise_buildings <- function(grid_points, buildings, max_match_distance_m = 45) {
  ref_pts <- suppressWarnings(sf::st_point_on_surface(buildings))
  nearest <- sf::st_nearest_feature(ref_pts, grid_points)
  dist_m <- as.numeric(sf::st_distance(ref_pts, grid_points[nearest, ], by_element = TRUE))
  out <- buildings
  out$nearest_point_id <- grid_points$point_id[nearest]
  out$match_distance_m <- dist_m
  out$svf <- ifelse(dist_m <= max_match_distance_m, grid_points$svf[nearest], NA_real_)
  out$mean_horizon_deg <- ifelse(dist_m <= max_match_distance_m, grid_points$mean_horizon_deg[nearest], NA_real_)
  out
}

.uh_svf_analytics <- function(street_points = NULL, street_summary = NULL, building_svf = NULL, canopy_raster = NULL) {
  street_vals <- if (!is.null(street_points)) street_points$svf[is.finite(street_points$svf)] else numeric()
  street_seg_vals <- if (!is.null(street_summary)) street_summary$svf_mean[is.finite(street_summary$svf_mean)] else numeric()
  building_vals <- if (!is.null(building_svf)) building_svf$svf[is.finite(building_svf$svf)] else numeric()
  canopy_vals <- if (!is.null(canopy_raster)) terra::values(canopy_raster) else numeric()
  canopy_vals <- canopy_vals[is.finite(canopy_vals)]

  data.frame(
    layer = c("street_points", "street_segments", "building_adjacent", "canopy"),
    n = c(length(street_vals), length(street_seg_vals), length(building_vals), length(canopy_vals)),
    mean = c(mean(street_vals, na.rm = TRUE), mean(street_seg_vals, na.rm = TRUE), mean(building_vals, na.rm = TRUE), mean(canopy_vals, na.rm = TRUE)),
    median = c(.uh_svf_safe_stat(street_vals, stats::median), .uh_svf_safe_stat(street_seg_vals, stats::median), .uh_svf_safe_stat(building_vals, stats::median), .uh_svf_safe_stat(canopy_vals, stats::median)),
    p10 = c(.uh_svf_safe_stat(street_vals, stats::quantile, probs = 0.10, names = FALSE), .uh_svf_safe_stat(street_seg_vals, stats::quantile, probs = 0.10, names = FALSE), .uh_svf_safe_stat(building_vals, stats::quantile, probs = 0.10, names = FALSE), .uh_svf_safe_stat(canopy_vals, stats::quantile, probs = 0.10, names = FALSE)),
    p90 = c(.uh_svf_safe_stat(street_vals, stats::quantile, probs = 0.90, names = FALSE), .uh_svf_safe_stat(street_seg_vals, stats::quantile, probs = 0.90, names = FALSE), .uh_svf_safe_stat(building_vals, stats::quantile, probs = 0.90, names = FALSE), .uh_svf_safe_stat(canopy_vals, stats::quantile, probs = 0.90, names = FALSE)),
    share_below_025 = c(mean(street_vals < 0.25, na.rm = TRUE), mean(street_seg_vals < 0.25, na.rm = TRUE), mean(building_vals < 0.25, na.rm = TRUE), NA_real_),
    share_below_050 = c(mean(street_vals < 0.50, na.rm = TRUE), mean(street_seg_vals < 0.50, na.rm = TRUE), mean(building_vals < 0.50, na.rm = TRUE), NA_real_),
    gini = c(.uh_svf_gini(street_vals), .uh_svf_gini(street_seg_vals), .uh_svf_gini(building_vals), NA_real_),
    stringsAsFactors = FALSE
  )
}

.uh_svf_palette <- c("#1f0f3b", "#433d84", "#2e78a6", "#84c7d3", "#dff0ef", "#f4d06f", "#ec8b3b")

.uh_svf_fetch_basemap <- function(boundary, provider = "CartoDB.Positron", zoom = 15, cache_dir = file.path("data_cache", "basemaps")) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  maptiles::get_tiles(sf::st_transform(boundary, 3857), provider = provider, crop = TRUE, zoom = zoom, cachedir = cache_dir)
}

.uh_svf_static_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 18, color = "#1e242b"),
      plot.subtitle = ggplot2::element_text(size = 11, color = "#4d5966"),
      plot.caption = ggplot2::element_text(size = 9, color = "#475569", hjust = 0, lineheight = 1.25, margin = ggplot2::margin(t = 12, b = 15)),
      legend.position = "right",
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    )
}

.uh_svf_plot_static <- function(street_summary, boundary, buildings = NULL, basemap = NULL, title, subtitle, linewidth = 0.9) {
  boundary_plot <- sf::st_transform(boundary, sf::st_crs(street_summary))
  buildings_plot <- if (!is.null(buildings)) sf::st_transform(buildings, sf::st_crs(street_summary)) else NULL

  use_raster_buildings <- FALSE
  r_bld <- NULL
  if (!is.null(buildings_plot)) {
    if (nrow(buildings_plot) > 2000) {
      message(sprintf("[svf] %d buildings found. Using ultra-fast tidyterra SpatRaster rasterization for static map rendering.", nrow(buildings_plot)))
      b_clean <- sf::st_collection_extract(buildings_plot, "POLYGON")
      if (nrow(b_clean) > 0) {
        ext_b <- terra::ext(b_clean)
        r_temp <- terra::rast(extent = ext_b, resolution = 5, crs = sf::st_crs(b_clean)$wkt)
        r_bld <- terra::rasterize(terra::vect(b_clean), r_temp, field = 1)
        use_raster_buildings <- TRUE
      }
    }
  }

  data_bb <- sf::st_bbox(street_summary)
  dx <- (data_bb[["xmax"]] - data_bb[["xmin"]]) * 0.05
  dy <- (data_bb[["ymax"]] - data_bb[["ymin"]]) * 0.05

  val_range <- range(street_summary$svf_mean, na.rm = TRUE)
  if (diff(val_range) < 0.01) val_range <- c(0, 1)

  ggplot2::ggplot() +
    { if (!is.null(basemap)) ggspatial::layer_spatial(basemap, alpha = 0.95) } +
    {
      if (!is.null(buildings_plot)) {
        if (use_raster_buildings && !is.null(r_bld)) {
          tidyterra::geom_spatraster(data = r_bld, alpha = 0.35, show.legend = FALSE)
        } else {
          ggplot2::geom_sf(data = buildings_plot, fill = "#ddd6cd", color = NA, alpha = 0.35)
        }
      }
    } +
    { if (use_raster_buildings && !is.null(r_bld)) ggplot2::scale_fill_gradient(low = "#ddd6cd", high = "#ddd6cd", na.value = "transparent") } +
    ggplot2::geom_sf(data = street_summary, ggplot2::aes(color = svf_mean), linewidth = linewidth, alpha = 0.98) +
    ggplot2::geom_sf(data = boundary_plot, fill = NA, color = "#2f3842", linewidth = 0.4) +
    ggplot2::scale_color_gradientn(colours = .uh_svf_palette, limits = val_range, name = "Street SVF") +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.18, style = "ticks", text_cex = 0.85) +
    ggspatial::annotation_north_arrow(location = "tl", which_north = "true", style = ggspatial::north_arrow_minimal()) +
    ggplot2::coord_sf(
      crs = sf::st_crs(street_summary),
      xlim = c(data_bb[["xmin"]] - dx, data_bb[["xmax"]] + dx),
      ylim = c(data_bb[["ymin"]] - dy, data_bb[["ymax"]] + dy),
      datum = sf::st_crs(4326),
      expand = FALSE
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = "Generated by greenR") +
    .uh_svf_static_theme()
}

.uh_svf_plot_grid_static <- function(grid_points, boundary, buildings = NULL, basemap = NULL, title, subtitle) {
  boundary_plot <- sf::st_transform(boundary, sf::st_crs(grid_points))
  buildings_plot <- if (!is.null(buildings)) sf::st_transform(buildings, sf::st_crs(grid_points)) else NULL

  use_raster_buildings <- FALSE
  r_bld <- NULL
  if (!is.null(buildings_plot)) {
    if (nrow(buildings_plot) > 2000) {
      message(sprintf("[svf] %d buildings found. Using ultra-fast tidyterra SpatRaster rasterization for static map rendering.", nrow(buildings_plot)))
      b_clean <- sf::st_collection_extract(buildings_plot, "POLYGON")
      if (nrow(b_clean) > 0) {
        ext_b <- terra::ext(b_clean)
        r_temp <- terra::rast(extent = ext_b, resolution = 5, crs = sf::st_crs(b_clean)$wkt)
        r_bld <- terra::rasterize(terra::vect(b_clean), r_temp, field = 1)
        use_raster_buildings <- TRUE
      }
    }
  }

  data_bb <- sf::st_bbox(grid_points)
  dx <- (data_bb[["xmax"]] - data_bb[["xmin"]]) * 0.05
  dy <- (data_bb[["ymax"]] - data_bb[["ymin"]]) * 0.05

  val_range <- range(grid_points$svf, na.rm = TRUE)
  if (diff(val_range) < 0.01) val_range <- c(0, 1)

  ggplot2::ggplot() +
    { if (!is.null(basemap)) ggspatial::layer_spatial(basemap, alpha = 0.95) } +
    {
      if (!is.null(buildings_plot)) {
        if (use_raster_buildings && !is.null(r_bld)) {
          tidyterra::geom_spatraster(data = r_bld, alpha = 0.30, show.legend = FALSE)
        } else {
          ggplot2::geom_sf(data = buildings_plot, fill = "#ddd6cd", color = NA, alpha = 0.30)
        }
      }
    } +
    { if (use_raster_buildings && !is.null(r_bld)) ggplot2::scale_fill_gradient(low = "#ddd6cd", high = "#ddd6cd", na.value = "transparent") } +
    ggplot2::geom_sf(data = grid_points, ggplot2::aes(color = svf), size = 0.65, alpha = 0.95) +
    ggplot2::geom_sf(data = boundary_plot, fill = NA, color = "#2f3842", linewidth = 0.4) +
    ggplot2::scale_color_gradientn(colours = .uh_svf_palette, limits = val_range, name = "Grid SVF") +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.18, style = "ticks", text_cex = 0.85) +
    ggspatial::annotation_north_arrow(location = "tl", which_north = "true", style = ggspatial::north_arrow_minimal()) +
    ggplot2::coord_sf(
      crs = sf::st_crs(grid_points),
      xlim = c(data_bb[["xmin"]] - dx, data_bb[["xmax"]] + dx),
      ylim = c(data_bb[["ymin"]] - dy, data_bb[["ymax"]] + dy),
      datum = sf::st_crs(4326),
      expand = FALSE
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = "Generated by greenR") +
    .uh_svf_static_theme()
}
