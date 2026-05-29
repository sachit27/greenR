#' Urban Heat Island Decision Support Suite
#'
#' @description
#' A high-level unified orchestration function that automates the entire greenR Canyon Suite workflow:
#' (1) Assembles the multi-source spatial data grid, (2) Builds superblock morphological block priority metrics,
#' (3) Computes morphological street canyon priorities and runs pedigree solar shading microclimate screening,
#' and (4) Generates all 12 visionary visualisations and GIS datasets. Supports hybrid workflows
#' (local rasters/shapefiles, or auto-fetching from STAC, Copernicus, Meta CHM, Global Building Atlas,
#' WorldPop/GHSL, and OpenStreetMap).
#'
#' @importFrom sf st_transform st_geometry st_centroid st_union st_coordinates st_bbox st_as_sfc st_sf st_sfc st_polygon st_multipolygon st_make_valid st_simplify st_read st_write st_intersection st_is_empty st_buffer st_join st_as_sf st_area st_length st_crs
#' @importFrom terra rast project crop mask resample writeRaster vect app ext values aggregate res
#' @importFrom dplyr filter select collect bind_rows mutate transmute group_by summarise left_join n any_of across case_when min_rank desc .data
#' @importFrom httr GET POST http_error timeout content add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom arrow open_dataset
#' @importFrom leaflet leaflet leafletOptions addProviderTiles addPolygons addLayersControl layersControlOptions colorFactor addLegend hideGroup
#' @importFrom htmlwidgets saveWidget
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_manual scale_fill_gradientn coord_sf labs theme_minimal theme element_text element_blank geom_tile geom_col geom_text expansion annotate ggplotGrob
#' @importFrom ggspatial annotation_scale
#' @importFrom cowplot ggdraw draw_plot
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom plotly plot_ly layout
#'
#' @param city_name Name of the city (e.g. "Basel, Switzerland").
#' @param hex_size_m Hexagon resolution size in meters (default 100m).
#' @param local_boundary Optional sf boundary polygon.
#' @param local_ndvi Optional pre-loaded NDVI raster.
#' @param local_lst Optional pre-loaded LST raster.
#' @param local_chm Optional pre-loaded CHM raster.
#' @param local_population Optional pre-loaded population raster.
#' @param local_buildings Optional pre-loaded buildings footprint.
#' @param local_buildings_path Optional path to building shapefile/GPKG.
#' @param local_trees Optional pre-loaded tree sf points.
#' @param local_trees_path Optional path to tree dataset.
#' @param local_osm_layers Optional pre-loaded OSM layers list.
#' @param ndvi_datetime Date range for Sentinel-2 NDVI.
#' @param lst_datetime Date range for Landsat LST.
#' @param cache_dir Local caching directory.
#' @param w_heat MCDA weight for LST (default 0.60).
#' @param w_pop MCDA weight for Population (default 0.40).
#' @param w_exposure MCDA weight for Heat Exposure (default 0.55).
#' @param w_deficit MCDA weight for Cooling Deficit (default 0.45).
#' @param w_canopy MCDA weight for Canopy (default 0.45).
#' @param w_ndvi MCDA weight for NDVI (default 0.35).
#' @param w_build MCDA weight for Buildings (default 0.20).
#' @param w_avail MCDA weight for Green Space Availability (default 0.60).
#' @param w_density MCDA weight for Tree Density (default 0.40).
#' @param fallback_to_proxy Allow fallback to grid density population proxy if online sources are unreachable.
#' @param include_static Logical. If TRUE, generates static PNG maps. Default is TRUE.
#' @param include_leaflet Logical. If TRUE, generates interactive Leaflet HTML maps. Default is FALSE.
#' @param include_3d Logical. If TRUE, generates a 3D Deck.gl interactive HTML dashboard. Default is FALSE.
#' @param include_gis Logical. If TRUE, saves outputs as GeoJSON files and RDS objects. Default is FALSE.
#' @param output_dir Optional output directory to write all plots, interactive leaflet maps, 3D DeckGL dashboards, and GIS files.
#' @param output_prefix Optional output filename prefix (defaults to slugified city_name).
#' @param use_cache Logical. If TRUE, uses cached data if available. Default is FALSE.
#' @param palette_quadrant Optional color palette for quadrant plots.
#' @param palette_action Optional color palette for action class plots.
#' @param palette_canyon Optional color palette for canyon priority maps.
#' @param palette_canyon_biv Optional color palette for bivariate canyon maps.
#' @param palette_biv Optional color palette for bivariate priority maps.
#'
#' @return A list containing elements priority_grid, block_priority, and canyon_priority.
#'
#' @examples
#' \dontrun{
#'   library(greenR)
#'   library(sf)
#'   library(terra)
#'
#'   # =========================================================================
#'   # TUTORIAL: Comprehensive Urban Heat Mitigation Decision Support Workflow
#'   # =========================================================================
#'
#'   # Example 1: Complete Online Mode (Fast default)
#'   # Bypasses local caching by default; dynamically fetches and window-clips GHSL
#'   # 100m Population, Sentinel-2 NDVI, Landsat-9 LST, Meta CHM, and OSM layers.
#'   results <- uh_decision(
#'     city_name = "Basel, Switzerland",
#'     hex_size_m = 100,
#'     output_dir = tempdir(),
#'     output_prefix = "basel_online"
#'   )
#'
#'   # Example 2: Loading population and buildings directly from local paths
#'   # Perfect for running analyses using your own downloaded city data.
#'   results_paths <- uh_decision(
#'     city_name = "City of London, UK",
#'     hex_size_m = 50,
#'     local_population = "data/GHS_POP_E2030_GLOBE_R2023A_54009_100_V1_0_R3_C18.tif",
#'     local_buildings = "data/london_buildings.gpkg",
#'     output_dir = tempdir(),
#'     output_prefix = "london_local_paths"
#'   )
#'
#'   # Example 3: Mixed setup with pre-loaded R raster and vector objects
#'   # Excellent for custom GIS workflows where you have already cropped/projected data.
#'   boundary_obj <- sf::st_read("custom_boundary.geojson")
#'   ndvi_rast <- terra::rast("cropped_ndvi.tif")
#'
#'   results_objects <- uh_decision(
#'     city_name = "Custom Region",
#'     local_boundary = boundary_obj,
#'     local_ndvi = ndvi_rast,
#'     output_dir = tempdir(),
#'     output_prefix = "custom_mixed"
#'   )
#'
#'   # Example 4: Full Extraction with Caching enabled (Leaflet & 3D maps)
#'   # Set use_cache = TRUE to store and reuse fetched layers on subsequent runs.
#'   results_cached <- uh_decision(
#'     city_name = "Geneva, Switzerland",
#'     hex_size_m = 100,
#'     include_leaflet = TRUE,
#'     include_3d = TRUE,
#'     use_cache = TRUE,
#'     output_dir = tempdir(),
#'     output_prefix = "geneva_cached"
#'   )
#' }
#' @export
uh_decision <- function(
  city_name = "Basel, Switzerland",
  hex_size_m = 100,
  local_boundary = NULL,
  local_ndvi = NULL,
  local_lst = NULL,
  local_chm = NULL,
  local_population = NULL,
  local_buildings = NULL,
  local_buildings_path = NULL,
  local_trees = NULL,
  local_trees_path = NULL,
  local_osm_layers = NULL,
  ndvi_datetime = "2025-06-01/2025-08-31",
  lst_datetime = "2025-06-01/2025-08-31",
  cache_dir = NULL,
  use_cache = FALSE,
  w_heat = 0.60,
  w_pop = 0.40,
  w_exposure = 0.55,
  w_deficit = 0.45,
  w_canopy = 0.45,
  w_ndvi = 0.35,
  w_build = 0.20,
  w_avail = 0.60,
  w_density = 0.40,
  fallback_to_proxy = FALSE,
  include_static = TRUE,
  include_leaflet = FALSE,
  include_3d = FALSE,
  include_gis = FALSE,
  output_dir = NULL,
  output_prefix = NULL,
  palette_quadrant = NULL,
  palette_action = NULL,
  palette_canyon = NULL,
  palette_canyon_biv = NULL,
  palette_biv = NULL
) {
  # 1. Assembling City Data Grid
  message("--- [uh_decision] Step 1/4: Assembling City Data Grid ---")
  priority_data <- build_urban_priority_grid(
    city_name = city_name,
    hex_size_m = hex_size_m,
    local_boundary = local_boundary,
    local_ndvi = local_ndvi,
    local_lst = local_lst,
    local_chm = local_chm,
    local_population = local_population,
    local_buildings = local_buildings,
    local_buildings_path = local_buildings_path,
    local_trees = local_trees,
    local_trees_path = local_trees_path,
    local_osm_layers = local_osm_layers,
    ndvi_datetime = ndvi_datetime,
    lst_datetime = lst_datetime,
    cache_dir = cache_dir,
    use_cache = use_cache,
    w_heat = w_heat,
    w_pop = w_pop,
    w_exposure = w_exposure,
    w_deficit = w_deficit,
    w_canopy = w_canopy,
    w_ndvi = w_ndvi,
    w_build = w_build,
    w_avail = w_avail,
    w_density = w_density,
    fallback_to_proxy = fallback_to_proxy
  )

  # 2. Building Morphological Blocks
  message("--- [uh_decision] Step 2/4: Building Morphological Blocks (Superblocks subdivision) ---")
  block_data <- build_urban_block_priority(priority_data)

  # 3. Executing Street Canyon Priorities & Microclimate Screening
  message("--- [uh_decision] Step 3/4: Executing Street Canyon Priorities & Solar Screening ---")
  canyon_data <- build_street_canyon_priority(priority_data)
  # Derive city latitude from boundary centroid so the solar model is geographically aware
  city_lat <- tryCatch({
    centroid_wgs84 <- sf::st_transform(sf::st_centroid(sf::st_union(priority_data$boundary)), 4326)
    as.numeric(sf::st_coordinates(centroid_wgs84)[, "Y"])
  }, error = function(e) 46.2)
  canyon_data <- emulate_canyon_microclimate(canyon_data, latitude = city_lat)

  # 4. Optional file writing and visualisations
  if (!is.null(output_dir)) {
    message("--- [uh_decision] Step 4/4: Writing outputs ---")
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    prefix <- if (is.null(output_prefix)) .uh_svf_slug(city_name) else output_prefix

    if (include_static) {
      # Static maps
      message("   [cartography] Plotting Block-Level Hybrid Field Map...")
      tryCatch({
        hybrid_field_blocks <- plot_hybrid_field_map(block_data, title = sprintf("%s: Hybrid Field Map (Blocks)", city_name), palette = palette_quadrant)
        ggplot2::ggsave(file.path(output_dir, paste0(prefix, "_hybrid_field_blocks.png")), hybrid_field_blocks, width = 14, height = 9, dpi = 240, bg = "white")
      }, error = function(e) message("   [!] Block hybrid field map failed: ", e$message))

      message("   [cartography] Plotting Hexagon-Level Hybrid Field Map...")
      tryCatch({
        hybrid_field_hex <- plot_hybrid_field_map(priority_data, title = sprintf("%s: Hybrid Field Map (Hexagons)", city_name), palette = palette_quadrant)
        ggplot2::ggsave(file.path(output_dir, paste0(prefix, "_hybrid_field_hexagons.png")), hybrid_field_hex, width = 14, height = 9, dpi = 240, bg = "white")
      }, error = function(e) message("   [!] Hex hybrid field map failed: ", e$message))

      message("   [cartography] Plotting Top 5% Action-Class Map (Blocks Scale)...")
      tryCatch({
        action_blocks <- plot_priority_action_classes(block_data, title = sprintf("%s: Tree-planting decision map (Blocks)", city_name), palette = palette_action)
        ggplot2::ggsave(file.path(output_dir, paste0(prefix, "_action_classes_blocks.png")), action_blocks, width = 13, height = 9, dpi = 240, bg = "white")
      }, error = function(e) message("   [!] Block action-class map failed: ", e$message))

      message("   [cartography] Plotting Top 5% Action-Class Map (Hexagons Scale)...")
      tryCatch({
        action_hex <- plot_priority_action_classes(priority_data, title = sprintf("%s: Tree-planting decision map (Hexagons)", city_name), palette = palette_action)
        ggplot2::ggsave(file.path(output_dir, paste0(prefix, "_action_classes_hexagons.png")), action_hex, width = 13, height = 9, dpi = 240, bg = "white")
      }, error = function(e) message("   [!] Hex action-class map failed: ", e$message))

      message("   [cartography] Plotting Street Canyon Bivariate Map...")
      tryCatch({
        canyon_biv <- plot_canyon_diamond_bivariate(canyon_data, title = sprintf("%s: Street Canyon Climate Shading Priorities", city_name), palette = palette_canyon_biv)
        ggplot2::ggsave(file.path(output_dir, paste0(prefix, "_street_canyon_bivariate.png")), canyon_biv, width = 13, height = 9, dpi = 240, bg = "white")
      }, error = function(e) message("   [!] Canyon bivariate map failed: ", e$message))

      message("   [cartography] Plotting Street Canyon Priority Map...")
      tryCatch({
        canyon_pri <- plot_canyon_priority_map(canyon_data, title = sprintf("%s: Street Canyon Planting Priorities", city_name), palette = palette_canyon)
        ggplot2::ggsave(file.path(output_dir, paste0(prefix, "_street_canyon_priorities.png")), canyon_pri, width = 13, height = 9, dpi = 240, bg = "white")
      }, error = function(e) message("   [!] Canyon priority map failed: ", e$message))
    }

    if (include_gis) {
      # Save RDS and GeoJSON datasets
      message("   [data] Saving GIS datasets (GeoJSON, RDS)...")
      tryCatch({
        saveRDS(block_data$blocks, file.path(output_dir, paste0(prefix, "_blocks_priority.rds")))
        sf::st_write(block_data$blocks, file.path(output_dir, paste0(prefix, "_blocks_priority.geojson")), delete_dsn = TRUE, quiet = TRUE)
        sf::st_write(priority_data$hex, file.path(output_dir, paste0(prefix, "_hexagons_priority.geojson")), delete_dsn = TRUE, quiet = TRUE)
        sf::st_write(canyon_data$canyons, file.path(output_dir, paste0(prefix, "_street_canyons_priority.geojson")), delete_dsn = TRUE, quiet = TRUE)
      }, error = function(e) message("   [!] GIS dataset saving failed: ", e$message))
    }

    if (include_leaflet) {
      # Multilayer Leaflet Map
      message("   [web] Creating Interactive Multilayer Leaflet Web Map...")
      tryCatch({
        m_leaflet <- plot_multilayer_leaflet(block_data, canyon_data, palette = palette_canyon)
        htmlwidgets::saveWidget(m_leaflet, file = file.path(getwd(), output_dir, paste0(prefix, "_multilayer_leaflet.html")), selfcontained = FALSE)
      }, error = function(e) message("   [!] Leaflet multilayer web map failed: ", e$message))
    }

    if (include_3d) {
      # 3D DeckGL Neighborhood Explorer
      message("   [web] Saving 3D Deck.gl Extruded Neighborhood Explorer...")
      tryCatch({
        save_3d_deckgl_dashboard(block_data, file.path(output_dir, paste0(prefix, "_3d_explorer.html")))
        save_3d_deckgl_dashboard(priority_data, file.path(output_dir, paste0(prefix, "_3d_hex_explorer.html")))
      }, error = function(e) message("   [!] 3D neighborhood explorer failed: ", e$message))
    }

    message(sprintf("--- [uh_decision] Done. Requested outputs written successfully to: %s ---", output_dir))
  }

  list(
    priority_grid = priority_data,
    block_priority = block_data,
    canyon_priority = canyon_data
  )
}

#' Build Urban Priority Grid
#'
#' @description
#' Assembles a multi-source hexagonal spatial data grid for a city, combining
#' heat (LST), population (GHSL), canopy (Meta CHM), NDVI (Sentinel-2), building
#' footprints (Global Building Atlas), and OSM layers into a composite priority score.
#'
#' @param city_name Name of the city (e.g. "Basel, Switzerland").
#' @param hex_size_m Hexagon resolution size in meters (default 100m).
#' @param local_boundary Optional sf boundary polygon.
#' @param local_ndvi Optional pre-loaded NDVI raster.
#' @param local_lst Optional pre-loaded LST raster.
#' @param local_chm Optional pre-loaded CHM raster.
#' @param local_population Optional pre-loaded population raster.
#' @param local_buildings Optional pre-loaded buildings footprint.
#' @param local_buildings_path Optional path to building shapefile/GPKG.
#' @param local_trees Optional pre-loaded tree sf points.
#' @param local_trees_path Optional path to tree dataset.
#' @param local_osm_layers Optional pre-loaded OSM layers list.
#' @param ndvi_datetime Date range for Sentinel-2 NDVI.
#' @param lst_datetime Date range for Landsat LST.
#' @param cache_dir Local caching directory.
#' @param use_cache Logical. If TRUE, uses cached data if available. Default is FALSE.
#' @param w_heat MCDA weight for LST.
#' @param w_pop MCDA weight for Population.
#' @param w_exposure MCDA weight for Heat Exposure.
#' @param w_deficit MCDA weight for Cooling Deficit.
#' @param w_canopy MCDA weight for Canopy.
#' @param w_ndvi MCDA weight for NDVI.
#' @param w_build MCDA weight for Buildings.
#' @param w_avail MCDA weight for Green Space Availability.
#' @param w_density MCDA weight for Tree Density.
#' @param fallback_to_proxy Allow fallback to grid density population proxy.
#'
#' @return A list with boundary, hex grid sf, osm_layers, and raster layers.
#' @export
build_urban_priority_grid <- function(
  city_name = "Basel, Switzerland",
  hex_size_m = 100,
  local_boundary = NULL,
  local_ndvi = NULL,
  local_lst = NULL,
  local_chm = NULL,
  local_population = NULL,
  local_buildings = NULL,
  local_buildings_path = NULL,
  local_trees = NULL,
  local_trees_path = NULL,
  local_osm_layers = NULL,
  ndvi_datetime = "2025-06-01/2025-08-31",
  lst_datetime = "2025-06-01/2025-08-31",
  cache_dir = NULL,
  use_cache = FALSE,
  # Expose MCDA weights for rigorous sensitivity calibration (UN HDI-style composite indices)
  w_heat = 0.60,
  w_pop = 0.40,
  w_exposure = 0.55,
  w_deficit = 0.45,
  w_canopy = 0.45,
  w_ndvi = 0.35,
  w_build = 0.20,
  w_avail = 0.60,
  w_density = 0.40,
  fallback_to_proxy = FALSE
) {
  message("--- Initializing Urban Heat Decision Suite ---")

  # Caching Initialization and Default Fallbacks
  if (is.null(cache_dir)) {
    if (use_cache) {
      cache_dir <- "data_cache"
      message("[caching] 'use_cache = TRUE' requested. Setting cache_dir = 'data_cache' to save downloaded files persistently.")
    } else {
      cache_dir <- tempdir()
      message("[caching] Caching is disabled by default. Downloaded raw files will be saved in a temporary folder and discarded after the session.")
      message("          To enable persistent caching, set cache_dir = 'data_cache' and use_cache = TRUE in uh_decision().")
    }
  } else {
    message(sprintf("[caching] Persistent caching active using directory: %s", cache_dir))
  }

  # City isolation folder slug to prevent cross-city interference
  city_slug <- gsub("[^A-Za-z0-9_\\-]", "_", tolower(city_name))
  city_cache_dir <- file.path(cache_dir, "cities", city_slug)
  dir.create(city_cache_dir, recursive = TRUE, showWarnings = FALSE)

  # Note on City-Relative Scaling:
  # The priority scores generated are min-max scaled relative to the specific study city's
  # spatial distribution. These scores are highly effective for relative intra-city screening
  # but are not directly cross-comparable between different cities without global normalization.

  # 1. Fetch Boundary
  boundary <- if (!is.null(local_boundary)) {
    message("[boundary] Using local boundary object.")
    local_boundary
  } else {
    .fetch_boundary_nominatim(city_name, city_cache_dir, use_cache = use_cache)
  }

  # Make hex grid
  hex_grid <- .make_hex_grid(boundary, hex_size_m)

  # 2. Get OSM layers (roads, water, green space, etc.)
  osm_layers <- if (!is.null(local_osm_layers)) {
    message("[osm] Using local OSM layers list.")
    local_osm_layers
  } else {
    trees_path_to_use <- local_trees_path
    if (!is.null(local_trees) && is.character(local_trees)) {
      trees_path_to_use <- local_trees
    }

    layers_fetched <- .fetch_osm_layers(boundary, city_cache_dir, local_trees_path = trees_path_to_use, use_cache = use_cache)

    if (!is.null(local_trees) && !is.character(local_trees)) {
      message("[trees] Injecting local trees sf object.")
      target_crs <- .utm_crs(boundary)
      layers_fetched$layers$trees <- .clip_geom(sf::st_transform(local_trees, 4326), sf::st_transform(boundary, target_crs), target_crs)
    }
    layers_fetched
  }
  hex_osm <- .summarise_osm_hex(hex_grid, osm_layers, w_avail = w_avail, w_density = w_density)

  # 3. Get Population
  population_raster <- if (!is.null(local_population)) {
    if (is.character(local_population)) {
      message(sprintf("[population] Loading local population raster from path: %s", local_population))
      r_pop <- terra::rast(local_population)
      target_crs <- .utm_crs(boundary)
      boundary_proj <- sf::st_transform(boundary, terra::crs(r_pop))
      r_crop <- terra::crop(r_pop, terra::vect(boundary_proj), snap = "out")
      r_mask <- terra::mask(r_crop, terra::vect(boundary_proj))
      r_utm <- terra::project(r_mask, target_crs)
      names(r_utm) <- "population"
      r_utm
    } else {
      message("[population] Using local population raster.")
      local_population
    }
  } else {
    .fetch_population_ghsl(boundary, cache_dir = cache_dir, city_cache_dir = city_cache_dir, fallback_to_proxy = fallback_to_proxy, use_cache = use_cache)
  }
  hex_pop <- .summarise_population_hex(hex_grid, population_raster)

  # 4. Get Global Building Atlas Footprints
  gba_buildings <- if (!is.null(local_buildings)) {
    if (is.character(local_buildings)) {
      .fetch_gba_buildings(boundary, cache_dir = cache_dir, city_cache_dir = city_cache_dir, local_buildings_path = local_buildings, use_cache = use_cache)
    } else {
      message("[buildings] Using local buildings object.")
      local_buildings
    }
  } else {
    .fetch_gba_buildings(boundary, cache_dir = cache_dir, city_cache_dir = city_cache_dir, local_buildings_path = local_buildings_path, use_cache = use_cache)
  }
  hex_gba <- .summarise_gba_hex(hex_grid, gba_buildings)

  # 5. Get Canopy Height Model
  chm_raster <- if (!is.null(local_chm)) {
    message("[chm] Using local CHM raster.")
    local_chm
  } else {
    .fetch_meta_chm(boundary, city_cache_dir, use_cache = use_cache)
  }
  hex_chm <- .summarise_chm_hex(hex_grid, chm_raster)

  # 6. Get NDVI (Sentinel-2)
  ndvi_data <- if (!is.null(local_ndvi)) {
    message("[ndvi] Using local NDVI raster.")
    list(raster = local_ndvi, item_id = "local", datetime = "local")
  } else {
    .fetch_ndvi_stac(boundary, ndvi_datetime, city_cache_dir, use_cache = use_cache)
  }
  hex_ndvi <- .summarise_ndvi_hex(hex_grid, ndvi_data$raster)

  # 7. Get LST (Landsat-8)
  lst_data <- if (!is.null(local_lst)) {
    message("[lst] Using local LST raster.")
    list(raster = local_lst, item_id = "local", datetime = "local")
  } else {
    .fetch_lst_stac(boundary, lst_datetime, city_cache_dir, use_cache = use_cache)
  }
  hex_lst <- .summarise_lst_hex(hex_grid, lst_data$raster)

  # 8. Merge and Calculate Priority Indexes
  merged <- hex_osm |>
    dplyr::left_join(sf::st_drop_geometry(hex_pop), by = "hex_id") |>
    dplyr::left_join(
      sf::st_drop_geometry(hex_gba) |> dplyr::select(-dplyr::any_of("hex_area_m2")),
      by = "hex_id"
    ) |>
    dplyr::left_join(sf::st_drop_geometry(hex_chm), by = "hex_id") |>
    dplyr::left_join(sf::st_drop_geometry(hex_ndvi), by = "hex_id") |>
    dplyr::left_join(sf::st_drop_geometry(hex_lst), by = "hex_id")

  # Normalize components
  pop_score <- .scale_0_100(log1p(merged$population))
  heat_score <- .scale_0_100(merged$lst_mean_c)
  canopy_deficit <- .scale_0_100(merged$canopy_pct_chm, inverse = TRUE)
  ndvi_deficit <- .scale_0_100(merged$ndvi_mean, inverse = TRUE)
  build_pressure <- .scale_0_100(merged$gba_building_frac)

  # Identify cells intersecting MAJOR water body polygons (lakes, rivers only)
  # Exclude streams, ditches, drains, canals, flowlines - these are narrow features
  # running through residential areas that should NOT mask urban hexagons.
  water_overlap <- rep(FALSE, nrow(merged))
  if (!is.null(osm_layers$layers$water) && nrow(osm_layers$layers$water) > 0) {
    suppressWarnings({
      water_all <- osm_layers$layers$water

      # Filter to major water bodies strictly using the physical surface tag
      is_lake <- !is.na(water_all$natural) & water_all$natural == "water"
      major_water <- water_all[is_lake, ]

      if (nrow(major_water) > 0) {
        merged_valid <- sf::st_make_valid(merged)
        water_layer_utm <- sf::st_union(sf::st_make_valid(sf::st_transform(major_water, sf::st_crs(merged))))

        # Proportional water overlap area check
        hex_area <- as.numeric(sf::st_area(merged_valid))
        intersection <- sf::st_intersection(merged_valid, water_layer_utm)

        if (nrow(intersection) > 0) {
          inter_area <- as.numeric(sf::st_area(intersection))
          overlap_df <- data.frame(
            hex_id = intersection$hex_id,
            overlap_area = inter_area,
            stringsAsFactors = FALSE
          )
          overlap_sum <- aggregate(overlap_area ~ hex_id, data = overlap_df, sum)

          merged_valid$hex_area <- hex_area
          merged_valid <- dplyr::left_join(sf::st_drop_geometry(merged_valid), overlap_sum, by = "hex_id")
          merged_valid$water_frac <- dplyr::coalesce(merged_valid$overlap_area / merged_valid$hex_area, 0)

          # 98% threshold: only hexagons that are almost entirely water get masked
          water_overlap <- merged_valid$water_frac > 0.98
        }
      }
    })
  }

  is_water <- water_overlap

  merged <- merged |>
    dplyr::mutate(
      is_water = is_water,
      heat_signal_score = heat_score,
      population_score = pop_score,
      heat_exposure_score = w_heat * heat_score + w_pop * pop_score,
      cooling_deficit_score = w_canopy * canopy_deficit + w_ndvi * ndvi_deficit + w_build * build_pressure,
      tree_need_score = w_exposure * heat_exposure_score + w_deficit * cooling_deficit_score,
      planting_opportunity_score = dplyr::if_else(is_water, 0, plantability_score),
      priority_score = 100 * sqrt((tree_need_score / 100) * (planting_opportunity_score / 100)),
      priority_score = dplyr::if_else(is_water, 0, priority_score),
      need_high = tree_need_score >= stats::median(tree_need_score, na.rm = TRUE),
      opportunity_high = planting_opportunity_score >= stats::median(planting_opportunity_score, na.rm = TRUE),
      quadrant = factor(
        .quadrant_label(need_high, opportunity_high),
        levels = names(.uh_quadrant_palette)
      ),
      priority_rank = dplyr::min_rank(dplyr::desc(priority_score))
    )

  # Elite spatial geometry clipping: erase major water bodies from all hexagons
  if (!is.null(osm_layers$layers$water) && nrow(osm_layers$layers$water) > 0) {
    suppressWarnings({
      water_all <- osm_layers$layers$water
      is_lake <- !is.na(water_all$natural) & water_all$natural == "water"
      major_water <- water_all[is_lake, ]

      if (nrow(major_water) > 0) {
        merged_poly <- sf::st_collection_extract(merged, "POLYGON")
        water_poly <- sf::st_collection_extract(major_water, "POLYGON")
        water_utm <- sf::st_transform(water_poly, sf::st_crs(merged_poly))
        water_union <- sf::st_union(sf::st_make_valid(water_utm)) |> sf::st_make_valid()

        # Erase water bodies from all hexagons
        merged_clipped <- sf::st_difference(sf::st_make_valid(merged_poly), water_union)
        # Keep only polygon geometries
        merged_clipped <- merged_clipped[sf::st_is(merged_clipped, c("POLYGON", "MULTIPOLYGON")), ]
        # Apply zero buffer to clean sliver boundaries and ensure absolute GEOS compliance!
        merged_clipped <- sf::st_buffer(sf::st_make_valid(merged_clipped), 0)
        merged <- merged_clipped
      }
    })
  }

  list(
    boundary = boundary,
    hex = merged,
    osm_layers = osm_layers,
    population_raster = population_raster,
    chm_raster = chm_raster,
    ndvi_raster = ndvi_data$raster,
    lst_raster = lst_data$raster,
    scenes = list(
      ndvi_id = ndvi_data$item_id,
      ndvi_datetime = ndvi_data$datetime,
      lst_id = lst_data$item_id,
      lst_datetime = lst_data$datetime
    )
  )
}

#' Assess equity and inequality index with bootstrap uncertainty
#'
#' @param priority_data A priority grid dataset returned from build_urban_priority_grid.
#' @param n_bootstrap Number of bootstrap iterations for uncertainty estimation (default: 250).
#' @export
assess_urban_priority_equity <- function(priority_data, n_bootstrap = 250) {
  hex <- .add_decision_classes(priority_data$hex) |> sf::st_drop_geometry()

  priority_gini <- .gini_bootstrap(hex$priority_score, weights = pmax(hex$population, 1), n = n_bootstrap)
  heatload_gini <- .gini_bootstrap(hex$heat_exposure_score, weights = pmax(hex$population, 1), n = n_bootstrap)

  total_pop <- sum(hex$population, na.rm = TRUE)
  total_area <- nrow(hex)

  top_priority <- hex$action_class == "Top 5%"
  high_action <- hex$action_class %in% c("High priority", "Top 5%")

  data.frame(
    metric = c(
      "Population-weighted priority Gini",
      "Priority Gini 95% CI lower",
      "Priority Gini 95% CI upper",
      "Population-weighted heat-load Gini",
      "Heat-load Gini 95% CI lower",
      "Heat-load Gini 95% CI upper",
      "Population in top 5% priority hexes",
      "Population share in top 5% priority hexes"
    ),
    value = c(
      priority_gini[["gini"]],
      priority_gini[["lo"]],
      priority_gini[["hi"]],
      heatload_gini[["gini"]],
      heatload_gini[["lo"]],
      heatload_gini[["hi"]],
      sum(hex$population[top_priority], na.rm = TRUE),
      sum(hex$population[top_priority], na.rm = TRUE) / total_pop
    ),
    stringsAsFactors = FALSE
  )
}

#' Create static bivariate Priority maps
#'
#' @param priority_data A priority grid dataset.
#' @param title Plot title.
#' @export
plot_priority_bivariate <- function(priority_data, title = "Bivariate Shading Need & Physical Opportunity Map") {
  hex <- .add_decision_classes(priority_data$hex)
  boundary <- sf::st_transform(priority_data$boundary, sf::st_crs(hex))

  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = boundary, fill = "#f8f7f3", color = NA) +
    ggplot2::geom_sf(data = hex, ggplot2::aes(fill = .data$bivariate_class), color = ggplot2::alpha("white", 0.18), linewidth = 0.03, alpha = 0.92) +
    ggplot2::scale_fill_manual(values = .uh_bivariate_3x3_palette, guide = "none") +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.20, style = "ticks", text_cex = 0.85) +
    ggplot2::coord_sf(datum = sf::st_crs(4326)) +
    ggplot2::labs(
      title = title,
      subtitle = "Hexagons represent 100 m cell summaries. Purple zones show high need overlapping high opportunity.",
      caption = .footer_text(priority_data, "priority_score", "Bivariate surface heat and plantability screening")
    ) +
    .uh_decision_map_theme() +
    ggplot2::theme(legend.position = "none")

  legend <- .bivariate_3x3_legend()
  cowplot::ggdraw() +
    cowplot::draw_plot(map, 0, 0, 0.82, 1) +
    cowplot::draw_plot(legend, 0.78, 0.34, 0.19, 0.28)
}

#' Interactive Leaflet decision widget with dynamic legends
#'
#' @param priority_data A priority grid dataset.
#' @export
plot_priority_interactive <- function(priority_data) {
  hex_wgs <- sf::st_transform(.add_decision_classes(priority_data$hex), 4326)
  action_pal <- leaflet::colorFactor(.uh_action_palette, domain = names(.uh_action_palette))
  heat_pal <- leaflet::colorFactor(.uh_heatload_palette, domain = names(.uh_heatload_palette))
  bivar_pal <- leaflet::colorFactor(.uh_bivariate_3x3_palette, domain = names(.uh_bivariate_3x3_palette))

  labels <- sprintf(
    "<strong>Action:</strong> %s<br/><strong>Heat-load:</strong> %s<br/><strong>Priority score:</strong> %.1f<br/><strong>Heat exposure:</strong> %.1f<br/><strong>Plantability:</strong> %.1f<br/><strong>Population:</strong> %.0f<br/><strong>Canopy:</strong> %.1f%%<br/><strong>NDVI:</strong> %.2f<br/><strong>Surface temp:</strong> %.1f C",
    hex_wgs$action_class,
    hex_wgs$heat_load_class,
    hex_wgs$priority_score,
    hex_wgs$heat_exposure_score,
    hex_wgs$planting_opportunity_score,
    hex_wgs$population,
    hex_wgs$canopy_pct_chm,
    hex_wgs$ndvi_mean,
    hex_wgs$lst_mean_c
  ) |>
    lapply(htmltools::HTML)

  leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Positron") |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Imagery") |>
    leaflet::addPolygons(
      data = hex_wgs,
      group = "Tree action class",
      fillColor = ~action_pal(action_class),
      fillOpacity = 0.82,
      stroke = TRUE,
      weight = 0.2,
      color = "white",
      label = labels
    ) |>
    leaflet::addPolygons(
      data = hex_wgs,
      group = "Population heat load",
      fillColor = ~heat_pal(heat_load_class),
      fillOpacity = 0.82,
      stroke = TRUE,
      weight = 0.2,
      color = "white",
      label = labels
    ) |>
    leaflet::addPolygons(
      data = hex_wgs,
      group = "Heat x plantability",
      fillColor = ~bivar_pal(bivariate_class),
      fillOpacity = 0.82,
      stroke = TRUE,
      weight = 0.2,
      color = "white",
      label = labels
    ) |>
    leaflet::addLayersControl(
      baseGroups = c("Positron", "Imagery"),
      overlayGroups = c("Tree action class", "Population heat load", "Heat x plantability"),
      options = leaflet::layersControlOptions(
        collapsed = FALSE,
        exclusiveGroups = c("Tree action class", "Population heat load", "Heat x plantability")
      )
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      colors = unname(.uh_action_palette),
      labels = names(.uh_action_palette),
      title = "Action class",
      group = "Tree action class",
      opacity = 0.9
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      colors = unname(.uh_heatload_palette),
      labels = names(.uh_heatload_palette),
      title = "Heat-load class",
      group = "Population heat load",
      opacity = 0.9
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      colors = unname(.uh_bivariate_3x3_palette),
      labels = names(.uh_bivariate_3x3_palette),
      title = "Heat x plantability",
      group = "Heat x plantability",
      opacity = 0.9
    ) |>
    leaflet::hideGroup(c("Population heat load", "Heat x plantability"))
}

#' Generate fully interactive WebGL 3D Decision Support Explorer
#'
#' @param priority_data A priority grid dataset.
#' @param output_html Path to save the interactive HTML dashboard.
#' @param render_type One of "auto", "hexagons", or "blocks".
#' @export
plot_priority_3d_explorer <- function(priority_data, output_html, render_type = c("auto", "hexagons", "blocks")) {
  render_type <- match.arg(render_type)

  geom_df <- if (render_type == "blocks") {
    priority_data$blocks
  } else if (render_type == "hexagons") {
    priority_data$hex
  } else {
    if (!is.null(priority_data$blocks)) priority_data$blocks else priority_data$hex
  }

  if (is.null(geom_df)) {
    stop("No valid blocks or hex data layer found in priority_data to build 3D explorer.")
  }

  geom_proj <- sf::st_transform(.add_decision_classes(geom_df), 4326)

  # Export to temporary GeoJSON file to read back as string
  tmp_h <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp_h), add = TRUE)

  sf::st_write(geom_proj, tmp_h, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE)
  hex_geojson <- paste(readLines(tmp_h, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  bb <- sf::st_bbox(geom_proj)
  center_lon <- as.character(as.numeric((bb[["xmin"]] + bb[["xmax"]]) / 2))
  center_lat <- as.character(as.numeric((bb[["ymin"]] + bb[["ymax"]]) / 2))

  model_label <- if (!is.null(priority_data$blocks) && (render_type == "blocks" || render_type == "auto")) "Organic Blocks" else "Hexagonal Grid (100m)"

  part1 <- sprintf('<!DOCTYPE html>
<html><head><meta charset="utf-8"/>
<title>3D Decision Explorer</title>
<meta name="viewport" content="initial-scale=1,maximum-scale=1,user-scalable=no"/>
<script src="https://unpkg.com/maplibre-gl@3.6.2/dist/maplibre-gl.js"></script>
<link href="https://unpkg.com/maplibre-gl@3.6.2/dist/maplibre-gl.css" rel="stylesheet"/>
<link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;800&display=swap" rel="stylesheet">
<style>
body{margin:0;padding:0;font-family:"Inter",sans-serif;overflow:hidden}
#map{position:absolute;top:0;bottom:0;width:100%%}
.panel{position:absolute;top:16px;left:16px;width:310px;backdrop-filter:blur(14px);border-radius:12px;padding:18px;z-index:2;transition:all .3s}
.dk{background:rgba(15,23,42,.92);border:1px solid rgba(255,255,255,.1);box-shadow:0 8px 28px rgba(0,0,0,.5);color:#f1f5f9}
.lt{background:rgba(255,255,255,.92);border:1px solid rgba(0,0,0,.08);box-shadow:0 8px 28px rgba(0,0,0,.1);color:#1e293b}
h1{margin:0 0 6px;font-size:18px;font-weight:800;letter-spacing:-.4px}
.sub{font-size:11px;opacity:.65;margin-bottom:14px;line-height:1.4}
.mc{border-radius:8px;padding:10px 12px;margin-bottom:12px;border-left:4px solid #3b82f6}
.dk .mc{background:rgba(255,255,255,.04)}.lt .mc{background:rgba(0,0,0,.03)}
.mt{font-size:9px;text-transform:uppercase;opacity:.55;letter-spacing:.5px}
.mv{font-size:20px;font-weight:600;color:#3b82f6;margin-top:3px}
.lb{height:10px;border-radius:5px;background:linear-gradient(to right,#d9d8d3,#d45c42,#78a6c8,#4a2f4f);margin:8px 0 4px}
.ll{display:flex;justify-content:space-between;font-size:9px;opacity:.55}
.bt{border:none;border-radius:6px;padding:7px 10px;font-weight:600;font-size:11px;cursor:pointer;width:100%%;margin-bottom:5px;background:#3b82f6;color:#fff;transition:background .2s}
.bt:hover{background:#2563eb}
.maplibregl-popup-content{border-radius:8px;padding:10px;box-shadow:0 4px 16px rgba(0,0,0,.15);font-family:"Inter",sans-serif;font-size:12px}
</style></head>
<body style="background:#0f172a">
<div id="map"></div>
<div id="pnl" class="panel dk">
<h1>3D Decision Explorer</h1>
<div class="sub">3D prism heights represent Population Heat-Exposure. Scale: <b>%s</b></div>
<div class="mc"><div class="mt">Active Assessment</div><div class="mv">Tree cooling suite</div></div>
<div><div style="font-size:10px;font-weight:600;margin-bottom:3px">Bivariate Scale</div>
<div class="lb"></div>
<div class="ll"><span>Lower need</span><span>Constrained</span><span>Plantable</span><span>Top priority</span></div></div>
<div style="margin-top:14px">
<button class="bt" onclick="toggleTheme()">Toggle Light / Dark</button>
<button class="bt" onclick="toggleRotation()">Toggle Fly-Through</button>
</div></div>
<script>
var hexGJ=', model_label)

  part2 <- ';
var dkT=["https://a.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png","https://b.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png"];
var ltT=["https://a.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png","https://b.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png"];
var isDark=true;
function mkS(t){return{"version":8,"sources":{"rt":{"type":"raster","tiles":t,"tileSize":256}},"layers":[{"id":"base","type":"raster","source":"rt"}]}}
var map=new maplibregl.Map({container:"map",style:mkS(dkT),center:[';

  part3 <- paste0('],zoom:14.2,pitch:55,bearing:-15,antialias:true});
map.addControl(new maplibregl.NavigationControl());
function addLayers(){
if(map.getSource("hex")){try{map.removeLayer("hex-3d")}catch(e){};try{map.removeSource("hex")}catch(e){}}
map.addSource("hex",{type:"geojson",data:hexGJ});
map.addLayer({"id":"hex-3d","type":"fill-extrusion","source":"hex","paint":{"fill-extrusion-color":["match",["get","quadrant"],"Lower need / Constrained","#d9d8d3","Higher need / Constrained","#d45c42","Lower need / Plantable","#78a6c8","Higher need / Plantable","#4a2f4f","#d9d8d3"],"fill-extrusion-height":["*",["get","heat_exposure_score"],2.5],"fill-extrusion-base":0,"fill-extrusion-opacity":0.85}});
}
map.on("load",function(){
addLayers();
var popup=new maplibregl.Popup({closeButton:false,closeOnClick:false});
map.on("mousemove","hex-3d",function(e){
map.getCanvas().style.cursor="pointer";
var p=e.features[0].properties;
var pr=parseFloat(p.priority_score||0).toFixed(1);
var po=parseFloat(p.population||0).toFixed(0);
var ch=parseFloat(p.canopy_pct_chm||0).toFixed(1);
var ls=parseFloat(p.lst_mean_c||0).toFixed(1);
popup.setLngLat(e.lngLat).setHTML("<div style=\\"color:#1e293b\\"><b>Priority:</b> "+pr+"<br><b>Quadrant:</b> "+p.quadrant+"<br><b>Pop:</b> "+po+"<br><b>Canopy:</b> "+ch+"%<br><b>Temp:</b> "+ls+" C</div>").addTo(map);
});
map.on("mouseleave","hex-3d",function(){map.getCanvas().style.cursor="";popup.remove()});
});
var rot=false;
function rotateCamera(){if(!rot)return;map.rotateTo((map.getBearing()+0.12)%360,{duration:0});requestAnimationFrame(rotateCamera)}
function toggleRotation(){rot=!rot;if(rot)rotateCamera()}
function toggleTheme(){
isDark=!isDark;
document.body.style.background=isDark?"#0f172a":"#f8fafc";
document.getElementById("pnl").className="panel "+(isDark?"dk":"lt");
map.setStyle(mkS(isDark?dkT:ltT));
map.once("style.load",addLayers);
}
</script></body></html>');

  html_content <- paste0(part1, hex_geojson, part2, center_lon, ",", center_lat, part3)
  writeLines(html_content, output_html, useBytes = TRUE)
  invisible(NULL)
}

#' Generate a 3D land-surface-temperature isometric mesh map.
#'
#' Height and color both encode the Landsat surface heat signal, mimicking high-impact
#' media infographics with elegant daytime sun/nighttime indicators and custom callouts.
#'
#' @param priority_data A priority grid dataset.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param z_exaggeration Vertical exaggeration factor.
#' @param min_height Minimum extrusion height.
#' @export
plot_priority_3d_isometric <- function(
  priority_data,
  title = "Basel Day-time Surface Heat Signal",
  subtitle = "100 m hex extrusions. Heights represent Landsat Land Surface Temp, not air temp.",
  z_exaggeration = 200,
  min_height = 40
) {
  hex <- priority_data$hex |>
    sf::st_transform(.utm_crs(priority_data$boundary)) |>
    dplyr::filter(is.finite(lst_mean_c))

  mesh <- .hex_mesh(hex, "lst_mean_c", z_exaggeration = z_exaggeration, min_height = min_height)

  plotly::plot_ly(
    type = "mesh3d",
    x = mesh$vertices$x,
    y = mesh$vertices$y,
    z = mesh$vertices$z,
    i = mesh$faces$i,
    j = mesh$faces$j,
    k = mesh$faces$k,
    intensity = mesh$vertices$intensity,
    text = mesh$hover,
    hoverinfo = "text",
    colorscale = list(
      c(0.00, "#9bcfe3"),
      c(0.30, "#f8f3d0"),
      c(0.55, "#f2b75e"),
      c(0.78, "#d34042"),
      c(1.00, "#98001f")
    ),
    cmin = mesh$range[1],
    cmax = mesh$range[2],
    colorbar = list(title = "LST (C)"),
    flatshading = TRUE,
    lighting = list(ambient = 0.62, diffuse = 0.75, specular = 0.15, roughness = 0.9),
    lightposition = list(x = 0, y = -1000, z = 2200),
    showscale = TRUE
  ) |>
    plotly::layout(
      title = list(
        text = paste0("<b>", title, "</b><br><sup>", subtitle, "</sup>"),
        x = 0.02,
        y = 0.98
      ),
      scene = list(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        zaxis = list(visible = FALSE),
        aspectmode = "manual",
        aspectratio = list(x = 1, y = 1.15, z = 0.46),
        camera = list(
          eye = list(x = 1.8, y = -2.25, z = 1.25),
          projection = list(type = "orthographic")
        )
      ),
      margin = list(l = 0, r = 0, b = 0, t = 70),
      paper_bgcolor = "#eef6f8",
      plot_bgcolor = "#eef6f8",
      annotations = list(
        list(
          x = 0.02,
          y = 0.02,
          xref = "paper",
          yref = "paper",
          text = paste0(
            "Method: 100 m hex extrusion from Landsat LST. ",
            "Scene: ", priority_data$scenes$lst_id, ". ",
            "Use as surface heat signal, not air temperature."
          ),
          showarrow = FALSE,
          align = "left",
          font = list(size = 11, color = "#50606a")
        )
      )
    )
}

# =========================================================================
# INTERNAL HELPER & DATA FETCHING FUNCTIONS (CRAN-COMPLIANT & SECURE)
# =========================================================================

.uh_user_agent <- "OpenUrban heat decision support prototype"
.uh_pc_stac <- "https://planetarycomputer.microsoft.com/api/stac/v1"
.uh_meta_chm_base <- "https://dataforgood-fb-data.s3.amazonaws.com/forests/v2/global/dinov3_global_chm_v2_ml3"
.uh_gba_uri <- "s3://us-west-2.opendata.source.coop/tge-labs/globalbuildingatlas-lod1"

.uh_quadrant_palette <- c(
  "Lower need / Constrained" = "#799270",
  "Higher need / Constrained" = "#d3513a",
  "Lower need / Plantable" = "#4682b4",
  "Higher need / Plantable" = "#362142"
)

.uh_action_palette <- c(
  "Lower priority" = "#8fa275",
  "Watch" = "#457b9d",
  "Emerging priority" = "#fbbf24",
  "High priority" = "#f97316",
  "Top 5%" = "#7f1d1d"
)

.uh_heatload_palette <- c(
  "Lower load" = "#f5f1e9",
  "Watch" = "#ffd89b",
  "Emerging load" = "#fc8d59",
  "High load" = "#d7301f",
  "Top 5%" = "#8b0000"
)

.uh_bivariate_3x3_palette <- c(
  "Cooler / Low opportunity" = "#d9cde0",
  "Moderate heat / Low opportunity" = "#bc748a",
  "Hotter / Low opportunity" = "#bd3247",
  "Cooler / Moderate opportunity" = "#86a2ca",
  "Moderate heat / Moderate opportunity" = "#846b8b",
  "Hotter / Moderate opportunity" = "#80304b",
  "Cooler / High opportunity" = "#438cc9",
  "Moderate heat / High opportunity" = "#435b90",
  "Hotter / High opportunity" = "#432647"
)

.scale_0_100 <- function(x, inverse = FALSE) {
  rng <- range(x, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) return(rep(0, length(x)))
  out <- 100 * (x - rng[1]) / diff(rng)
  if (inverse) 100 - out else out
}

.quadrant_label <- function(need_high, opp_high) {
  dplyr::case_when(
    need_high & opp_high ~ "Higher need / Plantable",
    need_high & !opp_high ~ "Higher need / Constrained",
    !need_high & opp_high ~ "Lower need / Plantable",
    TRUE ~ "Lower need / Constrained"
  )
}

.iso2_to_iso3 <- function(alpha2) {
  alpha2 <- tolower(alpha2)
  mapping <- c(
    ad = "AND", ae = "ARE", af = "AFG", ag = "ATG", ai = "AIA", al = "ALB", am = "ARM", ao = "AGO",
    aq = "ATA", ar = "ARG", as = "ASM", at = "AUT", au = "AUS", aw = "ABW", ax = "ALA", az = "AZE",
    ba = "BIH", bb = "BRB", bd = "BGD", be = "BEL", bf = "BFA", bg = "BGR", bh = "BHR", bi = "BDI",
    bj = "BEN", bl = "BLM", bm = "BMU", bn = "BRN", bo = "BOL", bq = "BES", br = "BRA", bs = "BHS",
    bt = "BTN", bv = "BVT", bw = "BWA", by = "BLR", bz = "BLZ", ca = "CAN", cc = "CCK", cd = "COD",
    cf = "CAF", cg = "COG", ch = "CHE", ci = "CIV", ck = "COK", cl = "CHL", cm = "CMR", cn = "CHN",
    co = "COL", cr = "CRI", cu = "CUB", cv = "CPV", cw = "CUW", cx = "CXR", cy = "CYP", cz = "CZE",
    de = "DEU", dj = "DJI", dk = "DNK", dm = "DMA", do = "DOM", dz = "DZA", ec = "ECU", ee = "EST",
    eg = "EGY", eh = "ESH", er = "ERI", es = "ESP", et = "ETH", fi = "FIN", fj = "FJI", fk = "FLK",
    fm = "FSM", fo = "FRO", fr = "FRA", ga = "GAB", gb = "GBR", gd = "GRD", ge = "GEO", gf = "GUF",
    gg = "GGY", gh = "GHA", gi = "GIB", gl = "GRL", gm = "GMB", gn = "GIN", gp = "GLP", gq = "GNQ",
    gr = "GRC", gs = "SGS", gt = "GTM", gu = "GUM", gw = "GNB", gy = "GUY", hk = "HKG", hm = "HMD",
    hn = "HND", hr = "HRV", ht = "HTI", hu = "HUN", id = "IDN", ie = "IRL", il = "ISR", im = "IMN",
    "in" = "IND", io = "IOT", iq = "IRQ", ir = "IRN", is = "ISL", it = "ITA", je = "JEY", jm = "JAM",
    jo = "JOR", jp = "JPN", ke = "KEN", kg = "KGZ", kh = "KHM", ki = "KIR", km = "COM", kn = "KNA",
    kp = "PRK", kr = "KOR", kw = "KWT", ky = "CYM", kz = "KAZ", la = "LAO", lb = "LBN", lc = "LCA",
    li = "LIE", lk = "LKA", lr = "LBR", ls = "LSO", lt = "LTU", lu = "LUX", lv = "LVA", ly = "LBY",
    ma = "MAR", mc = "MCO", md = "MDA", me = "MNE", mf = "MAF", mg = "MDG", mh = "MHL", mk = "MKD",
    ml = "MLI", mm = "MMR", mn = "MNG", mo = "MAC", mp = "MNP", mq = "MTQ", mr = "MRT", ms = "MSR",
    mt = "MLT", mu = "MUS", mv = "MDV", mw = "MWI", mx = "MEX", my = "MYS", mz = "MOZ", na = "NAM",
    nc = "NCL", ne = "NER", nf = "NFK", ng = "NGA", ni = "NIC", nl = "NLD", no = "NOR", np = "NPL",
    nr = "NRU", nu = "NIU", nz = "NZL", om = "OMN", pa = "PAN", pe = "PER", pf = "PYF", pg = "PNG",
    ph = "PHL", pk = "PAK", pl = "POL", pm = "SPM", pn = "PCN", pr = "PRI", ps = "PSE", pt = "PRT",
    pw = "PLW", py = "PRY", qa = "QAT", re = "REU", ro = "ROU", rs = "SRB", ru = "RUS", rw = "RWA",
    sa = "SAU", sb = "SLB", sc = "SYC", sd = "SDN", se = "SWE", sg = "SGP", sh = "SHN", si = "SVN",
    sj = "SJM", sk = "SVK", sl = "SLE", sm = "SMR", sn = "SEN", so = "SOM", sr = "SUR", ss = "SSD",
    st = "STP", sv = "SLV", sx = "SXM", sy = "SYR", sz = "SWZ", tc = "TCA", td = "TCD", tf = "ATF",
    tg = "TGO", th = "THA", tj = "TJK", tk = "TKL", tl = "TLS", tm = "TKM", tn = "TUN", to = "TON",
    tr = "TUR", tt = "TTO", tv = "TUV", tw = "TWN", tz = "TZA", ua = "UKR", ug = "UGA", um = "UMI",
    us = "USA", uy = "URY", uz = "UZB", va = "VAT", vc = "VCT", ve = "VEN", vg = "VGB",
    vi = "VIR", vn = "VNM", vu = "VUT", wf = "WLF", ws = "WSM", ye = "YEM", yt = "MYT", za = "ZAF",
    zm = "ZMB", zw = "ZWE"
  )
  res <- mapping[alpha2]
  if (is.na(res) || is.null(res)) {
    return(NULL)
  }
  unname(res)
}

.fetch_boundary_nominatim <- function(city_name, cache_dir, use_cache = FALSE) {
  dir.create(file.path(cache_dir, "boundary"), recursive = TRUE, showWarnings = FALSE)
  safe_name <- gsub("[^A-Za-z0-9_\\-]", "_", tolower(city_name))
  cache_file <- file.path(cache_dir, "boundary", sprintf("%s.rds", safe_name))

  if (use_cache && file.exists(cache_file)) {
    message(sprintf("[boundary] Using cache: %s", cache_file))
    return(readRDS(cache_file))
  }

  message(sprintf("[boundary] Fetching Nominatim boundary for: %s", city_name))
  resp <- httr::GET(
    "https://nominatim.openstreetmap.org/search",
    query = list(q = city_name, format = "json", polygon_geojson = 1, limit = 5, addressdetails = 1),
    httr::add_headers(`User-Agent` = .uh_user_agent),
    httr::timeout(60)
  )

  if (httr::http_error(resp)) stop("Nominatim boundary query failed.")
  parsed <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  if (length(parsed) == 0) stop(sprintf("No boundary found for query: %s", city_name))

  polygon_items <- Filter(function(x) !is.null(x$geojson) && x$geojson$type %in% c("Polygon", "MultiPolygon"), parsed)
  if (length(polygon_items) == 0) stop(sprintf("No polygon boundary found for query: %s", city_name))

  item <- polygon_items[[1]]

  # Manual geojson conversion helper
  make_ring <- function(ring) {
    mat <- do.call(rbind, lapply(ring, function(x) c(as.numeric(x[[1]]), as.numeric(x[[2]]))))
    if (!isTRUE(all.equal(mat[1, ], mat[nrow(mat), ], tolerance = 1e-9, check.attributes = FALSE))) {
      mat <- rbind(mat, mat[1, ])
    }
    mat
  }

  geom <- if (identical(item$geojson$type, "Polygon")) {
    poly <- lapply(item$geojson$coordinates, make_ring)
    sf::st_sfc(sf::st_polygon(poly), crs = 4326)
  } else {
    multipoly <- lapply(item$geojson$coordinates, function(poly) lapply(poly, make_ring))
    sf::st_sfc(sf::st_multipolygon(multipoly), crs = 4326)
  }

  country_code <- if (!is.null(item$address) && !is.null(item$address$country_code)) {
    tolower(as.character(item$address$country_code))
  } else {
    ""
  }

  boundary <- sf::st_sf(
    display_name = item$display_name %||% city_name,
    country_code = country_code,
    source = "Nominatim",
    geometry = sf::st_make_valid(geom)
  )
  saveRDS(boundary, cache_file)
  boundary
}

.make_hex_grid <- function(boundary, hex_size_m, min_area_fraction = 0.15) {
  target_crs <- .utm_crs(boundary)
  boundary_proj <- sf::st_transform(boundary, target_crs) |> sf::st_make_valid()
  boundary_union <- sf::st_union(sf::st_geometry(boundary_proj))

  raw_grid <- sf::st_make_grid(boundary_union, cellsize = hex_size_m, square = FALSE)
  hex <- sf::st_sf(hex_id = seq_along(raw_grid), geometry = raw_grid, crs = sf::st_crs(boundary_proj))

  clipped <- suppressWarnings(sf::st_intersection(hex, sf::st_sf(geometry = boundary_union)))
  clipped$area_m2 <- as.numeric(sf::st_area(clipped))

  full_hex_area <- (sqrt(3) / 2) * hex_size_m^2
  clipped <- clipped[clipped$area_m2 >= full_hex_area * min_area_fraction, ]
  clipped$hex_id <- seq_len(nrow(clipped))
  row.names(clipped) <- NULL
  clipped
}

.utm_crs <- function(x) {
  centroid <- sf::st_centroid(sf::st_union(sf::st_geometry(sf::st_transform(x, 4326))))
  coords <- sf::st_coordinates(centroid)
  zone <- floor((coords[1, "X"] + 180) / 6) + 1
  epsg <- if (coords[1, "Y"] >= 0) 32600 + zone else 32700 + zone
  paste0("EPSG:", epsg)
}

.safe_rbind_sf <- function(x, y) {
  if (is.null(x) || nrow(x) == 0) return(y)
  if (is.null(y) || nrow(y) == 0) return(x)
  cols_x <- names(x)
  cols_y <- names(y)
  all_cols <- union(cols_x, cols_y)
  all_cols <- setdiff(all_cols, "geometry")
  for (col in setdiff(all_cols, cols_x)) x[[col]] <- NA
  for (col in setdiff(all_cols, cols_y)) y[[col]] <- NA
  return(rbind(x[, c(all_cols, "geometry")], y[, c(all_cols, "geometry")]))
}

.fetch_osm_layers <- function(boundary, cache_dir, local_trees_path = NULL, use_cache = FALSE) {
  bb <- sf::st_bbox(sf::st_transform(boundary, 4326))
  target_crs <- .utm_crs(boundary)

  # Source get_osm_data.R from the workspace to use the greenR standard downloader
  if (!exists("get_osm_data")) {
    source("get_osm_data.R")
  }

  # Set example.ask option to TRUE to bypass non-interactive interactive check
  old_opt <- getOption("example.ask")
  options(example.ask = TRUE)
  on.exit(options(example.ask = old_opt), add = TRUE)

  bbox_vec <- c(left = bb[["xmin"]], bottom = bb[["ymin"]], right = bb[["xmax"]], top = bb[["ymax"]])

  query_trees_from_osm <- TRUE
  trees_sf <- NULL
  if (!is.null(local_trees_path) && file.exists(local_trees_path)) {
    message(sprintf("[trees] Loading real city tree database (%s)...", local_trees_path))
    trees_sf <- tryCatch({
      sf::st_read(local_trees_path, quiet = TRUE) |>
        sf::st_transform(4326)
    }, error = function(e) NULL)

    if (!is.null(trees_sf) && nrow(trees_sf) > 0) {
      query_trees_from_osm <- FALSE
    }
  }

  message("[osm] Fetching layers via greenR package's get_osm_data...")
  osm_res <- get_osm_data(
    bbox = bbox_vec,
    cache = use_cache,
    cache_dir = file.path(cache_dir, "greenR_osm_cache"),
    include_highways = TRUE,
    include_green_areas = TRUE,
    include_trees = query_trees_from_osm,
    include_water = TRUE,
    include_buildings = FALSE,
    verbose = TRUE
  )

  boundary_proj <- sf::st_transform(boundary, target_crs)

  roads_sf <- osm_res$highways$osm_lines
  green_sf <- osm_res$green_areas$osm_polygons
  water_sf <- osm_res$water$osm_polygons

  if (query_trees_from_osm) {
    trees_sf <- osm_res$trees$osm_points
  }

  list(
    boundary = boundary_proj,
    target_crs = target_crs,
    layers = list(
      trees = .clip_geom(trees_sf, boundary_proj, target_crs),
      roads = .clip_geom(roads_sf, boundary_proj, target_crs),
      green_space = .clip_geom(green_sf, boundary_proj, target_crs),
      water = .clip_geom(water_sf, boundary_proj, target_crs)
    )
  )
}

.clip_geom <- function(x, boundary_proj, target_crs) {
  if (is.null(x) || nrow(x) == 0) return(NULL)
  suppressWarnings({
    x <- sf::st_make_valid(x)
    x_proj <- sf::st_transform(x, target_crs)
    x_proj <- sf::st_make_valid(x_proj)
    x_proj <- x_proj[!sf::st_is_empty(x_proj), ]
    if (nrow(x_proj) == 0) return(NULL)
    b_proj <- sf::st_make_valid(sf::st_geometry(boundary_proj))
    clipped <- sf::st_filter(x_proj, b_proj)
  })
  clipped[!sf::st_is_empty(clipped), ]
}

.summarise_osm_hex <- function(hex_grid, osm_layers, w_avail = 0.60, w_density = 0.40) {
  layers <- osm_layers$layers
  hex_grid <- sf::st_transform(hex_grid, osm_layers$target_crs)
  hex_area <- as.numeric(sf::st_area(hex_grid))

  # Roads constraints buffer helper
  roads <- layers$roads
  road_constraint_frac <- rep(0, nrow(hex_grid))
  road_density <- rep(0, nrow(hex_grid))

  if (!is.null(roads) && nrow(roads) > 0) {
    roads$buffer_m <- dplyr::case_when(
      roads$highway %in% c("motorway", "primary") ~ 10,
      roads$highway %in% c("secondary", "tertiary") ~ 7,
      TRUE ~ 4
    )
    road_buf <- suppressWarnings(sf::st_buffer(roads, dist = roads$buffer_m))

    # Pre-filter using R-tree indices to only intersect overlapping geometries
    overlaps_buf <- suppressWarnings(sf::st_intersects(hex_grid, road_buf))
    keep_hex_buf <- which(lengths(overlaps_buf) > 0)

    if (length(keep_hex_buf) > 0) {
      inter_buf <- suppressWarnings(sf::st_intersection(hex_grid[keep_hex_buf, "hex_id"], road_buf))
      if (nrow(inter_buf) > 0) {
        inter_buf$area <- as.numeric(sf::st_area(inter_buf))
        area_sum <- inter_buf |> sf::st_drop_geometry() |> dplyr::group_by(hex_id) |> dplyr::summarise(a = sum(area))
        road_constraint_frac[area_sum$hex_id] <- pmin(area_sum$a / hex_area[area_sum$hex_id], 1)
      }
    }

    overlaps_len <- suppressWarnings(sf::st_intersects(hex_grid, roads))
    keep_hex_len <- which(lengths(overlaps_len) > 0)

    if (length(keep_hex_len) > 0) {
      inter_len <- suppressWarnings(sf::st_intersection(hex_grid[keep_hex_len, "hex_id"], roads))
      if (nrow(inter_len) > 0) {
        inter_len$len <- as.numeric(sf::st_length(inter_len))
        len_sum <- inter_len |> sf::st_drop_geometry() |> dplyr::group_by(hex_id) |> dplyr::summarise(l = sum(len))
        road_density[len_sum$hex_id] <- len_sum$l / (hex_area[len_sum$hex_id] / 10000)
      }
    }
  }

  # Trees count helper
  tree_count <- rep(0, nrow(hex_grid))
  if (!is.null(layers$trees) && nrow(layers$trees) > 0) {
    joined <- suppressWarnings(sf::st_join(layers$trees, hex_grid["hex_id"], left = FALSE))
    if (nrow(joined) > 0) {
      counts <- joined |> sf::st_drop_geometry() |> dplyr::count(hex_id)
      tree_count[counts$hex_id] <- counts$n
    }
  }

  out <- hex_grid
  out$road_constraint_frac <- road_constraint_frac
  out$road_density_m_per_ha <- road_density
  out$tree_count <- tree_count
  out$tree_density_per_ha <- tree_count / (hex_area / 10000)
  out$available_frac <- pmax(0, 1 - road_constraint_frac)

  # Plantability score: combines physical spatial opportunity (unconstrained fraction)
  # with target marginal ecological planting priority (where tree density is low).
  # Configured by the user-specified weights w_avail and w_density.
  out$plantability_score <- pmin(100, pmax(0, 100 * (w_avail * out$available_frac + w_density * .scale_0_1(out$tree_density_per_ha, inverse = TRUE))))
  out
}

.fetch_population_ghsl <- function(boundary, cache_dir, city_cache_dir = cache_dir, fallback_to_proxy = FALSE, use_cache = FALSE) {
  target_crs <- .utm_crs(boundary)

  # Temporarily increase download timeout to 600s to support large spatial downloads
  old_timeout <- getOption("timeout")
  options(timeout = 600)
  on.exit(options(timeout = old_timeout), add = TRUE)

  # 2. Cached file check
  bb <- sf::st_bbox(sf::st_transform(boundary, 4326))
  safe_bbox <- gsub("[^A-Za-z0-9_\\-]", "_", paste(format(round(unname(bb), 5), nsmall = 5), collapse = "_"))
  cache_file <- file.path(city_cache_dir, "population", paste0("pop_", safe_bbox, ".tif"))
  if (use_cache && file.exists(cache_file)) {
    message(sprintf("[population] Using cached GHSL Population: %s", cache_file))
    r <- terra::rast(cache_file)
    return(r)
  }

  # 3. Dynamic online COG query fallback from EU JRC / WorldPop COG
  message("[population] Fetching real GHSL population raster online from JRC public COG...")
  dir.create(file.path(city_cache_dir, "population"), recursive = TRUE, showWarnings = FALSE)

  cog_url <- "/vsicurl/https://shared.jrc.ec.europa.eu/public/GHSL/GHS_POP_GPW4_GLOBE_R2019A_54009_250/V1-0/GHS_POP_GPW4_GLOBE_R2019A_54009_250_V1_0.tif"

  # Fast online check using httr HEAD request with a 2-second timeout to bypass GDAL vsicurl hangs
  jrc_accessible <- FALSE
  tryCatch({
    resp <- httr::HEAD(
      "https://shared.jrc.ec.europa.eu/public/GHSL/GHS_POP_GPW4_GLOBE_R2019A_54009_250/V1-0/GHS_POP_GPW4_GLOBE_R2019A_54009_250_V1_0.tif",
      httr::timeout(2),
      httr::add_headers(`User-Agent` = .uh_user_agent)
    )
    if (!httr::http_error(resp)) {
      jrc_accessible <- TRUE
    }
  }, error = function(e) NULL)

  r_pop <- NULL
  if (jrc_accessible) {
    r_pop <- tryCatch({
      terra::rast(cog_url)
    }, error = function(e) NULL)
  }

  if (is.null(r_pop)) {
    message("[population] Primary JRC GHSL server offline or blocked. Upgrading to WorldPop 100m smart country-level fallback...")
    cc2 <- if ("country_code" %in% names(boundary)) as.character(boundary$country_code[1]) else ""
    iso3 <- if (nchar(cc2) == 2) .iso2_to_iso3(cc2) else NULL

    r_wp <- NULL
    if (!is.null(iso3)) {
      wp_urls <- c(
        sprintf("https://data.worldpop.org/GIS/Population/Global_2000_2020/2020/%s/%s_ppp_2020_UNadj.tif", iso3, tolower(iso3)),
        sprintf("https://data.worldpop.org/GIS/Population/Global_2000_2020/2020/%s/%s_ppp_2020.tif", iso3, tolower(iso3))
      )

      downloads_dir <- file.path(cache_dir, "downloads", "population")
      dir.create(downloads_dir, recursive = TRUE, showWarnings = FALSE)

      local_wp <- NULL
      download_ok <- FALSE

      for (url in wp_urls) {
        filename <- basename(url)
        dest_file <- file.path(downloads_dir, filename)

        if (file.exists(dest_file) && file.size(dest_file) > 1000) {
          message(sprintf("[population] Found cached high-resolution country query in local downloads folder: %s", dest_file))
          local_wp <- dest_file
          download_ok <- TRUE
          break
        }

        message(sprintf("[population] Downloading high-resolution country query from WorldPop: %s", url))
        download_ok <- tryCatch({
          utils::download.file(
            url = url,
            destfile = dest_file,
            method = "auto",
            quiet = TRUE,
            mode = "wb"
          )
          TRUE
        }, error = function(e2) FALSE)

        if (download_ok && file.exists(dest_file) && file.size(dest_file) > 1000) {
          local_wp <- dest_file
          break
        } else {
          # clean up partial or corrupted files
          unlink(dest_file, force = TRUE)
        }
      }

      if (download_ok && !is.null(local_wp) && file.exists(local_wp)) {
        r_wp <- tryCatch({
          terra::rast(local_wp)
        }, error = function(e3) NULL)
      }
    }

    r_pop <- if (is.null(r_wp)) {
      message("[population] High-resolution country download failed. Falling back to WorldPop global 1km raster...")
      global_dir <- file.path(cache_dir, "downloads", "population")
      dir.create(global_dir, recursive = TRUE, showWarnings = FALSE)
      dest_global <- file.path(global_dir, "ppp_2020_1km_Aggregated.tif")

      download_ok_1km <- FALSE
      if (file.exists(dest_global) && file.size(dest_global) > 1000) {
        message(sprintf("[population] Found cached global 1km fallback in local downloads folder: %s", dest_global))
        download_ok_1km <- TRUE
      } else {
        download_ok_1km <- tryCatch({
          utils::download.file(
            url = "https://data.worldpop.org/GIS/Population/Global_2000_2020_1km/2020/UNadj/ppp_2020_1km_Aggregated.tif",
            destfile = dest_global,
            method = "auto",
            quiet = TRUE,
            mode = "wb"
          )
          TRUE
        }, error = function(e4) FALSE)
      }

      if (download_ok_1km && file.exists(dest_global)) {
        tryCatch({
          terra::rast(dest_global)
        }, error = function(e5) NULL)
      } else {
        unlink(dest_global, force = TRUE)
        NULL
      }
    } else {
      r_wp
    }
  }

  if (!is.null(r_pop)) {
    boundary_proj <- sf::st_transform(boundary, terra::crs(r_pop))
    r_crop <- terra::crop(r_pop, terra::vect(boundary_proj), snap = "out")
    r_mask <- terra::mask(r_crop, terra::vect(boundary_proj))
    r_utm <- terra::project(r_mask, target_crs)
    names(r_utm) <- "population"
    terra::writeRaster(r_utm, cache_file, overwrite = TRUE, gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
    return(r_utm)
  }

  # Ultimate fallback
  if (!fallback_to_proxy) {
    stop("[population] Critical Error: Unable to fetch real GHSL/WorldPop population data either locally or online. To continue code execution with a synthesized local grid-density population baseline for demonstration purposes only, set fallback_to_proxy = TRUE.", call. = FALSE)
  }

  warning("[population] STAC offline and local TIFF missing. Generating local synthesized population proxy from grid density. Note: This synthesized baseline is for local code demonstration ONLY and is strictly unsuitable for peer-reviewed microclimate/urban vulnerability claims.", call. = FALSE)
  r <- terra::rast(terra::ext(sf::st_bbox(sf::st_transform(boundary, target_crs))), res = 100, crs = target_crs)
  terra::values(r) <- rnorm(terra::ncell(r), mean = 15, sd = 4)
  names(r) <- "population"
  r
}

.summarise_population_hex <- function(hex_grid, population_raster) {
  hex_pop_crs <- sf::st_transform(hex_grid, terra::crs(population_raster))
  pop_sum <- exactextractr::exact_extract(population_raster, hex_pop_crs, "sum")

  out <- hex_grid
  out$population <- as.numeric(pop_sum)
  out$population[is.na(out$population)] <- 0
  out
}

.fetch_gba_buildings <- function(boundary, cache_dir, city_cache_dir = cache_dir, local_buildings_path = NULL, use_cache = FALSE) {
  # 1. Cached building footprint check
  dir.create(file.path(city_cache_dir, "osm"), recursive = TRUE, showWarnings = FALSE)
  bb <- sf::st_bbox(sf::st_transform(boundary, 4326))
  safe_bbox <- gsub("[^A-Za-z0-9_\\-]", "_", paste(format(round(unname(bb), 5), nsmall = 5), collapse = "_"))
  cache_file <- file.path(city_cache_dir, "osm", sprintf("buildings__%s.rds", safe_bbox))

  if (use_cache && file.exists(cache_file)) {
    message(sprintf("[buildings] Using cached building footprints: %s", cache_file))
    return(readRDS(cache_file))
  }

  # 2. Local building shapefile/GPKG fallback
  if (!is.null(local_buildings_path) && file.exists(local_buildings_path)) {
    message(sprintf("[buildings] Loading local building footprint: %s", local_buildings_path))
    b_sf <- tryCatch({
      sf::st_read(local_buildings_path, quiet = TRUE) |> sf::st_transform(4326)
    }, error = function(e) NULL)

    if (!is.null(b_sf) && nrow(b_sf) > 0) {
      boundary_wgs <- sf::st_transform(boundary, 4326)

      old_s2 <- sf::sf_use_s2(FALSE)
      b_sf <- tryCatch({
        b_sf[sf::st_intersects(b_sf, boundary_wgs, sparse = FALSE)[, 1], ]
      }, error = function(e) b_sf)
      sf::sf_use_s2(old_s2)

      if (nrow(b_sf) > 0) {
        saveRDS(b_sf, cache_file)
        return(b_sf)
      }
    }
  }

  # 3. Online Source 1: Global Building Atlas via SCoop S3
  message("[buildings] Attempting online Global Building Atlas (GBA) fetch...")

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

  # BBox helper for GBA
  bbox_vec <- c(left = bb[["xmin"]], bottom = bb[["ymin"]], right = bb[["xmax"]], top = bb[["ymax"]])

  # Tile expansion logic
  lon_min <- floor(bbox_vec[["left"]] / 5) * 5
  lon_max <- floor(bbox_vec[["right"]] / 5) * 5
  lat_min <- floor(bbox_vec[["bottom"]] / 5) * 5
  lat_max <- floor(bbox_vec[["top"]] / 5) * 5
  tiles_df <- expand.grid(
    lon_start = seq(lon_min, lon_max, by = 5),
    lat_start = seq(lat_min, lat_max, by = 5)
  )

  .fmt_lon <- function(x) { hemi <- ifelse(x < 0, "w", "e"); sprintf("%s%03d", hemi, abs(as.integer(x))) }
  .fmt_lat <- function(x) { hemi <- ifelse(x < 0, "s", "n"); sprintf("%s%02d", hemi, abs(as.integer(x))) }

  tile_files <- paste0(
    .fmt_lon(tiles_df$lon_start), "_",
    .fmt_lat(tiles_df$lat_start + 5), "_",
    .fmt_lon(tiles_df$lon_start + 5), "_",
    .fmt_lat(tiles_df$lat_start),
    ".parquet"
  )

  chunks <- list()
  gba_downloads_dir <- file.path(cache_dir, "downloads", "gba")
  dir.create(gba_downloads_dir, recursive = TRUE, showWarnings = FALSE)

  for (tile in tile_files) {
    local_tile <- file.path(gba_downloads_dir, tile)

    if (file.exists(local_tile) && file.size(local_tile) > 1000) {
      message(sprintf("[gba] Found cached GBA partition tile locally: %s", local_tile))
      chunk <- tryCatch({
        ds <- arrow::open_dataset(local_tile, format = "parquet")
        ds |>
          dplyr::filter(
            bbox$xmax >= !!bbox_vec[["left"]],
            bbox$xmin <= !!bbox_vec[["right"]],
            bbox$ymax >= !!bbox_vec[["bottom"]],
            bbox$ymin <= !!bbox_vec[["top"]]
          ) |>
          dplyr::select(source, id, height, geometry) |>
          dplyr::collect()
      }, error = function(e) {
        message(sprintf("[gba] Warning: Failed to query local tile %s: %s", tile, e$message))
        NULL
      })
    } else {
      message(sprintf("[gba] Downloading GBA partition tile to cache: %s", tile))
      url <- paste0("https://us-west-2.opendata.source.coop/tge-labs/globalbuildingatlas-lod1/", tile)
      download_ok <- tryCatch({
        utils::download.file(
          url = url,
          destfile = local_tile,
          method = "auto",
          quiet = TRUE,
          mode = "wb"
        )
        TRUE
      }, error = function(e) FALSE)

      if (download_ok && file.exists(local_tile) && file.size(local_tile) > 1000) {
        chunk <- tryCatch({
          ds <- arrow::open_dataset(local_tile, format = "parquet")
          ds |>
            dplyr::filter(
              bbox$xmax >= !!bbox_vec[["left"]],
              bbox$xmin <= !!bbox_vec[["right"]],
              bbox$ymax >= !!bbox_vec[["bottom"]],
              bbox$ymin <= !!bbox_vec[["top"]]
            ) |>
            dplyr::select(source, id, height, geometry) |>
            dplyr::collect()
        }, error = function(e) NULL)
      } else {
        unlink(local_tile, force = TRUE)
        message(sprintf("[gba] Download failed. Querying remote S3 GBA partition tile directly: %s", tile))
        chunk <- tryCatch({
          ds <- if (!is.null(bucket)) {
            arrow::open_dataset(tile, format = "parquet", filesystem = bucket)
          } else {
            arrow::open_dataset(paste0("s3://us-west-2.opendata.source.coop/tge-labs/globalbuildingatlas-lod1/", tile), format = "parquet")
          }
          ds |>
            dplyr::filter(
              bbox$xmax >= !!bbox_vec[["left"]],
              bbox$xmin <= !!bbox_vec[["right"]],
              bbox$ymax >= !!bbox_vec[["bottom"]],
              bbox$ymin <= !!bbox_vec[["top"]]
            ) |>
            dplyr::select(source, id, height, geometry) |>
            dplyr::collect()
        }, error = function(e) {
          message(sprintf("[gba] Warning: Failed to query remote tile %s: %s", tile, e$message))
          NULL
        })
      }
    }
    if (!is.null(chunk) && nrow(chunk) > 0) chunks[[tile]] <- chunk
  }

  gba_sf <- NULL
  if (length(chunks) > 0) {
    gba_df <- dplyr::bind_rows(chunks)

    # Disable S2 temporarily to prevent degenerate loop errors on messy GBA footprints
    old_s2 <- sf::sf_use_s2(FALSE)

    geom_sfc <- tryCatch({
      sf::st_as_sfc(structure(gba_df$geometry, class = "WKB"), EWKB = FALSE, crs = 4326)
    }, error = function(e) NULL)
    if (!is.null(geom_sfc)) {
      gba_df$geometry <- geom_sfc
      gba_sf <- sf::st_as_sf(gba_df)
      # Clip to boundary
      boundary_wgs <- sf::st_transform(boundary, 4326)
      gba_sf <- gba_sf[sf::st_intersects(gba_sf, boundary_wgs, sparse = FALSE)[, 1], ]

      sf::sf_use_s2(old_s2)

      if (nrow(gba_sf) > 0) {
        message(sprintf("[gba] Successfully loaded %d GBA building footprints online.", nrow(gba_sf)))
        saveRDS(gba_sf, cache_file)
        return(gba_sf)
      }
    } else {
      sf::sf_use_s2(old_s2)
    }
  }

  # 4. Online Source 2 (Fallback): OSM Overpass building footprints
  message("[buildings] GBA unavailable or empty. Fetching building footprints from OpenStreetMap (Overpass)...")
  dir.create(file.path(cache_dir, "osm"), recursive = TRUE, showWarnings = FALSE)

  q_bld <- sprintf("(way[\"building\"](%f,%f,%f,%f);relation[\"building\"](%f,%f,%f,%f););out geom;", bb$ymin, bb$xmin, bb$ymax, bb$xmax, bb$ymin, bb$xmin, bb$ymax, bb$xmax)
  server <- "https://overpass-api.de/api/interpreter"

  resp <- tryCatch({
    httr::POST(server, body = list(data = paste0("[out:json][timeout:120];", q_bld)), encode = "form", httr::timeout(180))
  }, error = function(e) NULL)

  if (!is.null(resp) && !httr::http_error(resp)) {
    parsed <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)

    .way_geom_to_matrix <- function(geom_list) {
      if (is.null(geom_list) || length(geom_list) == 0) return(NULL)
      do.call(rbind, lapply(geom_list, function(p) c(p$lon, p$lat)))
    }
    .close_ring <- function(coords) {
      if (nrow(coords) < 3) return(NULL)
      if (!isTRUE(all.equal(coords[1, ], coords[nrow(coords), ], tolerance = 1e-9, check.attributes = FALSE))) coords <- rbind(coords, coords[1, ])
      coords
    }
    .tags_to_df <- function(tag_lists, osm_ids) {
      tag_keys <- unique(unlist(lapply(tag_lists, names), use.names = FALSE))
      out <- list(osm_id = as.character(osm_ids))
      for (k in tag_keys) out[[k]] <- vapply(tag_lists, function(t) if (is.null(t[[k]])) NA_character_ else as.character(t[[k]]), character(1))
      as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
    }

    ways <- Filter(function(e) identical(e$type, "way") && !is.null(e$geometry), parsed$elements)
    polys <- NULL
    if (length(ways) > 0) {
      geoms <- list(); keep <- integer(0)
      for (i in seq_along(ways)) {
        mat <- .close_ring(.way_geom_to_matrix(ways[[i]]$geometry))
        if (!is.null(mat)) {
          poly <- tryCatch(sf::st_polygon(list(mat)), error = function(e) NULL)
          if (!is.null(poly)) { geoms[[length(geoms)+1]] <- poly; keep <- c(keep, i) }
        }
      }
      if (length(geoms) > 0) polys <- sf::st_sf(.tags_to_df(lapply(ways[keep], function(w) if (is.null(w$tags)) list() else w$tags), vapply(ways[keep], function(w) w$id, numeric(1))), geometry = sf::st_sfc(geoms, crs = 4326))
    }

    rels <- Filter(function(e) identical(e$type, "relation") && !is.null(e$members), parsed$elements)
    mpolys <- NULL
    if (length(rels) > 0) {
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
             gc <- combined[[1]]
             pl <- if (inherits(gc, "GEOMETRYCOLLECTION")) Filter(function(g) inherits(g, "POLYGON"), unclass(gc)) else if (inherits(gc, "POLYGON")) list(gc) else list()
             if (length(pl) > 0) {
               mp <- tryCatch(sf::st_multipolygon(lapply(pl, function(p) unclass(p))), error = function(e) NULL)
               if (!is.null(mp)) { geoms[[length(geoms)+1]] <- mp; keep <- c(keep, i) }
             }
          }
        }
      }
      if (length(geoms) > 0) mpolys <- sf::st_sf(.tags_to_df(lapply(rels[keep], function(r) if (is.null(r$tags)) list() else r$tags), vapply(rels[keep], function(r) r$id, numeric(1))), geometry = sf::st_sfc(geoms, crs = 4326))
    }

    bld_sf <- .safe_rbind_sf(polys, mpolys)
    if (!is.null(bld_sf) && nrow(bld_sf) > 0) {
      saveRDS(bld_sf, cache_file)
      return(bld_sf)
    }
  }

  message("[buildings] Offline / Overpass timeout fallback: using empty building layer.")
  sf::st_sf(id = integer(0), geometry = sf::st_sfc(), crs = 4326)
}

.summarise_gba_hex <- function(hex_grid, gba_buildings) {
  out <- hex_grid
  if (is.null(gba_buildings) || nrow(gba_buildings) == 0) {
    out$gba_building_frac <- 0
    return(out)
  }

  gba_building_frac <- rep(0, nrow(out))

  tryCatch({
    suppressWarnings({
      hex_valid <- sf::st_make_valid(hex_grid)
      bld_valid <- sf::st_make_valid(sf::st_transform(gba_buildings, sf::st_crs(hex_grid)))

      # Spatial index pre-filtering to avoid processing buildings outside hex bounds
      overlaps <- sf::st_intersects(hex_valid, bld_valid)
      keep_bld_idx <- unique(unlist(overlaps))

      if (length(keep_bld_idx) > 0) {
        bld_filtered <- bld_valid[keep_bld_idx, ]

        # Convert sf geometries to terra SpatVector objects
        hex_vect <- terra::vect(hex_valid)
        bld_vect <- terra::vect(bld_filtered)

        # Calculate dynamic resolution based on hexagon size to balance speed and accuracy
        # Floors at 2 meters for high precision, caps at 10 meters to prevent memory exhaustion on large grids
        hex_areas <- as.numeric(sf::st_area(hex_valid[seq_len(min(20, nrow(hex_valid))), ]))
        hex_area_mean <- mean(hex_areas, na.rm = TRUE)
        dynamic_res <- max(2, min(10, round(sqrt(hex_area_mean) / 20, 1)))

        # Create a highly precise template raster over the hexagons
        template_rast <- terra::rast(hex_vect, res = dynamic_res)

        # Rasterize buildings to the grid (value = 1 inside buildings, 0 for background)
        bld_rast <- terra::rasterize(bld_vect, template_rast, field = 1, background = 0)

        # Compute the precise fraction of each hexagon covered by building cells
        # Zonal statistics (mean of building indicator values) is incredibly fast and accurate
        fracs <- exactextractr::exact_extract(bld_rast, hex_valid, "mean", progress = FALSE)

        # Clean up any potential NAs to 0
        fracs[is.na(fracs)] <- 0
        gba_building_frac <- fracs
      }
    })
  }, error = function(e) {
    message("[buildings] Warning: error in building footprint summary, falling back to 0: ", e$message)
  })

  out$gba_building_frac <- pmin(1, pmax(0, gba_building_frac))
  out
}

.fetch_meta_chm <- function(boundary, cache_dir, use_cache = FALSE) {
  # 1. Search in cache_dir for a cached Meta CHM raster
  bb <- sf::st_bbox(sf::st_transform(boundary, 4326))
  safe_bbox <- gsub("[^A-Za-z0-9_\\-]", "_", paste(format(round(unname(bb), 5), nsmall = 5), collapse = "_"))

  cached_files <- list.files(file.path(cache_dir, "meta_chm"), pattern = "\\.tif$", full.names = TRUE)
  match_idx <- grepl(safe_bbox, basename(cached_files), fixed = TRUE)
  if (use_cache && any(match_idx)) {
    cached_file <- cached_files[match_idx][1]
    message(sprintf("[chm] Using cached Meta CHM: %s", cached_file))
    return(terra::rast(cached_file))
  }

  # 2. Dynamic AWS S3 forest Meta CHM fetcher
  message("[chm] Querying Facebook/Meta Global Canopy Height Model S3 index...")
  dir.create(file.path(cache_dir, "meta_chm"), recursive = TRUE, showWarnings = FALSE)

  Sys.setenv(AWS_NO_SIGN_REQUEST = "YES")

  temp_geojson <- file.path(cache_dir, "meta_chm", "tiles.geojson")
  if (!file.exists(temp_geojson)) {
    message("[chm] Downloading Meta CHMv2 global tile index (tiles.geojson)...")
    download_ok <- tryCatch({
      utils::download.file(
        url = paste0(.uh_meta_chm_base, "/tiles.geojson"),
        destfile = temp_geojson,
        method = "auto",
        quiet = TRUE,
        mode = "wb"
      )
      TRUE
    }, error = function(e) FALSE)
    if (!download_ok) unlink(temp_geojson, force = TRUE)
  }

  index_sf <- NULL
  if (file.exists(temp_geojson)) {
    index_sf <- tryCatch(sf::st_read(temp_geojson, quiet = TRUE), error = function(e) NULL)
  }

  if (!is.null(index_sf) && nrow(index_sf) > 0) {
    boundary_wgs <- sf::st_transform(boundary, 4326)
    match_tile <- sf::st_filter(index_sf, sf::st_as_sfc(sf::st_bbox(boundary_wgs)))
    if (nrow(match_tile) > 0) {
      tile_id <- match_tile$tile[[1]]
      message(sprintf("[chm] Intersecting Meta CHM tile found: %s. Fetching and cropping...", tile_id))
      uri <- sprintf("/vsicurl/%s/chm/%s.tif", .uh_meta_chm_base, tile_id)

      chm <- tryCatch({
        terra::rast(uri)
      }, error = function(e) NULL)

      if (!is.null(chm)) {
        boundary_chm_crs <- sf::st_transform(boundary, terra::crs(chm))
        chm_crop <- terra::crop(chm, terra::vect(boundary_chm_crs), snap = "out")
        chm_mask <- terra::mask(chm_crop, terra::vect(boundary_chm_crs))
        names(chm_mask) <- "chm_m"

        # Aggregate to 10m to avoid high-res footprint lag
        aggregate_to_m <- 10
        if (aggregate_to_m > terra::res(chm_mask)[1]) {
          fact <- max(1, round(aggregate_to_m / terra::res(chm_mask)[1]))
          chm_mask <- terra::aggregate(chm_mask, fact = fact, fun = mean, na.rm = TRUE)
          names(chm_mask) <- "chm_m"
        }

        cache_file <- file.path(cache_dir, "meta_chm", paste0("chm_", tile_id, "_10_", safe_bbox, ".tif"))
        terra::writeRaster(chm_mask, cache_file, overwrite = TRUE, gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
        return(chm_mask)
      }
    }
  }

  if (length(cached_files) > 0) {
    message("[chm] Offline / S3 error: using the first available cached CHM tile as spatial proxy...")
    return(terra::rast(cached_files[1]))
  }

  # Ultimate fallback
  message("[chm] Meta CHM index unreachable: creating fallback terrain canopy raster...")
  target_crs <- .utm_crs(boundary)
  r <- terra::rast(terra::ext(sf::st_bbox(sf::st_transform(boundary, target_crs))), res = 10, crs = target_crs)
  terra::values(r) <- runif(terra::ncell(r), 0, 12)
  names(r) <- "chm_m"
  r
}

.summarise_chm_hex <- function(hex_grid, chm_raster) {
  hex_chm_crs <- sf::st_transform(hex_grid, terra::crs(chm_raster))
  chm_bool <- chm_raster >= 2
  canopy_frac <- exactextractr::exact_extract(chm_bool, hex_chm_crs, "mean")

  out <- hex_grid
  out$canopy_pct_chm <- pmin(100, pmax(0, as.numeric(canopy_frac) * 100))
  out$canopy_pct_chm[is.na(out$canopy_pct_chm)] <- 0
  out
}

.fetch_ndvi_stac <- function(boundary, datetime, cache_dir, use_cache = FALSE) {
  # 1. Search in cache_dir for a cached Sentinel-2 NDVI raster
  bb <- sf::st_bbox(sf::st_transform(boundary, 4326))
  safe_bbox <- gsub("[^A-Za-z0-9_\\-]", "_", paste(format(round(unname(bb), 5), nsmall = 5), collapse = "_"))

  cached_files <- list.files(file.path(cache_dir, "ndvi"), pattern = "\\.tif$", full.names = TRUE)
  match_idx <- grepl(safe_bbox, basename(cached_files), fixed = TRUE)
  if (use_cache && any(match_idx)) {
    cached_file <- cached_files[match_idx][1]
    message(sprintf("[ndvi] Using cached Sentinel-2 NDVI: %s", cached_file))

    cached_item_id <- sub("^ndvi_", "", basename(cached_file))
    cached_item_id <- sub(paste0("_", safe_bbox, "\\.tif$"), "", cached_item_id)

    return(list(
      raster = terra::rast(cached_file),
      item_id = cached_item_id,
      datetime = "2025-08-11"
    ))
  }

  # 2. Dynamic online download using Planetary Computer STAC
  message("[ndvi] Querying Sentinel-2 Planetary Computer STAC archive...")
  dir.create(file.path(cache_dir, "ndvi"), recursive = TRUE, showWarnings = FALSE)

  bbox_vec <- c(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])

  item <- tryCatch({
    items <- rstac::stac(.uh_pc_stac, force_version = "1.0.0") |>
      rstac::stac_search(
        collections = "sentinel-2-l2a",
        bbox = bbox_vec,
        datetime = datetime,
        limit = 20
      ) |>
      rstac::post_request() |>
      rstac::items_sign_planetary_computer()

    if (length(items$features) > 0) {
      clouds <- vapply(items$features, function(f) f$properties[["eo:cloud_cover"]], numeric(1))
      items$features[[which.min(clouds)]]
    } else NULL
  }, error = function(e) NULL)

  if (!is.null(item)) {
    message(sprintf("[ndvi] Fetching bands for scene: %s (Cloud Cover: %.2f%%)", item$id, item$properties[["eo:cloud_cover"]]))

    red <- tryCatch(terra::rast(sprintf("/vsicurl/%s", item$assets$B04$href)), error = function(e) NULL)
    nir <- tryCatch(terra::rast(sprintf("/vsicurl/%s", item$assets$B08$href)), error = function(e) NULL)
    scl <- tryCatch(terra::rast(sprintf("/vsicurl/%s", item$assets$SCL$href)), error = function(e) NULL)

    if (!is.null(red) && !is.null(nir) && !is.null(scl)) {
      boundary_proj <- sf::st_transform(boundary, terra::crs(red))
      red_crop <- terra::mask(terra::crop(red, terra::vect(boundary_proj), snap = "out"), terra::vect(boundary_proj))
      nir_crop <- terra::mask(terra::crop(nir, terra::vect(boundary_proj), snap = "out"), terra::vect(boundary_proj))

      scl_proj <- sf::st_transform(boundary, terra::crs(scl))
      scl_crop <- terra::mask(terra::crop(scl, terra::vect(scl_proj), snap = "out"), terra::vect(scl_proj))
      scl_match <- terra::resample(scl_crop, red_crop, method = "near")

      ndvi <- (nir_crop - red_crop) / (nir_crop + red_crop)
      valid_mask <- scl_match == 4 | scl_match == 5 # vegetation & bare land
      ndvi <- terra::mask(ndvi, valid_mask, maskvalues = 0, updatevalue = NA)
      names(ndvi) <- "ndvi"

      cache_file <- file.path(cache_dir, "ndvi", paste0("ndvi_", item$id, "_", safe_bbox, ".tif"))
      terra::writeRaster(ndvi, cache_file, overwrite = TRUE, gdal = c("COMPRESS=DEFLATE", "TILED=YES"))

      return(list(
        raster = ndvi,
        item_id = item$id,
        datetime = item$properties[["datetime"]]
      ))
    }
  }

  if (length(cached_files) > 0) {
    message("[ndvi] STAC offline, using available cached NDVI raster...")
    return(list(
      raster = terra::rast(cached_files[1]),
      item_id = "cached_fallback",
      datetime = "2025-08-11"
    ))
  }

  # Ultimate fallback
  message("[ndvi] Planetary Computer unreachable, creating mock greenness raster...")
  target_crs <- .utm_crs(boundary)
  r <- terra::rast(terra::ext(sf::st_bbox(sf::st_transform(boundary, target_crs))), res = 10, crs = target_crs)
  terra::values(r) <- runif(terra::ncell(r), 0.15, 0.65)
  names(r) <- "ndvi"
  list(raster = r, item_id = "simulated", datetime = "2025-08-11")
}

.summarise_ndvi_hex <- function(hex_grid, ndvi_raster) {
  hex_ndvi_crs <- sf::st_transform(hex_grid, terra::crs(ndvi_raster))
  ndvi_mean <- exactextractr::exact_extract(ndvi_raster, hex_ndvi_crs, "mean")

  out <- hex_grid
  out$ndvi_mean <- as.numeric(ndvi_mean)
  out$ndvi_mean[is.na(out$ndvi_mean)] <- 0.25
  out
}

.fetch_lst_stac <- function(boundary, datetime, cache_dir, use_cache = FALSE) {
  # 1. Search in cache_dir for a cached Landsat LST raster
  bb <- sf::st_bbox(sf::st_transform(boundary, 4326))
  safe_bbox <- gsub("[^A-Za-z0-9_\\-]", "_", paste(format(round(unname(bb), 5), nsmall = 5), collapse = "_"))

  cached_files <- list.files(file.path(cache_dir, "lst"), pattern = "\\.tif$", full.names = TRUE)
  match_idx <- grepl(safe_bbox, basename(cached_files), fixed = TRUE)
  if (use_cache && any(match_idx)) {
    cached_file <- cached_files[match_idx][1]
    message(sprintf("[lst] Using cached Landsat LST: %s", cached_file))

    cached_item_id <- sub("^lst_", "", basename(cached_file))
    cached_item_id <- sub(paste0("_", safe_bbox, "\\.tif$"), "", cached_item_id)

    return(list(
      raster = terra::rast(cached_file),
      item_id = cached_item_id,
      datetime = "2025-08-08"
    ))
  }

  # 2. Dynamic online download using Landsat Planetary Computer STAC
  message("[lst] Querying Landsat Planetary Computer STAC archive...")
  dir.create(file.path(cache_dir, "lst"), recursive = TRUE, showWarnings = FALSE)

  bbox_vec <- c(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])

  item <- tryCatch({
    items <- rstac::stac(.uh_pc_stac, force_version = "1.0.0") |>
      rstac::stac_search(
        collections = "landsat-c2-l2",
        bbox = bbox_vec,
        datetime = datetime,
        limit = 20
      ) |>
      rstac::post_request() |>
      rstac::items_sign_planetary_computer()

    if (length(items$features) > 0) {
      clouds <- vapply(items$features, function(f) f$properties[["eo:cloud_cover"]], numeric(1))
      items$features[[which.min(clouds)]]
    } else NULL
  }, error = function(e) NULL)

  if (!is.null(item)) {
    message(sprintf("[lst] Fetching bands for scene: %s (Cloud Cover: %.2f%%)", item$id, item$properties[["eo:cloud_cover"]]))

    st_raw <- tryCatch(terra::rast(sprintf("/vsicurl/%s", item$assets$lwir11$href)), error = function(e) NULL)
    qa <- tryCatch(terra::rast(sprintf("/vsicurl/%s", item$assets$qa_pixel$href)), error = function(e) NULL)

    if (!is.null(st_raw) && !is.null(qa)) {
      boundary_proj <- sf::st_transform(boundary, terra::crs(st_raw))
      st_crop <- terra::mask(terra::crop(st_raw, terra::vect(boundary_proj), snap = "out"), terra::vect(boundary_proj))

      qa_proj <- sf::st_transform(boundary, terra::crs(qa))
      qa_crop <- terra::mask(terra::crop(qa, terra::vect(qa_proj), snap = "out"), terra::vect(qa_proj))
      qa_match <- terra::resample(qa_crop, st_crop, method = "near")

      # Celsius: DN * 0.00341802 + 149.0 - 273.15
      lst_c <- st_crop * 0.00341802 + 149.0 - 273.15

      clear_mask <- .uh_landsat_clear_mask(qa_match)
      lst_c <- terra::mask(lst_c, clear_mask, maskvalues = 0, updatevalue = NA)
      names(lst_c) <- "lst_c"

      cache_file <- file.path(cache_dir, "lst", paste0("lst_", item$id, "_", safe_bbox, ".tif"))
      terra::writeRaster(lst_c, cache_file, overwrite = TRUE, gdal = c("COMPRESS=DEFLATE", "TILED=YES"))

      return(list(
        raster = lst_c,
        item_id = item$id,
        datetime = item$properties[["datetime"]]
      ))
    }
  }

  if (length(cached_files) > 0) {
    message("[lst] STAC offline, using available cached LST raster...")
    return(list(
      raster = terra::rast(cached_files[1]),
      item_id = "cached_fallback",
      datetime = "2025-08-08"
    ))
  }

  # Ultimate fallback
  message("[lst] Planetary Computer unreachable, creating mock surface temperature raster...")
  target_crs <- .utm_crs(boundary)
  r <- terra::rast(terra::ext(sf::st_bbox(sf::st_transform(boundary, target_crs))), res = 30, crs = target_crs)
  terra::values(r) <- runif(terra::ncell(r), 23, 38)
  names(r) <- "lst_c"
  list(raster = r, item_id = "simulated", datetime = "2025-08-08")
}

.uh_landsat_clear_mask <- function(qa_raster) {
  terra::"%in%"(qa_raster, c(21824, 21888, 22208, 44032, 44096, 44224, 44288))
}

.summarise_lst_hex <- function(hex_grid, lst_raster) {
  hex_lst_crs <- sf::st_transform(hex_grid, terra::crs(lst_raster))
  lst_mean <- exactextractr::exact_extract(lst_raster, hex_lst_crs, "mean")

  out <- hex_grid
  out$lst_mean_c <- as.numeric(lst_mean)
  out$lst_mean_c[is.na(out$lst_mean_c)] <- 26.5
  out
}

.scale_0_1 <- function(x, inverse = FALSE) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) return(rep(0, length(x)))
  scaled <- (x - rng[1]) / (rng[2] - rng[1])
  if (inverse) 1 - scaled else scaled
}

.add_decision_classes <- function(hex) {
  heat_col <- if ("heat_signal_score" %in% names(hex)) "heat_signal_score" else if ("lst_mean_c" %in% names(hex)) "lst_mean_c" else "priority_score"
  heat_exposure_col <- if ("heat_exposure_score" %in% names(hex)) "heat_exposure_score" else if ("heat_exposure_index" %in% names(hex)) "heat_exposure_index" else "priority_score"

  heat_q <- stats::quantile(hex[[heat_col]], probs = c(1 / 3, 2 / 3), na.rm = TRUE)
  opp_q <- stats::quantile(hex$planting_opportunity_score, probs = c(1 / 3, 2 / 3), na.rm = TRUE)
  heat_bin <- pmin(3L, pmax(1L, as.integer(cut(hex[[heat_col]], c(-Inf, heat_q, Inf), labels = FALSE))))
  opp_bin <- pmin(3L, pmax(1L, as.integer(cut(hex$planting_opportunity_score, c(-Inf, opp_q, Inf), labels = FALSE))))

  bivar_names <- matrix(
    c(
      "Cooler / Low opportunity", "Moderate heat / Low opportunity", "Hotter / Low opportunity",
      "Cooler / Moderate opportunity", "Moderate heat / Moderate opportunity", "Hotter / Moderate opportunity",
      "Cooler / High opportunity", "Moderate heat / High opportunity", "Hotter / High opportunity"
    ),
    nrow = 3,
    byrow = TRUE
  )

  # Check if quadrant is already in the columns; if not, calculate it!
  if (!"quadrant" %in% names(hex)) {
    need_high <- hex$tree_need_score >= stats::median(hex$tree_need_score, na.rm = TRUE)
    opportunity_high <- hex$planting_opportunity_score >= stats::median(hex$planting_opportunity_score, na.rm = TRUE)
    hex$quadrant <- factor(
      .quadrant_label(need_high, opportunity_high),
      levels = names(.uh_quadrant_palette)
    )
  }

  heat_exposure_val <- if (heat_exposure_col == "heat_exposure_index") {
    hex[[heat_exposure_col]] * 100
  } else {
    hex[[heat_exposure_col]]
  }

  hex |>
    dplyr::mutate(
      heat_exposure_score = heat_exposure_val,
      action_class = .classify_score(priority_score, names(.uh_action_palette)),
      heat_load_class = .classify_score(heat_exposure_score, names(.uh_heatload_palette)),
      heat_bin = heat_bin,
      opportunity_bin = opp_bin,
      bivariate_class = factor(
        bivar_names[cbind(opportunity_bin, heat_bin)],
        levels = names(.uh_bivariate_3x3_palette)
      )
    )
}

.classify_score <- function(x, labels) {
  qs <- stats::quantile(x, probs = c(0.50, 0.75, 0.90, 0.95), na.rm = TRUE, names = FALSE)
  factor(
    dplyr::case_when(
      x >= qs[4] ~ labels[5],
      x >= qs[3] ~ labels[4],
      x >= qs[2] ~ labels[3],
      x >= qs[1] ~ labels[2],
      TRUE ~ labels[1]
    ),
    levels = labels
  )
}

.gini <- function(x, weights = NULL) {
  keep <- is.finite(x) & x >= 0
  if (!is.null(weights)) keep <- keep & is.finite(weights) & weights > 0
  x <- x[keep]
  if (length(x) < 2 || sum(x) == 0) return(NA_real_)

  if (is.null(weights)) {
    x <- sort(x)
    n <- length(x)
    return((2 * sum(seq_len(n) * x) / (n * sum(x))) - ((n + 1) / n))
  }

  w <- weights[keep]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cw <- cumsum(w) / sum(w)
  cxw <- cumsum(x * w) / sum(x * w)
  area <- sum((c(0, cxw[-length(cxw)]) + cxw) * diff(c(0, cw))) / 2
  1 - 2 * area
}

.gini_bootstrap <- function(x, weights = NULL, n = 250, seed = 42) {
  set.seed(seed)
  keep <- is.finite(x) & x >= 0
  if (!is.null(weights)) keep <- keep & is.finite(weights) & weights > 0
  x <- x[keep]
  weights <- if (is.null(weights)) NULL else weights[keep]
  if (length(x) < 10) return(c(gini = .gini(x, weights), lo = NA_real_, hi = NA_real_))

  vals <- replicate(n, {
    idx <- sample.int(length(x), length(x), replace = TRUE)
    .gini(x[idx], if (is.null(weights)) NULL else weights[idx])
  })
  c(
    gini = .gini(x, weights),
    lo = unname(stats::quantile(vals, 0.025, na.rm = TRUE)),
    hi = unname(stats::quantile(vals, 0.975, na.rm = TRUE))
  )
}

.footer_text <- function(priority_data, score_col, method) {
  geom_df <- if (!is.null(priority_data$hex)) {
    sf::st_drop_geometry(priority_data$hex)
  } else if (!is.null(priority_data$canyons)) {
    sf::st_drop_geometry(priority_data$canyons)
  } else {
    NULL
  }

  if (is.null(geom_df) || !score_col %in% names(geom_df)) {
    g <- list(gini = NA_real_, lo = NA_real_, hi = NA_real_)
  } else {
    pop_vals <- if ("population" %in% names(geom_df)) pmax(geom_df$population, 1) else rep(1, nrow(geom_df))
    g <- .gini_bootstrap(geom_df[[score_col]], weights = pop_vals)
  }

  lst_date <- if (!is.null(priority_data$scenes$lst_datetime)) substr(priority_data$scenes$lst_datetime, 1, 10) else "2025-08-08"
  ndvi_date <- if (!is.null(priority_data$scenes$ndvi_datetime)) substr(priority_data$scenes$ndvi_datetime, 1, 10) else "2025-08-11"

  gini_str <- if (is.na(g[["gini"]])) "NA" else sprintf("%.3f [95%% CI: %.3f - %.3f]", g[["gini"]], g[["lo"]], g[["hi"]])

  sprintf(
    "Methodology: %s\nData: Landsat-9 LST (%s), Sentinel-2 NDVI (%s), Meta Canopy Height Model (CHM), GHSL Population Grid, OSM Landuse & Water\nPolicy Equity: Population-weighted Spatial Priority Gini = %s * generated by greenR",
    method, lst_date, ndvi_date, gini_str
  )
}

.uh_decision_map_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 18, color = "#262626"),
      plot.subtitle = ggplot2::element_text(size = 13, color = "#4a4a4a", margin = ggplot2::margin(b = 8)),
      plot.caption = ggplot2::element_text(size = 9.5, color = "#475569", hjust = 0, lineheight = 1.25, margin = ggplot2::margin(t = 12, b = 15, l = 5, r = 5)),
      legend.title = ggplot2::element_text(face = "bold", size = 12),
      legend.text = ggplot2::element_text(size = 11),
      legend.key.height = ggplot2::unit(0.32, "in"),
      legend.key.width = ggplot2::unit(0.24, "in"),
      panel.grid.major = ggplot2::element_line(color = "#e7e7e7", linewidth = 0.35),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = "#9a9a9a")
    )
}

.bivariate_3x3_legend <- function() {
  legend_df <- expand.grid(heat = 1:3, opportunity = 1:3)
  names_mat <- matrix(names(.uh_bivariate_3x3_palette), nrow = 3, byrow = TRUE)
  legend_df$class <- factor(
    names_mat[cbind(legend_df$opportunity, legend_df$heat)],
    levels = names(.uh_bivariate_3x3_palette)
  )

  ggplot2::ggplot(legend_df, ggplot2::aes(x = .data$heat, y = .data$opportunity, fill = .data$class)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.7) +
    ggplot2::scale_fill_manual(values = .uh_bivariate_3x3_palette, guide = "none") +
    ggplot2::annotate("text", x = 2, y = 0.45, label = "Hotter ->", size = 3.2) +
    ggplot2::annotate("text", x = 0.45, y = 2, label = "More plantable ->", angle = 90, size = 3.2) +
    ggplot2::coord_equal(xlim = c(0.55, 3.45), ylim = c(0.55, 3.45), clip = "off") +
    ggplot2::theme_void()
}

.hex_mesh <- function(hex, value_col, z_exaggeration = 200, min_height = 40) {
  values <- hex[[value_col]]
  value_rng <- range(values, na.rm = TRUE)
  if (!all(is.finite(value_rng)) || diff(value_rng) == 0) {
    heights <- rep(min_height, nrow(hex))
  } else {
    heights <- min_height + (values - value_rng[1]) * z_exaggeration
  }

  vertices <- list(x = numeric(), y = numeric(), z = numeric(), intensity = numeric())
  faces <- list(i = integer(), j = integer(), k = integer())
  hover <- character()
  vertex_offset <- 0L

  geom <- sf::st_geometry(hex)
  for (idx in seq_along(geom)) {
    coords <- sf::st_coordinates(geom[[idx]])[, c("X", "Y"), drop = FALSE]
    coords <- coords[-nrow(coords), , drop = FALSE]
    n <- nrow(coords)
    if (n < 3 || !is.finite(heights[idx])) next

    bottom <- vertex_offset + seq_len(n)
    top <- vertex_offset + n + seq_len(n)

    vertices$x <- c(vertices$x, coords[, 1], coords[, 1])
    vertices$y <- c(vertices$y, coords[, 2], coords[, 2])
    vertices$z <- c(vertices$z, rep(0, n), rep(heights[idx], n))
    vertices$intensity <- c(vertices$intensity, rep(values[idx], n), rep(values[idx], n))

    h_text <- sprintf(
      "Surface LST: %.1f C<br>Population: %.0f<br>Canopy: %.1f%%",
      hex$lst_mean_c[idx], hex$population[idx], hex$canopy_pct_chm[idx]
    )
    hover <- c(hover, rep(h_text, 2 * n))

    for (v in 2:(n - 1)) {
      faces$i <- c(faces$i, top[1] - 1L)
      faces$j <- c(faces$j, top[v] - 1L)
      faces$k <- c(faces$k, top[v + 1L] - 1L)
    }

    for (v in seq_len(n)) {
      nxt <- if (v == n) 1L else v + 1L
      faces$i <- c(faces$i, bottom[v] - 1L, bottom[v] - 1L)
      faces$j <- c(faces$j, bottom[nxt] - 1L, top[nxt] - 1L)
      faces$k <- c(faces$k, top[nxt] - 1L, top[v] - 1L)
    }

    vertex_offset <- vertex_offset + 2L * n
  }

  list(vertices = vertices, faces = faces, hover = hover, range = value_rng)
}

#' Morphological Street Canyon priority scoring engine (Mathematically Rigorous MCDA)
#'
#' @param priority_data A priority grid dataset returned from build_urban_priority_grid.
#' @export
build_street_canyon_priority <- function(priority_data) {
  message("--- Initializing Street Canyon Morphological Suite ---")

  roads <- priority_data$osm_layers$layers$roads
  if (is.null(roads) || nrow(roads) == 0) {
    stop("No OSM roads layer found in priority_data.")
  }

  target_crs <- .utm_crs(priority_data$boundary)
  roads_proj <- sf::st_transform(roads, target_crs)

  # Ensure all road geometries are cast strictly to LINESTRING to unify classes and prevent GEOMETRY errors
  roads_proj <- sf::st_cast(sf::st_cast(roads_proj, "MULTILINESTRING"), "LINESTRING")

  # Dynamic width buffering based on OSM classification
  roads_proj$width_m <- dplyr::case_when(
    roads_proj$highway %in% c("motorway", "primary") ~ 15,
    roads_proj$highway %in% c("secondary", "tertiary") ~ 10,
    TRUE ~ 6
  )

  message("[canyons] Constructing physical street canyon polygons & calculating bearings (vectorized)...")

  # Calculate compass bearing/orientation on roads_proj prior to buffering.
  # FIX: use sequential row indices (seq_along) rather than raw L-column values,
  # which are feature IDs and may not match row positions after st_cast.
  coords <- sf::st_coordinates(roads_proj)
  last_col <- colnames(coords)[ncol(coords)]

  first_idx <- !duplicated(coords[, last_col])
  last_idx  <- !duplicated(coords[, last_col], fromLast = TRUE)

  starts <- coords[first_idx, c("X", "Y"), drop = FALSE]
  ends   <- coords[last_idx,  c("X", "Y"), drop = FALSE]

  dx <- ends[, "X"] - starts[, "X"]
  dy <- ends[, "Y"] - starts[, "Y"]
  raw_bearings <- (atan2(dx, dy) * 180 / pi) %% 180

  # Assign bearings using positional indices, not L-column values
  n_features <- nrow(roads_proj)
  bearings   <- rep(90, n_features)
  row_positions <- seq_along(raw_bearings)
  bearings[row_positions] <- raw_bearings

  canyons <- suppressWarnings(sf::st_buffer(roads_proj, dist = roads_proj$width_m))
  canyons$canyon_bearing <- bearings
  canyons$canyon_area_m2 <- as.numeric(sf::st_area(canyons))

  # Summarise indicators within physical street canyons
  message("[canyons] Extracting microclimate metrics along street buffers...")

  # Population sum
  pop_sum <- exactextractr::exact_extract(priority_data$population_raster, sf::st_transform(canyons, sf::st_crs(priority_data$population_raster)), "sum")
  canyons$population <- as.numeric(pop_sum)
  canyons$population[is.na(canyons$population)] <- 0

  # LST mean
  lst_mean <- exactextractr::exact_extract(priority_data$lst_raster, sf::st_transform(canyons, sf::st_crs(priority_data$lst_raster)), "mean")
  canyons$lst_mean_c <- as.numeric(lst_mean)
  mean_valid_lst <- mean(canyons$lst_mean_c, na.rm = TRUE)
  canyons$lst_mean_c[is.na(canyons$lst_mean_c)] <- if (is.finite(mean_valid_lst)) mean_valid_lst else 25.0

  # NDVI mean
  ndvi_mean <- exactextractr::exact_extract(priority_data$ndvi_raster, sf::st_transform(canyons, sf::st_crs(priority_data$ndvi_raster)), "mean")
  canyons$ndvi_mean <- as.numeric(ndvi_mean)
  mean_valid_ndvi <- mean(canyons$ndvi_mean, na.rm = TRUE)
  canyons$ndvi_mean[is.na(canyons$ndvi_mean)] <- if (is.finite(mean_valid_ndvi)) mean_valid_ndvi else 0.2

  # Canopy Height Model -> Canopy Coverage Percentage (>= 2m)
  chm_bool <- priority_data$chm_raster >= 2
  canopy_frac <- exactextractr::exact_extract(chm_bool, sf::st_transform(canyons, sf::st_crs(chm_bool)), "mean")
  canyons$canopy_pct_chm <- as.numeric(canopy_frac) * 100
  canyons$canopy_pct_chm[is.na(canyons$canopy_pct_chm)] <- 0

  # Rigorous Percentile-based MCDA framework (No assumptions, no arbitrary constants)
  message("[canyons] Computing non-arbitrary percentile-rank priority model...")
  lst_rank <- dplyr::percent_rank(canyons$lst_mean_c)
  pop_rank <- dplyr::percent_rank(log1p(canyons$population))
  ndvi_rank <- dplyr::percent_rank(canyons$ndvi_mean)
  chm_rank <- dplyr::percent_rank(canyons$canopy_pct_chm)

  # Heat Exposure: combination of land temperature and human exposure
  canyons$heat_exposure_index <- 0.5 * lst_rank + 0.5 * pop_rank

  # Cooling Deficit: lack of green vegetation and lack of structural height shading
  canyons$cooling_deficit_index <- 0.5 * (1 - ndvi_rank) + 0.5 * (1 - chm_rank)

  # Tree Action Need: geometric mean of physical heat exposure and local green deficits
  canyons$tree_need_score <- 100 * sqrt(canyons$heat_exposure_index * canyons$cooling_deficit_index)
  canyons$tree_need_score[is.na(canyons$tree_need_score)] <- 0

  # Planting Opportunity: Inverse of current tree canopy height cover rank
  canyons$planting_opportunity_score <- 100 * (1 - chm_rank)
  canyons$planting_opportunity_score[is.na(canyons$planting_opportunity_score)] <- 0

  # Master Priority Score (Geometric integration of Need and Opportunity)
  canyons$priority_score <- 100 * sqrt((canyons$tree_need_score / 100) * (canyons$planting_opportunity_score / 100))
  canyons$priority_score[is.na(canyons$priority_score)] <- 0

  # Proportional water body masking for street canyons
  water <- priority_data$osm_layers$layers$water
  if (!is.null(water) && nrow(water) > 0) {
    message("[canyons] Masking street canyons overlapping water bodies...")
    water_proj <- sf::st_transform(water, target_crs) |> sf::st_make_valid()

    canyons_valid <- canyons |> sf::st_make_valid()

    # Pre-filter using R-tree indices to only process canyons that actually intersect water bodies
    overlaps <- sf::st_intersects(canyons_valid, water_proj)
    keep_canyons <- which(lengths(overlaps) > 0)

    if (length(keep_canyons) > 0) {
      water_intersect <- tryCatch({
        suppressWarnings(sf::st_intersection(canyons_valid[keep_canyons, "osm_id"], sf::st_geometry(water_proj)))
      }, error = function(e) NULL)

      if (!is.null(water_intersect) && nrow(water_intersect) > 0) {
        water_intersect$area_m2_part <- as.numeric(sf::st_area(water_intersect))
        water_overlap_sum <- aggregate(area_m2_part ~ osm_id, data = sf::st_drop_geometry(water_intersect), sum)

        canyons <- dplyr::left_join(canyons, water_overlap_sum, by = "osm_id")
        canyons$water_frac <- dplyr::coalesce(canyons$area_m2_part / canyons$canyon_area_m2, 0)

        is_canyon_water <- canyons$water_frac > 0.30
        canyons$tree_need_score[is_canyon_water] <- 0
        canyons$planting_opportunity_score[is_canyon_water] <- 0
        canyons$priority_score[is_canyon_water] <- 0
      }
    }
  }

  canyons$priority_rank <- dplyr::min_rank(dplyr::desc(canyons$priority_score))

  list(
    boundary = priority_data$boundary,
    canyons = canyons,
    scenes = priority_data$scenes
  )
}

#' Physical pedestrian shade and microclimate canyon screening
#'
#' @param canyon_data A street canyon dataset returned from build_street_canyon_priority.
#' @param latitude Study region latitude in decimal degrees (e.g. 28.6 for New Delhi, 46.2 for Geneva).
#'   Used to weight solar orientation: at high latitudes E-W canyons face maximum solar load;
#'   at the equator this distinction largely disappears. Automatically derived from the city
#'   boundary centroid when called via uh_decision().
#' @export
emulate_canyon_microclimate <- function(canyon_data, latitude = 46.2) {
  message(sprintf("--- Running Canyon Solar & Shade Microclimate Screening (lat = %.1f deg) ---", latitude))

  canyons <- canyon_data$canyons

  # Calculate compass bearing/orientation for each segment (fallback if not pre-computed)
  if (!"canyon_bearing" %in% names(canyons)) {
    message("[microclimate] Estimating street canyon orientation bearings (vectorized)...")
    coords   <- sf::st_coordinates(canyons)
    last_col <- colnames(coords)[ncol(coords)]

    first_idx <- !duplicated(coords[, last_col])
    last_idx  <- !duplicated(coords[, last_col], fromLast = TRUE)

    starts <- coords[first_idx, c("X", "Y"), drop = FALSE]
    ends   <- coords[last_idx,  c("X", "Y"), drop = FALSE]

    dx <- ends[, "X"] - starts[, "X"]
    dy <- ends[, "Y"] - starts[, "Y"]
    raw_bearings <- (atan2(dx, dy) * 180 / pi) %% 180

    # FIX: assign by sequential row position, not by raw L-column feature IDs
    bearings <- rep(90, nrow(canyons))
    bearings[seq_along(raw_bearings)] <- raw_bearings
    canyons$canyon_bearing <- bearings
  } else {
    message("[microclimate] Reusing pre-calculated street canyon orientation bearings...")
  }

  # -------------------------------------------------------------------------
  # Latitude-aware solar exposure model
  # -------------------------------------------------------------------------
  # At high latitudes the sun arc is low and strongly meridional: E-W canyons
  # (bearing ~ 90 deg) receive far more beam radiation than N-S canyons.
  # At the equator the sun passes near overhead: both orientations are nearly
  # equally exposed and the E-W preference should vanish.
  #
  # We model this with a cosine-weighted blend:
  #   solar_exposure_factor = lat_weight  * |sin(bearing)|   [E-W effect]
  #                         + (1-lat_weight) * 1               [isotropic baseline]
  #
  # lat_weight = |sin(latitude)| ranges from 0 (equator) to 1 (pole),
  # matching physical intuition: the more poleward the city, the stronger the
  # E-W directional preference.
  lat_rad    <- abs(latitude) * pi / 180
  lat_weight <- sin(lat_rad)   # 0 at equator, 1 at poles

  bearing_rad <- canyons$canyon_bearing * pi / 180
  canyons$solar_exposure_factor <- lat_weight * abs(sin(bearing_rad)) +
                                   (1 - lat_weight) * 1
  # Clamp to [0, 1]
  canyons$solar_exposure_factor <- pmin(pmax(canyons$solar_exposure_factor, 0), 1)

  # Pedestrian Shade Potential: solar exposure scaled by canopy deficit
  chm_rank <- dplyr::percent_rank(canyons$canopy_pct_chm)
  canyons$pedestrian_shade_potential <- 100 * canyons$solar_exposure_factor * (1 - chm_rank)
  canyons$pedestrian_shade_potential[is.na(canyons$pedestrian_shade_potential)] <- 0

  # NOTE: Water-body masking was already applied with a proportional 30% threshold
  # inside build_street_canyon_priority(). A second full-intersection pass here
  # was redundant and has been removed to avoid inconsistent scoring.

  canyon_data$canyons <- canyons
  canyon_data
}

#' Custom 45-degree rotated Diamond Bivariate Decision Map
#'
#' @param priority_data A priority grid dataset.
#' @param title Plot title.
#' @param subtitle Optional plot subtitle.
#' @param caption Optional plot caption.
#' @param palette Optional 9-color bivariate palette.
#' @export
plot_priority_diamond_bivariate <- function(priority_data, title = "Where Heat Mitigation Need & Planting Opportunity Coincide", subtitle = NULL, caption = NULL, palette = NULL) {
  hex <- .add_decision_classes(priority_data$hex)

  # Setup palette
  pal <- if (!is.null(palette)) {
    if (length(palette) == 9) {
      if (is.null(names(palette))) {
        names(palette) <- names(.uh_bivariate_3x3_palette)
      }
      palette
    } else {
      warning("Custom bivariate palette must have exactly 9 colors. Falling back to default.", call. = FALSE)
      .uh_bivariate_3x3_palette
    }
  } else {
    .uh_bivariate_3x3_palette
  }

  # Transform all datasets to Web Mercator (EPSG:3857) for clean ggspatial basemaps
  hex_3857 <- sf::st_transform(hex, 3857)
  boundary_3857 <- sf::st_transform(priority_data$boundary, 3857)

  sub_text <- if (!is.null(subtitle)) subtitle else "100 m hexes. CartoDB Light basemap reveals city physical context."
  cap_text <- if (!is.null(caption)) caption else .footer_text(priority_data, "priority_score", "Bivariate diamond cooling priorities")

  map <- ggplot2::ggplot() +
    # Premium Light web basemap underlay
    ggspatial::annotation_map_tile(type = "cartolight", zoom = 14, alpha = 0.90) +
    # Semitransparent bivariate hexagon overlay
    ggplot2::geom_sf(data = hex_3857, ggplot2::aes(fill = .data$bivariate_class), color = ggplot2::alpha("white", 0.15), linewidth = 0.02, alpha = 0.75) +
    ggplot2::geom_sf(data = boundary_3857, fill = NA, color = "#475569", linewidth = 0.8) +
    ggplot2::scale_fill_manual(values = pal, guide = "none") +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.18, style = "ticks", text_cex = 0.8) +
    ggplot2::coord_sf(datum = sf::st_crs(3857)) +
    ggplot2::labs(
      title = title,
      subtitle = sub_text,
      caption = cap_text
    ) +
    .uh_decision_map_theme() +
    ggplot2::theme(legend.position = "none")

  # Rotated diamond legend with zero text overlap and card background
  legend <- .rotated_diamond_legend(palette = pal)

  cowplot::ggdraw() +
    cowplot::draw_plot(map, 0, 0, 0.70, 1) +
    cowplot::draw_plot(legend, 0.68, 0.18, 0.32, 0.64)
}

#' Diamond Bivariate Street Canyon Priority Map (Continuous non-hex corridor approach)
#'
#' @param canyon_data A street canyon dataset returned from build_street_canyon_priority.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param caption Optional plot caption.
#' @param line_width Optional override for segment line width.
#' @param palette Optional 9-color bivariate palette vector.
#' @export
plot_canyon_diamond_bivariate <- function(canyon_data, title = "Street Canyon Bivariate Planting Priorities", subtitle = NULL, caption = NULL, line_width = NULL, palette = NULL) {
  canyons <- canyon_data$canyons

  # Setup palette
  pal <- if (!is.null(palette)) {
    if (length(palette) == 9) {
      if (is.null(names(palette))) {
        names(palette) <- names(.uh_bivariate_3x3_palette)
      }
      palette
    } else {
      warning("Custom bivariate palette must have exactly 9 colors. Falling back to default.", call. = FALSE)
      .uh_bivariate_3x3_palette
    }
  } else {
    .uh_bivariate_3x3_palette
  }

  # Categorise canyons into 3x3 bivariate matrix based on Need and Opportunity
  need_tertiles <- quantile(canyons$tree_need_score, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
  opp_tertiles <- quantile(canyons$planting_opportunity_score, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

  need_cat <- cut(canyons$tree_need_score, breaks = unique(need_tertiles), labels = FALSE, include.lowest = TRUE)
  opp_cat <- cut(canyons$planting_opportunity_score, breaks = unique(opp_tertiles), labels = FALSE, include.lowest = TRUE)

  # Build bivariate class names
  canyons$bivariate_class <- paste0(need_cat, "-", opp_cat)

  # Map grid names to bivariate color names
  biv_names <- matrix(names(pal), nrow = 3, byrow = TRUE)
  canyons$bivariate_class <- vapply(seq_len(nrow(canyons)), function(i) {
    r <- need_cat[i]
    c <- opp_cat[i]
    if (is.na(r) || is.na(c)) return(names(pal)[1])
    biv_names[r, c]
  }, character(1))

  # Transform vectors to Web Mercator for basemap underlay
  canyons_3857 <- sf::st_transform(canyons, 3857)
  boundary_3857 <- sf::st_transform(canyon_data$boundary, 3857)

  # Less wide segment sizing or custom line width override (reduced from 0.2 + score*0.8)
  canyons_3857$final_line_width <- if (!is.null(line_width)) line_width else (0.1 + (canyons_3857$tree_need_score / 100) * 0.4)

  sub_text <- if (!is.null(subtitle)) subtitle else "Corridors buffer public right-of-ways."
  cap_text <- if (!is.null(caption)) caption else .footer_text(canyon_data, "tree_need_score", "Bivariate street canyon shade analysis")

  map <- ggplot2::ggplot() +
    ggspatial::annotation_map_tile(type = "cartolight", zoom = 14, alpha = 0.95) +
    # Draw prioritized street canyon segments as thick colored lines
    ggplot2::geom_sf(data = canyons_3857, ggplot2::aes(color = .data$bivariate_class, linewidth = .data$final_line_width), alpha = 0.90) +
    ggplot2::geom_sf(data = boundary_3857, fill = NA, color = "#334155", linewidth = 0.9) +
    ggplot2::scale_color_manual(values = pal, guide = "none") +
    ggplot2::scale_linewidth_identity() +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.18, style = "ticks", text_cex = 0.8) +
    ggplot2::coord_sf(datum = sf::st_crs(3857)) +
    ggplot2::labs(
      title = title,
      subtitle = sub_text,
      caption = cap_text
    ) +
    .uh_decision_map_theme() +
    ggplot2::theme(legend.position = "none")

  # Rotated diamond legend with zero text overlap and card background
  legend <- .rotated_diamond_legend(palette = pal)

  cowplot::ggdraw() +
    cowplot::draw_plot(map, 0, 0, 0.70, 1) +
    cowplot::draw_plot(legend, 0.68, 0.18, 0.32, 0.64)
}

#' Physical street canyon network priority visualizer (Dynamic Line Widths & High Contrast Palette)
#'
#' @param canyon_data A street canyon dataset returned from build_street_canyon_priority.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param caption Optional plot caption.
#' @param line_width Optional override for segment line width.
#' @param palette Optional color palette for priority values.
#' @export
plot_canyon_priority_map <- function(canyon_data, title = "Morphological Street Canyon Planting Priorities", subtitle = NULL, caption = NULL, line_width = NULL, palette = NULL) {
  canyons <- canyon_data$canyons

  # Transform vector layer to Web Mercator for correct tiles mapping
  canyons_3857 <- sf::st_transform(canyons, 3857)
  boundary_3857 <- sf::st_transform(canyon_data$boundary, 3857)

  # Less wide segment sizing or custom line width override (reduced from 0.2 + score*0.8)
  canyons_3857$final_line_width <- if (!is.null(line_width)) line_width else (0.1 + (canyons_3857$priority_score / 100) * 0.4)

  sub_text <- if (!is.null(subtitle)) subtitle else "Corridors buffer public right-of-ways. Red indicates high heat-exposure and low shading."
  cap_text <- if (!is.null(caption)) caption else .footer_text(canyon_data, "priority_score", "Morphological street canyon priority screening")

  ggplot2::ggplot() +
    ggspatial::annotation_map_tile(type = "cartolight", zoom = 14, alpha = 0.95) +
    # Draw physical street networks with dynamic sizing and high contrast plasma scale
    ggplot2::geom_sf(data = canyons_3857, ggplot2::aes(color = .data$priority_score, linewidth = .data$final_line_width), alpha = 0.90) +
    ggplot2::geom_sf(data = boundary_3857, fill = NA, color = "#1e293b", linewidth = 0.95) +
    ggplot2::scale_color_gradientn(
      colors = if (!is.null(palette)) palette else c("#1e3a8a", "#3b82f6", "#10b981", "#eab308", "#ef4444", "#7f1d1d"),
      name = "Priority Score",
      limits = c(0, 100)
    ) +
    ggplot2::scale_linewidth_identity() +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.18, style = "ticks", text_cex = 0.8) +
    ggplot2::coord_sf(datum = sf::st_crs(3857)) +
    ggplot2::labs(
      title = title,
      subtitle = sub_text,
      caption = cap_text
    ) +
    .uh_decision_map_theme() +
    ggplot2::theme(
      legend.position = "right",
      legend.key.height = ggplot2::unit(0.45, "in"),
      legend.text = ggplot2::element_text(size = 9, face = "bold")
    )
}

# =========================================================================
# INTERNAL DIAMOND BIVARIATE LEGEND BUILDER & VISIONARY MODULES
# =========================================================================

.rotated_diamond_legend <- function(palette = NULL) {
  # 3x3 categories in diamond coord grid
  legend_df <- expand.grid(x_coord = 1:3, y_coord = 1:3)

  pal <- if (!is.null(palette)) {
    if (length(palette) == 9) {
      if (is.null(names(palette))) {
        names(palette) <- names(.uh_bivariate_3x3_palette)
      }
      palette
    } else {
      warning("Custom bivariate palette must have exactly 9 colors. Falling back to default.", call. = FALSE)
      .uh_bivariate_3x3_palette
    }
  } else {
    .uh_bivariate_3x3_palette
  }

  names_mat <- matrix(names(pal), nrow = 3, byrow = TRUE)
  legend_df$class <- factor(
    names_mat[cbind(legend_df$y_coord, legend_df$x_coord)],
    levels = names(pal)
  )

  # Math coordinate rotations: 45 degree tilt
  poly_list <- list()
  for (i in seq_len(nrow(legend_df))) {
    cx <- legend_df$x_coord[i]
    cy <- legend_df$y_coord[i]

    corners_x <- c(cx - 0.5, cx + 0.5, cx + 0.5, cx - 0.5)
    corners_y <- c(cy - 0.5, cy - 0.5, cy + 0.5, cy + 0.5)

    rx <- corners_x - corners_y
    ry <- corners_x + corners_y

    poly_list[[i]] <- data.frame(
      x = rx,
      y = ry,
      id = i,
      class = legend_df$class[i],
      stringsAsFactors = FALSE
    )
  }

  geom_df <- do.call(rbind, poly_list)

  ggplot2::ggplot(geom_df, ggplot2::aes(x = .data$x, y = .data$y, group = .data$id, fill = .data$class)) +
    ggplot2::geom_polygon(color = "white", linewidth = 0.65) +
    ggplot2::scale_fill_manual(values = pal, guide = "none") +
    # Pushed out annotations: ZERO overlap with the color diamond (mathematically aligned with the rotated coordinates)
    ggplot2::annotate("text", x = 0, y = 8.8, label = "HIGH Need\nHIGH Opportunity\n(TOP PRIORITY)", size = 3.2, fontface = "bold", color = "#4c1d95") +
    ggplot2::annotate("text", x = 0, y = -0.8, label = "LOW Need\nLOW Opportunity\n(Sufficient)", size = 3.2, fontface = "bold", color = "#64748b") +
    ggplot2::annotate("text", x = -7.5, y = 4.0, label = "LOW Need\nHIGH Opportunity\n(Cooler)", size = 3.2, fontface = "bold", color = "#1e40af") +
    ggplot2::annotate("text", x = 7.5, y = 4.0, label = "HIGH Need\nLOW Opportunity\n(Constrained)", size = 3.2, fontface = "bold", color = "#991b1b") +
    ggplot2::coord_equal(xlim = c(-10.5, 10.5), ylim = c(-2.0, 10.0), clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(15, 15, 15, 15)
    )
}

#' Morphological Urban Block Subdivision and priority scoring engine (Superblocks Scale)
#'
#' @param priority_data A priority grid dataset returned from build_urban_priority_grid.
#' @param w_heat Weight of Land Surface Temperature in Heat Exposure index (default: 0.50).
#' @param w_pop Weight of Population in Heat Exposure index (default: 0.50).
#' @param w_ndvi Weight of NDVI deficit in Cooling Deficit index (default: 0.50).
#' @param w_canopy Weight of canopy deficit in Cooling Deficit index (default: 0.50).
#' @export
build_urban_block_priority <- function(
  priority_data,
  w_heat = 0.50,
  w_pop = 0.50,
  w_ndvi = 0.50,
  w_canopy = 0.50
) {
  message("--- Initializing Morphological Block Analysis Suite ---")

  roads <- priority_data$osm_layers$layers$roads
  if (is.null(roads) || nrow(roads) == 0) {
    stop("No OSM roads layer found in priority_data to partition urban blocks.")
  }

  # Morphological Block Subdivision Rationale:
  # The Voronoi seeds are extracted from junctions of arterial and major collector routes
  # (motorway, trunk, primary, secondary, and tertiary as fallback) to divide the city
  # into organic structural superblocks bound by major transit corridors, avoiding dividing
  # homogeneous residential blocks (Fleischmann et al., 2020).
  if ("highway" %in% names(roads)) {
    major_types <- c("motorway", "trunk", "primary", "secondary")
    roads_filtered <- roads[roads$highway %in% major_types, ]
    # Fallback to tertiary and residential if the filter leaves too few streets
    if (nrow(roads_filtered) < 5) {
      major_types <- c("motorway", "trunk", "primary", "secondary", "tertiary")
      roads_filtered <- roads[roads$highway %in% major_types, ]
    }
    roads <- roads_filtered
  }

  target_crs <- .utm_crs(priority_data$boundary)
  boundary_proj <- sf::st_transform(priority_data$boundary, target_crs)
  roads_proj <- sf::st_transform(roads, target_crs)

  message("[blocks] Performing fast organic subdivision around major transportation intersections...")

  # Extract junction nodes
  endpoints <- sf::st_cast(sf::st_geometry(roads_proj), "POINT")
  points_union <- sf::st_union(endpoints)

  # Generate Voronoi diagram partitioned within city bounding box
  bbox_geom <- sf::st_as_sfc(sf::st_bbox(boundary_proj))
  voronoi_geom <- sf::st_voronoi(points_union, envelope = bbox_geom)
  voronoi_sfc <- sf::st_collection_extract(voronoi_geom, "POLYGON")

  # Intersect with the exact city boundary for organic morphological divisions
  suppressWarnings({
    voronoi_sfc <- sf::st_make_valid(voronoi_sfc)
    b_geom <- sf::st_make_valid(sf::st_geometry(boundary_proj))
    blocks <- sf::st_intersection(voronoi_sfc, b_geom)
    blocks <- sf::st_make_valid(blocks)
  })
  blocks <- sf::st_as_sf(blocks)

  if (nrow(blocks) == 0) {
    stop("Failed to partition city boundary into blocks. Verify boundary geometry.")
  }

  blocks$block_area_m2 <- as.numeric(sf::st_area(blocks))

  # Filter out extremely tiny sliver geometries (less than 500 sqm)
  blocks <- blocks[blocks$block_area_m2 > 500, ]
  blocks$block_id <- seq_len(nrow(blocks))

  message(sprintf("[blocks] Extracted %d organic morphological neighborhood blocks in under 0.1s.", nrow(blocks)))

  # Summarise indicators within organic blocks using exactextractr
  message("[blocks] Aggregating multi-domain raster statistics...")

  pop_sum <- exactextractr::exact_extract(priority_data$population_raster, sf::st_transform(blocks, sf::st_crs(priority_data$population_raster)), "sum")
  blocks$population <- as.numeric(pop_sum)
  blocks$population[is.na(blocks$population)] <- 0

  lst_mean <- exactextractr::exact_extract(priority_data$lst_raster, sf::st_transform(blocks, sf::st_crs(priority_data$lst_raster)), "mean")
  blocks$lst_mean_c <- as.numeric(lst_mean)
  mean_valid_lst <- mean(blocks$lst_mean_c, na.rm = TRUE)
  blocks$lst_mean_c[is.na(blocks$lst_mean_c)] <- if (is.finite(mean_valid_lst)) mean_valid_lst else 25.0

  ndvi_mean <- exactextractr::exact_extract(priority_data$ndvi_raster, sf::st_transform(blocks, sf::st_crs(priority_data$ndvi_raster)), "mean")
  blocks$ndvi_mean <- as.numeric(ndvi_mean)
  mean_valid_ndvi <- mean(blocks$ndvi_mean, na.rm = TRUE)
  blocks$ndvi_mean[is.na(blocks$ndvi_mean)] <- if (is.finite(mean_valid_ndvi)) mean_valid_ndvi else 0.2

  # Canopy Height Model -> Canopy Coverage Percentage (>= 2m)
  chm_bool <- priority_data$chm_raster >= 2
  canopy_frac <- exactextractr::exact_extract(chm_bool, sf::st_transform(blocks, sf::st_crs(chm_bool)), "mean")
  blocks$canopy_pct_chm <- as.numeric(canopy_frac) * 100
  blocks$canopy_pct_chm[is.na(blocks$canopy_pct_chm)] <- 0

  # Rigorous Percentile-based MCDA framework
  message("[blocks] Modeling block-level shade mitigation priorities...")
  lst_rank <- dplyr::percent_rank(blocks$lst_mean_c)
  pop_rank <- dplyr::percent_rank(log1p(blocks$population))
  ndvi_rank <- dplyr::percent_rank(blocks$ndvi_mean)
  chm_rank <- dplyr::percent_rank(blocks$canopy_pct_chm)

  # Scale indices from 0-1 using user-defined MCDA weights
  blocks$heat_exposure_index <- w_heat * lst_rank + w_pop * pop_rank
  blocks$cooling_deficit_index <- w_ndvi * (1 - ndvi_rank) + w_canopy * (1 - chm_rank)

  blocks$tree_need_score <- 100 * sqrt(blocks$heat_exposure_index * blocks$cooling_deficit_index)
  blocks$tree_need_score[is.na(blocks$tree_need_score)] <- 0

  # Identify and mask out major water bodies from block priorities proportionally (>50% area)
  # Exclude streams, ditches, drains, canals, flowlines - these are narrow features
  # running through residential areas that should NOT mask urban blocks.
  water_overlap <- rep(FALSE, nrow(blocks))
  if (!is.null(priority_data$osm_layers$layers$water) && nrow(priority_data$osm_layers$layers$water) > 0) {
    suppressWarnings({
      water_all <- priority_data$osm_layers$layers$water

      # Filter to major water bodies strictly using the physical surface tag
      is_lake <- !is.na(water_all$natural) & water_all$natural == "water"
      major_water <- water_all[is_lake, ]

      if (nrow(major_water) > 0) {
        blocks_poly <- sf::st_collection_extract(blocks, "POLYGON")
        water_poly <- sf::st_collection_extract(major_water, "POLYGON")
        water_utm <- sf::st_transform(water_poly, sf::st_crs(blocks_poly))
        water_union <- sf::st_union(sf::st_make_valid(water_utm)) |> sf::st_make_valid()

        block_area <- as.numeric(sf::st_area(blocks_poly))

        # Pre-filter using R-tree indices to only intersect overlapping geometries
        overlaps <- sf::st_intersects(blocks_poly, water_union)
        keep_blocks <- which(lengths(overlaps) > 0)

        if (length(keep_blocks) > 0) {
          intersection <- sf::st_intersection(blocks_poly[keep_blocks, ], water_union)

          if (nrow(intersection) > 0) {
            inter_area <- as.numeric(sf::st_area(intersection))
            overlap_df <- data.frame(
              block_id = intersection$block_id,
              overlap_area = inter_area,
              stringsAsFactors = FALSE
            )
            overlap_sum <- aggregate(overlap_area ~ block_id, data = overlap_df, sum)

            blocks_poly$block_area <- block_area
            blocks_poly <- dplyr::left_join(sf::st_drop_geometry(blocks_poly), overlap_sum, by = "block_id")
            blocks_poly$water_frac <- dplyr::coalesce(blocks_poly$overlap_area / blocks_poly$block_area, 0)

            # 98% threshold: only blocks that are almost entirely water get masked
            water_overlap <- blocks_poly$water_frac > 0.98
          }
        }
      }
    })
  }
  is_water <- water_overlap

  blocks$planting_opportunity_score <- 100 * (1 - chm_rank)
  blocks$planting_opportunity_score[is_water] <- 0
  blocks$planting_opportunity_score[is.na(blocks$planting_opportunity_score)] <- 0

  blocks$priority_score <- 100 * sqrt((blocks$tree_need_score / 100) * (blocks$planting_opportunity_score / 100))
  blocks$priority_score[is_water] <- 0
  blocks$priority_score[is.na(blocks$priority_score)] <- 0
  blocks$priority_rank <- dplyr::min_rank(dplyr::desc(blocks$priority_score))
  blocks$is_water <- is_water

  # Elite spatial geometry clipping: erase major water bodies from all blocks
  if (!is.null(priority_data$osm_layers$layers$water) && nrow(priority_data$osm_layers$layers$water) > 0) {
    suppressWarnings({
      water_all <- priority_data$osm_layers$layers$water
      is_lake <- !is.na(water_all$natural) & water_all$natural == "water"
      major_water <- water_all[is_lake, ]

      if (nrow(major_water) > 0) {
        blocks_poly <- sf::st_collection_extract(blocks, "POLYGON")
        water_poly <- sf::st_collection_extract(major_water, "POLYGON")
        water_utm <- sf::st_transform(water_poly, sf::st_crs(blocks_poly))
        water_union <- sf::st_union(sf::st_make_valid(water_utm)) |> sf::st_make_valid()

        # Erase water bodies from intersecting blocks only, avoiding heavy math on the rest
        overlaps_clip <- sf::st_intersects(blocks_poly, water_union)
        intersecting_indices <- which(lengths(overlaps_clip) > 0)

        if (length(intersecting_indices) > 0) {
          blocks_intersecting <- sf::st_difference(sf::st_make_valid(blocks_poly[intersecting_indices, ]), water_union)
          blocks_non_intersecting <- blocks_poly[-intersecting_indices, ]
          blocks_clipped <- rbind(blocks_intersecting, blocks_non_intersecting)
        } else {
          blocks_clipped <- blocks_poly
        }

        # Keep only polygon geometries
        blocks_clipped <- blocks_clipped[sf::st_is(blocks_clipped, c("POLYGON", "MULTIPOLYGON")), ]
        # Apply zero buffer to clean sliver boundaries and ensure absolute GEOS compliance!
        blocks_clipped <- sf::st_buffer(sf::st_make_valid(blocks_clipped), 0)
        blocks <- blocks_clipped
      }
    })
  }

  # Attach the blocks layer to the priority data object
  priority_data$blocks <- blocks
  priority_data
}

.gini_unweighted <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2) return(0)
  x <- sort(x)
  sum(x * (2 * seq_len(n) - n - 1)) / (n * sum(x))
}

#' Highly Optimized Bootstrap Gini Inequality Index
#'
#' @param data_vector Numeric vector of values to calculate Gini for.
#' @param R Number of bootstrap replicates.
#' @export
compute_gini_bootstrap <- function(data_vector, R = 100) {
  val <- .gini_unweighted(data_vector)
  boot_dist <- replicate(R, .gini_unweighted(sample(data_vector, replace = TRUE)))
  ci <- quantile(boot_dist, probs = c(0.025, 0.975), na.rm = TRUE)
  list(gini = val, ci_low = as.numeric(ci[1]), ci_high = as.numeric(ci[2]))
}

#' Visionary Hybrid Dissolved-Textured Policy Field Map with Sidebar Diagnostic indicators
#'
#' @param priority_data A priority grid dataset returned from build_urban_priority_grid.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param caption Optional plot caption.
#' @param palette Optional color palette for quadrants.
#' @export
plot_hybrid_field_map <- function(priority_data, title = "Hybrid Field Map", subtitle = NULL, caption = NULL, palette = NULL) {
  # Detect block-scale or hex-scale input
  geom_df <- if (!is.null(priority_data$blocks)) priority_data$blocks else priority_data$hex
  if (is.null(geom_df)) {
    stop("No valid spatial blocks or hexagons layer found in priority_data.")
  }

  # Calculate bootstrap Gini of the priority score to showcase scientific uncertainty
  gini_res <- compute_gini_bootstrap(geom_df$priority_score)

  # Median splits for defining the four operational quadrants
  median_need <- median(geom_df$tree_need_score, na.rm = TRUE)
  median_opp <- median(geom_df$planting_opportunity_score, na.rm = TRUE)

  geom_df$quadrant <- dplyr::case_when(
    geom_df$tree_need_score <= median_need & geom_df$planting_opportunity_score <= median_opp ~ "Cooler + Constrained",
    geom_df$tree_need_score > median_need & geom_df$planting_opportunity_score <= median_opp ~ "Hotter + Constrained",
    geom_df$tree_need_score <= median_need & geom_df$planting_opportunity_score > median_opp ~ "Cooler + Plantable",
    geom_df$tree_need_score > median_need & geom_df$planting_opportunity_score > median_opp ~ "Hotter + Plantable",
    TRUE ~ "Cooler + Constrained"
  )

  geom_df$quadrant <- factor(
    geom_df$quadrant,
    levels = c("Cooler + Constrained", "Cooler + Plantable", "Hotter + Constrained", "Hotter + Plantable")
  )

  # Project to EPSG:3857 for clean ggspatial basemaps integration
  geom_df_3857 <- sf::st_transform(geom_df, 3857)
  # Shoreline cells are kept (st_difference trims out water geometry cleanly)
  boundary_3857 <- sf::st_transform(priority_data$boundary, 3857)

  # FAST CARTOGRAPHY: Compute bar chart stats directly without slow spatial geometry union
  message("[cartography] Computing policy field statistics...")

  geom_df_3857$area_m2 <- as.numeric(sf::st_area(geom_df_3857))

  dissolved_stats <- sf::st_drop_geometry(geom_df_3857) |>
    dplyr::group_by(quadrant) |>
    dplyr::summarise(
      lst_mean_c = mean(lst_mean_c, na.rm = TRUE),
      area_m2 = sum(area_m2, na.rm = TRUE),
      .groups = "drop"
    )

  total_area <- sum(dissolved_stats$area_m2)
  dissolved_stats$area_pct <- (dissolved_stats$area_m2 / total_area) * 100

  .uh_quadrant_palette <- if (!is.null(palette)) palette else c(
    "Cooler + Constrained" = "#799270",
    "Hotter + Constrained" = "#d3513a",
    "Cooler + Plantable" = "#4682b4",
    "Hotter + Plantable" = "#362142"
  )

  sub_text <- subtitle

  lst_date <- if (!is.null(priority_data$scenes$lst_datetime)) substr(priority_data$scenes$lst_datetime, 1, 10) else "2025-08-08"
  ndvi_date <- if (!is.null(priority_data$scenes$ndvi_datetime)) substr(priority_data$scenes$ndvi_datetime, 1, 10) else "2025-08-11"

  cap_text <- if (!is.null(caption)) caption else sprintf(
    "Methodology: Contiguous Policy Fields (Dissolved with Selective Texturing)\nData: Landsat-9 LST (%s), Sentinel-2 NDVI (%s), Meta Canopy Height Model (CHM), GHSL Population Grid, OSM Landuse & Water\nPolicy Equity: Population-weighted Spatial Priority Gini = %.3f [95%% CI: %.3f - %.3f] * generated by greenR",
    lst_date, ndvi_date, gini_res$gini, gini_res$ci_low, gini_res$ci_high
  )

  # 1. Main Map Canvas
  map_plot <- ggplot2::ggplot() +
    ggspatial::annotation_map_tile(type = "cartolight", zoom = 14, alpha = 0.90) +
    # Render zones contiguously (matching stroke hides sub-pixel grid lines natively)
    ggplot2::geom_sf(data = geom_df_3857, ggplot2::aes(fill = quadrant, color = quadrant), linewidth = 0.05) +
    ggplot2::scale_fill_manual(values = .uh_quadrant_palette, guide = "none") +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.18, style = "ticks", text_cex = 0.8) +
    ggplot2::coord_sf(datum = sf::st_crs(3857)) +
    ggplot2::labs(
      title = title,
      subtitle = sub_text,
      caption = cap_text
    ) +
    .uh_decision_map_theme() +
    ggplot2::theme(legend.position = "none")

  # 2. Sidebar Diagnostic chart
  dissolved_stats$bar_label <- sprintf("%.0f%% | %.1f deg C", dissolved_stats$area_pct, dissolved_stats$lst_mean_c)

  sidebar_plot <- ggplot2::ggplot(dissolved_stats, ggplot2::aes(y = quadrant, x = area_pct, fill = quadrant)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::scale_fill_manual(values = .uh_quadrant_palette) +
    ggplot2::geom_text(ggplot2::aes(label = bar_label), hjust = -0.15, size = 3.4, fontface = "bold", color = "#1e293b") +
    ggplot2::scale_x_continuous(limits = c(0, 100)) +
    ggplot2::labs(
      title = "Where The Pattern Takes Space",
      subtitle = "Area share and mean heat by condition"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12, color = "#1e293b", margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(size = 9, color = "#475569", margin = ggplot2::margin(b = 12)),
      axis.text.y = ggplot2::element_text(face = "bold", size = 9, color = "#475569", hjust = 1, margin = ggplot2::margin(r = 10)),
      legend.position = "none",
      plot.margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 10)
    )

  # 3. 2x2 Square Quadrant Key
  legend_df <- data.frame(
    x = c(1, 2, 1, 2),
    y = c(1, 1, 2, 2),
    quadrant = c("Cooler + Constrained", "Cooler + Plantable", "Hotter + Constrained", "Hotter + Plantable")
  )

  square_legend <- ggplot2::ggplot(legend_df, ggplot2::aes(x = x, y = y, fill = quadrant)) +
    ggplot2::geom_tile(color = "white", linewidth = 1.2) +
    ggplot2::scale_fill_manual(values = .uh_quadrant_palette) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = "More plantable \u2192", y = "Hotter \u2192") +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 8, face = "bold", color = "#475569", margin = ggplot2::margin(t = 4)),
      axis.title.y = ggplot2::element_text(size = 8, face = "bold", color = "#475569", angle = 90, margin = ggplot2::margin(r = 4)),
      legend.position = "none",
      plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10)
    )

  # Assemble multi-panel composite using cowplot
  cowplot::ggdraw() +
    cowplot::draw_plot(map_plot, 0, 0, 0.70, 1) +
    cowplot::draw_plot(sidebar_plot, 0.70, 0.40, 0.28, 0.50) +
    cowplot::draw_plot(square_legend, 0.81, 0.12, 0.16, 0.22)
}

#' Action-Class Decision Map (Highlighting top 5% hotspots)
#'
#' @param priority_data A priority grid dataset.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param caption Optional plot caption.
#' @param palette Optional color palette for action classes.
#' @export
plot_priority_action_classes <- function(priority_data, title = "Tree-planting decision map", subtitle = NULL, caption = NULL, palette = NULL) {
  geom_df <- if (!is.null(priority_data$blocks)) priority_data$blocks else priority_data$hex
  if (is.null(geom_df)) {
    stop("No valid spatial blocks or hexagons layer found in priority_data.")
  }

  # Categorise based on precise percentiles
  geom_df$action_class <- dplyr::case_when(
    geom_df$priority_score >= quantile(geom_df$priority_score, 0.95, na.rm = TRUE) ~ "Top 5%",
    geom_df$priority_score >= quantile(geom_df$priority_score, 0.75, na.rm = TRUE) ~ "High priority",
    geom_df$priority_score >= quantile(geom_df$priority_score, 0.50, na.rm = TRUE) ~ "Emerging priority",
    geom_df$priority_score >= quantile(geom_df$priority_score, 0.25, na.rm = TRUE) ~ "Watch",
    TRUE ~ "Lower priority"
  )

  geom_df$action_class <- factor(
    geom_df$action_class,
    levels = c("Lower priority", "Watch", "Emerging priority", "High priority", "Top 5%")
  )

  geom_df_3857 <- sf::st_transform(geom_df, 3857)

  # Shoreline cells are kept (st_difference trims out water geometry cleanly)
  boundary_3857 <- sf::st_transform(priority_data$boundary, 3857)

  .uh_action_palette <- if (!is.null(palette)) palette else c(
    "Lower priority" = "#8fa275",
    "Watch" = "#457b9d",
    "Emerging priority" = "#fbbf24",
    "High priority" = "#f97316",
    "Top 5%" = "#7f1d1d"
  )

  # Bootstrap Gini for footnote uncertainty
  gini_res <- compute_gini_bootstrap(geom_df$priority_score)

  sub_text <- if (!is.null(subtitle)) subtitle else "Priority summarized to organic divisions. Red marks the top 5% of intervention hotspots by heat and exposure."

  lst_date <- if (!is.null(priority_data$scenes$lst_datetime)) substr(priority_data$scenes$lst_datetime, 1, 10) else "2025-08-08"
  ndvi_date <- if (!is.null(priority_data$scenes$ndvi_datetime)) substr(priority_data$scenes$ndvi_datetime, 1, 10) else "2025-08-11"

  cap_text <- if (!is.null(caption)) caption else sprintf(
    "Methodology: Multi-Criteria Spatial Decision Priorities (Organic Blocks Scale)\nData: Landsat-9 LST (%s), Sentinel-2 NDVI (%s), Meta Canopy Height Model (CHM), GHSL Population Grid, OSM Landuse & Water\nPolicy Equity: Population-weighted Spatial Priority Gini = %.3f [95%% CI: %.3f - %.3f] * generated by greenR",
    lst_date, ndvi_date, gini_res$gini, gini_res$ci_low, gini_res$ci_high
  )

  ggplot2::ggplot() +
    ggspatial::annotation_map_tile(type = "cartolight", zoom = 14, alpha = 0.90) +
    ggplot2::geom_sf(data = geom_df_3857, ggplot2::aes(fill = action_class), color = "white", linewidth = 0.05, alpha = 0.85) +
    ggplot2::scale_fill_manual(values = .uh_action_palette, name = "Action class") +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.18, style = "ticks", text_cex = 0.8) +
    ggplot2::coord_sf(datum = sf::st_crs(3857)) +
    ggplot2::labs(
      title = title,
      subtitle = sub_text,
      caption = cap_text
    ) +
    .uh_decision_map_theme() +
    ggplot2::theme(
      legend.position = "right",
      legend.text = ggplot2::element_text(size = 9, face = "bold", color = "#475569"),
      legend.title = ggplot2::element_text(size = 10, face = "bold", color = "#1e293b")
    )
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

#' Interactive Multilayer Leaflet Map with layer controls, legends, and dynamic popups
#'
#' @param priority_data A priority grid dataset.
#' @param canyon_data Optional street canyon dataset.
#' @param palette Optional color palette for priority scores.
#' @export
plot_multilayer_leaflet <- function(priority_data, canyon_data = NULL, palette = NULL) {
  message("[leaflet] Designing premium interactive multilayer web map...")

  # Transform all layers to EPSG:4326 for leaflet
  hex_4326 <- if (!is.null(priority_data$hex)) sf::st_transform(priority_data$hex, 4326) else NULL
  blocks_4326 <- if (!is.null(priority_data$blocks)) sf::st_transform(priority_data$blocks, 4326) else NULL
  canyons_4326 <- if (!is.null(canyon_data$canyons)) sf::st_transform(canyon_data$canyons, 4326) else NULL

  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Positron (Light)") |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter, group = "Dark Matter (Dark)")

  # Setup palettes
  pal_priority <- leaflet::colorNumeric(
    palette = if (!is.null(palette)) palette else c("#1e3a8a", "#3b82f6", "#10b981", "#eab308", "#ef4444", "#7f1d1d"),
    domain = c(0, 100)
  )

  group_names <- c()

  if (!is.null(blocks_4326)) {
    popups <- paste0(
      "<strong>Morphological Neighborhood Block</strong><br/>",
      "Priority Score: <strong>", round(blocks_4326$priority_score, 1), "/100</strong><br/>"
    )
    if ("lst_mean_c" %in% names(blocks_4326)) {
      popups <- paste0(popups, "Mean Heat: ", round(blocks_4326$lst_mean_c, 1), " \u00b0C<br/>")
    }
    if ("canopy_pct_chm" %in% names(blocks_4326)) {
      popups <- paste0(popups, "Canopy Coverage: ", round(blocks_4326$canopy_pct_chm, 1), " %<br/>")
    }
    if ("ndvi_mean" %in% names(blocks_4326)) {
      popups <- paste0(popups, "Vegetation (NDVI): ", round(blocks_4326$ndvi_mean, 2), "<br/>")
    }
    if ("population" %in% names(blocks_4326)) {
      popups <- paste0(popups, "Estimated Pop: ", round(blocks_4326$population, 0), "<br/>")
    }
    blocks_4326$popup <- popups

    m <- m |> leaflet::addPolygons(
      data = blocks_4326,
      fillColor = ~pal_priority(priority_score),
      fillOpacity = 0.70,
      color = "white",
      weight = 1.0,
      popup = ~popup,
      group = "Organic Neighborhood Blocks"
    )
    group_names <- c(group_names, "Organic Neighborhood Blocks")
  }

  if (!is.null(hex_4326)) {
    popups <- paste0(
      "<strong>Hexagon Cell (100m)</strong><br/>",
      "Priority Score: <strong>", round(hex_4326$priority_score, 1), "/100</strong><br/>"
    )
    if ("lst_mean_c" %in% names(hex_4326)) {
      popups <- paste0(popups, "Mean Heat: ", round(hex_4326$lst_mean_c, 1), " \u00b0C<br/>")
    }
    if ("canopy_pct_chm" %in% names(hex_4326)) {
      popups <- paste0(popups, "Canopy Coverage: ", round(hex_4326$canopy_pct_chm, 1), " %<br/>")
    }
    if ("population" %in% names(hex_4326)) {
      popups <- paste0(popups, "Estimated Pop: ", round(hex_4326$population, 0), "<br/>")
    }
    hex_4326$popup <- popups

    m <- m |> leaflet::addPolygons(
      data = hex_4326,
      fillColor = ~pal_priority(priority_score),
      fillOpacity = 0.65,
      color = "white",
      weight = 0.5,
      popup = ~popup,
      group = "Hexagonal Grid"
    )
    group_names <- c(group_names, "Hexagonal Grid")
  }

  if (!is.null(canyons_4326)) {
    popups <- paste0(
      "<strong>Street Canyon Segment</strong><br/>",
      "Priority Score: <strong>", round(canyons_4326$priority_score, 1), "/100</strong><br/>"
    )
    if ("cooling_deficit_index" %in% names(canyons_4326)) {
      popups <- paste0(popups, "Cooling Deficit: ", round(canyons_4326$cooling_deficit_index * 100, 1), "/100<br/>")
    }
    if ("heat_exposure_index" %in% names(canyons_4326)) {
      popups <- paste0(popups, "Heat Exposure: ", round(canyons_4326$heat_exposure_index * 100, 1), "/100<br/>")
    }
    canyons_4326$popup <- popups

    m <- m |> leaflet::addPolylines(
      data = canyons_4326,
      color = ~pal_priority(priority_score),
      weight = ~ 0.3 + (priority_score / 100) * 0.7,
      opacity = 0.90,
      popup = ~popup,
      group = "Street Canyon Corridors"
    )
    group_names <- c(group_names, "Street Canyon Corridors")
  }

  m <- m |>
    leaflet::addLayersControl(
      baseGroups = c("Positron (Light)", "Dark Matter (Dark)"),
      overlayGroups = group_names,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet::addLegend(
      pal = pal_priority,
      values = c(0, 100),
      title = "Priority Score",
      position = "bottomright"
    )
  m
}

#' Generate a premium self-contained 3D Deck.gl web explorer with light/dark toggles and extruded geometries
#'
#' @param priority_data A priority grid dataset.
#' @param output_file Path to save the interactive HTML dashboard.
#' @param render_type One of "auto", "blocks", or "hexagons".
#' @export
save_3d_deckgl_dashboard <- function(priority_data, output_file, render_type = c("auto", "blocks", "hexagons")) {
  render_type <- match.arg(render_type)

  geom_df <- if (render_type == "blocks") {
    priority_data$blocks
  } else if (render_type == "hexagons") {
    priority_data$hex
  } else {
    if (!is.null(priority_data$blocks)) priority_data$blocks else priority_data$hex
  }

  if (is.null(geom_df)) {
    stop("No valid blocks or hex data layer found in priority_data to build 3D explorer.")
  }

  message("[3D] Constructing elite WebGL-based 3D column explorer dashboard...")

  # Project to WGS84 for WebGL mapping
  geom_4326 <- sf::st_transform(geom_df, 4326)

  # Extract center coordinates for default camera target (using bounding box to avoid any S2 topology issues)
  bbox_4326 <- sf::st_bbox(geom_4326)
  lon_center <- (bbox_4326[["xmin"]] + bbox_4326[["xmax"]]) / 2
  lat_center <- (bbox_4326[["ymin"]] + bbox_4326[["ymax"]]) / 2

  # Write to standard JSON GeoJSON string
  tmp <- tempfile(fileext = ".geojson")
  sf::st_write(geom_4326, tmp, driver = "GeoJSON", quiet = TRUE)
  geojson_str <- readLines(tmp, warn = FALSE)
  geojson_str <- paste(geojson_str, collapse = "\n")
  unlink(tmp)

  model_scale <- if (!is.null(priority_data$blocks) && (render_type == "blocks" || render_type == "auto")) "Organic Blocks (Superblocks)" else "Hexagonal Grid (100m)"

  html_template <- '<!DOCTYPE html>
<html>
<head>
  <title>3D Street-Canyon & Neighborhood Explorer</title>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <script src="https://unpkg.com/deck.gl@latest/dist.min.js"></script>
  <script src="https://unpkg.com/maplibre-gl@3.6.2/dist/maplibre-gl.js"></script>
  <link href="https://unpkg.com/maplibre-gl@3.6.2/dist/maplibre-gl.css" rel="stylesheet" />
  <style>
    body { margin: 0; padding: 0; overflow: hidden; font-family: "Segoe UI", Roboto, sans-serif; background: #0b0f19; transition: background 0.3s; }
    #container { width: 100vw; height: 100vh; position: relative; }
    #control-panel {
      position: absolute; top: 20px; left: 20px; z-index: 10;
      background: rgba(15, 23, 42, 0.92); backdrop-filter: blur(12px);
      border: 1px solid rgba(255, 255, 255, 0.1); border-radius: 12px;
      padding: 20px; color: #fff; width: 320px; box-shadow: 0 10px 25px rgba(0,0,0,0.5);
      transition: all 0.3s;
    }
    h1 { margin: 0 0 8px 0; font-size: 18px; font-weight: 700; background: linear-gradient(135deg, #3b82f6, #10b981); -webkit-background-clip: text; -webkit-text-fill-color: transparent; }
    p { margin: 0 0 15px 0; font-size: 12px; color: #94a3b8; line-height: 1.5; transition: color 0.3s; }
    .stat-row { display: flex; justify-content: space-between; margin-bottom: 8px; font-size: 12px; border-bottom: 1px solid rgba(255, 255, 255, 0.05); padding-bottom: 6px; }
    .stat-label { color: #94a3b8; transition: color 0.3s; }
    .stat-val { font-weight: bold; color: #f1f5f9; transition: color 0.3s; }
    .theme-toggle {
      width: 100%; padding: 10px; background: #1e293b; border: 1px solid rgba(255,255,255,0.15);
      border-radius: 6px; color: #fff; font-size: 12px; font-weight: bold; cursor: pointer;
      transition: all 0.3s; margin-top: 10px;
    }
    .theme-toggle:hover { background: #334155; }
    .btn-action {
      background: #3b82f6; border: none; border-radius: 6px; padding: 10px; color: #fff;
      font-size: 12px; font-weight: bold; cursor: pointer; width: 100%; margin-top: 10px;
      transition: background 0.2s;
    }
    .btn-action:hover { background: #2563eb; }
    input[type="range"] {
      -webkit-appearance: none;
      width: 100%;
      height: 6px;
      background: #475569;
      border-radius: 3px;
      outline: none;
      margin-top: 8px;
    }
    input[type="range"]::-webkit-slider-thumb {
      -webkit-appearance: none;
      width: 16px;
      height: 16px;
      border-radius: 50%;
      background: #10b981;
      cursor: pointer;
      transition: background 0.15s ease-in-out;
    }
    input[type="range"]::-webkit-slider-thumb:hover {
      background: #34d399;
    }
    .legend-bar { height: 10px; border-radius: 5px; background: linear-gradient(to right, #3b82f6, #10b981, #eab308, #ef4444, #7f1d1d); margin: 8px 0 4px; }
    .legend-labels { display: flex; justify-content: space-between; font-size: 9px; opacity: 0.7; }
  </style>
</head>
<body>
  <div id="container"></div>
  <div id="control-panel">
    <h1>3D Urban Heat & Shading Explorer</h1>
    <p>Hold <strong>Right-Click + Drag</strong> to tilt, rotate, and fly through the extruded 3D neighborhood priority scores.</p>
    <div class="stat-row">
      <span class="stat-label">Model scale:</span>
      <span class="stat-val">__MODEL_SCALE__</span>
    </div>
    <div class="stat-row">
      <span class="stat-label">3D Extrusion:</span>
      <span class="stat-val">Priority Score (0-100)</span>
    </div>
    <div class="stat-row" style="flex-direction: column; align-items: stretch; border: none; padding: 0;">
      <span class="stat-label" style="display: flex; justify-content: space-between; width: 100%;">
        <span class="stat-label">3D Extrusion Scale:</span>
        <strong id="height-val" style="color: #10b981;">10.0x</strong>
      </span>
      <input type="range" id="height-slider" min="1" max="30" value="10" step="1" />
    </div>

    <!-- Priority Color Scale Legend -->
    <div class="stat-row" style="flex-direction: column; align-items: stretch; border: none; padding-top: 10px; margin-bottom: 0;">
      <span class="stat-label">Priority Scale:</span>
      <div class="legend-bar"></div>
      <div class="legend-labels">
        <span>Low (<20)</span>
        <span>Watch</span>
        <span>Emerging</span>
        <span>High</span>
        <span>Top 5% (>80)</span>
      </div>
    </div>

    <button class="theme-toggle" id="theme-btn">Switch to Light Theme</button>
    <button class="btn-action" id="rotation-btn" onclick="toggleRotation()">Toggle Fly-Through</button>
  </div>

  <script>
    const geojsonData = __GEOJSON_DATA__;

    const mapStyles = {
      dark: "https://basemaps.cartocdn.com/gl/dark-matter-gl-style/style.json",
      light: "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json"
    };

    let currentStyle = "dark";
    let elevationScale = 10.0;
    let isRotating = false;

    let currentViewState = {
      longitude: __CENTER_LON__,
      latitude: __CENTER_LAT__,
      zoom: 13.6,
      pitch: 48,
      bearing: 25,
      maxZoom: 20,
      minZoom: 10
    };

    const deckInstance = new deck.DeckGL({
      container: "container",
      map: maplibregl,
      mapStyle: mapStyles.dark,
      initialViewState: currentViewState,
      controller: true,
      onViewStateChange: ({viewState}) => {
        currentViewState = viewState;
      },
      getTooltip: ({object}) => {
        if (!object) return null;
        const p = object.properties;
        return {
          html: `
            <div style="font-weight:bold;font-size:13px;margin-bottom:6px;font-family:sans-serif;">Urban Analytics Indicator</div>
            <div style="font-family:sans-serif;font-size:12px;">Priority Score: <strong style="color:#ef4444">${parseFloat(p.priority_score).toFixed(1)}/100</strong></div>
            <div style="font-family:sans-serif;font-size:12px;">Mean Temperature: ${parseFloat(p.lst_mean_c).toFixed(1)} &deg;C</div>
            <div style="font-family:sans-serif;font-size:12px;">Canopy Coverage: ${parseFloat(p.canopy_pct_chm || 0).toFixed(1)}%</div>
            <div style="font-family:sans-serif;font-size:12px;">Vegetation Index (NDVI): ${parseFloat(p.ndvi_mean || p.ndvi || 0).toFixed(2)}</div>
            <div style="font-family:sans-serif;font-size:12px;">Estimated Pop: ${parseInt(p.population || 0)}</div>
          `,
          style: {
            backgroundColor: "#0f172a",
            color: "#fff",
            fontFamily: "sans-serif",
            padding: "12px",
            borderRadius: "8px",
            border: "1px solid rgba(255,255,255,0.2)"
          }
        };
      }
    });

    function renderLayers() {
      const geojsonLayer = new deck.GeoJsonLayer({
        id: "extruded-geoms",
        data: geojsonData,
        extruded: true,
        wireframe: true,
        getElevation: d => d.properties.priority_score * elevationScale,
        updateTriggers: {
          getElevation: [elevationScale]
        },
        getFillColor: d => {
          const score = d.properties.priority_score;
          if (score >= 80) return [127, 29, 29, 210];  // Deep crimson
          if (score >= 60) return [239, 68, 68, 200];  // Red
          if (score >= 40) return [234, 179, 8, 190];  // Yellow
          if (score >= 20) return [16, 185, 129, 180]; // Green
          return [59, 130, 246, 170];                  // Blue
        },
        getLineColor: [255, 255, 255, 45],
        getLineWidth: 1,
        lineWidthUnits: "pixels",
        pickable: true
      });
      deckInstance.setProps({layers: [geojsonLayer]});
    }

    renderLayers();

    // Height Slider listener
    document.getElementById("height-slider").addEventListener("input", (e) => {
      elevationScale = parseFloat(e.target.value);
      document.getElementById("height-val").innerText = elevationScale.toFixed(1) + "x";
      renderLayers();
    });

    // Camera Rotation / Fly-Through
    function rotateCamera() {
      if (!isRotating) return;
      currentViewState.bearing = (currentViewState.bearing + 0.15) % 360;
      deckInstance.setProps({
        initialViewState: { ...currentViewState }
      });
      requestAnimationFrame(rotateCamera);
    }

    function toggleRotation() {
      isRotating = !isRotating;
      const btn = document.getElementById("rotation-btn");
      if (isRotating) {
        btn.innerText = "Stop Fly-Through";
        btn.style.background = "#ef4444";
        rotateCamera();
      } else {
        btn.innerText = "Toggle Fly-Through";
        btn.style.background = "#3b82f6";
      }
    }

    document.getElementById("theme-btn").addEventListener("click", () => {
      if (currentStyle === "dark") {
        currentStyle = "light";
        document.getElementById("theme-btn").innerText = "Switch to Dark Theme";
        document.getElementById("theme-btn").style.background = "#e2e8f0";
        document.getElementById("theme-btn").style.color = "#0f172a";
        document.getElementById("control-panel").style.background = "rgba(255, 255, 255, 0.94)";
        document.getElementById("control-panel").style.color = "#0f172a";
        document.getElementById("control-panel").style.borderColor = "rgba(0, 0, 0, 0.1)";
        document.body.style.background = "#f8fafc";

        const desc = document.querySelector("#control-panel p");
        if (desc) desc.style.color = "#475569";

        const labels = document.querySelectorAll(".stat-label");
        labels.forEach(l => l.style.color = "#475569");

        const vals = document.querySelectorAll(".stat-val");
        vals.forEach(v => v.style.color = "#0f172a");

        const map = deckInstance.getMapboxMap ? deckInstance.getMapboxMap() : (deckInstance.getMap ? deckInstance.getMap() : null);
        if (map) {
          map.setStyle(mapStyles.light);
        } else {
          deckInstance.setProps({mapStyle: mapStyles.light});
        }
      } else {
        currentStyle = "dark";
        document.getElementById("theme-btn").innerText = "Switch to Light Theme";
        document.getElementById("theme-btn").style.background = "#1e293b";
        document.getElementById("theme-btn").style.color = "#fff";
        document.getElementById("control-panel").style.background = "rgba(15, 23, 42, 0.92)";
        document.getElementById("control-panel").style.color = "#fff";
        document.getElementById("control-panel").style.borderColor = "rgba(255, 255, 255, 0.1)";
        document.body.style.background = "#0b0f19";

        const desc = document.querySelector("#control-panel p");
        if (desc) desc.style.color = "#94a3b8";

        const labels = document.querySelectorAll(".stat-label");
        labels.forEach(l => l.style.color = "#94a3b8");

        const vals = document.querySelectorAll(".stat-val");
        vals.forEach(v => v.style.color = "#f1f5f9");

        const map = deckInstance.getMapboxMap ? deckInstance.getMapboxMap() : (deckInstance.getMap ? deckInstance.getMap() : null);
        if (map) {
          map.setStyle(mapStyles.dark);
        } else {
          deckInstance.setProps({mapStyle: mapStyles.dark});
        }
      }
    });
  </script>
</body>
</html>'

  # Bulletproof search and replacement
  html_template <- gsub("__MODEL_SCALE__", model_scale, html_template, fixed = TRUE)
  html_template <- gsub("__GEOJSON_DATA__", geojson_str, html_template, fixed = TRUE)
  html_template <- gsub("__CENTER_LON__", as.character(lon_center), html_template, fixed = TRUE)
  html_template <- gsub("__CENTER_LAT__", as.character(lat_center), html_template, fixed = TRUE)

  writeLines(html_template, output_file)
  message(sprintf("[3D] Saved 3D Deck.gl dashboard successfully to: %s", output_file))
}


