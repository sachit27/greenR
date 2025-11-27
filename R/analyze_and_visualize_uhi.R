# Suppress R CMD check notes for NSE variables used in dplyr/ggplot2
utils::globalVariables(c(
  "h3_address", "LST_mean", "LST_diff", "Green_Pct", "Built_Pct",
  "Hotspot_Category", "resid_g", "x_mid", "slope", "slope_clipped", "highway"
))

#' Urban Heat Island Analysis & Visualization
#'
#' Performs a comprehensive UHI analysis: fetches thermal data (ECOSTRESS or Landsat),
#' retrieves environmental data using `get_osm_data` (green spaces, trees, highways),
#' computes green coverage using fast raster-based methods, calculates built-up coverage,
#' performs spatial hotspot analysis, and generates interactive and static maps.
#'
#' @details
#' **CRAN policy note:** This function downloads data from the internet (OpenStreetMap,
#' Microsoft Planetary Computer STAC API for satellite imagery). Internet access is
#' required for this function to work. The function will fail gracefully with
#' informative error messages if network access is unavailable.
#'
#' **Data Sources:**
#' - Land Surface Temperature: ECOSTRESS (70m) or Landsat 8/9 (100m thermal) via
#'   Microsoft Planetary Computer STAC API
#' - Green spaces, trees, buildings, water bodies: OpenStreetMap via osmdata
#' - Optional: GHSL Built-S raster for built-up coverage
#' - Optional: NDVI from Landsat for satellite-based vegetation index
#'
#' **Analysis Components:**
#' - H3 hexagonal grid aggregation at configurable resolution
#' - Green coverage from OSM polygons, tree points (with canopy buffer), and NDVI
#' - Built-up coverage from GHSL or OSM buildings (fallback)
#' - Water body masking to exclude water hexagons from land-based analyses
#' - Getis-Ord Gi* hotspot analysis with significance testing
#' - Moran's I spatial autocorrelation
#' - Correlation and regression analysis (LST vs Green/Built)
#'
#' **Output Maps:**
#' - Interactive Leaflet map with toggleable layers (LST, Deviation, Green, Built, Hotspots)
#' - Static ggplot2 maps for publication
#' - Scatter plot dashboard with regression diagnostics
#'
#' @param location Character or numeric vector. Either a city name (e.g., "Paris, France")
#'        or a bounding box as c(xmin, ymin, xmax, ymax) in EPSG:4326.
#' @param date_range Character vector of length 2. Date range for satellite imagery
#'        in ISO format, e.g., c("2023-06-01", "2023-08-31"). Summer months recommended
#'        for UHI analysis.
#' @param hex_resolution Integer. H3 hexagon resolution (default 9).
#'        Higher values = smaller hexagons. Range: 0-15.
#' @param ghsl_path Character or NULL. Path to GHSL Built-S raster (.tif) for built-up
#'        coverage. If NULL (default), uses OSM buildings as fallback.
#' @param tree_canopy_radius Numeric. Buffer radius for tree points in meters (default 5).
#'        Represents approximate canopy spread.
#' @param thermal_source Character. One of "auto", "ecostress", or "landsat".
#'        Default "auto" tries ECOSTRESS first, then Landsat.
#' @param composite_scenes Logical. Whether to composite multiple satellite scenes
#'        using median (default FALSE). Useful for reducing cloud gaps.
#' @param max_scenes Integer. Maximum number of scenes to composite if composite_scenes=TRUE
#'        (default 5).
#' @param lst_percentile_filter Numeric vector of length 2 or NULL. Lower and upper
#'        percentiles for LST outlier removal (default c(0.01, 0.99)).
#'        Set to NULL to disable filtering.
#' @param correlation_method Character. Correlation method: "spearman" (default, robust)
#'        or "pearson".
#' @param use_exactextract Logical. Use exactextractr package for faster raster extraction
#'        if available (default TRUE). Falls back to terra::extract if not installed.
#' @param parallel Logical. Reserved for future parallel processing (currently ignored).
#' @param n_cores Integer or NULL. Reserved for future use (currently ignored).
#'
#' @return A list with the following components:
#' \describe{
#'   \item{results}{An sf object with hexagon-level results including LST_mean, LST_diff,
#'     Green_Pct, Built_Pct, Water_Pct, Gi_Star, Hotspot_Category, etc.}
#'   \item{maps}{A list of map objects:
#'     \itemize{
#'       \item interactive: Leaflet map with all layers
#'       \item lst, deviation, green, built, hotspot: Individual ggplot2 maps
#'       \item combined: Patchwork combined static map
#'       \item scatter: Scatter plot dashboard
#'     }
#'   }
#'   \item{stats}{A list with descriptive statistics, correlations, regression results,
#'     and spatial autocorrelation (Moran's I)}
#'   \item{meta}{Metadata including location, data sources, processing parameters,
#'     and timing information}
#'   \item{export_geojson}{Function to export results to GeoJSON}
#'   \item{export_results}{Function to export results to multiple formats (geojson, gpkg, csv, shp)}
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter rename left_join select mutate case_when
#' @importFrom sf st_as_sfc st_bbox st_as_sf st_crs st_transform st_make_valid st_intersection st_union st_area st_drop_geometry st_centroid st_coordinates st_collection_extract st_buffer st_crop st_write
#' @importFrom rstac stac stac_search get_request sign_planetary_computer
#' @importFrom terra rast crop mask project values app extract vect crs clamp ext rasterize res focal focalMat
#' @importFrom h3jsr polygon_to_cells cell_to_polygon
#' @importFrom spdep knn2nb knearneigh nb2listw localG moran.test
#' @importFrom ggplot2 ggplot aes geom_sf geom_point geom_smooth geom_bin2d geom_line geom_hline annotate coord_sf theme_void theme theme_minimal element_text element_blank element_line labs scale_fill_distiller scale_fill_brewer scale_fill_manual scale_fill_gradientn guides guide_legend unit margin
#' @importFrom leaflet leaflet addProviderTiles addPolygons addLayersControl addLegend layersControlOptions providers colorNumeric colorFactor hideGroup
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom stats cor.test lm coef residuals pnorm median quantile sd IQR shapiro.test kruskal.test pf na.omit var cor
#' @importFrom scales squish
#' @importFrom osmdata opq add_osm_feature osmdata_sf getbb
#' @importFrom htmlwidgets onRender
#' @importFrom utils write.csv
#'
#' @examples
#' \dontrun{
#' # Basic usage with city name
#' result <- analyze_and_visualize_uhi(
#'   location = "Basel, Switzerland",
#'   date_range = c("2023-06-01", "2023-08-31")
#' )
#'
#' # View interactive map
#' result$maps$interactive
#'
#' # View static combined map
#' print(result$maps$combined)
#'
#' # View statistics
#' print(result$stats$descriptive)
#' print(result$stats$correlations)
#'
#' # Export results
#' result$export_geojson("basel_uhi_results.geojson")
#' result$export_results("basel_uhi", formats = c("geojson", "csv"))
#'
#' # Using bounding box instead of city name
#' result_bbox <- analyze_and_visualize_uhi(
#'   location = c(7.55, 47.53, 7.65, 47.58),  # Basel area
#'   date_range = c("2023-07-01", "2023-07-31"),
#'   thermal_source = "landsat",
#'   hex_resolution = 9  # Larger hexagons
#' )
#'
#' # With GHSL built-up data for more accurate built coverage
#' result_ghsl <- analyze_and_visualize_uhi(
#'   location = "Zurich, Switzerland",
#'   date_range = c("2023-06-01", "2023-08-31"),
#'   ghsl_path = "path/to/ghsl_built.tif"
#' )
#' }
#'
#' @export
analyze_and_visualize_uhi <- function(
    location,
    date_range = c("2023-06-01", "2023-08-31"),
    hex_resolution = 9,
    ghsl_path = NULL,
    tree_canopy_radius = 5,
    thermal_source = c("auto", "ecostress", "landsat"),
    composite_scenes = FALSE,
    max_scenes = 5,
    lst_percentile_filter = c(0.01, 0.99),
    correlation_method = c("spearman", "pearson"),
    use_exactextract = TRUE,
    parallel = FALSE,
    n_cores = NULL
) {

  # Match arguments
  thermal_source <- match.arg(thermal_source)
  correlation_method <- match.arg(correlation_method)

  # Null coalescing operator
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

  # --- Check for exactextractr ---
  exactextract_available <- FALSE
  if (use_exactextract) {
    exactextract_available <- requireNamespace("exactextractr", quietly = TRUE)
    if (!exactextract_available) {
      message("   [i] Install 'exactextractr' for 5-10x faster raster extraction")
    }
  }

  # --- Fast extraction helper (defined early for use throughout) ---
  fast_extract <- function(r, p, f = "mean") {
    if (exactextract_available) {
      exactextractr::exact_extract(r, p, fun = f, progress = FALSE)
    } else {
      terra::extract(r, terra::vect(p), fun = match.fun(f), na.rm = TRUE)[, 2]
    }
  }

  message("\n+========================================+")
  message("|    greenR UHI ANALYSIS - OPTIMIZED     |")
  message("+========================================+\n")

  overall_start <- Sys.time()

  # ===========================================================================
  # STEP 1: RESOLVE BOUNDARY POLYGON (ADMIN LEVEL 8 WHERE POSSIBLE)
  # ===========================================================================
  message("[Step] Step 1: Resolving Study Area Boundary")
  step1_start <- Sys.time()

  city_boundary_sf <- NULL
  bbox_vec <- NULL
  location_name <- ""

  if (is.numeric(location) && length(location) == 4) {
    message("   -> Using provided bounding box")
    bbox_vec <- c(left = location[1], bottom = location[2],
                  right = location[3], top = location[4])

    # Create bbox object with explicit numeric values (no names in st_bbox)
    bbox_obj <- sf::st_bbox(
      c(xmin = unname(location[1]), ymin = unname(location[2]),
        xmax = unname(location[3]), ymax = unname(location[4])),
      crs = sf::st_crs(4326)
    )
    city_boundary_sf <- sf::st_as_sfc(bbox_obj) %>% sf::st_as_sf()

    location_name <- sprintf("bbox(%.2f,%.2f,%.2f,%.2f)",
                             bbox_vec["left"], bbox_vec["bottom"],
                             bbox_vec["right"], bbox_vec["top"])

  } else if (is.character(location)) {
    message(sprintf("   -> Geocoding: %s", location))
    location_name <- location

    # Try admin_level=8 boundary first
    city_boundary_sf <- tryCatch({
      message("   -> Trying admin_level = 8 administrative boundary from OSM...")
      q <- osmdata::opq(bbox = location) %>%
        osmdata::add_osm_feature(key = "boundary", value = "administrative") %>%
        osmdata::add_osm_feature(key = "admin_level", value = "8")
      od <- osmdata::osmdata_sf(q)

      poly <- od$osm_multipolygons
      if (is.null(poly) || nrow(poly) == 0) {
        poly <- od$osm_polygons
      }
      if (is.null(poly) || nrow(poly) == 0) stop("No admin_level = 8 polygon found for this name.")

      poly <- poly[which.max(sf::st_area(poly)), ]
      poly <- sf::st_make_valid(poly)
      # Ensure single multipart polygon
      poly_union <- sf::st_union(poly)
      sf::st_as_sf(poly_union)
    }, error = function(e) {
      message("   [!] Admin-level 8 lookup failed, falling back to getbb(): ", e$message)
      NULL
    })

    if (is.null(city_boundary_sf)) {
      # Fallback to getbb polygon
      city_boundary_sf <- tryCatch({
        poly_res <- osmdata::getbb(location, format_out = "sf_polygon")
        if (inherits(poly_res, "list")) poly_res <- poly_res$multipolygon %||% poly_res$polygon
        if (is.null(poly_res)) stop("No polygon found via getbb().")
        poly_res <- poly_res[which.max(sf::st_area(poly_res)), ]
        sf::st_as_sf(poly_res)
      }, error = function(e) {
        stop("[ERROR] Boundary resolution failed: ", e$message)
      })
    }

    bbox_obj <- sf::st_bbox(city_boundary_sf)
    bbox_vec <- c(
      left = unname(bbox_obj["xmin"]), bottom = unname(bbox_obj["ymin"]),
      right = unname(bbox_obj["xmax"]), top = unname(bbox_obj["ymax"])
    )
  } else {
    stop("[ERROR] 'location' must be a character string or numeric vector of length 4")
  }

  message(sprintf("   [OK] Boundary resolved: %.4f, %.4f to %.4f, %.4f [%.1fs]",
                  bbox_vec["left"], bbox_vec["bottom"],
                  bbox_vec["right"], bbox_vec["top"],
                  as.numeric(difftime(Sys.time(), step1_start, units = "secs"))))

  # ===========================================================================
  # STEP 2: FETCH & CLIP OSM DATA
  # ===========================================================================
  message("\n[Step] Step 2: Fetching Environmental & Context Data (OSM)")
  step2_start <- Sys.time()

  # Call get_osm_data
  osm_raw <- tryCatch({
    get_osm_data(location)
  }, error = function(e) {
    stop("[ERROR] get_osm_data failed: ", e$message)
  })

  # Extract components safely
  osm_green_poly   <- osm_raw$green_areas$osm_polygons %||% NULL
  osm_trees_points <- osm_raw$trees$osm_points %||% NULL
  osm_highways     <- osm_raw$highways$osm_lines %||% NULL

  # Get counts
  n_green_poly <- if (is.null(osm_green_poly)) 0 else nrow(osm_green_poly)
  n_trees      <- if (is.null(osm_trees_points)) 0 else nrow(osm_trees_points)
  n_roads      <- if (is.null(osm_highways)) 0 else nrow(osm_highways)

  if (!is.null(osm_highways) && nrow(osm_highways) > 0) {
    osm_highways <- osm_highways %>%
      dplyr::filter(highway %in% c("motorway", "trunk", "primary", "secondary",
                                   "tertiary", "residential", "unclassified"))
  }

  message(sprintf("   [OK] OSM data fetched: %d green polygons, %d trees, %d road segments [%.1fs]",
                  n_green_poly, n_trees, n_roads,
                  as.numeric(difftime(Sys.time(), step2_start, units = "secs"))))

  if (n_trees > 50000) {
    message(sprintf("   [i] Large tree dataset (%s trees) - using optimized raster method",
                    format(n_trees, big.mark = ",")))
  }

  # ===========================================================================
  # STEP 3: SATELLITE THERMAL DATA ACQUISITION
  # ===========================================================================
  message("\n[Step]  Step 3: Thermal Data Acquisition")
  step3_start <- Sys.time()

  # --- Helper: ECOSTRESS ---
  fetch_ecostress <- function(bbox, dates, max_n, do_composite) {
    message("   -> Attempting ECOSTRESS retrieval...")

    s_obj <- tryCatch({
      rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")
    }, error = function(e) {
      message("   [X] Could not connect to Planetary Computer")
      return(NULL)
    })

    if (is.null(s_obj)) return(NULL)

    items <- tryCatch({
      s_obj %>%
        rstac::stac_search(collections = "ecostress-lst", bbox = bbox,
                           datetime = paste(dates, collapse = "/"), limit = 50) %>%
        rstac::get_request()
    }, error = function(e) {
      message("   [X] ECOSTRESS search failed: ", e$message)
      return(NULL)
    })

    if (is.null(items)) return(NULL)

    valid_items <- Filter(function(x) !is.null(x$assets$LST), items$features)
    if (length(valid_items) == 0) {
      message("   [X] No ECOSTRESS scenes found")
      return(NULL)
    }

    message(sprintf("   [OK] Found %d ECOSTRESS scenes", length(valid_items)))
    signer <- rstac::sign_planetary_computer()

    tryCatch({
      if (do_composite && length(valid_items) > 1) {
        n_use <- min(length(valid_items), max_n)
        message(sprintf("   -> Compositing %d scenes...", n_use))

        first <- signer(valid_items[[1]])
        ref_r <- terra::rast(paste0("/vsicurl/", first$assets$LST$href))
        ref_lst <- (ref_r * 0.02) - 273.15
        lst_list <- list(ref_lst)

        for (i in 2:n_use) {
          tryCatch({
            s <- signer(valid_items[[i]])
            r <- terra::rast(paste0("/vsicurl/", s$assets$LST$href))
            r_aligned <- terra::project((r * 0.02) - 273.15, ref_lst, method = "bilinear")
            lst_list[[length(lst_list) + 1]] <- r_aligned
          }, error = function(e) NULL)
        }

        lst_celsius <- terra::app(terra::rast(lst_list), fun = median, na.rm = TRUE)
        id <- paste0("ECOSTRESS_composite_n", length(lst_list))
      } else {
        s <- signer(valid_items[[1]])
        lst_celsius <- (terra::rast(paste0("/vsicurl/", s$assets$LST$href)) * 0.02) - 273.15
        id <- valid_items[[1]]$id
      }

      list(raster = lst_celsius, ndvi = NULL, scene_id = id,
           source = "ECOSTRESS", overpass_info = "Variable", resolution = "70m native")
    }, error = function(e) {
      message("   [X] ECOSTRESS data fetch failed: ", e$message)
      return(NULL)
    })
  }

  # --- Helper: Landsat ---
  fetch_landsat <- function(bbox, dates, max_n, do_composite) {
    message("   -> Attempting Landsat retrieval...")

    s_obj <- tryCatch({
      rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")
    }, error = function(e) {
      message("   [X] Could not connect to Planetary Computer")
      return(NULL)
    })

    if (is.null(s_obj)) return(NULL)

    items <- tryCatch({
      s_obj %>%
        rstac::stac_search(collections = "landsat-c2-l2", bbox = bbox,
                           datetime = paste(dates, collapse = "/"), limit = 50) %>%
        rstac::get_request()
    }, error = function(e) {
      message("   [X] Landsat search failed: ", e$message)
      return(NULL)
    })

    if (is.null(items)) return(NULL)

    # Filter for low cloud cover and Landsat 8/9
    clean <- Filter(function(x) {
      cc <- x$properties$`eo:cloud_cover`
      !is.null(cc) && cc < 20 && grepl("landsat-[89]", x$properties$platform, ignore.case = TRUE)
    }, items$features)

    if (length(clean) == 0) {
      message("   [X] No suitable Landsat scenes found (cloud cover < 20%)")
      return(NULL)
    }

    # Sort by cloud cover
    clean <- clean[order(sapply(clean, function(x) x$properties$`eo:cloud_cover`))]
    message(sprintf("   [OK] Found %d suitable Landsat scenes", length(clean)))

    signer <- rstac::sign_planetary_computer()

    # Helper to fetch thermal and NDVI bands
    fetch_bands <- function(item) {
      s <- signer(item)
      therm_url <- s$assets[["lwir11"]]$href %||% s$assets[["ST_B10"]]$href
      nir_url   <- s$assets[["nir08"]]$href  %||% s$assets[["SR_B5"]]$href
      red_url   <- s$assets[["red"]]$href   %||% s$assets[["SR_B4"]]$href

      res <- list(t = NULL, n = NULL)

      # Thermal band (LST)
      if (!is.null(therm_url)) {
        res$t <- (terra::rast(paste0("/vsicurl/", therm_url)) * 0.00341802 + 149.0) - 273.15
      }

      # NDVI bands
      if (!is.null(nir_url) && !is.null(red_url)) {
        nir <- terra::clamp(terra::rast(paste0("/vsicurl/", nir_url)) * 0.0000275 - 0.2, 0.001, 1)
        red <- terra::clamp(terra::rast(paste0("/vsicurl/", red_url)) * 0.0000275 - 0.2, 0.001, 1)
        ndvi <- (nir - red) / (nir + red)
        ndvi[ndvi < -1 | ndvi > 1] <- NA
        res$n <- ndvi
      }

      res
    }

    tryCatch({
      if (do_composite && length(clean) > 1) {
        n_use <- min(length(clean), max_n)
        message(sprintf("   -> Compositing %d scenes...", n_use))

        ref <- fetch_bands(clean[[1]])
        if (is.null(ref$t)) return(NULL)

        lst_list  <- list(ref$t)
        ndvi_list <- if (!is.null(ref$n)) list(ref$n) else list()

        for (i in 2:n_use) {
          tryCatch({
            b <- fetch_bands(clean[[i]])
            if (!is.null(b$t)) {
              lst_list[[length(lst_list) + 1]] <- terra::project(b$t, ref$t, method = "bilinear")
            }
            if (!is.null(b$n) && length(ndvi_list) > 0) {
              ndvi_list[[length(ndvi_list) + 1]] <- terra::project(b$n, ref$n, method = "bilinear")
            }
          }, error = function(e) NULL)
        }

        lst_celsius <- terra::app(terra::rast(lst_list), fun = median, na.rm = TRUE)
        ndvi_raster <- if (length(ndvi_list) > 0) {
          terra::app(terra::rast(ndvi_list), fun = median, na.rm = TRUE)
        } else NULL
        id <- paste0("Landsat_composite_n", length(lst_list))
      } else {
        b <- fetch_bands(clean[[1]])
        if (is.null(b$t)) return(NULL)
        lst_celsius <- b$t
        ndvi_raster <- b$n
        id <- clean[[1]]$id
      }

      list(raster = lst_celsius, ndvi = ndvi_raster, scene_id = id,
           source = "Landsat", overpass_info = "~10:00 AM local", resolution = "100m thermal")
    }, error = function(e) {
      message("   [X] Landsat data fetch failed: ", e$message)
      return(NULL)
    })
  }

  # --- Execute thermal data acquisition ---
  thermal_result <- NULL

  if (thermal_source == "ecostress") {
    thermal_result <- fetch_ecostress(bbox_vec, date_range, max_scenes, composite_scenes)
  } else if (thermal_source == "landsat") {
    thermal_result <- fetch_landsat(bbox_vec, date_range, max_scenes, composite_scenes)
  } else {
    # Auto mode: try ECOSTRESS first, then Landsat
    thermal_result <- fetch_ecostress(bbox_vec, date_range, max_scenes, composite_scenes)
    if (is.null(thermal_result)) {
      thermal_result <- fetch_landsat(bbox_vec, date_range, max_scenes, composite_scenes)
    }
  }

  if (is.null(thermal_result)) {
    stop("[ERROR] No suitable thermal imagery found. Try expanding date_range or check location.")
  }

  message(sprintf("   [OK] Using %s: %s [%.1fs]",
                  thermal_result$source, thermal_result$scene_id,
                  as.numeric(difftime(Sys.time(), step3_start, units = "secs"))))

  # --- Crop and mask to study area ---
  local_proj     <- terra::crs(thermal_result$raster)
  city_vect_utm  <- terra::vect(sf::st_transform(city_boundary_sf, local_proj))

  lst_celsius <- terra::mask(terra::crop(thermal_result$raster, city_vect_utm), city_vect_utm)
  ndvi_raster <- if (!is.null(thermal_result$ndvi)) {
    terra::mask(terra::crop(thermal_result$ndvi, city_vect_utm), city_vect_utm)
  } else NULL

  # --- Filter outliers ---
  raw_min <- min(terra::values(lst_celsius), na.rm = TRUE)
  raw_max <- max(terra::values(lst_celsius), na.rm = TRUE)

  lst_celsius[lst_celsius < -30 | lst_celsius > 80] <- NA

  if (!is.null(lst_percentile_filter) && length(lst_percentile_filter) == 2) {
    vals <- terra::values(lst_celsius)
    vals_valid <- vals[!is.na(vals)]
    if (length(vals_valid) > 100) {
      q <- quantile(vals_valid, lst_percentile_filter, na.rm = TRUE)
      lst_celsius[lst_celsius < q[1] | lst_celsius > q[2]] <- NA
      message(sprintf("   [i] LST filtered to %.1f-%.1fdeg C", q[1], q[2]))
    }
  }

  # ===========================================================================
  # STEP 4: H3 HEXAGONAL GRID
  # ===========================================================================
  message("\n[Step] Step 4: Generating Hexagonal Grid")
  step4_start <- Sys.time()

  hex_ids <- h3jsr::polygon_to_cells(city_boundary_sf, res = hex_resolution, simple = TRUE)
  hex_sf <- h3jsr::cell_to_polygon(unlist(hex_ids), simple = FALSE) %>%
    sf::st_as_sf() %>%
    dplyr::rename(hex_id = h3_address)

  hex_utm <- sf::st_transform(hex_sf, local_proj)
  hex_areas <- sf::st_area(hex_utm)
  names(hex_areas) <- hex_sf$hex_id

  message(sprintf("   [OK] Generated %d hexagons at resolution %d [%.1fs]",
                  nrow(hex_sf), hex_resolution,
                  as.numeric(difftime(Sys.time(), step4_start, units = "secs"))))

  # ===========================================================================
  # STEP 5: METRIC CALCULATIONS (OPTIMIZED)
  # ===========================================================================
  message("\n[Step] Step 5: Calculating Metrics")
  step5_start <- Sys.time()

  # --- A. LST Extraction ---
  message("   -> Extracting LST...")
  hex_sf$LST_mean <- fast_extract(lst_celsius, hex_utm)

  if (all(is.na(hex_sf$LST_mean))) {
    warning("[!] No valid LST data extracted! Check satellite coverage/clouds.")
    hex_sf$LST_diff <- NA
    city_mean_lst <- NA
  } else {
    # Temporary city mean (will later implicitly be land-only when masking)
    city_mean_lst <- mean(hex_sf$LST_mean, na.rm = TRUE)
    hex_sf$LST_diff <- hex_sf$LST_mean - city_mean_lst
  }

  # --- B. Green Coverage + Water Coverage (Raster-based) ---
  message("   -> Calculating green coverage (OSM + NDVI) and water mask...")
  green_start <- Sys.time()

  hex_sf$Green_Pct_OSM  <- 0
  hex_sf$Green_Pct_NDVI <- 0
  hex_sf$NDVI_mean      <- NA_real_
  hex_sf$Green_Pct      <- 0
  hex_sf$Water_Pct      <- NA_real_  # will be filled where OSM water exists

  if (n_green_poly > 0 || n_trees > 0) {
    message(sprintf("   [i] Using raster method (%s polygons, %s trees)",
                    format(n_green_poly, big.mark = ","),
                    format(n_trees, big.mark = ",")))

    ref_extent <- terra::ext(hex_utm)
    green_raster <- terra::rast(
      xmin = ref_extent[1], xmax = ref_extent[2],
      ymin = ref_extent[3], ymax = ref_extent[4],
      resolution = 10,
      crs = local_proj
    )
    terra::values(green_raster) <- 0

    # --- B.1: Rasterize Green Polygons ---
    if (n_green_poly > 0) {
      message(sprintf("   ... Rasterizing %s green polygons...", format(n_green_poly, big.mark = ",")))
      poly_start <- Sys.time()

      tryCatch({
        green_poly_proj <- sf::st_transform(osm_green_poly, local_proj)
        green_poly_proj <- sf::st_make_valid(green_poly_proj)

        green_poly_proj <- tryCatch({
          sf::st_crop(green_poly_proj, sf::st_bbox(hex_utm))
        }, error = function(e) {
          suppressWarnings(sf::st_intersection(
            green_poly_proj,
            sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(hex_utm)))
          ))
        })

        if (!is.null(green_poly_proj) && nrow(green_poly_proj) > 0) {
          green_poly_vect <- terra::vect(green_poly_proj)
          green_raster <- terra::rasterize(
            green_poly_vect, green_raster,
            field = 1, background = 0, update = TRUE
          )
        }
        message(sprintf("   [OK] Polygons rasterized [%.1fs]",
                        as.numeric(difftime(Sys.time(), poly_start, units = "secs"))))
      }, error = function(e) {
        message(sprintf("   [!] Green polygon processing failed: %s", e$message))
      })
    }

    # --- B.2: Rasterize Trees with Focal Buffer ---
    if (n_trees > 0) {
      message(sprintf("   ... Rasterizing %s trees (focal buffer method)...", format(n_trees, big.mark = ",")))
      tree_start <- Sys.time()

      tryCatch({
        trees_proj <- sf::st_transform(osm_trees_points, local_proj)
        trees_proj <- tryCatch({
          sf::st_crop(trees_proj, sf::st_bbox(hex_utm))
        }, error = function(e) trees_proj)

        if (!is.null(trees_proj) && nrow(trees_proj) > 0) {
          tree_vect <- terra::vect(trees_proj)
          tree_points_raster <- terra::rasterize(
            tree_vect, green_raster,
            field = 1, background = 0
          )

          raster_res <- terra::res(green_raster)[1]
          kernel_cells <- ceiling(tree_canopy_radius / raster_res)
          kernel_size  <- max(3, kernel_cells * 2 + 1)
          center <- (kernel_size + 1) / 2

          focal_weights <- matrix(0, nrow = kernel_size, ncol = kernel_size)
          for (i in 1:kernel_size) {
            for (j in 1:kernel_size) {
              dist <- sqrt((i - center)^2 + (j - center)^2)
              if (dist <= kernel_cells) {
                focal_weights[i, j] <- 1
              }
            }
          }

          tree_buffered <- terra::focal(
            tree_points_raster, w = focal_weights,
            fun = "max", na.rm = TRUE
          )
          tree_buffered[is.na(tree_buffered)] <- 0

          raster_stack <- c(green_raster, tree_buffered)
          green_raster <- terra::app(raster_stack, fun = max, na.rm = TRUE)
        }
        message(sprintf("   [OK] Trees processed [%.1fs]",
                        as.numeric(difftime(Sys.time(), tree_start, units = "secs"))))
      }, error = function(e) {
        message(sprintf("   [!] Tree processing failed: %s", e$message))
      })
    }

    # --- B.3: Extract OSM green coverage ---
    message("   ... Extracting green coverage per hexagon...")

    tryCatch({
      if (exactextract_available) {
        hex_sf$Green_Pct_OSM <- exactextractr::exact_extract(
          green_raster, hex_utm,
          fun = "mean", progress = FALSE
        ) * 100
      } else {
        extracted <- terra::extract(
          green_raster, terra::vect(hex_utm),
          fun = mean, na.rm = TRUE
        )
        hex_sf$Green_Pct_OSM <- extracted[, 2] * 100
      }
      hex_sf$Green_Pct_OSM[is.na(hex_sf$Green_Pct_OSM)] <- 0
    }, error = function(e) {
      message(sprintf("   [!] Green extraction failed: %s", e$message))
      hex_sf$Green_Pct_OSM <- 0
    })

    message(sprintf("   [OK] OSM green coverage: mean %.1f%% [%.1fs]",
                    mean(hex_sf$Green_Pct_OSM, na.rm = TRUE),
                    as.numeric(difftime(Sys.time(), green_start, units = "secs"))))

    # --- B.4: Water coverage via OSM water polygons (for masking) ---
    message("   ... Extracting water coverage per hexagon (OSM water)...")
    water_start <- Sys.time()
    tryCatch({
      q_w <- osmdata::opq(bbox = bbox_vec, timeout = 120) %>%
        osmdata::add_osm_feature(key = "natural", value = "water") %>%
        osmdata::osmdata_sf()

      water_polys <- q_w$osm_multipolygons
      if ((is.null(water_polys) || nrow(water_polys) == 0) &&
          !is.null(q_w$osm_polygons) && nrow(q_w$osm_polygons) > 0) {
        water_polys <- q_w$osm_polygons
      }

      if (!is.null(water_polys) && nrow(water_polys) > 0) {
        water_proj <- sf::st_transform(water_polys, local_proj)
        water_proj <- sf::st_make_valid(water_proj)
        water_proj <- tryCatch({
          sf::st_crop(water_proj, sf::st_bbox(hex_utm))
        }, error = function(e) {
          suppressWarnings(sf::st_intersection(
            water_proj,
            sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(hex_utm)))
          ))
        })

        if (!is.null(water_proj) && nrow(water_proj) > 0) {
          water_raster <- terra::rast(
            xmin = ref_extent[1], xmax = ref_extent[2],
            ymin = ref_extent[3], ymax = ref_extent[4],
            resolution = 10,
            crs = local_proj
          )
          terra::values(water_raster) <- 0
          water_vect <- terra::vect(water_proj)
          water_raster <- terra::rasterize(
            water_vect, water_raster,
            field = 1, background = 0
          )

          if (exactextract_available) {
            water_vals <- exactextractr::exact_extract(
              water_raster, hex_utm,
              fun = "mean", progress = FALSE
            )
          } else {
            water_vals <- terra::extract(
              water_raster, terra::vect(hex_utm),
              fun = mean, na.rm = TRUE
            )[, 2]
          }

          hex_sf$Water_Pct <- pmax(0, pmin(100, water_vals * 100))
        }
      }
      message(sprintf("   [OK] Water coverage estimated [%.1fs]",
                      as.numeric(difftime(Sys.time(), water_start, units = "secs"))))
    }, error = function(e) {
      message(sprintf("   [!] Water extraction failed (will not mask water): %s", e$message))
    })
  }

  # --- B.5: NDVI-based green coverage ---
  if (!is.null(ndvi_raster)) {
    message("   ... Extracting NDVI-based coverage...")
    hex_sf$NDVI_mean <- fast_extract(ndvi_raster, hex_utm)
    hex_sf$Green_Pct_NDVI <- pmax(0, pmin(100, ((hex_sf$NDVI_mean - 0.2) / 0.5) * 100))
    hex_sf$Green_Pct_NDVI[is.na(hex_sf$Green_Pct_NDVI)] <- 0
    message(sprintf("   [OK] NDVI green coverage: mean %.1f%%",
                    mean(hex_sf$Green_Pct_NDVI, na.rm = TRUE)))
  }

  # --- B.6: Combine Green_Pct (max of OSM and NDVI) ---
  hex_sf$Green_Pct <- pmax(hex_sf$Green_Pct_OSM, hex_sf$Green_Pct_NDVI, na.rm = TRUE)
  hex_sf$Green_Pct <- round(pmin(hex_sf$Green_Pct, 100), 2)

  # --- C. Built-up Coverage ---
  message("   -> Calculating built-up density...")
  hex_sf$Built_Pct <- NA_real_
  ghsl_success <- FALSE

  if (!is.null(ghsl_path) && file.exists(ghsl_path)) {
    tryCatch({
      r_g <- terra::rast(ghsl_path)
      if (!identical(terra::crs(r_g, proj = TRUE), local_proj)) {
        r_g <- terra::project(r_g, local_proj)
      }
      r_g <- terra::crop(r_g, city_vect_utm)
      vals <- fast_extract(r_g, hex_utm)

      max_v <- max(vals, na.rm = TRUE)
      pixel_area <- terra::res(r_g)[1] * terra::res(r_g)[2]

      if (max_v > 100) {
        hex_sf$Built_Pct <- (vals / pixel_area) * 100
      } else if (max_v <= 1) {
        hex_sf$Built_Pct <- vals * 100
      } else {
        hex_sf$Built_Pct <- vals
      }
      ghsl_success <- TRUE
      message(sprintf("   [OK] GHSL built-up: mean %.1f%%", mean(hex_sf$Built_Pct, na.rm = TRUE)))
    }, error = function(e) {
      message("   [!] GHSL failed, using OSM fallback: ", e$message)
    })
  }

  # Fallback to OSM Buildings
  if (!ghsl_success) {
    message("   [i] Using OSM buildings (fallback)...")
    tryCatch({
      q_b <- osmdata::opq(bbox_vec, timeout = 120) %>%
        osmdata::add_osm_feature("building") %>%
        osmdata::osmdata_sf()

      if (!is.null(q_b$osm_polygons) && nrow(q_b$osm_polygons) > 0) {
        message(sprintf("   ... Processing %d buildings...", nrow(q_b$osm_polygons)))

        b_proj <- sf::st_transform(q_b$osm_polygons, local_proj)
        b_crop <- tryCatch({
          sf::st_crop(b_proj, sf::st_bbox(hex_utm))
        }, error = function(e) b_proj)

        if (nrow(b_crop) > 0) {
          ref_extent <- terra::ext(hex_utm)
          build_raster <- terra::rast(
            xmin = ref_extent[1], xmax = ref_extent[2],
            ymin = ref_extent[3], ymax = ref_extent[4],
            resolution = 10,
            crs = local_proj
          )
          b_raster <- terra::rasterize(
            terra::vect(b_crop), build_raster,
            field = 1, background = 0
          )
          hex_sf$Built_Pct <- fast_extract(b_raster, hex_utm, "mean") * 100
          message(sprintf("   [OK] OSM built-up: mean %.1f%%", mean(hex_sf$Built_Pct, na.rm = TRUE)))
        }
      }
    }, error = function(e) {
      warning("   [!] OSM buildings failed: ", e$message)
    })
  }

  # --- Normalize built-up and apply masking later ---
  hex_sf$Built_Pct <- pmin(hex_sf$Built_Pct, 100)

  # --- Water mask (Option B): keep LST over water, mask other metrics ---
  is_water_hex <- !is.na(hex_sf$Water_Pct) & hex_sf$Water_Pct >= 50

  if (any(is_water_hex)) {
    message(sprintf("   [i] Water hexagons identified: %d (>= 50%% water)", sum(is_water_hex)))

    # Mask green and built metrics over water
    hex_sf$Green_Pct[is_water_hex]       <- NA
    hex_sf$Green_Pct_OSM[is_water_hex]   <- NA
    hex_sf$Green_Pct_NDVI[is_water_hex]  <- NA
    hex_sf$Built_Pct[is_water_hex]       <- NA

    # Mask LST deviation and derivative analyses over water (keep absolute LST)
    hex_sf$LST_diff[is_water_hex]        <- NA
  }

  # Set non-water NA built to 0 (water stays NA)
  hex_sf$Built_Pct[!is_water_hex & is.na(hex_sf$Built_Pct)] <- 0
  hex_sf$Built_Pct <- round(hex_sf$Built_Pct, 2)

  message(sprintf("   [OK] Metrics complete [%.1fs]",
                  as.numeric(difftime(Sys.time(), step5_start, units = "secs"))))

  # ===========================================================================
  # STEP 6: HOTSPOT ANALYSIS (Gi*) - LAND ONLY
  # ===========================================================================
  message("\n[Step] Step 6: Hotspot Analysis with Significance Testing")
  step6_start <- Sys.time()

  # Land-only hex for hotspot and spatial autocorrelation
  hex_valid <- hex_sf[!is_water_hex & !is.na(hex_sf$LST_mean), ]
  moran_result <- NULL

  hotspot_levels <- c("Cold Spot (99%)", "Cold Spot (95%)", "Cold Spot (90%)",
                      "Not Significant",
                      "Hot Spot (90%)", "Hot Spot (95%)", "Hot Spot (99%)")

  hex_sf$Gi_Star          <- NA_real_
  hex_sf$Gi_pvalue        <- NA_real_
  hex_sf$Hotspot_Category <- factor(NA_character_, levels = hotspot_levels)

  if (nrow(hex_valid) > 10) {
    coords <- sf::st_coordinates(sf::st_centroid(hex_valid))
    k <- min(8, nrow(hex_valid) - 1)
    nb <- spdep::knn2nb(spdep::knearneigh(coords, k = k))
    lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)

    gi <- spdep::localG(hex_valid$LST_mean, lw, zero.policy = TRUE)
    hex_valid$Gi_Star   <- as.numeric(gi)
    hex_valid$Gi_pvalue <- 2 * pnorm(-abs(hex_valid$Gi_Star))

    hex_valid$Hotspot_Category <- dplyr::case_when(
      hex_valid$Gi_Star >=  2.58 ~ "Hot Spot (99%)",
      hex_valid$Gi_Star >=  1.96 ~ "Hot Spot (95%)",
      hex_valid$Gi_Star >=  1.65 ~ "Hot Spot (90%)",
      hex_valid$Gi_Star <= -2.58 ~ "Cold Spot (99%)",
      hex_valid$Gi_Star <= -1.96 ~ "Cold Spot (95%)",
      hex_valid$Gi_Star <= -1.65 ~ "Cold Spot (90%)",
      TRUE                       ~ "Not Significant"
    )

    hex_valid$Hotspot_Category <- factor(
      hex_valid$Hotspot_Category,
      levels = hotspot_levels
    )

    match_idx <- match(hex_valid$hex_id, hex_sf$hex_id)
    hex_sf$Gi_Star[match_idx]          <- hex_valid$Gi_Star
    hex_sf$Gi_pvalue[match_idx]        <- hex_valid$Gi_pvalue
    hex_sf$Hotspot_Category[match_idx] <- hex_valid$Hotspot_Category

    moran_result <- tryCatch({
      spdep::moran.test(hex_valid$LST_mean, lw, zero.policy = TRUE)
    }, error = function(e) NULL)

    if (!is.null(moran_result)) {
      message(sprintf("   [OK] Moran's I (land only): %.3f (p = %.4f)",
                      moran_result$estimate[1], moran_result$p.value))
    }

    n_hot  <- sum(grepl("Hot Spot",  hex_valid$Hotspot_Category))
    n_cold <- sum(grepl("Cold Spot", hex_valid$Hotspot_Category))
    message(sprintf("   [OK] Significant clusters: %d hot spots, %d cold spots [%.1fs]",
                    n_hot, n_cold,
                    as.numeric(difftime(Sys.time(), step6_start, units = "secs"))))
  } else {
    message("   [!] Insufficient valid land hexagons for hotspot analysis")
  }

  # ===========================================================================
  # STEP 7: COMPREHENSIVE STATISTICS (LAND ONLY FOR HEAT-URBAN RELATIONS)
  # ===========================================================================
  message("\n[Step] Step 7: Statistical Analysis")

  stats_list <- list()

  land_idx <- !is_water_hex

  # Descriptive statistics
  stats_list$descriptive <- list(
    LST = list(
      mean   = mean(hex_sf$LST_mean[land_idx], na.rm = TRUE),
      sd     = sd(hex_sf$LST_mean[land_idx], na.rm = TRUE),
      min    = min(hex_sf$LST_mean[land_idx], na.rm = TRUE),
      max    = max(hex_sf$LST_mean[land_idx], na.rm = TRUE),
      median = median(hex_sf$LST_mean[land_idx], na.rm = TRUE)
    ),
    Green = list(
      mean      = mean(hex_sf$Green_Pct[land_idx], na.rm = TRUE),
      mean_osm  = mean(hex_sf$Green_Pct_OSM[land_idx], na.rm = TRUE),
      mean_ndvi = mean(hex_sf$Green_Pct_NDVI[land_idx], na.rm = TRUE)
    ),
    Built = list(
      mean = mean(hex_sf$Built_Pct[land_idx], na.rm = TRUE)
    ),
    Water = list(
      mean = mean(hex_sf$Water_Pct, na.rm = TRUE)
    )
  )

  # Correlations (land only)
  stats_list$correlations <- list()
  complete_cases <- land_idx &
    !is.na(hex_sf$LST_mean) &
    !is.na(hex_sf$Green_Pct) &
    !is.na(hex_sf$Built_Pct)

  if (sum(complete_cases) > 10) {
    green_var <- var(hex_sf$Green_Pct[complete_cases], na.rm = TRUE)
    built_var <- var(hex_sf$Built_Pct[complete_cases], na.rm = TRUE)

    if (green_var > 0) {
      cor_g <- tryCatch({
        cor.test(
          hex_sf$LST_mean[complete_cases],
          hex_sf$Green_Pct[complete_cases],
          method = correlation_method
        )
      }, error = function(e) NULL)

      if (!is.null(cor_g)) {
        stats_list$correlations$Green_LST <- cor_g
        message(sprintf("   LST~Green (land): r = %.3f (p = %.4f)",
                        cor_g$estimate, cor_g$p.value))
      }
    }

    if (built_var > 0) {
      cor_b <- tryCatch({
        cor.test(
          hex_sf$LST_mean[complete_cases],
          hex_sf$Built_Pct[complete_cases],
          method = correlation_method
        )
      }, error = function(e) NULL)

      if (!is.null(cor_b)) {
        stats_list$correlations$Built_LST <- cor_b
        message(sprintf("   LST~Built (land): r = %.3f (p = %.4f)",
                        cor_b$estimate, cor_b$p.value))
      }
    }
  }

  # Regression (land only)
  if (var(hex_sf$Green_Pct[land_idx], na.rm = TRUE) > 0) {
    stats_list$regression <- tryCatch({
      m <- lm(LST_mean ~ Green_Pct + Built_Pct, data = hex_sf[land_idx, ])
      list(
        coefficients   = coef(m),
        r_squared      = summary(m)$r.squared,
        adj_r_squared  = summary(m)$adj.r.squared
      )
    }, error = function(e) NULL)
  }

  # Spatial autocorrelation
  stats_list$spatial <- list(moran_I = moran_result)

  # ===========================================================================
  # STEP 8: VISUALIZATION
  # ===========================================================================
  message("\n[Step]  Step 8: Generating Visualizations")
  step8_start <- Sys.time()

  hex_map   <- sf::st_transform(hex_sf, 4326)
  bound_map <- sf::st_transform(city_boundary_sf, 4326)

  highway_map <- NULL
  if (!is.null(osm_highways) && nrow(osm_highways) > 0) {
    highway_map <- sf::st_transform(osm_highways, 4326)
  }

  # --- A. Leaflet Interactive Map ---

  get_safe_domain <- function(x) {
    v <- na.omit(x)
    if (length(v) == 0) return(c(0, 1))
    range(v)
  }

  # Palettes
  pal_lst <- leaflet::colorNumeric(
    "RdYlBu",
    domain  = get_safe_domain(hex_map$LST_mean),
    reverse = TRUE,
    na.color = "transparent"
  )

  dev_vals  <- na.omit(hex_map$LST_diff)
  dev_limit <- if (length(dev_vals) > 0) quantile(abs(dev_vals), 0.95) else 5

  pal_dev <- leaflet::colorNumeric(
    c("#08306B", "#6BAED6", "#F7F7F7", "#FDBB84", "#CB181D"),
    domain   = c(-dev_limit, dev_limit),
    na.color = "transparent"
  )

  pal_green <- leaflet::colorNumeric(
    "Greens",
    domain   = c(0, 100),
    na.color = "transparent"
  )

  pal_built <- leaflet::colorNumeric(
    palette  = c("#ffffcc", "#ffeda0", "#feb24c", "#f03b20", "#bd0026"),
    domain   = c(0, 100),
    na.color = "transparent"
  )

  hotspot_colors <- c(
    "Cold Spot (99%)" = "#08306b",
    "Cold Spot (95%)" = "#2171b5",
    "Cold Spot (90%)" = "#6baed6",
    "Not Significant" = "#f4e8d2",
    "Hot Spot (90%)"  = "#fcae91",
    "Hot Spot (95%)"  = "#fb6a4a",
    "Hot Spot (99%)"  = "#cb181d"
  )
  pal_hot <- leaflet::colorFactor(
    hotspot_colors,
    levels   = names(hotspot_colors),
    na.color = "transparent"
  )

  format_val <- function(v, suffix = "") {
    ifelse(is.na(v), "N/A", paste0(round(v, 1), suffix))
  }

  hex_map$popup_lst <- paste0(
    "<b>Land Surface Temperature</b><br>",
    "Temperature: ", format_val(hex_map$LST_mean, "deg C"), "<br>",
    "Deviation from city mean: ", format_val(hex_map$LST_diff, "deg C")
  )

  hex_map$popup_dev <- paste0(
    "<b>Temperature Deviation</b><br>",
    "Deviation from city mean: ", format_val(hex_map$LST_diff, "deg C"), "<br>",
    "<small>Positive = warmer than average<br>Negative = cooler than average</small>"
  )

  hex_map$popup_green <- paste0(
    "<b>Green Coverage</b><br>",
    "Combined: ", format_val(hex_map$Green_Pct, "%"), "<br>",
    "<hr style='margin:4px 0'>",
    "<small>",
    "* OSM (parks, mapped trees): ", format_val(hex_map$Green_Pct_OSM, "%"), "<br>",
    "* NDVI (satellite vegetation): ", format_val(hex_map$Green_Pct_NDVI, "%"), "<br>",
    "<i>Combined = max of both sources</i>",
    "</small>"
  )

  hex_map$popup_built <- paste0(
    "<b>Built-up Coverage</b><br>",
    "Built-up area: ", format_val(hex_map$Built_Pct, "%")
  )

  hex_map$popup_hot <- paste0(
    "<b>Hotspot Analysis (Gi*)</b><br>",
    "Classification: ", as.character(hex_map$Hotspot_Category), "<br>",
    "<hr style='margin:4px 0'>",
    "Gi* statistic: ", format_val(hex_map$Gi_Star, ""), "<br>",
    "p-value: ", format_val(hex_map$Gi_pvalue, "")
  )

  interactive_map <- leaflet::leaflet() %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Positron") %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter, group = "Dark") %>%
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite") %>%
    leaflet::addPolygons(data = bound_map, fill = FALSE, color = "black", weight = 2, group = "Boundary") %>%
    leaflet::addPolygons(
      data = hex_map,
      fillColor   = ~pal_lst(LST_mean),
      fillOpacity = 0.7,
      weight      = 0.5,
      color       = "white",
      group       = "LST",
      popup       = ~popup_lst
    ) %>%
    leaflet::addPolygons(
      data = hex_map,
      fillColor   = ~pal_dev(pmax(pmin(LST_diff, dev_limit), -dev_limit)),
      fillOpacity = 0.7,
      weight      = 0.5,
      color       = "white",
      group       = "Deviation",
      popup       = ~popup_dev
    ) %>%
    leaflet::addPolygons(
      data = hex_map,
      fillColor   = ~pal_green(Green_Pct),
      fillOpacity = 0.7,
      weight      = 0.5,
      color       = "white",
      group       = "Green",
      popup       = ~popup_green
    ) %>%
    leaflet::addPolygons(
      data = hex_map,
      fillColor   = ~pal_built(Built_Pct),
      fillOpacity = 0.7,
      weight      = 0.5,
      color       = "white",
      group       = "Built",
      popup       = ~popup_built
    ) %>%
    leaflet::addPolygons(
      data = hex_map,
      fillColor   = ~pal_hot(Hotspot_Category),
      fillOpacity = 0.7,
      weight      = 0.5,
      color       = "white",
      group       = "Hotspots",
      popup       = ~popup_hot
    ) %>%
    leaflet::addLayersControl(
      baseGroups    = c("Positron", "Dark", "Satellite"),
      overlayGroups = c("Boundary", "LST", "Deviation", "Green", "Built", "Hotspots"),
      options       = leaflet::layersControlOptions(collapsed = FALSE)
    ) %>%
    leaflet::hideGroup(c("Deviation", "Green", "Built", "Hotspots")) %>%
    leaflet::addLegend(
      pal      = pal_lst,
      values   = hex_map$LST_mean,
      title    = "LST (deg C)",
      position = "bottomright",
      className = "info legend legend-lst",
      layerId   = "legend-lst"
    ) %>%
    leaflet::addLegend(
      pal      = pal_dev,
      values   = c(-dev_limit, dev_limit),
      title    = "Deviation (deg C)",
      position = "bottomright",
      className = "info legend legend-dev",
      layerId   = "legend-dev"
    ) %>%
    leaflet::addLegend(
      pal      = pal_green,
      values   = c(0, 100),
      title    = "Green (%)",
      position = "bottomright",
      className = "info legend legend-green",
      layerId   = "legend-green"
    ) %>%
    leaflet::addLegend(
      pal      = pal_built,
      values   = c(0, 100),
      title    = "Built (%)",
      position = "bottomright",
      className = "info legend legend-built",
      layerId   = "legend-built"
    ) %>%
    leaflet::addLegend(
      pal      = pal_hot,
      values   = names(hotspot_colors),
      title    = "Hotspot",
      position = "bottomright",
      className = "info legend legend-hot",
      layerId   = "legend-hot"
    ) %>%
    htmlwidgets::onRender("
      function(el, x) {
        var map = this;

        var legendMap = {
          'LST': 'legend-lst',
          'Deviation': 'legend-dev',
          'Green': 'legend-green',
          'Built': 'legend-built',
          'Hotspots': 'legend-hot'
        };

        function updateLegends() {
          for (var key in legendMap) {
            var legendEls = document.getElementsByClassName(legendMap[key]);
            for (var i = 0; i < legendEls.length; i++) {
              legendEls[i].style.display = 'none';
            }
          }

          map.eachLayer(function(layer) {
            if (layer.options && layer.options.group && legendMap[layer.options.group]) {
              var legendEls = document.getElementsByClassName(legendMap[layer.options.group]);
              for (var i = 0; i < legendEls.length; i++) {
                legendEls[i].style.display = 'block';
              }
            }
          });
        }

        map.on('overlayadd', updateLegends);
        map.on('overlayremove', updateLegends);

        setTimeout(updateLegends, 100);
      }
    ")

  # --- B. Static ggplot2 Maps (with proper axes and grid) ---

  base_theme <- ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position   = "right",
      plot.title        = ggplot2::element_text(hjust = 0.5, face = "bold"),
      panel.grid.major  = ggplot2::element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor  = ggplot2::element_blank()
    )

  basemap_layers <- list(
    if (!is.null(highway_map)) ggplot2::geom_sf(data = highway_map, color = "grey90", linewidth = 0.2),
    ggplot2::geom_sf(data = bound_map, fill = NA, color = "black", linewidth = 0.5)
  )

  map_lst <- ggplot2::ggplot() +
    basemap_layers[[1]] +
    ggplot2::geom_sf(data = hex_map, ggplot2::aes(fill = LST_mean), color = NA) +
    basemap_layers[[2]] +
    ggplot2::scale_fill_distiller(
      palette   = "RdYlBu",
      direction = -1,
      name      = "deg C",
      na.value  = "transparent"
    ) +
    ggplot2::labs(
      title = paste("LST:", location_name),
      x     = "Longitude",
      y     = "Latitude"
    ) +
    ggplot2::coord_sf(expand = FALSE) +
    base_theme

  map_dev <- ggplot2::ggplot() +
    basemap_layers[[1]] +
    ggplot2::geom_sf(data = hex_map, ggplot2::aes(fill = LST_diff), color = NA) +
    basemap_layers[[2]] +
    ggplot2::scale_fill_distiller(
      palette   = "RdBu",
      direction = 1,
      limits    = c(-dev_limit, dev_limit),
      oob       = scales::squish,
      name      = "Dev (deg C)"
    ) +
    ggplot2::labs(
      title = "Deviation from Mean (Land Hexes)",
      x     = "Longitude",
      y     = "Latitude"
    ) +
    ggplot2::coord_sf(expand = FALSE) +
    base_theme

  map_green <- ggplot2::ggplot() +
    basemap_layers[[1]] +
    ggplot2::geom_sf(data = hex_map, ggplot2::aes(fill = Green_Pct), color = NA) +
    basemap_layers[[2]] +
    ggplot2::scale_fill_distiller(
      palette   = "Greens",
      direction = 1,
      limits    = c(0, 100),
      name      = "%"
    ) +
    ggplot2::labs(
      title = "Green Coverage (OSM + NDVI)",
      x     = "Longitude",
      y     = "Latitude"
    ) +
    ggplot2::coord_sf(expand = FALSE) +
    base_theme

  map_built <- ggplot2::ggplot() +
    basemap_layers[[1]] +
    ggplot2::geom_sf(data = hex_map, ggplot2::aes(fill = Built_Pct), color = NA) +
    basemap_layers[[2]] +
    ggplot2::scale_fill_gradientn(
      colours = c("#ffffcc", "#ffeda0", "#feb24c", "#f03b20", "#bd0026"),
      limits  = c(0, 100),
      name    = "Built (%)"
    ) +
    ggplot2::labs(
      title = "Built-up Coverage",
      x     = "Longitude",
      y     = "Latitude"
    ) +
    ggplot2::coord_sf(expand = FALSE) +
    base_theme

  map_hotspot <- ggplot2::ggplot() +
    basemap_layers[[1]] +
    ggplot2::geom_sf(data = hex_map, ggplot2::aes(fill = Hotspot_Category), color = NA) +
    basemap_layers[[2]] +
    ggplot2::scale_fill_manual(
      values   = hotspot_colors,
      na.value = "transparent",
      name     = "Class",
      drop     = FALSE
    ) +
    ggplot2::labs(
      title = "UHI Hotspot Analysis (Gi*, Land Only)",
      x     = "Longitude",
      y     = "Latitude"
    ) +
    ggplot2::coord_sf(expand = FALSE) +
    base_theme

  static_combined <- patchwork::wrap_plots(
    map_lst, map_dev, map_green, map_hotspot,
    ncol = 2
  ) +
    patchwork::plot_annotation(
      title    = "Urban Heat Island Analysis",
      subtitle = paste(location_name, "| Source:", thermal_result$source)
    )

  # Scatter plots (land hexes only, with inline stats)
  df <- sf::st_drop_geometry(hex_sf)
  df <- df[land_idx & is.finite(df$LST_mean), , drop = FALSE]

  # --- helpers ----------------------------------------------------------

  quick_stats_label <- function(x, y, method = correlation_method) {
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]; y <- y[ok]
    n <- length(x)
    if (n < 3) return("n < 3")
    r <- suppressWarnings(stats::cor(x, y, use = "complete.obs", method = method))
    m <- stats::lm(y ~ x)
    sm <- summary(m)
    adjR2 <- sm$adj.r.squared
    p <- sm$coefficients[2, 4]
    sprintf("n = %d\nr = %.2f\nAdj R^2 = %.2f\np = %.3g", n, r, adjR2, p)
  }

  # Annotation with background box - positioned at top-right to avoid data
  annot_stats <- function(label) {
    ggplot2::annotate(
      "label", x = Inf, y = Inf, label = label,
      hjust = 1.1, vjust = 1.1, size = 2.8, lineheight = 0.9,
      fill = "white", alpha = 0.85,
      family = "sans"
    )
  }

  # base-R moving window slope (no extra packages)
  window_slope <- function(x, y, k = 150, step = 10, min_x = 5) {
    ok <- is.finite(x) & is.finite(y) & x >= min_x  # Filter low Built%
    x <- x[ok]; y <- y[ok]
    o <- order(x); x <- x[o]; y <- y[o]
    n <- length(x)
    if (n < k) return(data.frame(x_mid = numeric(0), slope = numeric(0)))
    starts <- seq(1, n - k + 1, by = step)
    out <- lapply(starts, function(s) {
      idx <- s:(s + k - 1)
      slope <- tryCatch(unname(coef(lm(y[idx] ~ x[idx]))[2]), error = function(e) NA_real_)
      c(x_mid = stats::median(x[idx]), slope = slope)
    })
    out <- do.call(rbind, out)
    as.data.frame(out)
  }

  # ============================
  # 1) HEXBIN - LST vs BUILT
  # ============================

  label_built <- quick_stats_label(df$Built_Pct, df$LST_mean)

  scatter_lst_built <- ggplot2::ggplot(df, ggplot2::aes(Built_Pct, LST_mean)) +
    ggplot2::geom_bin2d(bins = 35) +
    ggplot2::scale_fill_gradientn(
      colours = c("#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C"),
      name = "Count"
    ) +
    ggplot2::geom_smooth(method = "lm", linewidth = 0.7, colour = "#bd0026", se = TRUE) +
    ggplot2::labs(title = "LST vs Built-up Intensity", x = "Built (%)", y = "LST (deg C)") +
    annot_stats(label_built) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", legend.key.width = ggplot2::unit(1, "cm"))

  # ============================
  # 2) HEXBIN - LST vs GREEN
  # ============================

  label_green <- quick_stats_label(df$Green_Pct, df$LST_mean)

  scatter_lst_green <- ggplot2::ggplot(df, ggplot2::aes(Green_Pct, LST_mean)) +
    ggplot2::geom_bin2d(bins = 35) +
    ggplot2::scale_fill_gradientn(
      colours = c("#f7fcf5","#c7e9c0","#74c476","#31a354","#006d2c"),
      name = "Count"
    ) +
    ggplot2::geom_smooth(method = "lm", linewidth = 0.7, colour = "#006d2c", se = TRUE) +
    ggplot2::labs(title = "LST vs Green Coverage", x = "Green (%)", y = "LST (deg C)") +
    annot_stats(label_green) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", legend.key.width = ggplot2::unit(1, "cm"))

  # ============================
  # 3) PARTIAL EFFECT - residuals of LST ~ GREEN vs BUILT
  # ============================

  m <- stats::lm(LST_mean ~ Green_Pct, data = df)
  df$resid_g <- stats::residuals(m)
  label_partial <- quick_stats_label(df$Built_Pct, df$resid_g)

  scatter_partial <- ggplot2::ggplot(df, ggplot2::aes(Built_Pct, resid_g)) +
    ggplot2::geom_point(alpha = 0.25, size = 0.7, colour = "#888888") +
    ggplot2::geom_smooth(method = "lm", linewidth = 0.7, colour = "#bd0026", se = TRUE) +
    ggplot2::labs(
      title = "Partial UHI Effect",
      subtitle = "Residuals of LST ~ Green, regressed on Built",
      x = "Built (%)", y = "Residual LST (deg C)"
    ) +
    annot_stats(label_partial) +
    ggplot2::theme_minimal()

  # ============================
  # 4) MOVING-WINDOW SLOPE - DeltaLST/DeltaBuilt
  # ============================

  ws <- window_slope(df$Built_Pct, df$LST_mean, k = 150, step = 10, min_x = 5)

  # Filter extreme slope values for better visualization
  if (nrow(ws) > 0) {
    slope_iqr <- stats::IQR(ws$slope, na.rm = TRUE)
    slope_med <- stats::median(ws$slope, na.rm = TRUE)
    slope_upper <- slope_med + 3 * slope_iqr
    slope_lower <- slope_med - 3 * slope_iqr
    ws$slope_clipped <- pmax(pmin(ws$slope, slope_upper), slope_lower)
  } else {
    ws$slope_clipped <- numeric(0)
  }

  scatter_slope <- ggplot2::ggplot(ws, ggplot2::aes(x_mid, slope_clipped)) +
    ggplot2::geom_line(linewidth = 0.7, colour = "#cb181d") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linetype = "dashed") +
    ggplot2::labs(
      title = "Slope Landscape",
      subtitle = "Rolling regression slope (dLST/dBuilt), filtered for stability",
      x = "Built (%)", y = "Local slope (deg C per %)"
    ) +
    ggplot2::theme_minimal()

  # ============================
  # Assemble dashboard
  # ============================

  scatter_dashboard <- patchwork::wrap_plots(
    scatter_lst_built, scatter_lst_green,
    scatter_partial, scatter_slope,
    ncol = 2
  )

  # Keep individual plots accessible
  scatter_plots <- list(
    dashboard = scatter_dashboard,
    lst_vs_built = scatter_lst_built,
    lst_vs_green = scatter_lst_green,
    partial_effect = scatter_partial,
    slope_landscape = scatter_slope
  )


  message(sprintf("   [OK] Visualizations complete [%.1fs]",
                  as.numeric(difftime(Sys.time(), step8_start, units = "secs"))))

  # ===========================================================================
  # FINAL OUTPUT
  # ===========================================================================
  overall_time <- difftime(Sys.time(), overall_start, units = "mins")

  meta_list <- list(
    location          = location_name,
    bbox              = bbox_vec,
    date_range        = date_range,
    thermal_source    = thermal_result$source,
    scene_id          = thermal_result$scene_id,
    resolution_info   = thermal_result$resolution,
    overpass_info     = thermal_result$overpass_info,
    hex_resolution    = hex_resolution,
    n_hexagons        = nrow(hex_sf),
    n_green_polygons  = n_green_poly,
    n_trees           = n_trees,
    correlation_method = correlation_method,
    lst_percentile_filter = lst_percentile_filter,
    ghsl_used         = ghsl_success,
    ndvi_available    = !is.null(ndvi_raster),
    green_sources     = list(
      osm  = (n_green_poly > 0 || n_trees > 0),
      ndvi = !is.null(ndvi_raster)
    ),
    exactextract_used   = exactextract_available,
    water_mask_used     = any(is_water_hex, na.rm = TRUE),
    total_time_minutes  = as.numeric(overall_time),
    analysis_timestamp  = Sys.time()
  )

  message("\n[Done] Analysis Complete!")
  message(sprintf("   [Step] %d hexagons analyzed", nrow(hex_sf)))
  message(sprintf("   [Temp]  Mean LST (land): %.1fdeg C (range: %.1f to %.1fdeg C)",
                  stats_list$descriptive$LST$mean,
                  stats_list$descriptive$LST$min,
                  stats_list$descriptive$LST$max))
  message(sprintf("   [Step] Mean Green (land): %.1f%% (OSM: %.1f%%, NDVI: %.1f%%)",
                  stats_list$descriptive$Green$mean,
                  stats_list$descriptive$Green$mean_osm,
                  stats_list$descriptive$Green$mean_ndvi))
  message(sprintf("   [Built] Mean Built (land): %.1f%%", stats_list$descriptive$Built$mean))
  message(sprintf("   [Water] Mean Water coverage: %.1f%%", stats_list$descriptive$Water$mean))
  message(sprintf("   [Time]  Total time: %.1f minutes", as.numeric(overall_time)))

  # --- Export Functions ---

  # Export results to GeoJSON file
  # @param filepath Character. Path to save GeoJSON file.
  # @return Invisible filepath
  export_geojson <- function(filepath = NULL) {
    if (is.null(filepath)) {
      loc_clean <- gsub("[^a-zA-Z0-9]", "_", location_name)
      loc_clean <- gsub("_+", "_", loc_clean)
      loc_clean <- gsub("^_|_$", "", loc_clean)
      filepath  <- paste0("uhi_results_", loc_clean, "_", format(Sys.Date(), "%Y%m%d"), ".geojson")
    }

    export_sf <- hex_sf
    popup_cols <- grep("^popup_", names(export_sf), value = TRUE)
    if (length(popup_cols) > 0) {
      export_sf <- export_sf[, !names(export_sf) %in% popup_cols]
    }

    if ("Hotspot_Category" %in% names(export_sf)) {
      export_sf$Hotspot_Category <- as.character(export_sf$Hotspot_Category)
    }

    export_sf <- sf::st_transform(export_sf, 4326)
    sf::st_write(export_sf, filepath, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
    message(sprintf("   [File] Results exported to: %s", filepath))
    invisible(filepath)
  }

  # Export results to multiple formats
  # @param base_name Character. Base filename (without extension).
  # @param formats Character vector. Formats to export: "geojson", "gpkg", "csv", "shp"
  # @return List of exported filepaths
  export_results <- function(base_name = NULL, formats = c("geojson")) {
    if (is.null(base_name)) {
      loc_clean <- gsub("[^a-zA-Z0-9]", "_", location_name)
      loc_clean <- gsub("_+", "_", loc_clean)
      loc_clean <- gsub("^_|_$", "", loc_clean)
      base_name <- paste0("uhi_results_", loc_clean, "_", format(Sys.Date(), "%Y%m%d"))
    }

    exported <- list()

    export_sf <- hex_sf
    popup_cols <- grep("^popup_", names(export_sf), value = TRUE)
    if (length(popup_cols) > 0) {
      export_sf <- export_sf[, !names(export_sf) %in% popup_cols]
    }
    if ("Hotspot_Category" %in% names(export_sf)) {
      export_sf$Hotspot_Category <- as.character(export_sf$Hotspot_Category)
    }
    export_sf <- sf::st_transform(export_sf, 4326)

    for (fmt in formats) {
      tryCatch({
        if (fmt == "geojson") {
          fp <- paste0(base_name, ".geojson")
          sf::st_write(export_sf, fp, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
          exported$geojson <- fp
        } else if (fmt == "gpkg") {
          fp <- paste0(base_name, ".gpkg")
          sf::st_write(export_sf, fp, driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
          exported$gpkg <- fp
        } else if (fmt == "csv") {
          fp <- paste0(base_name, ".csv")
          df <- sf::st_drop_geometry(export_sf)
          centroids <- sf::st_coordinates(sf::st_centroid(export_sf))
          df$longitude <- centroids[, 1]
          df$latitude  <- centroids[, 2]
          write.csv(df, fp, row.names = FALSE)
          exported$csv <- fp
        } else if (fmt == "shp") {
          fp <- paste0(base_name, ".shp")
          sf::st_write(export_sf, fp, driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
          exported$shp <- fp
        }
        message(sprintf("   [File] Exported: %s", fp))
      }, error = function(e) {
        warning(sprintf("   [!] Failed to export %s: %s", fmt, e$message))
      })
    }

    invisible(exported)
  }

  return(list(
    results = hex_sf,
    maps    = list(
      interactive = interactive_map,
      lst         = map_lst,
      deviation   = map_dev,
      green       = map_green,
      built       = map_built,
      hotspot     = map_hotspot,
      combined    = static_combined,
      scatter     = scatter_plots
    ),
    stats  = stats_list,
    meta   = meta_list,
    export_geojson = export_geojson,
    export_results = export_results
  ))
}
