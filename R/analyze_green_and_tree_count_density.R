#' Analyze Green Space or Tree Count Density with Research Metrics and Lorenz Curve
#'
#' This function analyzes the spatial distribution of green spaces or trees using counts per hexagon,
#' avoiding unreliable area estimates. It calculates inequality and distribution metrics and produces
#' an interactive map, analytics, and optional Lorenz curve and JSON export. Automatically selects binning
#' strategy if data are too sparse for quantile or Jenks categorization.
#'
#' @param osm_data Output from \code{get_osm_data()}, containing at least
#'   \code{osm_data$green_areas$osm_polygons} or \code{osm_data$trees$osm_points}.
#' @param mode Character. Either \code{"green_area"} (green polygon count) or
#'   \code{"tree_density"} (point count). Default: \code{"green_area"}.
#' @param h3_res Integer. H3 resolution (0–15). Default = 8.
#' @param color_palette Character vector of 3 colors for choropleth. Default =
#'   \code{c("#FFEDA0", "#74C476", "#005A32")}.
#' @param opacity Numeric. Fill opacity for hexes. Default = 0.7.
#' @param tile_provider Character. One of
#'   \code{c("OpenStreetMap", "Positron", "DarkMatter", "Esri.WorldImagery")}.
#'   Default = \code{"OpenStreetMap"}.
#' @param enable_hover Logical. Show hover labels. Default = \code{TRUE}.
#' @param categorization_method Character. One of
#'   \code{c("quantile", "jenks", "fixed")}. Default = \code{"quantile"}.
#' @param fixed_breaks Numeric vector of length 2. Thresholds for "fixed" method.
#'   Default = \code{NULL}.
#' @param save_html Logical. Save map as self-contained HTML. Default = \code{FALSE}.
#' @param html_map_path Character. Filepath for HTML. Default = \code{"density_map.html"}.
#' @param save_json Logical. Save hex centroid + value JSON. Default = \code{FALSE}.
#' @param json_file Character. Filepath for JSON. Default = \code{"density_data.json"}.
#' @param save_lorenz Logical. Save Lorenz curve PNG. Default = \code{FALSE}.
#' @param lorenz_plot_path Character. Filepath for Lorenz PNG. Default =
#'   \code{"lorenz_curve.png"}.
#'
#' @return A list with:
#'   \item{map}{Leaflet map object}
#'   \item{analytics}{Named list of summary statistics}
#'   \item{json_file}{Path to JSON file (if saved)}
#'   \item{lorenz_plot}{Path to Lorenz PNG (if saved)}
#'
#' @import sf leaflet dplyr h3jsr classInt moments ineq jsonlite ggplot2 htmlwidgets
#' @export
#'
#' @examples
#' \dontrun{
#' # Example: green area polygons (default mode)
#' osm_data <- get_osm_data("Zurich, Switzerland", features = c("green_areas", "trees"))
#' result <- analyze_green_and_tree_count_density(
#'   osm_data = osm_data,
#'   mode = "green_area",
#'   h3_res = 8,
#'   save_lorenz = TRUE
#' )
#' print(result$analytics)
#' result$map
#' result$lorenz_plot
#'
#' # Example: tree density
#' result2 <- analyze_green_and_tree_count_density(
#'   osm_data = osm_data,
#'   mode = "tree_density",
#'   h3_res = 8,
#'   color_palette = c("#F0E442", "#009E73", "#D55E00"),
#'   save_html = TRUE
#' )
#' result2$map
#' }
analyze_green_and_tree_count_density <- function(
    osm_data,
    mode = c("green_area", "tree_density"),
    h3_res = 8,
    color_palette = c("#FFEDA0", "#74C476", "#005A32"),
    opacity = 0.7,
    tile_provider = c("OpenStreetMap", "Positron", "DarkMatter", "Esri.WorldImagery"),
    enable_hover = TRUE,
    categorization_method = c("quantile", "jenks", "fixed"),
    fixed_breaks = NULL,
    save_html = FALSE,
    html_map_path = "density_map.html",
    save_json = FALSE,
    json_file = "density_data.json",
    save_lorenz = FALSE,
    lorenz_plot_path = "lorenz_curve.png"
) {
  mode <- match.arg(mode)
  tile_provider <- match.arg(tile_provider)
  categorization_method <- match.arg(categorization_method)

  # 1. ensure required packages
  required_pkgs <- c(
    "sf", "leaflet", "dplyr", "h3jsr",
    "classInt", "moments", "ineq",
    "jsonlite", "ggplot2", "htmlwidgets"
  )
  invisible(lapply(required_pkgs, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required but not installed.", pkg))
    }
  }))

  # 2. pull out and validate the feature layer
  if (mode == "green_area") {
    if (is.null(osm_data$green_areas$osm_polygons)) {
      stop("`osm_data$green_areas$osm_polygons` must be provided for mode = 'green_area'.")
    }
    feats <- osm_data$green_areas$osm_polygons
    if (!inherits(feats, "sf")) stop("`green_areas$osm_polygons` must be an sf object.")
  } else {
    if (is.null(osm_data$trees$osm_points)) {
      stop("`osm_data$trees$osm_points` must be provided for mode = 'tree_density'.")
    }
    feats <- osm_data$trees$osm_points
    if (!inherits(feats, "sf")) stop("`trees$osm_points` must be an sf object.")
  }

  # 3. Compute centroids safely (projected to avoid warnings, then back to WGS84 for H3)
  feats_proj <- sf::st_transform(feats, 3857)
  centroids_proj <- sf::st_point_on_surface(sf::st_geometry(feats_proj))
  centroids_ll <- sf::st_transform(sf::st_sf(geometry = centroids_proj, crs = 3857), 4326)

  idx <- h3jsr::point_to_cell(centroids_ll, res = h3_res)
  hex_data <- dplyr::tibble(h3_index = idx) %>%
    dplyr::count(h3_index, name = "value")

  # 4. build hexagon geometries (in lon/lat)
  hex_polys <- h3jsr::cell_to_polygon(hex_data$h3_index, simple = FALSE)
  hex_polys <- sf::st_as_sf(hex_polys)
  hex_polys$value <- hex_data$value

  # 5. Robust categorization: auto-select bins if too sparse
  vals <- hex_polys$value

  # Try requested method, fallback if not enough unique breaks
  get_bins <- function(vals, method, fixed_breaks = NULL) {
    if (method == "quantile") {
      b <- stats::quantile(vals, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
      if (length(unique(b)) < 4) return(NULL) else return(b)
    }
    if (method == "jenks") {
      b <- classInt::classIntervals(vals, n = 3, style = "jenks")$brks
      if (length(unique(b)) < 4) return(NULL) else return(b)
    }
    if (method == "fixed") {
      if (is.null(fixed_breaks) || length(fixed_breaks) != 2) return(NULL)
      fb <- sort(fixed_breaks)
      b <- c(min(vals, na.rm = TRUE), fb, max(vals, na.rm = TRUE))
      if (length(unique(b)) < 4) return(NULL) else return(b)
    }
    NULL
  }
  # Try requested method
  breaks <- get_bins(vals, categorization_method, fixed_breaks)
  fallback_method <- NULL

  # If failed, try alternatives (quantile → jenks → fixed → all Low)
  if (is.null(breaks)) {
    if (categorization_method != "jenks") {
      breaks <- get_bins(vals, "jenks")
      fallback_method <- "jenks"
    }
  }
  if (is.null(breaks)) {
    # Try fixed at tertiles
    tertiles <- unique(as.numeric(stats::quantile(vals, probs = c(1/3, 2/3), na.rm = TRUE)))
    if (length(tertiles) == 2) {
      breaks <- get_bins(vals, "fixed", tertiles)
      fallback_method <- "fixed"
    }
  }
  # If still null, assign all as Low
  if (is.null(breaks)) {
    warning("Not enough value diversity to assign bins. Assigning all hexes as 'Low'.")
    hex_polys$density_category <- factor(rep("Low", length(vals)), levels = c("Low", "Medium", "High"))
    used_method <- "none"
  } else {
    hex_polys$density_category <- cut(
      vals,
      breaks = breaks,
      labels = c("Low", "Medium", "High"),
      include.lowest = TRUE,
      right = TRUE
    )
    used_method <- fallback_method %||% categorization_method
  }

  # 6. summary analytics
  total_count  <- sum(vals, na.rm = TRUE)
  analytics <- list(
    total_count   = total_count,
    mean_value    = mean(vals, na.rm = TRUE),
    median_value  = stats::median(vals, na.rm = TRUE),
    max_value     = max(vals, na.rm = TRUE),
    skewness      = moments::skewness(vals, na.rm = TRUE),
    kurtosis      = moments::kurtosis(vals, na.rm = TRUE),
    gini_index    = ineq::Gini(vals, na.rm = TRUE),
    quartiles     = stats::quantile(vals, probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
    categorization_method = used_method,
    breaks_used   = if (is.null(breaks)) NA else breaks
  )

  # 7. compute count per km² over the actual hex union
  union_poly <- sf::st_union(hex_polys)
  union_m <- sf::st_transform(union_poly, crs = 3857)
  area_m2 <- as.numeric(sf::st_area(union_m))
  analytics$count_per_km2 <- total_count / (area_m2 / 1e6)

  # 8. optional: save Lorenz curve
  if (save_lorenz && length(vals) > 1 && any(vals != 0)) {
    lobj <- ineq::Lc(vals[!is.na(vals)])
    ldf  <- data.frame(p = lobj$p, L = lobj$L)
    p <- ggplot2::ggplot(ldf, ggplot2::aes(x = p, y = L)) +
      ggplot2::geom_line(size = 1.2) +
      ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      ggplot2::labs(
        title = "Lorenz Curve of Feature Counts",
        x     = "Cumulative Share of Hexagons",
        y     = "Cumulative Share of Counts"
      ) +
      ggplot2::theme_minimal(base_size = 14)
    ggplot2::ggsave(lorenz_plot_path, plot = p, width = 7, height = 6)
    lorenz_path <- lorenz_plot_path
  } else {
    lorenz_path <- NULL
  }

  # 9. build the leaflet map
  pal <- leaflet::colorFactor(palette = color_palette, domain = hex_polys$density_category)
  popup_text <- if (mode == "green_area") {
    ~paste0("Green count: ", value)
  } else {
    ~paste0("Tree count:  ", value)
  }
  providers <- list(
    OpenStreetMap    = leaflet::providers$OpenStreetMap,
    Positron         = leaflet::providers$CartoDB.Positron,
    DarkMatter       = leaflet::providers$CartoDB.DarkMatter,
    Esri.WorldImagery= leaflet::providers$Esri.WorldImagery
  )
  m <- leaflet::leaflet() %>%
    leaflet::addProviderTiles(providers[[tile_provider]]) %>%
    leaflet::addPolygons(
      data       = hex_polys,
      fillColor  = ~pal(density_category),
      fillOpacity= opacity,
      color      = "white",
      weight     = 1,
      popup      = popup_text
    ) %>%
    leaflet::addLegend(
      pal   = pal,
      values= hex_polys$density_category,
      title = ifelse(mode == "green_area", "Green count", "Tree count"),
      position = "bottomright"
    )
  if (enable_hover) {
    m <- m %>%
      leaflet::addPolygons(
        data       = hex_polys,
        fillColor  = "transparent",
        weight     = 0,
        label      = popup_text,
        labelOptions = leaflet::labelOptions(textsize = "13px")
      )
  }

  # 10. optional: save HTML widget
  if (save_html) {
    htmlwidgets::saveWidget(m, file = html_map_path, selfcontained = TRUE)
  }

  # 11. optional: export JSON of centroids + values
  json_path <- NULL
  if (save_json) {
    hex_wgs <- sf::st_transform(hex_polys, 4326)
    cents   <- sf::st_centroid(hex_wgs)
    coords  <- sf::st_coordinates(cents)
    dfjson  <- data.frame(
      lon   = coords[,1],
      lat   = coords[,2],
      value = hex_polys$value
    )
    jsonlite::write_json(dfjson, path = json_file, pretty = TRUE, auto_unbox = TRUE)
    json_path <- json_file
  }

  # 12. return
  return(list(
    map         = m,
    analytics   = analytics,
    json_file   = json_path,
    lorenz_plot = lorenz_path
  ))
}
