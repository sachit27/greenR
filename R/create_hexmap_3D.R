# Declare global variables to avoid R CMD check warnings
utils::globalVariables(c("lon", "lat", "value"))

#' Create a 3D Hexagon Map Using H3 and MapLibre GL JS
#'
#' This function creates a 3D hexagon map using H3 and MapLibre GL JS by default. The input data can be points, linestrings, polygons, or multipolygons.
#'
#' @param data An sf object containing geographical data.
#' @param value_col Character, the name of the value column.
#' @param label_col Character, the name of the label column (optional).
#' @param mapbox_token Optional Mapbox access token. If omitted, uses MapLibre and the configured greenR basemap without a Mapbox key.
#' @param output_file Character, the file path to save the HTML file. Default is "hexagon_map.html".
#' @param color_palette Character, the D3 color scheme to use. Default is "interpolateViridis".
#' @param max_height Numeric, the maximum height for the hexagons. Default is 5000.
#' @param map_center Numeric vector of length 2, the center of the map. Default is NULL.
#' @param map_zoom Numeric, the zoom level of the map. Default is 11.
#' @param h3_resolution Numeric, the H3 resolution for hexagons. Default is 9.
#' @return NULL. The function creates an HTML file and opens it in the viewer or browser if run interactively.
#' @importFrom jsonlite toJSON
#' @importFrom utils browseURL
#' @importFrom dplyr select rename mutate
#' @importFrom sf st_transform st_coordinates st_geometry st_centroid st_point st_cast st_as_sf
#' @examples
#' if (interactive()) {
#'   # Generate random data
#'   lon <- runif(100, min = 8.49, max = 8.56)
#'   lat <- runif(100, min = 47.35, max = 47.42)
#'   green_index <- runif(100, min = 0, max = 1)
#'   data <- data.frame(lon = lon, lat = lat, green_index = green_index)
#'   data_sf <- sf::st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
#'
#'   # Create the 3D hexagon map
#'   create_hexmap_3D(
#'     data = data_sf,
#'     value_col = "green_index",
#'     output_file = "map.html",
#'     color_palette = "interpolateViridis"
#'   )
#' }
#' @export
create_hexmap_3D <- function(data, value_col, label_col = NULL, mapbox_token = NULL,
                             output_file = "hexagon_map.html",
                             color_palette = "interpolateViridis",
                             max_height = 5000,
                             map_center = NULL,
                             map_zoom = 11,
                             h3_resolution = 9) {

  # Check if the data is an sf object
  if (!inherits(data, "sf")) {
    stop("Input data must be an sf object")
  }
  if (!value_col %in% names(data) || !is.numeric(data[[value_col]]) ||
      !nrow(data) || any(!is.finite(data[[value_col]])))
    stop("value_col must name a finite numeric column in a nonempty sf layer.", call. = FALSE)
  if (length(h3_resolution) != 1L || !is.finite(h3_resolution) ||
      h3_resolution != as.integer(h3_resolution) || h3_resolution < 0 || h3_resolution > 15)
    stop("h3_resolution must be an integer from 0 to 15.", call. = FALSE)

  # Transform data to WGS 84 (EPSG:4326)
  data <- sf::st_transform(data, 4326)

  # Use the same length-weighted line midpoint conversion as convert_to_point().
  data_points <- sf::st_geometry(convert_to_point(data, target_crs = 4326))

  # Get coordinates and values
  coords <- sf::st_coordinates(data_points)
  values <- data[[value_col]]

  if (nrow(coords) != length(values)) {
    stop("Mismatch between the number of coordinates and values")
  }

  # Prepare data
  data_prepared <- data.frame(lon = coords[, 1], lat = coords[, 2], value = values)
  data_json <- jsonlite::toJSON(data_prepared, dataframe = "rows")

  # Determine map center if not provided
  if (is.null(map_center)) {
    map_center <- c(mean(data_prepared$lon), mean(data_prepared$lat))
  }

  use_mapbox <- is.character(mapbox_token) && length(mapbox_token) == 1L && nzchar(mapbox_token)
  engine <- if (use_mapbox) "mapboxgl" else "maplibregl"
  library_url <- if (use_mapbox) "https://api.mapbox.com/mapbox-gl-js/v3.4.0/mapbox-gl.js" else "https://unpkg.com/maplibre-gl@5.6.0/dist/maplibre-gl.js"
  css_url <- if (use_mapbox) "https://api.mapbox.com/mapbox-gl-js/v3.4.0/mapbox-gl.css" else "https://unpkg.com/maplibre-gl@5.6.0/dist/maplibre-gl.css"
  styles_js <- if (use_mapbox) {
    as.character(jsonlite::toJSON(c("mapbox://styles/mapbox/dark-v11",
      "mapbox://styles/mapbox/light-v11", "mapbox://styles/mapbox/streets-v12")))
  } else paste0("[", .greenr_style_json(TRUE), ",", .greenr_style_json(FALSE), "]")
  style_options <- if (use_mapbox) {
    '<option value="0">Dark</option><option value="1">Light</option><option value="2">Streets</option>'
  } else {
    '<option value="0">Dark</option><option value="1">Light</option>'
  }
  token_js <- if (use_mapbox) paste0("mapboxgl.accessToken = ",
    as.character(jsonlite::toJSON(mapbox_token, auto_unbox = TRUE)), ";") else ""

  # Create HTML content
  html_content <- sprintf('
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Hexagon Map</title>
    <script src="%s"></script>
    <link href="%s" rel="stylesheet" />
    <script src="https://unpkg.com/h3-js@4.1.0/dist/h3-js.umd.js"></script>
    <script src="https://d3js.org/d3.v7.min.js"></script>
    <style>
        body { margin: 0; padding: 0; }
        #map { position: absolute; top: 0; bottom: 0; width: 100%%; }
        #legend {
            position: absolute;
            bottom: 30px;
            right: 10px;
            background: rgba(255,255,255,0.8);
            padding: 10px;
            border-radius: 3px;
        }
        .legend-key {
            display: inline-block;
            width: 20px;
            height: 20px;
            margin-right: 5px;
        }
        .legend {
            font-family: Arial, sans-serif;
            font-size: 12px;
        }
        .legend-title {
            font-weight: bold;
            margin-bottom: 5px;
        }
        #controls {
            position: absolute;
            top: 10px;
            left: 10px;
            background: rgba(255,255,255,0.8);
            padding: 10px;
            border-radius: 3px;
            font-family: Arial, sans-serif;
        }
        .control-group {
            margin-bottom: 10px;
        }
    </style>
</head>
<body>
    <div id="map"></div>
    <div id="legend" class="legend">
        <div class="legend-title">%s</div>
    </div>
    <div id="controls">
        <div class="control-group">
            <label for="heightSlider">Max Height: <span id="heightValue">%d</span></label>
            <input type="range" id="heightSlider" min="1000" max="10000" step="500" value="%d">
        </div>
        <div class="control-group">
            <label for="resolutionSlider">H3 Resolution: <span id="resolutionValue">%d</span></label>
            <input type="range" id="resolutionSlider" min="0" max="15" value="%d">
        </div>
        <div class="control-group">
            <label for="layerSelect">Select Layer:</label>
            <select id="layerSelect">
                %s
            </select>
        </div>
    </div>
    <script>
        %s
        const mapStyles = %s;
        const map = new %s.Map({
            container: "map",
            style: mapStyles[0],
            center: [%f, %f],
            zoom: %d,
            pitch: 60,
            bearing: -60
        });

        let data = %s;
        let h3Resolution = %d;
        let maxHeight = %d;

        function updateMap() {
            const hexagons = {};
            data.forEach(point => {
                const hexId = h3.latLngToCell(point.lat, point.lon, h3Resolution);
                if (hexagons[hexId]) {
                    hexagons[hexId].value += point.value;
                    hexagons[hexId].count += 1;
                } else {
                    hexagons[hexId] = { value: point.value, count: 1 };
                }
            });

            const features = Object.entries(hexagons).map(([hexId, data]) => {
                const boundary = h3.cellToBoundary(hexId);
                const avgValue = data.value / data.count;
                return {
                    type: "Feature",
                    properties: {
                        value: avgValue,
                        height: avgValue,
                        hexId: hexId
                    },
                    geometry: {
                        type: "Polygon",
                        coordinates: [boundary.map(([lat, lng]) => [lng, lat])]
                    }
                };
            });

            const maxValue = Math.max(...features.map(f => f.properties.value));
            const minValue = Math.min(...features.map(f => f.properties.value));
            const colorLow = minValue === maxValue ? minValue - 0.5 : minValue;
            const colorHigh = minValue === maxValue ? maxValue + 0.5 : maxValue;
            features.forEach(f => {
                f.properties.height = minValue === maxValue ? maxHeight / 2 :
                    ((f.properties.value - minValue) / (maxValue - minValue)) * maxHeight;
            });

            const colorScale = d3.scaleSequential(d3.%s).domain([colorLow, colorHigh]);

            if (map.getSource("hexagons")) {
                map.getSource("hexagons").setData({ type: "FeatureCollection", features: features });
            } else {
                map.addSource("hexagons", {
                    type: "geojson",
                    data: { type: "FeatureCollection", features: features }
                });

                map.addLayer({
                    id: "hexagons",
                    type: "fill-extrusion",
                    source: "hexagons",
                    paint: {
                        "fill-extrusion-color": [
                            "interpolate",
                            ["linear"],
                            ["get", "value"],
                            colorLow, colorScale(colorLow),
                            colorHigh, colorScale(colorHigh)
                        ],
                        "fill-extrusion-height": ["get", "height"],
                        "fill-extrusion-base": 0,
                        "fill-extrusion-opacity": 0.8
                    }
                });

                map.on("click", "hexagons", (e) => {
                    new %s.Popup()
                        .setLngLat(e.lngLat)
                        .setHTML(`<strong>Value:</strong> ${e.features[0].properties.value.toLocaleString()}`)
                        .addTo(map);
                });

                map.on("mouseenter", "hexagons", () => {
                    map.getCanvas().style.cursor = "pointer";
                });

                map.on("mouseleave", "hexagons", () => {
                    map.getCanvas().style.cursor = "";
                });
            }
        }

        document.getElementById("heightSlider").addEventListener("input", (event) => {
            maxHeight = event.target.value;
            document.getElementById("heightValue").innerText = maxHeight;
            updateMap();
        });

        document.getElementById("resolutionSlider").addEventListener("input", (event) => {
            h3Resolution = event.target.value;
            document.getElementById("resolutionValue").innerText = h3Resolution;
            updateMap();
        });

        document.getElementById("layerSelect").addEventListener("change", (event) => {
            map.setStyle(mapStyles[Number(event.target.value)]);
        });

        map.on("style.load", updateMap);

        map.addControl(new %s.NavigationControl());
    </script>
</body>
</html>',
                          library_url, css_url,
                          value_col, max_height, max_height, h3_resolution, h3_resolution,
                          style_options, token_js,
                          styles_js, engine,
                          map_center[1], map_center[2], map_zoom, data_json, h3_resolution, max_height,
                          color_palette, engine, engine)

  writeLines(html_content, output_file)

  message("Hexagon Map has been created: ", output_file)

  # Automatically open the map in RStudio Viewer if available
  if (interactive()) {
    if (rstudioapi::isAvailable()) {
      rstudioapi::viewer(output_file)
    } else {
      browseURL(output_file)
    }
  }

  invisible(NULL)
}
