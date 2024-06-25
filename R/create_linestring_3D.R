# Declare global variables to avoid R CMD check warnings
utils::globalVariables(c("mapboxgl", "mapbox", "d3", "document", "window", "navigator"))

#' Create a 3D Linestring Map
#'
#' This function creates a 3D linestring map using Mapbox GL JS and saves it as an HTML file.
#' The map visualizes linestring data with an associated green index, allowing for interactive
#' exploration of the data.
#'
#' @param data An `sf` object containing linestring geometries and associated data.
#' @param green_index_col Character, name of the column containing the green index values.
#' @param mapbox_token Character, Mapbox access token for rendering the map.
#' @param output_file Character, name of the output HTML file. Default is "linestring_map.html".
#' @param color_palette Character, name of the D3 color palette to use. Default is "interpolateViridis".
#' @param map_center Numeric vector, longitude and latitude of the map center. Default is NULL (computed from data).
#' @param map_zoom Numeric, initial zoom level of the map. Default is 11.
#' @return NULL. The function creates an HTML file and opens it in the viewer or browser.
#' @importFrom sf st_transform st_write st_bbox st_sf st_sfc st_linestring st_crs<-
#' @importFrom jsonlite read_json
#' @importFrom utils browseURL
#' @importFrom dplyr %>%
#' @importFrom rstudioapi viewer isAvailable
#' @export
#' @examples
#' if (interactive()) {
#'   # Create example data
#'   lines <- st_sf(
#'     id = 1:5,
#'     geometry = st_sfc(
#'       st_linestring(matrix(c(0,0, 1,1), ncol=2, byrow=TRUE)),
#'       st_linestring(matrix(c(1,1, 2,2), ncol=2, byrow=TRUE)),
#'       st_linestring(matrix(c(2,2, 3,3), ncol=2, byrow=TRUE)),
#'       st_linestring(matrix(c(3,3, 4,4), ncol=2, byrow=TRUE)),
#'       st_linestring(matrix(c(4,4, 5,5), ncol=2, byrow=TRUE))
#'     ),
#'     green_index = runif(5)
#'   )
#'   st_crs(lines) <- 4326
#'   mapbox_token <- "your_mapbox_token"
#'   create_linestring_3D(lines, "green_index", mapbox_token)
#' }
create_linestring_3D <- function(data, green_index_col, mapbox_token, output_file = "linestring_map.html",
                                 color_palette = "interpolateViridis", map_center = NULL, map_zoom = 11) {

  # Transform data to WGS 84 (EPSG:4326)
  data <- sf::st_transform(data, 4326)

  # Convert sf object to GeoJSON
  temp_geojson <- tempfile(fileext = ".geojson")
  sf::st_write(data, temp_geojson, driver = "GeoJSON")
  data_json <- paste(readLines(temp_geojson), collapse = "\n")

  # Determine map center if not provided
  if (is.null(map_center)) {
    bbox <- sf::st_bbox(data)
    map_center <- c((bbox$xmin + bbox$xmax) / 2, (bbox$ymin + bbox$ymax) / 2)
  }

  # Create HTML content
  html_content <- sprintf('
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Linestring Map</title>
    <script src="https://api.mapbox.com/mapbox-gl-js/v3.4.0/mapbox-gl.js"></script>
    <link href="https://api.mapbox.com/mapbox-gl-js/v3.4.0/mapbox-gl.css" rel="stylesheet" />
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
            <label for="lineWidthSlider">Line Width: <span id="lineWidthValue">5</span></label>
            <input type="range" id="lineWidthSlider" min="1" max="20" step="1" value="5">
        </div>
        <div class="control-group">
            <label for="showBuildings">Show Buildings:</label>
            <input type="checkbox" id="showBuildings">
        </div>
    </div>
    <script>
        mapboxgl.accessToken = "%s";
        const map = new mapboxgl.Map({
            container: "map",
            style: "mapbox://styles/mapbox/streets-v11",
            center: [%f, %f],
            zoom: %d,
            pitch: 45,
            bearing: -17.6,
            antialias: true
        });

        const data = %s;
        let lineWidth = 5;

        const colorScale = d3.scaleSequential(d3.%s).domain([0, 1]);

        map.on("load", () => {
            map.addSource("linestrings", {
                type: "geojson",
                data: data
            });

            map.addLayer({
                id: "linestrings",
                type: "line",
                source: "linestrings",
                paint: {
                    "line-width": lineWidth,
                    "line-color": [
                        "interpolate",
                        ["linear"],
                        ["get", "%s"],
                        0, colorScale(0),
                        1, colorScale(1)
                    ]
                }
            });

            // Insert the layer beneath any symbol layer.
            const layers = map.getStyle().layers;
            const labelLayerId = layers.find(
                (layer) => layer.type === "symbol" && layer.layout["text-field"]
            ).id;

            map.addLayer(
                {
                    id: "3d-buildings",
                    source: "composite",
                    "source-layer": "building",
                    filter: ["==", "extrude", "true"],
                    type: "fill-extrusion",
                    minzoom: 15,
                    paint: {
                        "fill-extrusion-color": "#aaa",
                        "fill-extrusion-height": [
                            "interpolate",
                            ["linear"],
                            ["zoom"],
                            15,
                            0,
                            15.05,
                            ["get", "height"]
                        ],
                        "fill-extrusion-base": [
                            "interpolate",
                            ["linear"],
                            ["zoom"],
                            15,
                            0,
                            15.05,
                            ["get", "min_height"]
                        ],
                        "fill-extrusion-opacity": 0.6
                    }
                },
                labelLayerId
            );

            document.getElementById("lineWidthSlider").addEventListener("input", (event) => {
                lineWidth = event.target.value;
                document.getElementById("lineWidthValue").innerText = lineWidth;
                map.setPaintProperty("linestrings", "line-width", parseInt(lineWidth));
            });

            document.getElementById("showBuildings").addEventListener("change", (event) => {
                const visibility = event.target.checked ? "visible" : "none";
                map.setLayoutProperty("3d-buildings", "visibility", visibility);
            });

            map.on("mouseenter", "linestrings", (e) => {
                map.getCanvas().style.cursor = "pointer";
                const coordinates = e.lngLat;
                const greenIndex = e.features[0].properties["%s"];
                new mapboxgl.Popup()
                    .setLngLat(coordinates)
                    .setHTML(`<strong>Green Index:</strong> ${greenIndex.toFixed(2)}`)
                    .addTo(map);
            });

            map.on("mouseleave", "linestrings", () => {
                map.getCanvas().style.cursor = "";
                const popups = document.getElementsByClassName("mapboxgl-popup");
                while (popups[0]) {
                    popups[0].remove();
                }
            });

            // Create continuous legend
            const legend = document.getElementById("legend");
            const legendWidth = 300;
            const legendHeight = 10;
            const canvas = document.createElement("canvas");
            canvas.width = legendWidth;
            canvas.height = legendHeight;
            const ctx = canvas.getContext("2d");
            const legendScale = d3.scaleSequential(d3.%s).domain([0, 1]);
            for (let i = 0; i < legendWidth; ++i) {
                const value = i / legendWidth;
                ctx.fillStyle = legendScale(value);
                ctx.fillRect(i, 0, 1, legendHeight);
            }
            legend.appendChild(canvas);

            const legendValues = document.createElement("div");
            legendValues.innerHTML = `
                <div style="display: flex; justify-content: space-between;">
                    <span>0</span>
                    <span>0.5</span>
                    <span>1</span>
                </div>
            `;
            legend.appendChild(legendValues);
        });

        map.addControl(new mapboxgl.NavigationControl());
    </script>
</body>
</html>
', green_index_col, mapbox_token, map_center[1], map_center[2], map_zoom, data_json, color_palette, green_index_col, green_index_col, color_palette)

  # Conditionally write the HTML content and open the file if interactive
  if (interactive()) {
    writeLines(html_content, output_file)
    message("Linestring Map has been created: ", output_file)

    # Automatically open the map in RStudio Viewer if available
    if (rstudioapi::isAvailable()) {
      rstudioapi::viewer(output_file)
    } else {
      browseURL(output_file)
    }
  }

  invisible(NULL)
}
