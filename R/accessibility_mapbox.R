# Declare global variables to avoid R CMD check warnings
utils::globalVariables(c("mapboxgl", "mapbox", "d3", "document", "window", "navigator"))

#' Create a dynamic Accessibility Map Using Mapbox GL JS
#'
#' This function creates a dynamic accessibility map using Mapbox GL JS. The map shows green areas and allows users to generate isochrones for walking times.
#'
#' @param green_area_data A list containing green area data.
#' @param mapbox_token Character, your Mapbox access token.
#' @param output_file Character, the file path to save the HTML file.
#' @param initial_zoom Numeric, the initial zoom level of the map. Default is 15.
#' @param initial_pitch Numeric, the initial pitch of the map. Default is 45.
#' @param initial_bearing Numeric, the initial bearing of the map. Default is -17.6.
#' @return NULL. The function creates an HTML file and opens it in the viewer or browser if run interactively.
#' @importFrom sf st_write st_bbox
#' @importFrom utils browseURL
#' @importFrom rstudioapi viewer isAvailable
#' @examples
#' if (interactive()) {
#'   data <- get_osm_data("Basel, Switzerland")
#'   green_areas_data <- data$green_areas
#'   mapbox_token <- "your_mapbox_access_token_here"
#'   accessibility_mapbox(green_areas_data, mapbox_token)
#' }
#' @export
accessibility_mapbox <- function(green_area_data, mapbox_token, output_file = "accessibility_map.html",
                                 initial_zoom = 15, initial_pitch = 45, initial_bearing = -17.6) {

  # Validate and prepare green area data
  if (is.null(green_area_data) || !inherits(green_area_data$osm_polygons, "sf")) {
    stop("Invalid green_area_data provided. It should be a non-null sf object.")
  }

  osm_sf <- green_area_data$osm_polygons

  # Calculate the center of the bounding box
  bbox <- sf::st_bbox(osm_sf)
  initial_lat <- (bbox$ymin + bbox$ymax) / 2
  initial_lon <- (bbox$xmin + bbox$xmax) / 2

  # Convert green area data to GeoJSON
  temp_geojson <- tempfile(fileext = ".geojson")
  sf::st_write(osm_sf, temp_geojson, driver = "GeoJSON")
  green_area_data_json <- paste(readLines(temp_geojson), collapse = "\n")

  # Create HTML content
  html_content <- sprintf('
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Accessibility Analysis</title>
    <script src="https://api.mapbox.com/mapbox-gl-js/v3.4.0/mapbox-gl.js"></script>
    <link href="https://api.mapbox.com/mapbox-gl-js/v3.4.0/mapbox-gl.css" rel="stylesheet">
    <script src="https://d3js.org/d3.v7.min.js"></script>
    <style>
        body { margin: 0; padding: 0; }
        #map { position: absolute; top: 0; bottom: 0; width: 100%%; }
        .legend {
            position: absolute;
            bottom: 30px;
            right: 10px;
            background: rgba(255,255,255,0.8);
            padding: 10px;
            border-radius: 3px;
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
    <div id="controls">
        <div class="control-group">
            <label for="walkTimeSlider">Walking Time (minutes): <span id="walkTimeValue">15</span></label>
            <input type="range" id="walkTimeSlider" min="1" max="30" step="1" value="15">
        </div>
    </div>
    <script>
        mapboxgl.accessToken = "%s";
        const map = new mapboxgl.Map({
            container: "map",
            style: "mapbox://styles/mapbox/light-v11",
            center: [%f, %f],
            zoom: %f,
            pitch: %f,
            bearing: %f,
            antialias: true
        });

        let currentMarker;

        map.on("load", () => {
            // Load green spaces data
            map.addSource("green_spaces", {
                type: "geojson",
                data: %s
            });

            map.addLayer({
                id: "green_spaces_layer",
                type: "fill",
                source: "green_spaces",
                paint: {
                    "fill-color": "green",
                    "fill-opacity": 0.5,
                    "fill-outline-color": "black"
                }
            });

            // Event listener for clicking on the map to add a marker
            map.on("click", (e) => {
                const coordinates = e.lngLat;
                if (currentMarker) {
                    currentMarker.remove();
                }
                currentMarker = addMarker(coordinates);
                generateIsochrones(coordinates, getWalkingTime());
            });

            // Function to add a marker at the specified coordinates
            function addMarker(coordinates) {
                return new mapboxgl.Marker({ color: "blue" })
                    .setLngLat(coordinates)
                    .addTo(map);
            }

            // Function to get the current walking time from the slider
            function getWalkingTime() {
                return parseInt(document.getElementById("walkTimeSlider").value);
            }

            // Function to generate isochrones
            function generateIsochrones(coordinates, walkingTime) {
                const url = `https://api.mapbox.com/isochrone/v1/mapbox/walking/${coordinates.lng},${coordinates.lat}?contours_minutes=5,10,${walkingTime}&polygons=true&access_token=${mapboxgl.accessToken}`;
                fetch(url)
                    .then(response => response.json())
                    .then(data => {
                        if (map.getSource("isochrones")) {
                            map.getSource("isochrones").setData(data);
                        } else {
                            map.addSource("isochrones", {
                                type: "geojson",
                                data: data
                            });

                            map.addLayer({
                                id: "isochrones_layer",
                                type: "fill",
                                source: "isochrones",
                                paint: {
                                    "fill-color": [
                                        "step",
                                        ["get", "contour"],
                                        "#ffffcc",
                                        5, "#a1dab4",
                                        10, "#41b6c4",
                                        15, "#2c7fb8",
                                        20, "#253494"
                                    ],
                                    "fill-opacity": 0.5
                                }
                            });

                            // Add hover functionality for the isochrones
                            map.on("mouseenter", "isochrones_layer", (e) => {
                                map.getCanvas().style.cursor = "pointer";
                                const properties = e.features[0].properties;
                                new mapboxgl.Popup()
                                    .setLngLat(e.lngLat)
                                    .setHTML(`<strong>Walking Time:</strong> ${properties.contour} minutes`)
                                    .addTo(map);
                            });

                            map.on("mouseleave", "isochrones_layer", () => {
                                map.getCanvas().style.cursor = "";
                                const popups = document.getElementsByClassName("mapboxgl-popup");
                                while (popups[0]) {
                                    popups[0].remove();
                                }
                            });
                        }
                    });
            }
        });

        // Update walking time value display
        document.getElementById("walkTimeSlider").addEventListener("input", (event) => {
            document.getElementById("walkTimeValue").innerText = event.target.value;
        });

        map.addControl(new mapboxgl.NavigationControl());
    </script>
</body>
</html>',
                          mapbox_token, initial_lon, initial_lat, initial_zoom, initial_pitch, initial_bearing, green_area_data_json)

  # Write the HTML content to the specified output file
  writeLines(html_content, output_file)

  message("Accessibility Map has been created: ", output_file)

  # Automatically open the map in RStudio Viewer if available
  if (rstudioapi::isAvailable() && interactive()) {
    rstudioapi::viewer(output_file)
  } else if (interactive()) {
    browseURL(output_file)
  }

  invisible(NULL)
}
