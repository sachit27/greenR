[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/greenR)](https://CRAN.R-project.org/package=greenR)


<p float="left">
  <img src="/vignettes/logo.jpg" width="220" />
  <img src="/vignettes/zh_network.jpg" width="250" />
  <img src="/vignettes/zh_gi.jpg" width="300" />
</p>

# greenR: An R Package for Quantifying Urban Greenness

## Introduction

greenR is an open-source R package designed to tackle the challenging task of quantifying urban greenness. Leveraging crowd-sourced data from OpenStreetMap (OSM), greenR provides a new and scalable method to assign green indices to individual street segments. The package offers a comprehensive solution by facilitating green index quantification, equity analysis, and visualization. Additionally, greenR supports Mapbox visualizations for interactive exploration of different types of geospatial data, enhancing the analysis with dynamic and engaging visual tools. Furthermore, it supports several other features like Canopy Height Modeling, Green View Index Calculation, Accessibility analysis, etc.


Accompanied by a Shiny app, greenR can empower researchers, planners, policy-makers, and citizens to study urban greenness patterns across cities.

For more detailed information about the motivation, methodology, and validation, please have a look at this paper published in [Ecological Indicators](https://www.sciencedirect.com/science/article/pii/S1470160X2400565X)

## Installation
It can be installed via CRAN
```r
install.packages("greenR")
```
In case there are difficulties in installing through CRAN, you can do it directly via GitHub by running the command below in R.

```r
# install.packages("remotes") # Uncomment if you do not have the 'remotes' package installed
remotes::install_github("sachit27/greenR", dependencies = TRUE)
```
Or you can also use devtools to install the package.

```r
library(devtools)
devtools::install_github("sachit27/greenR", dependencies = TRUE)
```
If you're updating from a previous version and encounter any issues, try clearing your package cache:
```r
remove.packages("greenR")
.rs.restartR()
devtools::install_github("sachit27/greenR", dependencies = TRUE, force = TRUE)
```

NOTE: If you encounter any issues with the installation of the "osmdata" R package as a dependency, you may bypass CRAN and install it directly from the [Github Repository](https://github.com/ropensci/osmdata). This alternative installation method can be useful if the standard CRAN installation is not working as expected. Here is how you can install it.

```r
remotes::install_github ("ropensci/osmdata")
```

After installing the greenR package, you can load the package into the R session using the following command.

```r
library(greenR)

```

## Specify the location and download the data

The first step is to acquire data. This provides a systematic approach to collecting the requisite geospatial data from OSM, thereby serving as the foundation for all subsequent analyses. The users can simply specify any city or neighborhood (that has data available in OSM database). This function looks in the database and finds any city and downloads OSM data for the specified spatial area with regard to three key environmental features: highways, green areas, and trees. Here green areas include all the areas with the following tags: "forest", "vineyard", "plant_nursery", "orchard", "greenfield", "recreation_ground", "allotments", "meadow","village_green","flowerbed", "grass", "farmland", "garden", "dog_park","nature_reserve", and "park".

### Option 1: City or neighborhood name


```R
data <- get_osm_data("City of London, United Kingdom")
```
Or
```R
data <- get_osm_data("Fulham, London, United Kingdom")
```

### Option 2: Bounding box coordinates

For more precise control over the area of interest, users can provide a bounding box using coordinates. This is particularly useful when you need to define a specific area around a point of interest.

```R
# Define bounding box: (left, bottom, right, top)
bbox <- c(-0.1, 51.5, 0.1, 51.7)  # Example bounding box for central London

data <- get_osm_data(bbox)
```


## Visualise green spaces and clustering

The visualize_green_spaces() function is designed to aid in the visual assessment of green space data. Utilizing an integrated leaflet map, users can explore the distribution and mapping quality of green spaces within a specified area. By plotting this data on an interactive leaflet map, users gain insights into the extent and accuracy of green space representation. After visualizing the green spaces within the desired area, users may wish to contribute to the OpenStreetMap project to enhance the data quality or add unrepresented areas. Additionally, the green_space_clustering() function initially transforms the green spaces into an equal-area projection to calculate the areas accurately. Post-transformation, the K-means algorithm is applied to these areas, clustering the green spaces based on the number of clusters specified.

```R
green_areas_data <- data$green_areas
visualize_green_spaces(green_areas_data)
```
```R
green_space_clustering(green_areas_data, num_clusters = 3)
```
![Green_Areas and Clusters](/vignettes/vis.jpg)

## Green and Tree Count Density Analysis

The analyze_green_and_tree_count_density() function quantifies the spatial distribution and inequality of green areas or tree locations within an urban area using hexagonal spatial bins. Users can select either green area polygons or tree point data from OpenStreetMap as the input.

- Method: The function divides the study area into hexagons (using the H3 spatial indexing system) and counts either the number of green polygons or trees in each hexagon. Multiple classification schemes (quantile, Jenks, fixed thresholds) are available to assign each hexagon to low, medium, or high density classes, automatically selecting a fallback if the data are sparse.Spatial metrics are calculated, including counts per km² over the hexagonal area.
- Analytics: The function produces a comprehensive set of summary statistics and inequality measures, including the total, mean, and median counts per hexagon, as well as the Gini index to quantify spatial inequality in the distribution of green areas or trees. It also calculates skewness and kurtosis to characterize the shape of the distribution, and generates the Lorenz curve with its area-under-curve to visualize and measure inequality. Additionally, the function reports the classification method and the bin thresholds used to categorize density across the study area.

Interactive maps (Leaflet) and plots (such as the Lorenz curve) are generated automatically. Results and metrics can also be exported as GeoJSON and JSON files for further analysis.

```R
osm_data <- get_osm_data("Zurich, Switzerland")
result <- analyze_green_and_tree_count_density(
  osm_data = osm_data,
  mode = "tree_density", #or you can use "green_area"
  h3_res = 8,
  save_lorenz = TRUE
)
result$map        # Interactive Leaflet map
result$analytics  # Summary statistics and Gini index
result$lorenz_plot # Lorenz curve plot (if saved)
```

## Accessibility analysis

Mapbox Version (Dynamic): The 'accessibility_mapbox' function creates an accessibility map using Mapbox GL JS. This map shows green areas and allows users to generate isochrones for walking times. The resulting HTML file includes interactive features for changing the walking time and moving the location marker dynamically.
```R
mapbox_token <- "your_mapbox_access_token_here"
accessibility_mapbox(green_areas_data, mapbox_token)
```
![Access Mapbox](https://github.com/sachit27/Accessibility-Analysis/blob/main/images/access_mapbox.gif)

Leaflet Version: The 'accessibility_greenspace' function creates an interactive leaflet map displaying accessible green spaces within a specified walking time from a provided location. It utilizes isochrones to visualize the areas reachable by 5, 10, and 15 minutes of walking. The function relies on pedestrian routing information and green space data to accurately delineate accessible areas. The default maximum walking time is set to 15 minutes but can be adjusted using the 'max_walk_time' parameter.  

```R
accessibility_greenspace(green_areas_data, 47.56427527336772, 7.595820936462059)
```

In addition to creating an interactive map, the 'accessibility_greenspace' function can also export the data in a format compatible with GIS software.

```R
result <- accessibility_greenspace(
  green_areas_data, 
  47.56427527336772, 
  7.595820936462059,
  output_file = "green_space_accessibility.gpkg"
)
```
![Isochrone](/vignettes/isochrone.jpg)

## Green Space Accessibility, Directionality, and Population Coverage

greenR provides functions to measure and visualize urban residents’ access to green spaces using actual street networks, multiple transport modes, and high-resolution population data.

- Accessibility Analysis: The analyze_green_accessibility() function calculates the network-based distance from a regular grid of locations across the city to the nearest mapped green space. This is done using real street networks for different modes of travel—walking, cycling, or driving. The analysis can also incorporate gridded population data (for example: GHSL GHS-POP, epoch 2025, 100m resolution, Mollweide projection) to compute population-weighted accessibility metrics. Accessibility is summarized as the percentage of the city’s area, and separately, the percentage of the population within 400m and 800m of a green space.
- Directionality of Access: The analysis quantifies how green space access varies by compass direction (N, NE, E, SE, S, SW, W, NW) from each grid cell. This directionality metric helps identify spatial patterns and potential barriers or corridors in green space exposure.
- Visualization: The create_accessibility_visualizations() function summarizes the results in three main plots. A grid map showing the minimum network distance to the nearest green space for each location. A barplot of spatial and population-weighted coverage within 400m and 800m thresholds. A radar plot representing directional coverage, where each axis shows the average green space access in that direction. An interactive Leaflet map is also produced, combining distance, population, and green space layers for exploration.

Below is a sample output for Basel, Switzerland, showing spatial, population-weighted, and directional green space accessibility.
![Direction](/vignettes/access.jpg)

```R
data <- get_osm_data("Basel, Switzerland")
library(terra)
library(sf)
# Load and reproject your GHSL raster
ghsl_path <- "GHS_POP_E2025_GLOBE_R2023A_54009_100_V1_0_R4_C19.tif"
# Read with terra
pop_raster_raw <- terra::rast(ghsl_path)
network <- data$highways$osm_lines
green   <- data$green_areas$osm_polygons
result <- analyze_green_accessibility(
  network_data      = network,
  green_areas       = green,
  mode              = "walking",
  grid_size         = 300,
  population_raster = pop_raster_raw
)
viz <- create_accessibility_visualizations(
  accessibility_analysis = result,
  green_areas = data$green_areas$osm_polygons,
  mode = "walking"
)
print(viz$distance_map)
print(viz$coverage_plot)
print(viz$directional_plot)
viz$leaflet_map
```
Note on edge effects: Grid cells located near the edges of the study area (city boundary or data clipping line) may show artificially high distances to the nearest green space. This is because green spaces located just outside the analysis boundary are not included in the calculation, leading to “edge effects” where accessibility is underestimated at the margins.
To mitigate this, consider expanding the analysis boundary or including green spaces from a buffer area surrounding the city. Alternatively, interpret results for boundary cells with caution.

## Calculate the green index for the specified city

This function takes as input the OSM data, a Coordinate Reference System (CRS) code, and parameter D for the distance decay functions. The algorithm extracts the highways, green areas, and trees data from the input list and transforms the data into the given CRS. The CRS affects how distances, areas, and other measurements are calculated. Different CRSs may represent the Earth's surface in ways that either exaggerate or minimize certain dimensions. So, using the wrong CRS can lead to incorrect calculations and analyses. If you're focusing on a city or other localized area, you'll likely want to use a CRS that is tailored to that specific location. This could be a local city grid system or other local CRS that has been designed to minimize distortions in that area. This function then defines distance decay functions for green areas and trees using the parameter D. For each edge in the highway data, the function calculates the green index using the decay functions and returns a data frame with the green index for each edge. By default, D is specified to 100 (distance decay parameter in meters) but it can be changed by the user. Similarly, the users must specify the CRS (https://epsg.io/). The green index ranges from 0 to 1 and it represents the relative greenness of each section, factoring in proximity to green spaces and tree density.

```R
green_index <- calculate_green_index(data, 4326, 100)
```

## Create the green index plot

This function visualizes the green index on a map, with options for both static and interactive display. Interactive maps are rendered using Leaflet and Mapbox, allowing users to zoom, pan, and interact with the map to explore the green index in more detail. 

### Features
- **Dynamic Mapping**: Create interactive, dynamic maps for a more engaging and detailed visualization.
- **Customization**: Modify color palette, text size, resolution, title, axis labels, legend position, line width, and line type to suit your preferences.

### Usage
```R
# Create a static plot
map <- plot_green_index(green_index)

# Customize static plot
map <- plot_green_index(green_index, colors = c("#FF0000", "#00FF00"), line_width = 1, line_type = "dashed")

```
### 3D linestring map using Mapbox GL JS

This map is designed to visualize linear features such as roads, trails, or any other types of linestring data. This can be particularly useful for visualizing connectivity, transportation networks, or other linear spatial patterns.The function supports interactive controls for adjusting the line width and toggling building visibility on the map. It accepts linestring data and automatically processes it to create a visually appealing 3D map.

```R
mapbox_token <- "your_mapbox_access_token_here"
create_linestring_3D(green_index, "green_index", mapbox_token)
```
![3D Linestring Map](https://github.com/sachit27/Accessibility-Analysis/blob/main/images/linestring.gif)

### Customize Interactive Base Map (Leaflet version)
In interactive mode, you can change the base map to various themes.

```R
# Create an interactive plot using Leaflet
map <- plot_green_index(green_index, interactive = TRUE, base_map = "CartoDB.DarkMatter")

# To view the plot in the console, use:
print(map)

# Use a light-themed base map
map <- plot_green_index(green_index, interactive = TRUE, base_map = "CartoDB.Positron")
print(map)
```
You can save the interactive map using the htmlwidgets library.
```R
library(htmlwidgets)
saveWidget(map, file = "my_plot.html")

```
![Map](/vignettes/darkmode.jpg)

### 3D Hex map (Mapbox version)

The 'create_hexmap_3D' function generates a 3D hexagon map using H3 hexagons and Mapbox GL JS. This map can visualize various types of geographical data, such as points, linestrings, polygons, and multipolygons. It is particularly useful for visualizing density or green indices over an area. It automatically processes these geometries, converting them to points for visualization. Users can dynamically change the radius of the hexagons and their heights to better represent the data. The resulting map includes controls for adjusting hexagon height and H3 resolution, and selecting different Mapbox styles.

```R
mapbox_token <- "your_mapbox_access_token_here"

create_hexmap_3D(
  data = green_index,
  value_col = "green_index",
  mapbox_token = mapbox_token,
  output_file = "map.html",
  color_palette = "interpolateViridis"
)
```
![3D Hex Map](https://github.com/sachit27/Accessibility-Analysis/blob/main/images/hex3d.gif)

## Calculate the percentage of edges with a certain green index

This function groups the edges by their respective green index and calculates the percentage of edges for each green index. For easier interpretation, we categorize the index into three tiers: Low ( < 0.4), Medium (0.4-0.7), and High (> 0.7). 

```R
percentage <- calculate_percentage(green_index)
```

## Data export and sharing

These functions allow the user to download the green index values as a GeoJson file as well as a Leaflet map. The GeoJSON file retains the geographical properties of the data and can be readily employed in a broad range of GIS applications. The Leaflet map, saved as an HTML file, provides an interactive user experience, facilitating dynamic exploration of the data. The users should specify the file path to save these files.

```R
download_file <- save_json(green_index, "File Path") #file path has to be specified. For example "/Users/.../map.geojson"
map <- save_as_leaflet(green_index, "File Path")
```
![Map](/vignettes/leaflet.jpg)

## Shiny Application

![Map](/vignettes/shiny.jpg)

You can make your own greenness analysis without having to code using an R Shiny implementation of the package. It is easily accessible from within R by calling the function `run_app()`.


## Canopy Height Model (CHM) Analysis with ALS GEDI Data

The chm_analysis() function enables robust analysis and visualization of canopy height using Meta & WRI’s global 1m ALS GEDI v6 dataset. This function automatically downloads, mosaics, and processes high-resolution canopy height raster tiles for any area of interest, defined by a city name, bounding box, GeoJSON, or user-supplied .tif file.

- Data Source: Meta & WRI 1m ALS GEDI v6 global canopy height model (2024), covering most vegetated land worldwide.

- Features: Computes statistics, generates publication-quality maps and interactive web maps, and quantifies tree cover above a user-defined height.

- Performance: Processing may take significant time for large regions due to high data volume and tile downloads.

The implementation is inspired by the excellent chmloader R package, adapted for greater flexibility and integration with the greenR urban analytics workflow.

![CHM](/vignettes/chm.jpg)

```R
# Analyze canopy structure for Basel, Switzerland (bounding box example)
result <- chm_analysis(
  bbox = c(7.55, 47.54, 7.62, 47.59),     # xmin, ymin, xmax, ymax (WGS84)
  output_dir = "chm_output",               # Output directory for files
  max_tiles = 5,                           # Limit tiles for demo
  height_threshold = 2,                    # Height threshold for tree cover stats (meters)
  create_plots = TRUE                      # Export static and interactive maps
)

# View main outputs
print(result$stats)          # Summary statistics
result$static_map            # Publication-ready map (tmap)
browseURL(result$mapview_file) # Interactive web map (HTML)
```

## Green View Index
This function allows the users to quantify urban greenness through image analysis. Utilizing the [SuperpixelImageSegmentation library](https://cran.r-project.org/web/packages/SuperpixelImageSegmentation/SuperpixelImageSegmentation.pdf), it reads an image of an urban landscape and segments it into superpixels. The Green View Index (GVI) is then calculated by identifying green pixels within these segments. The GVI provides an objective measure of the proportion of visible vegetation in an image and is an important indicator for understanding urban greenness and its impact on ecological and human health.

The GVI is calculated using the following formula:

GVI = $\frac{\text{Number of Green Pixels}}{\text{Total Number of Pixels}}$

Where "Green Pixels" are identified based on a threshold that considers the RGB values of each pixel.

```R
result <- calculate_and_visualize_GVI("/path/to/your/image.png")
OpenImageR::imageShow(result$segmented_image) #To visualize the segmented image
green_pixels_raster <- as.raster(result$green_pixels_image) #To visualize green pixels
plot(green_pixels_raster)

To save images directly within R, you can use:
# Save the segmented image
OpenImageR::writeImage(result$segmented_image, "/path/to/save/segmented_image.png")

# Save the green pixels image
OpenImageR::writeImage(result$green_pixels_image, "/path/to/save/green_pixels_image.png")
```
![GVI](/vignettes/gvi.jpg)

## Green Space Similarity Index (GSSI)

The `gssi()` function calculates the Green Space Similarity Index (GSSI), a composite metric for evaluating and comparing urban green spaces across different regions by analyzing their size and spatial connectivity.

#### Functionality
This function transforms spatial data into an equal-area projection for accurate area measurements, computes the total area of green spaces, and assesses their spatial connectivity using the Average Nearest Neighbor Distance (ANND). This dual approach provides a comprehensive view of the distribution and accessibility of green spaces.

#### Implementation
The GSSI is calculated by inversely weighting the coefficient of variation in area sizes with the ANND, offering a score that reflects the abundance and accessibility of green spaces. Scores are normalized against the highest scoring city in the dataset for relative comparisons:

```r
# Example of calculating GSSI for multiple cities
d1 <- get_osm_data("New Delhi, India")
dsf <- d1$green_areas$osm_polygons
d2 <- get_osm_data("Basel, Switzerland")
bsf <- d2$green_areas$osm_polygons
d3 <- get_osm_data("Medellin, Colombia")
msf <- d3$green_areas$osm_polygons

cities_data <- list(dsf, bsf, msf)
gssi_values <- gssi(cities_data, "ESRI:54009")
```

## Citation

To cite this package in publications, use:

**APA**: Mahajan, S., 2024. greenr: An open-source framework for quantifying urban greenness. Ecological Indicators 163, 112108. URL: https:// www.sciencedirect.com/science/article/pii/S1470160X2400565X, doi:https://doi.org/10.1016/j.ecolind.2024.112108

**BibTex Entry**: @article{MAHAJAN2024112108,
title = {greenR: An open-source framework for quantifying urban greenness},
journal = {Ecological Indicators},
volume = {163},
pages = {112108},
year = {2024},
issn = {1470-160X},
doi = {https://doi.org/10.1016/j.ecolind.2024.112108},
url = {https://www.sciencedirect.com/science/article/pii/S1470160X2400565X},
author = {Sachit Mahajan},
keywords = {Urban greenness, Open source, Urban analytics, Cities, Street network}
}

## Handling SSL Certificate Errors

If you encounter an error related to SSL certificate authentication, such as:

```R
Error in curl::curl_fetch_memory(url, handle = handle) :
Peer certificate cannot be authenticated with given CA certificates: SSL certificate problem: certificate has expired
```
It may be necessary to update the CA certificates on your system.

## Important Performance Notice

This package, 'greenR' provides tools to measure and visualize the 'greenness' of urban areas. It performs intensive computations that require robust computational resources, particularly when analyzing large urban networks.

Please be aware of the following:

1. Processing time: Depending on the size of the area under analysis, computations may take a considerable amount of time. Larger areas, like whole cities or metropolitan regions, will take longer to process compared to small neighborhoods or districts.
2. Computational resources: Due to the computational intensity of these tasks, it is recommended to run this package on a machine with a strong CPU and sufficient RAM. Please ensure your machine meets these requirements before starting the computation to prevent any interruptions or crashes.
3. Testing: If you are using 'greenR' for the first time, or if you're testing on a new machine, it is suggested to begin with a smaller area - such as a specific neighborhood or small town. This will give you a rough idea of how long the computations might take and how well your machine can handle them.

Remember, performance can greatly vary based on the size of the network and the hardware of your machine.

## Acknowledgments
The OSM data is available under the Open Database License.

