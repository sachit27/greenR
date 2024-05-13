<p float="left">
  <img src="/img/logo.png" width="230" />
  <img src="/img/zh_network.png" width="250" /> 
  <img src="/img/zh_gi.png" width="300" />
</p>

# greenR: An R Package for Quantifying Urban Greenness

## Introduction

greenR is an open-source R package designed to tackle the challenging task of quantifying urban greenness. Leveraging crowd-sourced data from OpenStreetMap (OSM), greenR provides a new and scalable method to assign green indices to individual street segments. The greenR package offers a comprehensive solution by facilitating green index quantification, analysis, and visualization.


Accompanied by a Shiny app, greenR can empower researchers, planners, policy-makers, and citizens to study urban greenness patterns across cities.

For more detailed information about the motivation, methodology, and validation, please have a look at this paper published in [Ecological Indicators](https://www.sciencedirect.com/science/article/pii/S1470160X2400565X)

## Installation

The functionality in this repository is implemented in the R package greenR. This package is currently not available on CRAN but can be obtained via GitHub by running the command below in R.

```r
# install.packages("remotes") # Uncomment if you do not have the 'remotes' package installed
remotes::install_github("sachit27/greenR", dependencies = TRUE)
```
Or you can also use devtools to install the package.

```r
library(devtools)
devtools::install_github("sachit27/greenR", dependencies = TRUE)
```

NOTE: If you encounter any issues with the installation of the "osmdata" R package as a dependency, you may bypass CRAN and install it directly from the [Github Repository](https://github.com/ropensci/osmdata). This alternative installation method can be useful if the standard CRAN installation is not working as expected. Here is how you can install it.

```r
remotes::install_github ("ropensci/osmdata")
```

After installing the greenR package, you can load the package into the R session using the following command.

```r
library(greenR)

```

## Specify the city or neighborhood and download the data

The first step is to acquire data. This provides a systematic approach to collecting the requisite geospatial data from OSM, thereby serving as the foundation for all subsequent analyses. The users can simply specify any city or neighborhood (that has data available in OSM database). This function looks in the database and finds any city and downloads OSM data for the specified spatial area with regard to three key environmental features: highways, green areas, and trees. Here green areas include all the areas with the following tags: "forest", "vineyard", "plant_nursery", "orchard", "greenfield", "recreation_ground", "allotments", "meadow","village_green","flowerbed", "grass", "farmland", "garden", "dog_park","nature_reserve", and "park".

```R
data <- get_osm_data("City of London, United Kingdom")
```
Or
```R
data <- get_osm_data("Fulham, London, United Kingdom")
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
![Green_Areas and Clusters](/img/vis.png)

## Accessibility map

This function creates an interactive leaflet map displaying accessible green spaces within a specified walking time from a provided location. It utilizes isochrones to visualize the areas reachable by 5, 10, and 15 minutes of walking. The function relies on pedestrian routing information and green space data to accurately delineate accessible areas. The default maximum walking time is set to 15 minutes but can be adjusted using the 'max_walk_time' parameter.  

```R
accessibility_greenspace(green_areas_data, 47.56427527336772, 7.595820936462059)
```
![Isochrone](/img/isochrone.png)

## Calculate the green index for the specified city

This function takes as input the OSM data, a Coordinate Reference System (CRS) code, and parameter D for the distance decay functions. The algorithm extracts the highways, green areas, and trees data from the input list and transforms the data into the given CRS. The CRS affects how distances, areas, and other measurements are calculated. Different CRSs may represent the Earth's surface in ways that either exaggerate or minimize certain dimensions. So, using the wrong CRS can lead to incorrect calculations and analyses. If you're focusing on a city or other localized area, you'll likely want to use a CRS that is tailored to that specific location. This could be a local city grid system or other local CRS that has been designed to minimize distortions in that area. This function then defines distance decay functions for green areas and trees using the parameter D. For each edge in the highway data, the function calculates the green index using the decay functions and returns a data frame with the green index for each edge. By default, D is specified to 100 (distance decay parameter in meters) but it can be changed by the user. Similarly, the users must specify the CRS (https://epsg.io/). The green index ranges from 0 to 1 and it represents the relative greenness of each section, factoring in proximity to green spaces and tree density.

```R
green_index <- calculate_green_index(data, 4326, 100)
```

## Create the green index plot

This function visualizes the green index on a map, with options for both static and interactive display. Interactive maps are rendered using Leaflet, allowing users to zoom, pan, and interact with the map to explore the green index in more detail.

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
### Customize Interactive Base Map
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
![Map](/img/darkmode.png)

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
![Map](/img/leaflet.png)

## Shiny Application

![Map](/img/shiny.png)

You can make your own greenness analysis without having to code using an R Shiny implementation of the package. It is easily accessible from within R by calling the function `run_app()`.

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
```
![GVI](/img/gvi.png)

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

