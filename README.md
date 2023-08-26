<p float="left">
  <img src="/img/logo.png" width="230" />
  <img src="/img/zh_network.png" width="250" /> 
  <img src="/img/zh_gi.png" width="300" />
</p>

# greenR: An R Package for Quantifying Urban Greenness

## Introduction

greenR is an open-source R package designed to tackle the challenging task of quantifying urban greenness. Leveraging crowd-sourced data from OpenStreetMap (OSM), greenR provides a new and scalable method to assign green indices to individual street segments. The greenR package offers a comprehensive solution by facilitating green index quantification, analysis, and visualization.


Accompanied by a Shiny app, greenR can empower researchers, planners, policy-makers, and citizens to study urban greenness patterns across cities.

For more detailed information about the motivation, methodology, and validation, please have a look at this [Preprint](https://www.researchgate.net/publication/372909423_greenR_An_Open-Source_Framework_for_Quantifying_Urban_Greenness)

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

The first step is to acquire data. This provides a systematic approach to collecting the requisite geospatial data from OSM, thereby serving as the foundation for all subsequent analyses. The users can simply specify any city or neighborhood (that has data available in OSM database). This function looks in the internal database and finds any city and downloads OSM data for the specified spatial area with regard to three key environmental features: highways, green areas, and trees.

```R
data <- get_osm_data("City of London, United Kingdom")
```
Or
```R
data <- get_osm_data("Fulham, London, United Kingdom")
```

## Calculate the green index for the specified city

This function takes as input the OSM data, a Coordinate Reference System (CRS) code, and parameter D for the distance decay functions. The algorithm extracts the highways, green areas, and trees data from the input list and transforms the data into the given CRS. The CRS affects how distances, areas, and other measurements are calculated. Different CRSs may represent the Earth's surface in ways that either exaggerate or minimize certain dimensions. So, using the wrong CRS can lead to incorrect calculations and analyses. If you're focusing on a city or other localized area, you'll likely want to use a CRS that is tailored to that specific location. This could be a local city grid system or other local CRS that has been designed to minimize distortions in that area. This function then defines distance decay functions for green areas and trees using the parameter D. For each edge in the highway data, the function calculates the green index using the decay functions and returns a data frame with the green index for each edge. By default, D is specified to 100 (distance decay parameter) but it can be changed by the user. Similarly, the users must specify the CRS (https://epsg.io/). The green index ranges from 0 to 1 and it represents the relative greenness of each section, factoring in proximity to green spaces and tree density.

```R
green_index <- calculate_green_index(data, 4326, 100)
```

## Create the green index plot

This function creates a color-coded map of the green index across the city's network, allowing for extensive customization and interactive exploration. Each edge is plotted and color-coded based on its green index, providing an intuitive, easy-to-understand depiction of the city's spatial distribution of greenery.

### Features
- **Customization**: Customize the appearance of the plot by adjusting parameters such as color palette, text size, resolution, title, axis labels, legend position, line width, and line type.
- **Interactive Plotting**: Choose to render the plot interactively using Plotly, enabling zooming, panning, and tooltips for a more immersive experience.
- **Export Options**: Save the plot in various formats like PNG, PDF, and JPEG.

### Usage
Use the `plot_green_index` function to create the plot. The function allows for various customization options and can also render an interactive plot using Plotly.

```R
# Create a static plot
map <- plot_green_index(green_index)

# Create an interactive plot using Plotly
map <- plot_green_index(green_index, interactive = TRUE)

# Customize the plot
map <- plot_green_index(green_index, colors = c("#FF0000", "#00FF00"), line_width = 1, line_type = "dashed")
```

## Calculate the percentage of edges with a certain green index

This function groups the edges by their respective green index and calculates the percentage of edges for each green index. For easier interpretation, we categorize the index into three tiers: Low ( < 0.4), Medium (0.4-0.7), and High (> 0.7). 

```R
percentage <- calculate_percentage(green_index)
```

## Data export and sharing

These functions allow the user to download the green index values as a GeoJson file as well as a Leaflet map. The GeoJSON file retains the geographical properties of the data and can be readily employed in a broad range of GIS applications. The Leaflet map, saved as an HTML file, provides an interactive user experience, facilitating dynamic exploration of the data. The users should specify the file path to save these files.

```R
download_file <- save_json(green_index, File Path)
map <- save_as_leaflet(green_index, File Path)
```
![Map](/img/leaflet.png)

## Shiny Application

![Map](/img/shiny.png)

You can make your own greenness analysis without having to code using an R Shiny implementation of the package. It is easily accessible from within R by calling the function `run_app()`.

## Citation

To cite this package in publications, use:

**APA**: Sachit Mahajan. (2023). greenR: An Open-Source Framework for Quantifying Urban Greenness. Unpublished. https://doi.org/10.13140/RG.2.2.36266.18888

**BibTex Entry**: @article{Sachit Mahajan_2023, title={greenR: An Open-Source Framework for Quantifying Urban Greenness}, url={https://rgdoi.net/10.13140/RG.2.2.36266.18888}, DOI={10.13140/RG.2.2.36266.18888}, journal={Unpublished}, author={Sachit Mahajan}, year={2023} }

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

