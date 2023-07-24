<p float="left">
  <img src="/img/logo.png" width="230" />
  <img src="/img/zh_network.png" width="250" /> 
  <img src="/img/zh_gi.png" width="300" />
</p>

# greenR: Green Index Quantification

## Introduction

greenR is a cutting-edge, open-source R package designed to tackle the challenging task of quantifying urban greenness. Leveraging crowd-sourced data from OpenStreetMap, greenR provides a new and scalable method to assign green indices to individual street segments. The greenR package offers a comprehensive solution by facilitating green index quantification, analysis, and visualization.


Accompanied by a Shiny app, greenR can empower researchers, planners, policy-makers, and citizens to study urban greenness patterns across cities.

## Installation

The functionality in this repository is implemented in the R package greenR. This package is currently not available on CRAN but can be obtained via GitHub by running the command below in R.

```r
# install.packages("remotes") # Uncomment if you do not have the 'remotes' package installed
remotes::install_github("sachit27/greenR", dependencies = TRUE)
```

After installation, you can load the package into the R session using the following command.

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
data <- get_osm_data("Fulham, City of London, United Kingdom")
```

## Calculate the green index for the specified city

This function takes as input the OSM data, a Coordinate Reference System (CRS) code, and parameter \texttt{D} for the distance decay functions. The algorithm extracts the highways, green areas, and trees data from the input list and transforms the data into the given CRS. It then defines distance decay functions for green areas and trees using the parameter \texttt{D}. For each edge in the highways data, the function calculates the green index using the decay functions and returns a data frame with the green index for each edge. By default, D is specified to 100 (distance decay parameter) but it can be changed by the user. Similarly, the users must specify the CRS (https://epsg.io/).

```R
green_index <- calculate_green_index(data, 4326, 100)
```

## Shiny Application

You can make your own greenness analysis without having to code using an R Shiny implementation of the package. A live version of the application can be found [here](link to your shiny app), but it is also easily accessible from within R by calling the function `greenR_shiny()`.

## Acknowledgements

The data used by the package is available under the Open Database License.
