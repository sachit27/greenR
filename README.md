<p float="left">
  <img src="/img/logo.png" width="230" />
  <img src="/img/zh_network.png" width="250" /> 
  <img src="/img/zh_gi.png" width="300" />
</p>

# greenR

## Introduction

greenR is a cutting-edge, open-source R package designed to tackle the challenging task of quantifying urban greenness. Leveraging crowd-sourced data from OpenStreetMap, greenR provides a new and scalable method to assign green indices to individual street segments. The greenR package offers a comprehensive solution by facilitating green index quantification, analysis, and visualization.


Accompanied by a Shiny app, greenR can empower researchers, planners, policy-makers, and citizens to study urban greenness patterns across cities.

## Installation

The functionality in this repository is implemented in the R package greenR. This package is currently not available on CRAN but can be obtained via GitHub by running the command below in R.

```r
# install.packages("remotes") # Uncomment if you do not have the 'remotes' package installed
remotes::install_github("sachit27/greenR", dependencies = TRUE)

After installation, you can load the package into the R session using the following command.

```r
library(greenR)

```

## Usage

(Here you can provide examples of how to use your package, similar to the "Create your own in R" section in the rcityviews README)

## Shiny Application

You can make your own greenness analysis without having to code using an R Shiny implementation of the package. A live version of the application can be found [here](link to your shiny app), but it is also easily accessible from within R by calling the function `greenR_shiny()`.

## Acknowledgements

The data used by the package is available under the Open Database License.
