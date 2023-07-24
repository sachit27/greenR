<img src="img/logo.png" width="200">

# greenR

## Introduction

How do we quantify the levels of greenness within urban street networks? Numerous attempts to quantify this factor have been made through survey methodologies, remote sensing data, and street view imagery. The results are promising, but are often limited by scalability constraints, including limited data availability, high requirements of computational power, and validation challenges. 

In this study, we introduce a novel method for assessing urban greenness by utilizing crowd-sourced OpenStreetMap data, allowing us to assign green indices to individual street segments. We rigorously test and validate our method using reference datasets, leading to the development of greenR, an R package as well as an accompanying Shiny app. This facilitates green index quantification, analysis, and visualization. 

We illustrate the efficacy of greenR by studying urban greenness patterns across several cities and highlight the potential impact of such an open-source frameworks for citizens, urban planners, and policy-makers.

## Installation

The functionality in this repository is implemented in the R package greenR. This package is not available on CRAN but can be obtained via GitHub by running the command below in R.

```r
# install.packages("remotes") # Uncomment if you do not have the 'remotes' package installed
remotes::install_github("sachit27/greenR", dependencies = TRUE)

After installation, you can load the package into the R session using the following command.

library(greenR)

```

## Usage

(Here you can provide examples of how to use your package, similar to the "Create your own in R" section in the rcityviews README)

## Shiny Application

You can make your own greenness analysis without having to code using an R Shiny implementation of the package. A live version of the application can be found [here](link to your shiny app), but it is also easily accessible from within R by calling the function `greenR_shiny()`.

## Acknowledgements

The data used by the package is available under the Open Database License.
