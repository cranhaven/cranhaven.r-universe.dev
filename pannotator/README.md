
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{pannotator}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/NunzioKnerr/pannotator/graph/badge.svg)](https://app.codecov.io/gh/NunzioKnerr/pannotator)

<!-- badges: end -->

## pannotator

The Panospheric Image Annotator in R (pannotator) software package
provides an easy-to-use interface for visualising 360 degree camera
images on satellite imagery and annotating the images with data selected
from user-defined drop-down menus. It is designed for use in ecological
and biogeographical research but can be used to extract data from any
spatially explicit 360 degree camera imagery. This vignette provides an
overview of the functionality of the package, including setup and
configuration, interface layout, image selection, drop-down menu
specification, annotation of image files, and exporting data.

## Installation

### Minimum Requirements

To use this package, please ensure your system meets the following
minimum requirements:

- **R version**: 4.4.0 or higher

- **RStudio version**: 2024.04.2+764 or higher

- **Shiny version**: 1.9.1 or higher

Additionally, ensure that all necessary system dependencies are
installed for optimal performance.

Below is some code to help ensure all dependencies are met:

The software makes extensive use of ExifTool by Phil Harvey
([Exiftool.org](https://exiftool.org/)). To make installation of
ExifTool accessible in R there is a package exiftoolr that you must
install by running the code below.

``` r
# First check if you have exiftoolr installed
check_for_package <- system.file(package = "exiftoolr")
print(check_for_package)

# If not run the following code
if (check_for_package == "") {
  print("exiftoolr package not found .....installing now")
  install.packages("exiftoolr")
} else {
  print("exiftoolr package is already installed")
}
```

Now that you have installed exiftoolr we can check to make sure that
ExifTool is on your system.

``` r
library(exiftoolr)  
check_for_ExifTool <- exiftoolr::exif_version(quiet = TRUE)

# Install ExifTool if not found
if (exists("check_for_ExifTool")) {
  print("ExifTool found on system")
  exiftoolr::exif_version()
} else {
  print("ExifTool not found ... installing now")   
  exiftoolr::install_exiftool()
}
```

You must also install the ‘remotes’ package which we will use to install
the pannotator package.

``` r
check_for_package <-  system.file(package = "remotes")

print(check_for_package)
# If not run the following code
if (check_for_package == "") {
  print("remotes package not found .....installing now")
  install.packages("remotes")
} else {
  print("remotes package is already installed")
}
```

You can now install the development version of the pannotator software.

``` r
library(remotes)

# to install from github use this code: 
remotes::install_github("nunzioknerr/pannotator_package_source")
```

## Running the Package

To run the application use the following code.

``` r
library(pannotator)

options(shiny.port = httpuv::randomPort(), shiny.launch.browser = .rs.invokeShinyWindowExternal, shiny.maxRequestSize = 5000 * 1024^2)

run_app()
```

Once run, the above code will popup a browser window with the shiny
application inside it.

## Help Vignette

If you want help you can find it using the following code:

``` r
vignette('pannotator', package = 'pannotator')
```
