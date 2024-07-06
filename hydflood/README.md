
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hydflood <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/hydflood)](https://cran.r-project.org/package=hydflood)
[![](https://cranlogs.r-pkg.org/badges/grand-total/hydflood?color=green)](https://cran.r-project.org/package=hydflood)
[![](https://cranlogs.r-pkg.org/badges/last-month/hydflood?color=green)](https://cran.r-project.org/package=hydflood)
[![](https://cranlogs.r-pkg.org/badges/last-week/hydflood?color=green)](https://cran.r-project.org/package=hydflood)
<!-- badges: end -->

The R package **hydflood** is designed to compute flood extents and
durations along the German federal waterways Elbe and Rhine.

## Installation

**hydflood** is available from CRAN. To install it run:

``` r
install.packages("hydflood")
```

To install the latest development version from Github run:

``` r
install.packages("devtools")
library(devtools)
devtools::install_github("bafg-bund/hydflood")
```

## Usage

The package **hydflood** is build around the packages `terra` and
`hyd1d`.

``` r
# load the package
options("hydflood.datadir" = tempdir())
library(hydflood)

# import the raster data and create a raster stack
x <- hydSpatRaster(filename_dem = "data-raw/raster.dem.tif",
                   filename_csa = "data-raw/raster.csa.tif")

# create a temporal sequence
seq <- seq(as.Date("2016-12-01"), as.Date("2016-12-31"), by = "day")

# compute a flood duration
fd <- flood3(x = x, seq = seq)

# and plot it
plot(fd)
```

<img src="man/figures/README-usage-1.png" style="display: block; margin: auto;" />
