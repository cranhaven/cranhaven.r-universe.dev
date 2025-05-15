[![CRAN version](http://www.r-pkg.org/badges/version/swfscMisc?color=red)](https://cran.r-project.org/package=swfscMisc)
[![CRAN last day downloads](http://cranlogs.r-pkg.org/badges/last-day/swfscMisc?color=red)](https://cran.r-project.org/package=swfscMisc)
[![CRAN last week downloads](http://cranlogs.r-pkg.org/badges/last-week/swfscMisc?color=red)](https://cran.r-project.org/package=swfscMisc)
[![CRAN last month downloads](http://cranlogs.r-pkg.org/badges/swfscMisc?color=red)](https://cran.r-project.org/package=swfscMisc)
[![CRAN total downloads](http://cranlogs.r-pkg.org/badges/grand-total/swfscMisc?color=red)](https://cran.r-project.org/package=swfscMisc) 
[![R-CMD-check](https://github.com/EricArcher/swfscMisc/workflows/R-CMD-check/badge.svg)](https://github.com/EricArcher/swfscMisc/actions)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/EricArcher/swfscMisc?branch=master&svg=true)](https://ci.appveyor.com/project/EricArcher/swfscMisc)

# swfscMisc

## Description

*swfscMisc* is a collection of utility functions used at the NOAA Southwest Fisheries 
Science Center in La Jolla, CA. The package contains functions for geodesic 
calculations, commonly used  mapping functions, plotting special symbols, and miscellaneous analytical and conversion functions.

## Installation

To install the stable version from CRAN:

```r
install.packages('swfscMisc')
```

To install the latest version from GitHub:

```r
# make sure you have devtools installed
if (!require('devtools')) install.packages('devtools')
# install from GitHub
devtools::install_github('ericarcher/swfscMisc')
```

## Contact

* submit suggestions and bug-reports: <https://github.com/ericarcher/swfscMisc/issues>
* send a pull request: <https://github.com/ericarcher/swfscMisc/>
* e-mail: <eric.archer@noaa.gov>

## Current functions

* Geodesic functions:
    * bearing
    * datum
    * distance
    * circle.polygon
    * convert.angle
    * convert.distance

* Plotting functions:
    * braces
    * catSpatInterp Categorical Spatial Interpolation
    * color.name
    * lab.wid
    * lat.lon.axes
    * row.col.page.fit
    * ggBiplot
    * scatterdens
    * scatterhist
    * sex.symbols

* Distribution functions:
    * betaParams
    * central.quantile
    * distSmry
    * fisher.p
    * gammaParams
    * pVal
    * sn.params
    * uniform.test

* Miscellaneous functions:
    * affin.prop
    * autoUnits
    * box.area
    * central.quantile
    * copy.tri
    * crossing.point
    * geometric.mean
    * harmonic.mean
    * isBetween
    * month2Season
    * mcmc2list
    * na.count
    * odds conversion (odds, invOdds, logOdds, invLogOdds)
    * one.arg
    * plotAssignments
    * ceiling, floor, trunc, round (.data.frame)
    * runjags2list
    * stan2list
    * setupClusters
    * which.nearest
    * zero.pad
  
## Changelog
    
### version 1.6.6 (devel)

* fixed bug in `runjags2list()` if variable in monitor vector wasn't in model
* added `betaParams()` and `gammaParams()`
* removed `diversity()` - now in `sprex` package
    
### version 1.6.5 (on CRAN)

* removed `sample.map()`
* added `imdo()`
* added `intersectingPoint()`
* added ability to use `modeest::mlv()` to estimate mode in `distSmry()` 
* fixed `pVal()` to handle NA and NaN values correctly
* deprecating `diversity()`. New function in `sprex` package.
* added `runjags2list()` to format posterior samples from `runjags::run.jags()`.
* corrected package name alias documentation for CRAN

### version 1.6

* reset setupClusters to choose correct function for OS
* added `perpDist()` and `perpPt()`
* added `distSmry()`
* added `sn.params` functions for computing skew normal parameters and moments

### version 1.5

* removed das.* functions

### version 1.4

* added catSpatInterp for Categorical Spatial Interpolation (based on code by Timo Grossenbacher)
* begin deprecation of das.* functions. to be removed in v1.5.
* change `destination()` to accept vectors and return a matrix.
* fixed `plotAssignments()` to not change class names.
* updated spatstat import

### version 1.3

* fixed plotAssignments to use facets and removed grids and space around panels
* added setupClusters

### version 1.2

* Added rounding functions for data.frames.
* Removed NEWS.md - changelog moved to README.md
* Added `plotAssignments` function.

### version 1.1

* Added `autoUnits` function
* Changed default arguments for `lat.range` and `lon.range` to `NULL` in `sample.map`. If not specified, the ranges will be set to the ranges of the `lat` and `lon`.

### version 1.0.9

* Added `transparent` function
* Changed distance and destination functions to accept partial matches for method 
of calculation, type of surface, and units

### version 1.0.8

* Fixed `das.read` to handle errors in position and suppress warnings about `NA`s
during numerical conversions.
* Fixed `das.map` to remove records with no position

### version 1.0.7

* Changed `isBetween` to accept a vector of numbers

### version 1.0.6

* Added NEWS.md
* Added `diversity` function (moved from strataG package)
* Added `isBetween` function to test if a number is between two numbers