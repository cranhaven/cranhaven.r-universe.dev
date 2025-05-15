[![CRAN version](http://www.r-pkg.org/badges/version/sprex?color=red)](https://cran.r-project.org/package=sprex)
[![CRAN last day downloads](http://cranlogs.r-pkg.org/badges/last-day/sprex?color=red)](https://cran.r-project.org/package=sprex)
[![CRAN last week downloads](http://cranlogs.r-pkg.org/badges/last-week/sprex?color=red)](https://cran.r-project.org/package=sprex)
[![CRAN last month downloads](http://cranlogs.r-pkg.org/badges/sprex?color=red)](https://cran.r-project.org/package=sprex)
[![CRAN total downloads](http://cranlogs.r-pkg.org/badges/grand-total/sprex?color=red)](https://cran.r-project.org/package=sprex)  
[![R-CMD-check](https://github.com/EricArcher/sprex/workflows/R-CMD-check/badge.svg)](https://github.com/EricArcher/sprex/actions)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/EricArcher/sprex?branch=master&svg=true)](https://ci.appveyor.com/project/EricArcher/sprex)

# sprex  [![DOI](https://zenodo.org/badge/23926/EricArcher/sprex.svg)](https://zenodo.org/badge/latestdoi/23926/EricArcher/sprex)

## Description

*sprex* is a package for calculating species richness for rarefaction and extrapolation. It contains functions for calculating non-parametric species richness such as jackknife, Chao1, and ACE. Also available are functions for plotting species richness and extrapolation curves, and standard diversity and entropy indices.

## Installation

To install the stable version from CRAN:

```r
install.packages('sprex')
```

To install the latest version from GitHub:

```r
# make sure you have devtools installed
if (!require('devtools')) install.packages('devtools')

# install from GitHub
devtools::install_github('ericarcher/sprex')
```

## Contact

* submit suggestions and bug-reports: <https://github.com/ericarcher/sprex/issues>
* send a pull request: <https://github.com/ericarcher/sprex/>
* e-mail: <eric.archer@noaa.gov>

## version 1.4.2 (on CRAN)

* removed `plot.discovery.curve()`
* redesigned `discovery.curve()` to return data frame defining curve and plotting curve if selected.
* added `diversity()` to compute diversity and entropy indices

## version 1.4.1

* added `pct.range` argument to `clench` function to specify minimum and maximum percentages of sample sizes
* changed argument `m` in `expected.num.species` function to accept vectors and return a matrix