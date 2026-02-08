
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ShinyLink - RShiny Based Record Linkage Tool

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ShinyLink)](https://cran.r-project.org/package=ShinyLink)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## About

ShinyLink is the bridge between existing vigorous open-source record
linkage algorithms and an urgently needed accessible platform that
eliminates cost and programming barriers and delivers a public health
and bioinformatics precedent toward increased data interchangeability.

## Installation

To install a stable version, you can install ShinyLink from CRAN.

``` r
install.packages("ShinyLink")
```

To get a bug fix, or use a feature from the development version, you can
install ShinyLink from GitHub.

### Windows platform

For windows users, please make sure that you have the latest R (\>=
4.2.2) and RTools (\>= 4.2) installed

- Download link for R: <https://cran.r-project.org/>
- Download link for RTools:
  <https://cran.r-project.org/bin/windows/Rtools/>

### MacOS X platform

Download and install the latest version of R (\>= 4.2.2) for your Mac
From CRAN: <https://cran.r-project.org/bin/macosx/>

### Install devtools and ShinyLink

Launch R and run these commands to install

``` r
if(!require(httpuv)) install.packages("httpuv")
if(!require(devtools)) install.packages("devtools")

devtools::install_github("cdc-addm/ShinyLink",dependencies=TRUE)
```

### Start using ShinyLink

By typing this command in the R Console to start using ShinyLink

``` r
ShinyLink::run_app()
```

## Code style

Since this is a collaborative project, please adhere to the following
code formatting conventions: \* We use the tidyverse style guide
(<https://style.tidyverse.org/>). \* Please write roxygen2 comments as
full sentences, starting with a capital letter and ending with a period.
Brevity is preferred (e.g., “Calculates standard deviation” is preferred
over “This method calculates and returns a standard deviation of given
set of numbers”).

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.
