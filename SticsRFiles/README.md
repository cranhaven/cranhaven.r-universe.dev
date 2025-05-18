SticsRFiles
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R build
status](https://github.com/SticsRPacks/SticsRFiles/workflows/R-CMD-check/badge.svg)](https://github.com/SticsRPacks/SticsRFiles/actions)
[![Codecov test
coverage](https://codecov.io/gh/SticsRPacks/SticsRFiles/branch/master/graph/badge.svg)](https://app.codecov.io/gh/SticsRPacks/SticsRFiles?branch=master)
[![DOI](https://zenodo.org/badge/187986787.svg)](https://zenodo.org/badge/latestdoi/187986787)
![](https://www.r-pkg.org/badges/version-ago/SticsRFiles)

<!-- badges: end -->

The goal of SticsRFiles is to perform manipulations of the
[STICS](https://stics.inrae.fr/eng/) model files either on XML files
(used by the JavaSTICS GUI) or on text files used by the model fortran
executable.

The basic functionalities allows to read parameters names and values
through XML queries and replace parameters values in files. A starting
guide is available on the [Get started
page](https://sticsrpacks.github.io/SticsRFiles/articles/SticsRFiles.html).

Advanced functionalities are dedicated to produce XML parameter files
using a mailing process like from XML templates and Excel sheets
containing multiple simulations contexts. A JavaSTICS workspace is
generated and directly usable from the JavaSTICS interface (GUI or
command line), or with an R JavaSTICS interface provided by the
[SticsOnR](https://sticsrpacks.github.io/SticsOnR/) package.

If you want to be notified when a new release of this package is made,
you can tick the Releases box in the “Watch / Unwatch =\> Custom” menu
at the top right of [this
page](https://github.com/SticsRPacks/SticsRFiles).

## Prerequisites and technical tips

> Some information about software requirements and operating system
> constraints are given in the SticsOnR package documentation
> [here](https://sticsrpacks.github.io/SticsOnR/).

## Installation

------------------------------------------------------------------------

***Warning:*** *If during the installation process, packages updates are
suggested.*

- First, abort the installation.
- Second, update the installed packages, except the `XML` package.
- Finally, install the SticsRFiles package.

------------------------------------------------------------------------

### Recommended installation: `SticsRPacks`

The best way to install the packages from `SticsRPacks`, from which
`SticsRFiles` is part of, is by installing the `[SticsRPacks]` package.
The package can be installed from [GitHub](https://github.com/) using
either the `devtools` package, or the more lightweight the `remotes`
package:

- With `devtools`

``` r
      devtools::install_github("SticsRPacks/SticsRPacks")
```

- With `remotes`

``` r
      remotes::install_github("SticsRPacks/SticsRPacks")
```

The package will install the packages for you at the latest release
version.

### Direct installation from the CRAN or [GitHub](https://github.com/)

- From the CRAN

``` r
      install.packages("SticsRFiles")
```

- From GitHub

``` r
      devtools::install_github("SticsRPacks/SticsRFiles@*release")

      or 
      
      remotes::install_github("SticsRPacks/SticsRFiles@*release")
```

Normally, all the package dependencies will be installed for CRAN
packages.

## Examples

### Files manipulations

- A description of how to use the functions for manipulating XML input
  files is detailed
  [here](https://sticsrpacks.github.io/SticsRFiles/articles/Manipulating_Stics_XML_files.html)

- A description of how to use the functions for manipulating text input
  files is available
  [here](https://sticsrpacks.github.io/SticsRFiles/articles/Manipulating_Stics_text_files).

### Files generation

- A description of useful functions for generating XML input files from
  usms parameters data stored in Excel files sheets or CSV files is
  available
  [here](https://sticsrpacks.github.io/SticsRFiles/articles/Generating_Stics_XML_files.html).

- A description of functions for generating text input files from XML
  one of usms parameters is available
  [here](https://sticsrpacks.github.io/SticsRFiles/articles/Generating_Stics_text_files.html).

## Getting help

If you have any question or suggestion or if you want to report a bug,
please do it via the GitHub
[issues](https://github.com/SticsRPacks/SticsRFiles/issues).

Thanks for that, this would greatly help us to improve this package.

## Citation

If you have used this package for a study that led to a publication or
report, please cite us. You can either use the citation tool from GitHub
if you used the last version, or use `citation("SticsRFiles")` from R
otherwise.

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/SticsRPacks/SticsRFiles/blob/master/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.

The package is under intensive development, so you can fill an issue or
request us a feature
[here](https://github.com/SticsRPacks/SticsRFiles/issues) at any time.
