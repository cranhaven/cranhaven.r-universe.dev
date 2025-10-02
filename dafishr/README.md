
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dafishr <a href='https://cbmc-gcmp.github.io/dafishr/'><img src="man/figures/logo.png" align="right" height="139"/></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/dafishr)](https://CRAN.R-project.org/package=dafishr)

<!-- badges: end -->

The goal of `dafishr` is to provide an easy way to download Vessel
Monitoring System (VMS) and analyse data from the Mexican Fishery
Commission available at [Datos Abiertos](https://www.datos.gob.mx/)
initiative searching for “Sistema de Monitoreo Satelital Embarcaciones”
in Spanish. Within the package you can find tools that allows you to
download VMS data, wrangle and clean raw data, and analyse tracks.

The VMS stands for Vessel Monitoring System, which is adopted on
industrial fishing vessels to monitor fishing activity. These data are
very important to understand the fishing activity within a country, its
dynamics in time and space, and to monitor the activity within Marine
Protected Areas (MPAs). Along with data from VMS we also provide layers
that are used to clean and map the information. We are currently working
on a scientific manuscript which will be related to this work that is
currently under review.

You can follow the instruction below using a sample dataset that comes
along with the package, or you can use the function on data you can
download yourself by using the `vms_download()` function. See ?vms_data
for details on its usage.

## Installation

``` r
install.packages("dafishr")
```

You can install development version of `dafishr` with:

``` r
# install.packages("devtools")
devtools::install_github("CBMC-GCMP/dafishr")
```

If you haven’t `devtools` package previously installed just delete the
comment \# from the code above and run both lines.

## Where to start

You can start using `dafishr` suit of functions using the
`sample_dataset` provided with this package, or you can download your
own raw-data files using the `vms_download()` function. Further details
are explained in the [documentation
vignette](https://cbmc-gcmp.github.io/dafishr/index.html) for this
package. You can see the suit of data and functions available within the
package
[here](https://cbmc-gcmp.github.io/dafishr/reference/index.html#all-functions).

## Dependencies

This package follows the `tidyverse` programming style and depends on
several package of the family that will be downloaded automatically once
installed. Some functions can be applied to a more general object, but
these are specifically built for the format of the raw data of the VMS
form CONAPESCA (Mexican Fishery Commission). Therefore, these package
focused mostly on that format to help user analyse and report data.

## How to contribute

The workflow provided here is a work in progress and there are probably
some errors we haven’t spotted or considered up to now. If you feel you
can contribute to this effort feel free to do so by creating a pull
request. If you are an undergrad and you which to help or develop
scientific projects using this data you are welcome to contact us.
Please, find contact information of the main author
[here](https://github.com/Fabbiologia/), or via
[twitter](https://twitter.com/FabioFavoretto/).
