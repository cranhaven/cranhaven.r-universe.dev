
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bupaverse <a href="https://bupaverse.github.io/bupaverse/"><img src="man/figures/logo.png" align="right" height="50" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/bupaverse)](https://CRAN.R-project.org/package=bupaverse)
[![GitHub
version](https://img.shields.io/badge/GitHub-0.1.0-blue)](https://github.com/bupaverse/bupaverse)
[![R-CMD-check](https://github.com/bupaverse/bupaverse/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bupaverse/bupaverse/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The **bupaverse** is an open-source, integrated suite of
[`R`](https://www.r-project.org/)-packages for handling and analysing
business process data, developed by the Business Informatics research
group at Hasselt University, Belgium. Profoundly inspired by the
[**tidyverse**](https://www.tidyverse.org/) package, the **bupaverse**
package is designed to facilitate the installation and loading of
multiple **bupaverse** packages in a single step. Learn more about
**bupaverse** at the [bupaR.net](https://bupar.net/) homepage.

## Installation

You can install **bupaverse** from [CRAN](https://cran.r-project.org/)
with:

``` r
install.packages("bupaverse")
```

### Development Version

You can install the development version of **bupaverse** from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bupaverse/bupaverse")
```

## Usage

`library(bupaverse)` will load the core **bupaverse** packages:

- [**bupaR**](https://bupaverse.github.io/bupaR/): Core package for
  business process analysis.
- [**edeaR**](https://bupaverse.github.io/edeaR/): Exploratory and
  descriptive analysis of event-based data.
- [**eventdataR**](https://bupaverse.github.io/eventdataR/): Repository
  of sample process data.
- [**processcheckR**](https://bupaverse.github.io/processcheckR/):
  Rule-based conformance checking and filtering.
- [**processmapR**](https://bupaverse.github.io/processmapR/): Visualise
  event-based data using, i.a., process maps.

An overview of the loaded packages and conflicts with other packages is
shown after loading **bupaverse**:

``` r
library(bupaverse)
#> 
#> .______    __    __  .______      ___   ____    ____  _______ .______          _______. _______
#> |   _  \  |  |  |  | |   _  \    /   \  \   \  /   / |   ____||   _  \        /       ||   ____|
#> |  |_)  | |  |  |  | |  |_)  |  /  ^  \  \   \/   /  |  |__   |  |_)  |      |   (----`|  |__
#> |   _  <  |  |  |  | |   ___/  /  /_\  \  \      /   |   __|  |      /        \   \    |   __|
#> |  |_)  | |  `--'  | |  |     /  _____  \  \    /    |  |____ |  |\  \----.----)   |   |  |____
#> |______/   \______/  | _|    /__/     \__\  \__/     |_______|| _| `._____|_______/    |_______|
#>                                                                                                 
#> ── Attaching packages ─────────────────────────────────────── bupaverse 0.1.0 ──
#> ✔ bupaR         0.5.2     ✔ processcheckR 0.2.0
#> ✔ edeaR         0.9.1     ✔ processmapR   0.5.2
#> ✔ eventdataR    0.3.1     
#> ── Conflicts ────────────────────────────────────────── bupaverse_conflicts() ──
#> ✖ bupaR::filter()          masks stats::filter()
#> ✖ processmapR::frequency() masks stats::frequency()
#> ✖ edeaR::setdiff()         masks base::setdiff()
#> ✖ bupaR::timestamp()       masks utils::timestamp()
#> ✖ processcheckR::xor()     masks base::xor()
```
