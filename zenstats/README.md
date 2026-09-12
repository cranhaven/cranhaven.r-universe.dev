
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zenstats

<!-- badges: start -->

[![R-CMD-check](https://github.com/rfsaldanha/zenstats/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rfsaldanha/zenstats/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

A Zenodo deposit contains some statistics of interest, like the number
of views and downloads, that is not available to programmaticaly access
through its API. The `zenstats` package scraps deposit webpages and
return the deposits statistics as a tibble.

## Installation

You can install the development version of zenstats from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("rfsaldanha/zenstats")
```

## Example

``` r
library(zenstats)

ids <- c(10036212, 10947952)

deposit_stats(ids, all_versions_only = TRUE, progress = FALSE)
#> # A tibble: 2 × 5
#>   date                 deposit views downloads  volume
#>   <dttm>                 <dbl> <dbl>     <dbl>   <byt>
#> 1 2024-05-17 14:50:40 10036212   528       322 1.40 TB
#> 2 2024-05-17 14:50:41 10947952    72        39 1.70 GB
```

## Polite webpage crawling

This package respects the ‘robots.txt’ policies from Zenodo, including
the crawl delay of 10 seconds.
