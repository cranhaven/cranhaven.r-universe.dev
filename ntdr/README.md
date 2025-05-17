
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ntdr: Retrieve data from the National Transit Database <a href="https://vgxhc.github.io/ntdr/"><img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/vgXhc/ntdr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vgXhc/ntdr/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/ntdr)](https://cran.r-project.org/package=ntdr)
<!-- badges: end -->

The `ntdr` package provides a convenient interface to retrieve monthly
data from the National Transit Database (NTD). Without this package,
getting NTD data involves going to the NTD website and downloading and
processing a complex Excel workbook.

The NTD data returned by this package has many quirks and subtleties.
Please use it with care and [report any
issues](https://github.com/vgXhc/ntdr/issues).

To learn more about the package, visit the package website:
<https://vgxhc.github.io/ntdr/>.

## Installation

``` r
# The latest stable version can be installed from CRAN:
install.packages("ntdr")


# Alternatively, install the latest development version from GitHub:
remotes::install_github("vgXhc/ntdr")
```

## Example

Basic usage:

``` r
library(ntdr)

options(ntdr.cache = TRUE)

ntd_madison <- get_ntd(
  data_type = "adjusted",
  ntd_variable = "UPT",
  agency = "City of Madison",
  modes = "MB"
)

head(ntd_madison)
#> # A tibble: 6 × 13
#>   ntd_id_5 ntd_id_4 agency       active reporter_type uace  uza_name modes tos  
#>   <chr>    <chr>    <chr>        <chr>  <chr>         <chr> <chr>    <chr> <chr>
#> 1 50005    5005     City of Mad… Active Full Reporter 53200 Madison… MB    DO   
#> 2 50005    5005     City of Mad… Active Full Reporter 53200 Madison… MB    DO   
#> 3 50005    5005     City of Mad… Active Full Reporter 53200 Madison… MB    DO   
#> 4 50005    5005     City of Mad… Active Full Reporter 53200 Madison… MB    DO   
#> 5 50005    5005     City of Mad… Active Full Reporter 53200 Madison… MB    DO   
#> 6 50005    5005     City of Mad… Active Full Reporter 53200 Madison… MB    DO   
#> # ℹ 4 more variables: modes_simplified <chr>, month <date>, value <dbl>,
#> #   ntd_variable <chr>
```
