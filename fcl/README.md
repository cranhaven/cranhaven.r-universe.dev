
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fcl

<!-- badges: start -->

[![R-CMD-check](https://github.com/shrektan/fcl/workflows/R-CMD-check/badge.svg)](https://github.com/shrektan/fcl/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/fcl)](https://CRAN.R-project.org/package=fcl)
[![Coverage
Status](https://coveralls.io/repos/github/shrektan/fcl/badge.svg?branch=main)](https://coveralls.io/github/shrektan/fcl?branch=main)
<!-- badges: end -->

A financial calculator written in Rust. It provides simple calculations
for bond YTM, Duration, etc.

## Installation

You’ll need the rust toolchain to compile this package from the source.

## Example

``` r
library(fcl)
## basic example code
bond <- fixed_bond(
  value_date = 210101,
  mty_date = c(250101, 300201),
  redem_value = 100,
  cpn_rate = c(0.05, 0.03),
  cpn_freq = c(0, 1)
)
bond$ytm_dur(
  ref_date = c(220101, 220201),
  clean_price = 100
)
#>          YTM     MACD     MODD
#> 1 0.04552728 3.000000 2.869366
#> 2 0.02999755 7.212793 7.002728
bond$cf(
  ref_date = c(220101, 220131)
)
#>    ID       DATE     COUPON REDEM
#> 1   1 2025-01-01 20.0000000   100
#> 2   2 2023-01-01  3.0000000     0
#> 3   2 2024-01-01  3.0000000     0
#> 4   2 2025-01-01  3.0000000     0
#> 5   2 2026-01-01  3.0000000     0
#> 6   2 2027-01-01  3.0000000     0
#> 7   2 2028-01-01  3.0000000     0
#> 8   2 2029-01-01  3.0000000     0
#> 9   2 2030-01-01  3.0000000     0
#> 10  2 2030-02-01  0.2547945   100

rtn <- make_rtn(date = c(210101, 210105, 210110), mv = c(100, 123, 140), pl = c(0, 3, 7))
rtn$twrr_cr(210102, 210110)
#>               TWRR_CR
#> 2021-01-02 0.00000000
#> 2021-01-03 0.00000000
#> 2021-01-04 0.00000000
#> 2021-01-05 0.02500000
#> 2021-01-06 0.02500000
#> 2021-01-07 0.02500000
#> 2021-01-08 0.02500000
#> 2021-01-09 0.02500000
#> 2021-01-10 0.07894737
rtn$twrr_dr(210102, 210110)
#>               TWRR_DR
#> 2021-01-02 0.00000000
#> 2021-01-03 0.00000000
#> 2021-01-04 0.00000000
#> 2021-01-05 0.02500000
#> 2021-01-06 0.00000000
#> 2021-01-07 0.00000000
#> 2021-01-08 0.00000000
#> 2021-01-09 0.00000000
#> 2021-01-10 0.05263158
rtn$dietz(210102, 210110)
#>                 DIETZ
#> 2021-01-02 0.00000000
#> 2021-01-03 0.00000000
#> 2021-01-04 0.00000000
#> 2021-01-05 0.02857143
#> 2021-01-06 0.02777778
#> 2021-01-07 0.02727273
#> 2021-01-08 0.02692308
#> 2021-01-09 0.02666667
#> 2021-01-10 0.08737864
rtn$dietz_avc(210102, 210110)
#>            DIETZ_AVC
#> 2021-01-02  100.0000
#> 2021-01-03  100.0000
#> 2021-01-04  100.0000
#> 2021-01-05  105.0000
#> 2021-01-06  108.0000
#> 2021-01-07  110.0000
#> 2021-01-08  111.4286
#> 2021-01-09  112.5000
#> 2021-01-10  114.4444
```
