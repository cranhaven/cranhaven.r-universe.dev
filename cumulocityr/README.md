
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cumulocityr

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/SoftwareAG/cumulocityr.svg?branch=master)](https://travis-ci.org/SoftwareAG/cumulocityr)
[![Codecov test
coverage](https://codecov.io/gh/SoftwareAG/cumulocityr/branch/master/graph/badge.svg)](https://codecov.io/gh/SoftwareAG/cumulocityr?branch=master)
<!-- badges: end -->

R client for the Cumulocity API.

## Installation

You can install the released version of cumulocityr from GitHub with
devtools:

``` r
# install.packages("devtools)
devtools::install_github("SoftwareAG/cumulocityr")
```

## Example

``` r
library(cumulocityr)

devices <- get_devices()

measurements <- get_measurements(device_id = 1234,
                                 date_from = "2019-09-30T20:00:00Z")

events <- get_events(device_id = 1234,
                     date_from = "2019-09-30T20:00:00Z")
```

-----

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/SoftwareAG/cumulocityr/blob/master/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

These tools are provided as-is and without warranty or support. They do
not constitute part of the Software AG product suite. Users are free to
use, fork and modify them, subject to the license agreement. While
Software AG welcomes contributions, we cannot guarantee to include every
contribution in the master project.
