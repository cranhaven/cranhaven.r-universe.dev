
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mobilityIndexR

<!-- badges: start -->

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/bcmullins/mobilityIndexR?branch=master&svg=true)](https://ci.appveyor.com/project/bcmullins/mobilityIndexR)
<!-- badges: end -->

mobilityIndexR measures mobility in a population by generating
transition matrices and calculating mobility indices.

## Installation

<!-- You can install the released version of mobilityIndexR from [CRAN](https://CRAN.R-project.org) with: -->

<!-- #``` r -->

<!-- #install.packages("mobilityIndexR") -->

<!-- #``` -->

``` r
# Install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("bcmullins/mobilityIndexR")
```

## Basic Usage

Let’s use one of the built in datasets to create a transition matrix:

``` r
library(mobilityIndexR)
data("incomeMobility")
getTMatrix(dat = incomeMobility, col_x = 't0', col_y = 't5', type = 'relative', probs = TRUE, num_ranks = 5)
#> $tmatrix
#>    
#>         1     2     3     4     5
#>   1 0.152 0.040 0.008 0.000 0.000
#>   2 0.048 0.080 0.048 0.024 0.000
#>   3 0.000 0.048 0.064 0.056 0.032
#>   4 0.000 0.008 0.024 0.120 0.048
#>   5 0.000 0.024 0.056 0.000 0.120
#> 
#> $col_x_bounds
#>      0%     20%     40%     60%     80%    100% 
#>   462.0 21543.4 42469.8 64061.6 77888.4 99557.0 
#> 
#> $col_y_bounds
#>          0%         20%         40%         60%         80%        100% 
#>    340.2705  18204.9969  39710.3062  58494.6271  78178.6713 262909.3195
```

Using this data, let’s now calculate mobility indices:

``` r
  library(mobilityIndexR)
  data("incomeMobility")
  getMobilityIndices(dat = incomeMobility, col_x = 't0', col_y = 't5', type = 'relative', num_ranks = 5)
#> $average_movement
#> [1] 0.64
#> 
#> $os_far_bottom
#> [1] 0.04
#> 
#> $os_far_top
#> [1] 0.4
#> 
#> $os_total_bottom
#> [1] 0.24
#> 
#> $os_total_top
#> [1] 0.4
#> 
#> $prais_bibby
#> [1] 0.464
#> 
#> $wgm
#> [1] 0.58
```

## More examples coming soon\!
