<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/panstarrs)](https://CRAN.R-project.org/package=panstarrs) [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) [![CRAN status](https://www.r-pkg.org/badges/version/panstarrs)](https://CRAN.R-project.org/package=panstarrs)

<!-- badges: end -->

# panstarrs

The goal of panstarrs package is to access [PanSTARRS](https://outerspace.stsci.edu/display/PANSTARRS/) data archive.

## Installation

You can install the released version of panstarrs with:

``` r
# devtools::install_github('uskovgs/PanSTARRS')
install.packages("panstarrs")
```

## Example

``` r
 library(panstarrs)
 library(magrittr)
 library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
 coords <- ps1_mast_resolve('Antennae')
 
 ps1_image_color(ra = coords$ra, 
                 dec = coords$decl, 
                 format = 'png', 
                 size = 400) %>%
   magick::image_read()
```

![](https://i.imgur.com/Yb7jJiG.png)

``` r
 fits_image_url <- ps1_image_url(ra = coords$ra,
                                 dec = coords$decl,
                                 size = 1280,
                                 filter = "r",
                                 format = "fits")
 
 # fits_image <- FITSio::readFITS(fits_image_url)
 
 
 
 ps1_crossmatch(ra = c(268.70342, 168.87258), 
                dec = c(71.54292, 60.75153),
                release = 'dr2',
                table = 'mean') %>% 
   select(MatchRA, MatchDEC, dstArcSec, `_searchID_`, objName) %>% 
   arrange(`_searchID_`, dstArcSec)
#> # A tibble: 8 × 5
#>   MatchRA MatchDEC dstArcSec `_searchID_` objName              
#>     <dbl>    <dbl>     <dbl>        <int> <chr>                
#> 1    269.     71.5     1.25             0 PSO J268.7044+71.5430
#> 2    269.     71.5     1.72             0 PSO J268.7033+71.5434
#> 3    269.     71.5     1.83             0 PSO J268.7023+71.5425
#> 4    269.     71.5     2.23             0 PSO J268.7053+71.5428
#> 5    269.     71.5     2.91             0 PSO J268.7053+71.5424
#> 6    269.     71.5     2.97             0 PSO J268.7059+71.5431
#> 7    169.     60.8     0.470            1 PSO J168.8729+60.7515
#> 8    169.     60.8     1.61             1 PSO J168.8733+60.7512
```
