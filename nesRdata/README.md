
<!-- README.md is generated from README.Rmd. Please edit that file -->

# National Eutrophication Survey Data Package

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/nesRdata)](https://cran.r-project.org/package=nesRdata)

## Installation

``` r
# install stable version from CRAN
install.packages("nesRdata")

# install development version from Github
# install devtools if not found - install.packages("devtools")
# devtools::install_github("jsta/nesRdata", update_dependencies = TRUE)
```

## Usage

``` r
library(nesRdata)
```

### View static compilation of all lakes

``` r
data(nes)
head(nes)
#>   pdf pagenum storet_code     state           name         county lake_type
#> 1 474     100        2709 MINNESOTA      BIG STONE BIG STONE (MN)   NATURAL
#> 2 474     101        2710 MINNESOTA     BIRCH LAKE           CASS   NATURAL
#> 3 474     102        2711 MINNESOTA BLACKDUCK LAKE       BELTRAMI   NATURAL
#> 4 474     103        2712 MINNESOTA BLACKHOOF LAKE      CROW WING   NATURAL
#> 5 474     104        2713 MINNESOTA   BUFFALO LAKE         WRIGHT   NATURAL
#> 6 474     105        2714 MINNESOTA  CARRIGAN LAKE         WRIGHT   NATURAL
#>   drainage_area surface_area mean_depth total_inflow retention_time
#> 1       3004.40        51.03        3.4          3.3            1.7
#> 2            NA         5.19        3.0           NA             NA
#> 3         75.11        11.10        4.5          0.4            4.2
#> 4         20.72         0.74        4.4          0.1          257.0
#> 5        113.96         6.11        4.4          0.6            1.4
#> 6            NA         0.66         NA           NA             NA
#>   retention_time_units alkalinity conductivity secchi    tp   po4   tin tn
#> 1                years        131          800    1.0 0.159 0.126 0.335 NA
#> 2                years        106          210    2.4 0.019 0.009 0.090 NA
#> 3                years        128          243    1.7 0.038 0.019 0.195 NA
#> 4                 days        103          230    1.6 0.043 0.024 0.185 NA
#> 5                years        167          395    1.0 0.209 0.160 0.795 NA
#> 6                years        185          590    0.3 1.215 0.785 0.290 NA
#>   p_pnt_source_muni p_pnt_source_industrial p_pnt_source_septic p_nonpnt_source
#> 1              7696                      23                 213            8014
#> 2                NA                      NA                  NA              NA
#> 3               676                      NA                  NA             916
#> 4               639                      NA                   9             254
#> 5              4821                      NA                  23            1129
#> 6                NA                      NA                  NA              NA
#>   p_total n_pnt_source_muni n_pnt_source_industrial n_pnt_source_septic
#> 1   15946             22599                      NA                8036
#> 2      NA                NA                      NA                  NA
#> 3    1592              2023                      NA                  NA
#> 4     902              1914                      NA                 268
#> 5    5973             10086                      NA                 853
#> 6      NA                NA                      NA                  NA
#>   n_nonpnt_source n_total p_total_out p_percent_retention
#> 1          188812  219447       15846                   1
#> 2              NA      NA          NA                  NA
#> 3           27329   29351         717                  55
#> 4            6522    8703         141                  84
#> 5           18000   28939        3950                  34
#> 6              NA      NA          NA                  NA
#>   p_surface_area_loading n_total_out n_percent_retention n_surface_area_loading
#> 1                   0.31      214068                   2                    4.3
#> 2                     NA          NA                  NA                     NA
#> 3                   0.14       16091                  45                    2.6
#> 4                   1.22        5075                  42                   11.8
#> 5                   0.98       51429                  NA                    4.7
#> 6                     NA          NA                  NA                     NA
#>        lat      long  chl
#> 1 45.30833 -96.45667 16.5
#> 2 46.93333 -94.52500  6.2
#> 3 47.73889 -94.62917 14.5
#> 4 46.47167 -94.01056 12.8
#> 5 45.17083 -93.88000 38.0
#> 6 45.05833 -93.95833 84.3
```

### Download dynamic external data and cache in file system

``` r
nes_get(version_id = "5", dest_folder = cache_path())
```

#### List cached (non-temporary) versions

``` r
nes_versions()
```

#### Load data

``` r
dt <- nes_load(version_id = "5", folder = cache_path())
names(dt)
lapply(dt, head)
```

## Metadata

`help.search("^nes$", package = "nesRdata")`

## Contributing

We’ve tried to fix any transciption errors from the original data files
but it’s difficult to catch them all. If you find any errors please open
an issue or submit a pull request against the files at
<https://github.com/ReproducibleQM/NES>

## References

Stachelek, J., Ford, C., Kincaid, D., King, K., Miller, H. and
Nagelkirk, R., 2018. The National Eutrophication Survey: lake
characteristics and historical nutrient concentrations. Earth System
Science Data, 10(1), pp.81-86.
