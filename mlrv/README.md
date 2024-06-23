
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mlrv

<!-- badges: start -->

[![R-CMD-check](https://github.com/Lujia-Bai/mlrv/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Lujia-Bai/mlrv/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/mlrv)](https://CRAN.R-project.org/package=mlrv)
<!-- badges: end -->

The goal of mlrv is to provide plug-in and debiased difference-based
long-run covariance matrix estimation for time series regression. Two
applications of hypothesis testing are also provided. The first one is
for testing for structural stability in coefficient functions. The
second one is aimed at detecting long memory in time series regression.

## Installation

You can install the development version of mlrv from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Lujia-Bai/mlrv")
```

## Example

This is a basic example which shows you how to solve a common problem:
test if the coefficient function of “SO2”,“NO2”,“Dust” of the second
year is constant using debiased difference-based estimator for the
long-run covariance matrix function.

``` r
library(mlrv)
library(foreach)
library(magrittr)


data(hk_data)
colnames(hk_data) = c("SO2","NO2","Dust","Ozone","Temperature",
                      "Humidity","num_circu","num_respir","Hospital Admission",
                      "w1","w2","w3","w4","w5","w6")
n = nrow(hk_data)
t = (1:n)/n
hk = list()
setting = list(B = 5000, gcv = 1, neighbour = 1)
setting$lb = floor(20/7*n^(4/15)) - setting$neighbour 
setting$ub = max(floor(24/7*n^(4/15))+ setting$neighbour,             
                  setting$lb+2*setting$neighbour+1)
## basic example code
```

``` r
hk$x = as.matrix(cbind(rep(1,n), (hk_data[,1:3])))
hk$y = hk_data$`Hospital Admission`
setting$type = 0
setting$bw_set = c(0.1, 0.35)
setting$eta = 0.2
setting$lrvmethod = 1  #using debiased difference-based estimator
setting$lb  = 10
setting$ub  = 50
hk1 = list()
hk1$x = hk$x[366:730,]
hk1$y = hk$y[366:730]
p1 <- heter_gradient(hk1, setting, mvselect = -2, verbose = T)
#> [1] "m 26 tau_n 0.374190823993618"
#> [1] 10464.35
#>        V1       
#>  Min.   : 2669  
#>  1st Qu.: 5462  
#>  Median : 6844  
#>  Mean   : 7288  
#>  3rd Qu.: 8721  
#>  Max.   :19621
p1
#> [1] 0.1092
```

One can also use another scheme of MV selection based on the volatility
of the estimator by setting mvselect = -1.

``` r
p1 <- heter_gradient(hk1, setting, mvselect = -1)
p1
#> [1] 0.0048
```
