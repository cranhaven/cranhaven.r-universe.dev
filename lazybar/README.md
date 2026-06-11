
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lazybar

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/FinYang/lazybar.svg?branch=master)](https://travis-ci.org/FinYang/lazybar)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/lazybar)](https://cran.r-project.org/package=lazybar)
[![Monthly\_Downloads](http://cranlogs.r-pkg.org/badges/lazybar)](https://cran.r-project.org/package=lazybar)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
<!-- badges: end -->

The R package *lazybar* provides progress bar showing estimated
remaining time. Multiple forecast methods and user defined forecast
method for the remaining time are supported.

## Installation

You can install the **development** version from
[Github](https://github.com/FinYang/lazybar) with:

``` r
# install.packages("devtools")
devtools::install_github("FinYang/lazybar")
```

## Usage

``` r
pb <- lazyProgressBar(4)
pb$tick()
pb$tick()
pb$tick()
pb$tick()

# With linearly increasing run time
pb <- lazyProgressBar(4, method = "drift")
for(i in 1:4){
  Sys.sleep(i * 0.2)
  pb$tick()$print()
}

# With user defined forecast function
# The forecast function itself will
# require certain computational power
forecast_fn <- function(dtime, i, n, s = 10){
  # When the number of ticks is smaller than s
  # Estimate the future run time
  # as the average of the past
  if(i<s){
    eta <- mean(dtime)*(n-i)
  }
  
  # When the number of ticks is larger than s
  # Fit an arima model every s ticks
  # using forecast package
  if(i>=s){
    if(i %% s ==0){
      model <- forecast::auto.arima(dtime)
    }
    runtime <- forecast::forecast(model, h=n-i)$mean
    if(i %% s !=0){
      runtime <- runtime[-seq_len(i %% s)]
    }
    eta <- sum(runtime)
  }
  return(eta)
}

pb <- lazyProgressBar(10, fn = forecast_fn, s=3)
for(i in 1:10){
  Sys.sleep(i * 0.2)
  pb$tick()$print()
}
```

## License

This package is free and open source software, licensed under GPL-3.
