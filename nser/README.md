
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/nser)](https://cran.r-project.org/package=nser)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable-1)
[![Github Code Size](https://img.shields.io/github/languages/code-size/nandp1/gpbStat.svg)](https://github.com/nandp1/nser)
![Downloads](http://cranlogs.r-pkg.org/badges/nser)
[![](https://cranlogs.r-pkg.org/badges/grand-total/nser)](https://cran.r-project.org/package=nser)

<!-- badges: end -->

# Latest Version `1.5.2`

## NOTE 
`nselive`, `nseopen` `nseindex`, `nsetree`, `optbanknifty` and `optnifty` obtain data from NSE using python scripts. Thus, you need to install Python modules `pandas` and `requests` in R, 

Following steps should help you to get started, 
* Install latest version of `Python`, `Ananconda` and `Miniconda`. And add Python as PATH variable. 
* Then in R, 
``` r
install.packages('reticulate')  # Install package reticulate
library(reticulate) # Load package
# You can also install miniconda in R by,
# install_miniconda()
py_install("requests") # Install python package requests
py_install("pandas") # Install python package pandas
# Helpgul tip: you can also add interpreter in "Tools/Global options/Python/miniconda". 
```
You should be good to go now...

# Introduction

`nser` helps you to download historical bhavcopy of Equities and F&O
segment easily.

Package website [nser](https://nandp1.github.io/nser/)

## Installation

You can install “nser” from
[CRAN](https://cran.r-project.org/package=nser) with:

``` r
install.packages("nser")
```

Install it from github by:

``` r
install.packages("devtools")
library(devtools)
install_github("nandp1/nser")
```

## Example 1. Downloading Historical Equity Bhavcopy

``` r
library(nser)
# Download Bhavcopy of 1st July 2021
report1 = bhav("01072021")
```

## Example 2. Downloading Historical F&O Bhavcopy

``` r
library(nser)
# Download Bhavcopy of 1st July 2021
report2 = fobhav("01072021")
```

## Example 3. Downloading today’s Equity and F&O Bhavcopy

``` r
library(nser)
report3 = bhavtoday()
report4 = fobhavtoday()
```

## Example 4. Live F&O data.

``` r
library(nser)
nselive()
```

## Example 5. Pre market open data of F&O stocks

``` r
library(nser)
nseopen("fo")
```

## Example 6. Current and Upcoming IPO’s

library(nser)
nseipo()


## Example 7. NSE Treemap

``` r
library(nser)
# NIFTY 50 stocks
nsetree()

# F&O stocks
nsetree("fo")
```

## Example 8. Daily data to Weelkly data

    library(nser)
    data(dailydata)
    daytoweek(dailydata)

## Example 9. Daily data to Monthly data

    library(nser)
    data(dailydata)
    daytomonth(dailydata)

## Example 10. Option chain Nifty 50

    library(nser)
    optnifty()

## Example 11. Option chain Banknifty

    library(nser)
    optbanknifty()
    
