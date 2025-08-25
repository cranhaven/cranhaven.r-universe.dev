DSMolgenisArmadillo
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Build
Status](https://api.travis-ci.com/molgenis/molgenis-r-datashield.svg?branch=master)](https://app.travis-ci.com/github/molgenis/molgenis-r-datashield/)
[![CRAN
status](https://www.r-pkg.org/badges/version/DSMolgenisArmadillo)](https://CRAN.R-project.org/package=DSMolgenisArmadillo)
[![codecov](https://codecov.io/gh/molgenis/molgenis-r-datashield/branch/master/graph/badge.svg?token=b661qHT3BO)](https://app.codecov.io/gh/molgenis/molgenis-r-datashield)
<!-- badges: end -->

A DSI implementation for the [MOLGENIS Armadillo DataSHIELD
Service](https://github.com/molgenis/molgenis-service-armadillo/).

## Overview

You can use DSMolgenisArmadillo to analyse data shared in MOLGENIS
Armadillo servers using DataSHIELD. DataSHIELD allows execution of a
subset of analysis methods available in R. Methods such as:

`ds.mean()` `ds.glm()` `ds.lmerSLMA()`

For more detailed documentation check: <https://cran.datashield.org/>.

## Installation

You can install the released version of DSMolgenisArmadillo from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("DSI")
install.packages("DSMolgenisArmadillo")
```

Make sure you install the DataSHIELD client (`dsBaseClient`) to perform
the actual analysis. This needs to be a client which is version 6.0.0 or
higher.

``` r
# install the DataSHIELD client
install.packages("dsBaseClient", repos = c("http://cran.datashield.org", "https://cloud.r-project.org/"), dependencies = TRUE)
```

## Usage

To use the DataSHIELD Armadillo client and perform analysis in
DataSHIELD there a few basic steps you need to take.

### Get a token from the ID server

``` r
# Load the necessary packages.
library(dsBaseClient)
library(DSMolgenisArmadillo)

# specify server url
armadillo_url <- "https://armadillo.dev.molgenis.org"

# get token from central authentication server
token <- armadillo.get_token(armadillo_url)
```

### Build the login frame

You need to specify the project, the folder and the table name(s) you
want to access.

``` r
# build the login dataframe
builder <- DSI::newDSLoginBuilder()
builder$append(server = "armadillo",
               url = armadillo_url,
               token = token,
               table = "gecko/2_1-core-1_0/nonrep",
               driver = "ArmadilloDriver")

# create loginframe
logindata <- builder$build()
```

### Login and assign the data

Assigning the data means that you will assign the data to a symbol in
the analysis environment.

``` r
# login into server
conns <- datashield.login(logins = logindata, symbol = "core_nonrep", variables = c("coh_country"), assign = TRUE)
```

### Perform an analysis

DataSHIELD has a range of methods you can use to perform analysis.
Check: the
[dsBaseClient](https://cran.datashield.org/web/#client-packages)
documentation to see which methods are available.

``` r
# calculate the mean
ds.mean("core_nonrep$coh_country", datasources = conns)
```

``` r
# create a histogram
ds.histogram(x = "core_nonrep$coh_country", datasources = conns)
```

## Documentation

Check the [package
documentation](https://molgenis.github.io/molgenis-r-datashield/articles/DSMolgenisArmadillo.html)
for details.

## Armadillo 2

The newest version (2.x) of DSMolgenisArmadillo will be only compatible
with Armadillo version 3. If you still use Armadillo 2, you should use
the 1.4.1 version of DSMolgenisArmadillo You can install this specific
version the following commands:

``` r
packageurl <- "https://cran.r-project.org/src/contrib/Archive/DSMolgenisArmadillo/DSMolgenisArmadillo_1.4.1.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
```

If you already installed another version of the package, you might want to run this first:
``` r
remove.packages("MolgenisArmadillo")
```

On windows additional R packages like `rtools` might be required in order to install the `tar.gz` package from the url.
