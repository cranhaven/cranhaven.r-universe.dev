# epiCleanr <img src="man/figures/logo.png" align="right" height="128" style="float:right; height:110px;"/>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/epiCleanr)](https://cran.r-project.org/package=epiCleanr)
[![R build status](https://github.com/truenomad/epicleanr/workflows/R-CMD-check/badge.svg)](https://github.com/truenomad/epicleanr/actions)
[![CodeFactor](https://www.codefactor.io/repository/github/truenomad/epicleanr/badge)](https://www.codefactor.io/repository/github/truenomad/epicleanr)
[![codecov](https://codecov.io/gh/truenomad/epiCleanr/graph/badge.svg?token=F0H9LLAXAX)](https://app.codecov.io/gh/truenomad/epiCleanr?branch=main)

## Description

This package offers a tidy solution for epidemiological data. It houses
a range of functions for epidemiologists and public health data wizards
for data management and cleaning. For more details on how to use this package, visit the 
[epiCleanr website](https://truenomad.github.io/epiCleanr/articles/using_epicleanr.html).

## Installation

The package is available on Cran and can be installed in the following
way:

``` r
install.packages("epiCleanr")
library("epiCleanr")
```

Or install the development version from GitHub:

``` r
# If you haven't installed the 'devtools' package, run:
# install.packages("devtools")
devtools::install_github("truenomad/epiCleanr")
```

Load the package:

``` r
library(epiCleanr)
```

## Quick Workflow Overview

`epiCleanr` could be used as a helper package for end-to-end epidemiological 
data management, offering functionalities ranging from data importation and 
quality assessment to cleaning and exporting files. Below are some of the 
workflow steps this package streamlines:

### Import Data

Utilise `import()` to seamlessly read data from a wide array of file
formats, from CSV to Excel to JSON, all within one function.

### Data Quality Checks

-   `consistency_check()`: Generate plots to identify inconsistencies,
    such as when the number of tests exceeds the number of cases.

-   `missing_plot()`: Visualize patterns of missing data or reporting
    rates across different variables and factors.

-   `create_test()`: Establish unit-testing functions to automate data
    validation, ensuring the robustness of your dataset.

### Data Cleaning

-   `clean_admin_names()`: Normalize administrative names in your
    dataset using either user-supplied data or downloaded reference data
    via `get_admin_names()`.

-   `cleaning_names_strings()`: Use this function to clean and
    standardize string columns in your data.

-   `handle_outliers()`: Detect and manage outliers using a variety of
    statistical methods, providing you with options to either remove or
    impute them.

### Data Export

Finally, use `export()` to save your cleaned data back into multiple
file formats, be it CSV, Excel, or other specialized formats.
