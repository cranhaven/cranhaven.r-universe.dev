
<!-- README.md is generated from README.Rmd. Please edit that file -->
glancedata
==========

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/gbasulto/glancedata.svg?branch=master)](https://travis-ci.org/gbasulto/glancedata) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/gbasulto/glancedata?branch=master&svg=true)](https://ci.appveyor.com/project/gbasulto/glancedata) [![Codecov test coverage](https://codecov.io/gh/gbasulto/glancedata/branch/master/graph/badge.svg)](https://codecov.io/gh/gbasulto/glancedata?branch=master) [![CRAN status](https://www.r-pkg.org/badges/version/glancedata)](https://cran.r-project.org/package=glancedata) <!-- badges: end -->

Whenever I start working on a dataset, I need to take a glance at the data to see how the data are or it the format is the one that I am expecting. I found myself coding similar lines over and over again with each data set I put my hands on. I decided to put that lines together in an R package so I and others can use them. I called it **`glancedata`**.

There are already some cool R packages to summarize dataframes. The ones that I use more often are [`skimr`](https://github.com/ropensci/skimr) and [`GGally`](https://ggobi.github.io/ggally/).

Installation
------------

You can install the released version of glancedata from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("glancedata")
```

Functions
---------

These are the main functions in the package. You can see their documentation with `help`, e.g., `help("glance_data")`.

| Function                  | Description                                                                                                     |
|:--------------------------|:----------------------------------------------------------------------------------------------------------------|
| `glance_data`             | Alternative to `summary`. Emphasizes missing data and binary variables.                                         |
| `glance_data_in_workbook` | Similar to `glance_data`. Creates list of dataframes instead and saves XLSX file.                               |
| `plot_numerical_vars`     | Creates a plot per numerical variable. It might be histogram, density plot, qqplot, violin plot or scatterplot. |
| `plot_discrete_vars`      | Creates a plot per numerical variable. It might be histogram, density plot, qqplot, violin plot or scatterplot. |

Example
-------

You can copy and paste the code and use your own dataset. I am using the `mtcars` dataframe.

``` r

library(glancedata)

## This will generate a dataframe
glance_data(mtcars)

## Create a list of dataframes. If you provide the `filename` 
## parameter to be equal to, say, "myglance.xlsx", then it will create
## an Excel workbook and place the content of each dataframe in a 
## separate sheet.
glance_data_in_workbook(mtcars)

## These are the types of plots for numerical variables
plot_numerical_vars(mtcars, plot_type = "density")
plot_numerical_vars(mtcars, plot_type = "pairwise")
plot_numerical_vars(mtcars, plot_type = "histogram")
plot_numerical_vars(mtcars, plot_type = "violin")
plot_numerical_vars(mtcars, plot_type = "boxplot")
plot_numerical_vars(mtcars, plot_type = "qqplot")

## ... And here are barplots for variables with only a few different 
## values
plot_discrete_vars(mtcars)
plot_discrete_vars(mtcars, sort_by_frequency = TRUE)
```
