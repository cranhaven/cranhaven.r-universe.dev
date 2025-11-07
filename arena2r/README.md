---
output: github_document
---
<!-- rmarkdown v1 -->
<!-- README.md is generated from README.Rmd. Please edit that file -->



# Arena2R

[![Travis-CI Build Status](http://travis-ci.org/pedroliman/arena2r.svg?branch=master)](https://travis-ci.org/pedroliman/arena2r) [![Coverage Status](https://img.shields.io/codecov/c/github/pedroliman/arena2r/master.svg)](https://codecov.io/github/pedroliman/arena2r?branch=master)

The goal of arena2r is to facilitate the analysis of Arena Simulation Software output in R.

This package will be usefull to you, if:

a) You use Arena Simulation;
b) You have seen yourself struggling to summarise Arena simulation results by hand at excel;
c) You want to get your Arena Simulation Output directly to R and have a data.frame with all your results to run your analyses there seamlessly.

I have seen myself in this position many times and resolved to put together a package and stop doing repetitive work.


## Usage

You can use arena2r online with the demo shiny app at:

https://pedrolima.shinyapps.io/arena2r/

Alternatively, you can install it in your R envinronment.

## Installation

You can install arena2r from github in R with:


```r
# install.packages("devtools")
devtools::install_github("pedroliman/arena2r")
```

## Use the App locally:

After installing, load the library and run the demo app.


```r
library(arena2r)

runArenaApp()

```

## Tutorial

Please follow the [instructions on this tutorial](https://www.pedronl.com/post/arena2r-package-tutorial/) to use Arena2r.


## Wishlist

The following functions aren't available in the package, but are desirable. If you're willing to contribute with this package, please considering working on these features:

- The package won't run different simulations for you based on a Design of Experiments;
- There is no guarantee that the package will acomodate future changes in the csv format exported by Arena. The package is tested with the Arena 14.00 version;
- The package only uses the standard csv pattern (If you are Brazillian like me, it's easier to change your regional configurations).

Pull requests are welcome.
