
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dhlabR

<!-- badges: start -->
<!-- badges: end -->

The goal of dhlabR is to provide access to National Library text data
for quantitative analysis. It provides utility functions for requesting
data from the dhlab api (api.nb.no/dhlab)

## Installation

You can install the development version of dhlabR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("NationalLibraryOfNorway/dhlabR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dhlabR)
# Get corpus

corpus = get_document_corpus(doctype="digibok", "author" = "Ibsen", limit=100)
```
