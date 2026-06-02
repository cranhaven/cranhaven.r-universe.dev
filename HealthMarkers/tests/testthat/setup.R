# tests/testthat/setup.R

# Load once, before any tests run, and silence any "built under R version" notes
suppressWarnings({
  suppressPackageStartupMessages({
    library(testthat)
    library(tibble)
    library(dplyr)
    library(withr)
  })
})
