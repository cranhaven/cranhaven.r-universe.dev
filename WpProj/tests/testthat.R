library(testthat)
library(WpProj)

testthat::skip_if_not_installed("stats")
library(stats)

test_check("WpProj")
