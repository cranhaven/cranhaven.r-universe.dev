Sys.setenv("R_TESTS" = "")
library(testthat)
library(slasso)

test_check("slasso")
