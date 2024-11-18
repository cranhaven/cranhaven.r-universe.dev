# Work-around for bug (https://github.com/hadley/testthat/issues/144)
Sys.setenv("R_TESTS" = "")

# library 
library(testthat)
library(HTSSIP)

# test check
test_check('HTSSIP')
