library(testthat)
library(MDBED)

Sys.setenv("R_TESTS" = "")
test_check("MDBED")
