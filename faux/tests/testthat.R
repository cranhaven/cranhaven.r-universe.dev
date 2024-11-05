library(testthat)
library(faux)

expect_equal <- function(...) {
  testthat::expect_equal(..., check.environment=FALSE)
}

test_check("faux")
