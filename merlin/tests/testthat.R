# Required packages for testing
suppressMessages({
  library(testthat)
  library(merlin)
  library(nlme)
  library(survival)
})

# Run tests
testthat::test_check(package = "merlin")
