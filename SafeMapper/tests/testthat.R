if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(SafeMapper)
  test_check("SafeMapper")
} else {
  message("testthat not available, skipping tests")
}
