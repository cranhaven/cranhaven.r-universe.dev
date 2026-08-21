# Extracted from test-data-processing.R:498

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "FAfA", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
first_order <- paste(
    "F1 =~ m1 + m2 + m3",
    "F2 =~ m4 + m5 + m6",
    sep = "\n"
  )
result <- FAfA:::build_second_order_syntax(
    first_order,
    "HO",
    c("F1", "F2")
  )
