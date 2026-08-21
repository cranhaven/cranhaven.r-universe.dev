# Extracted from test-data-processing.R:476

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "FAfA", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
syntax <- FAfA:::set_lavaan_measurement("", "F1", c("m1", "m2"))
