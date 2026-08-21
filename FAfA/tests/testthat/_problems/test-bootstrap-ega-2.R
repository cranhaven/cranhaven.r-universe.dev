# Extracted from test-bootstrap-ega.R:2

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "FAfA", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
settings <- FAfA:::validate_bootega_settings(500, 2, 2026)
