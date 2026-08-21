# Extracted from test-bootstrap-ega.R:56

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "FAfA", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
html <- paste(as.character(FAfA:::ega_ui("ega")), collapse = "")
expect_match(html, "Bootstrap Exploratory Graph Analysis (bootEGA)", fixed = TRUE)
