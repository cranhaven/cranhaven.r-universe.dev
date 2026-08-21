# Extracted from test-data-processing.R:527

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "FAfA", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
dictionary <- list(
    F1 = c("m1", "m2", "m3"),
    F2 = c("m4", "m5", "m6")
  )
first_order <- paste(vapply(names(dictionary), function(factor) {
    paste0(factor, " =~ ", paste(dictionary[[factor]], collapse = " + "))
  }, character(1)), collapse = "\n")
result <- FAfA:::build_bifactor_syntax(
    first_order,
    dictionary,
    "G",
    c("F1", "F2"),
    orthogonal = TRUE
  )
