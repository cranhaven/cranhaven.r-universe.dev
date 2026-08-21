# Extracted from test-data-processing.R:458

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "FAfA", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_if_not_installed("qgraph")
set.seed(19)
source_data <- as.data.frame(replicate(4, sample(1:5, 100, replace = TRUE)))
result <- FAfA:::factor_ret(
    source_data,
    method = "pa_lubbe",
    n.iter = 8,
    quant = 0.90,
    seed = 44
  )
