# Extracted from test-data-processing.R:429

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "FAfA", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_if_not_installed("qgraph")
set.seed(71)
latent_1 <- stats::rnorm(120)
latent_2 <- stats::rnorm(120)
to_ordinal <- function(values) {
    as.integer(cut(
      values,
      breaks = stats::quantile(values, probs = seq(0, 1, length.out = 6)),
      include.lowest = TRUE
    ))
  }
source_data <- data.frame(
    item_1 = to_ordinal(latent_1 + stats::rnorm(120, sd = 0.35)),
    item_2 = to_ordinal(latent_1 + stats::rnorm(120, sd = 0.35)),
    item_3 = to_ordinal(latent_2 + stats::rnorm(120, sd = 0.35)),
    item_4 = to_ordinal(latent_2 + stats::rnorm(120, sd = 0.35))
  )
source_data$item_1 <- ordered(source_data$item_1)
source_data$item_2[c(4, 19)] <- NA_integer_
rng_before <- .Random.seed
first <- FAfA:::lubbe_parallel_analysis(
    source_data,
    fa = "pc",
    n.iter = 12,
    quant = 0.95,
    seed = 902
  )
