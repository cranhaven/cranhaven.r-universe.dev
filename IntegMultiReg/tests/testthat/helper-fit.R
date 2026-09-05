## Shared fixtures for the test suite.  Small MCMC runs keep R CMD check fast
## while still exercising the full fit -> predict -> cv pipeline.

data("simIMR", package = "IntegMultiReg")

fit_demo <- function(type = c("binary", "continuous", "right.censored"),
                     total = 300, burn = 150, seed = 42) {
  type <- match.arg(type)
  outcome <- switch(type,
    binary = simIMR$outcome.binary,
    continuous = simIMR$outcome.continuous,
    right.censored = simIMR$outcome.survival
  )
  imr(
    platform_data_list = simIMR$platforms,
    outcome = outcome,
    cov = simIMR$covariates,
    type_outcome = type,
    nu = c(-4, -3, -4),
    sample_mcmc = c(total, burn),
    ssize = 30,
    seed = seed
  )
}

## A binary fit reused across several test files.
fit_bin <- fit_demo("binary", seed = 42)
