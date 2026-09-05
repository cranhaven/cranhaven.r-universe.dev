test_that("outcome dimension is validated per outcome type", {
  # binary/continuous need exactly 2 columns (id + response)
  bad <- data.frame(id = simIMR$outcome.binary$id,
                    y = simIMR$outcome.binary$y, extra = 1)
  expect_error(
    imr(simIMR$platforms, bad,
      type_outcome = "binary", sample_mcmc = c(50, 25), ssize = 30),
    "id"
  )
  # right.censored needs exactly 3 columns
  expect_error(
    imr(simIMR$platforms, simIMR$outcome.binary,
      type_outcome = "right.censored", sample_mcmc = c(50, 25), ssize = 30),
    "right-censored"
  )
})

test_that("retained-draw and burn-in counts are validated", {
  # the number of retained draws must be positive
  expect_error(
    imr(simIMR$platforms, simIMR$outcome.binary,
      type_outcome = "binary", sample_mcmc = c(0, 25), ssize = 30),
    "retained draws"
  )
  # a retained count below the burn-in is allowed (sample_mcmc = retained + burn-in)
  expect_s3_class(
    imr(simIMR$platforms, simIMR$outcome.binary,
      type_outcome = "binary", sample_mcmc = c(60, 80), ssize = 30, seed = 1),
    "imr"
  )
})

test_that("an ssize that excludes every subgroup is an error", {
  expect_error(
    imr(simIMR$platforms, simIMR$outcome.binary,
      type_outcome = "binary", sample_mcmc = c(50, 25), ssize = 1e6),
    "No availability subgroup"
  )
})

test_that("type_outcome and method are matched against their choices", {
  expect_error(
    imr(simIMR$platforms, simIMR$outcome.binary,
      type_outcome = "poisson", sample_mcmc = c(50, 25)),
    "should be one of"
  )
  expect_error(
    imr(simIMR$platforms, simIMR$outcome.binary,
      type_outcome = "binary", method = "lasso", sample_mcmc = c(50, 25)),
    "should be one of"
  )
})

test_that("downstream functions reject non-imr input", {
  expect_error(cv_imr(list(1)), "imr")
  expect_error(IntegMultiReg:::predict.imr(list(1), newdata = list()), "imr")
})

test_that("input data frames have standard id and numeric-column validation", {
  no_id <- simIMR$platforms
  names(no_id[[1]])[1] <- "sample_id"
  expect_error(
    imr(no_id, simIMR$outcome.binary, cov = simIMR$covariates,
        type_outcome = "binary", sample_mcmc = c(50, 25), ssize = 30),
    "id.*first column"
  )

  dup_id <- simIMR$platforms
  dup_id[[1]]$id[2] <- dup_id[[1]]$id[1]
  expect_error(
    imr(dup_id, simIMR$outcome.binary, cov = simIMR$covariates,
        type_outcome = "binary", sample_mcmc = c(50, 25), ssize = 30),
    "unique subject identifiers"
  )

  non_numeric <- simIMR$platforms
  non_numeric[[1]]$G01 <- as.character(non_numeric[[1]]$G01)
  expect_error(
    imr(non_numeric, simIMR$outcome.binary, cov = simIMR$covariates,
        type_outcome = "binary", sample_mcmc = c(50, 25), ssize = 30),
    "must be numeric"
  )

  bad_binary <- simIMR$outcome.binary
  bad_binary$y[1] <- 2
  expect_error(
    imr(simIMR$platforms, bad_binary, cov = simIMR$covariates,
        type_outcome = "binary", sample_mcmc = c(50, 25), ssize = 30),
    "0/1"
  )

  bad_survival <- simIMR$outcome.survival
  bad_survival$time[1] <- 0
  expect_error(
    imr(simIMR$platforms, bad_survival, cov = simIMR$covariates,
        type_outcome = "right.censored", sample_mcmc = c(50, 25), ssize = 30),
    "positive"
  )
})

test_that("scalar and hyper-parameter arguments are validated before sampling", {
  expect_error(
    imr(simIMR$platforms, simIMR$outcome.binary,
        type_outcome = "binary", nu = c(-3, -3), sample_mcmc = c(50, 25)),
    "`nu`"
  )
  expect_error(
    imr(simIMR$platforms, simIMR$outcome.binary,
        type_outcome = "binary", hh = -1, sample_mcmc = c(50, 25)),
    "`hh`"
  )
  expect_error(
    imr(simIMR$platforms, simIMR$outcome.binary,
        type_outcome = "binary", sample_mcmc = c(50.5, 25)),
    "`sample_mcmc`"
  )
  expect_error(
    imr(simIMR$platforms, simIMR$outcome.binary,
        type_outcome = "binary", ssize = -1, sample_mcmc = c(50, 25)),
    "`ssize`"
  )
  expect_error(
    imr(simIMR$platforms, simIMR$outcome.binary,
        type_outcome = "binary", verbose = NA, sample_mcmc = c(50, 25)),
    "`verbose`"
  )
})
