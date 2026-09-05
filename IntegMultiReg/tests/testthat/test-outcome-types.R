test_that("all three outcome types fit without error and return valid output", {
  for (type in c("binary", "continuous", "right.censored")) {
    fit <- fit_demo(type, total = 200, burn = 100, seed = 3)
    expect_s3_class(fit, "imr")
    expect_identical(fit$type_outcome, type)
    expect_true(all(is.finite(fit$log_posterior)))
    expect_true(all(vapply(coef(fit), function(m) all(m >= 0 & m <= 1), logical(1))))
  }
})

test_that("the BMS (non-integrative) method also runs", {
  fit <- imr(
    simIMR$platforms, simIMR$outcome.binary, cov = simIMR$covariates,
    type_outcome = "binary", method = "BMS",
    nu = c(-4, -3, -4), sample_mcmc = c(150, 75), ssize = 30, seed = 1)
  expect_s3_class(fit, "imr")
  expect_identical(fit$method, "BMS")
})

test_that("the model runs without clinical covariates", {
  fit <- imr(
    simIMR$platforms, simIMR$outcome.binary, cov = NULL,
    type_outcome = "binary",
    nu = c(-4, -3, -4), sample_mcmc = c(150, 75), ssize = 30, seed = 1)
  expect_s3_class(fit, "imr")
  expect_equal(fit$data1[[7]], 0)  # n_cov == 0
})

test_that("integer-valued outcomes are handled (coerced to double)", {
  oc <- simIMR$outcome.binary
  oc$y <- as.integer(oc$y)
  expect_silent(
    suppressWarnings(imr(
      simIMR$platforms, oc, cov = simIMR$covariates, type_outcome = "binary",
      nu = c(-4, -3, -4), sample_mcmc = c(120, 60), ssize = 30, seed = 1))
  )
})
