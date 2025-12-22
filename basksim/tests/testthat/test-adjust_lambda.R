test_that("adjust_lambda default method works", {
  design <- setup_cpp(k = 3, p0 = 0.2)

  # Without simulated data
  set.seed(20230319)
  res1 <- adjust_lambda(design = design, n = 15, alpha = 0.05,
    design_params = list(tune_a = 1, tune_b = 1), iter = 1000)
  toer1 <- toer(design, n = 15, lambda = res1$lambda, design_params =
      list(tune_a = 1, tune_b = 1), iter = 1000)

  expect_lte(res1$toer, 0.05)
  expect_true(abs(res1$toer - toer1) < 0.02)

  set.seed(123)
  res2 <- adjust_lambda(design = design, n = 12, alpha = 0.05,
    design_params = list(tune_a = 2, tune_b = 2), iter = 1000)
  toer2 <- toer(design, n = 12, lambda = res2$lambda, design_params =
      list(tune_a = 2, tune_b = 2), iter = 1000)

  expect_lte(res2$toer, 0.05)
  expect_true(abs(res2$toer - toer2) < 0.02)

  set.seed(456)
  res3 <- adjust_lambda(design = design, n = 13, alpha = 0.05,
    design_params = list(tune_a = 4, tune_b = 1.5), iter = 1000)
  toer3 <- toer(design, n = 13, lambda = res3$lambda, design_params =
      list(tune_a = 4, tune_b = 1.5), iter = 1000)

  expect_lte(res3$toer, 0.05)
  expect_true(abs(res3$toer - toer3) < 0.02)

  # With simulated data
  simdata <- get_data(k = 3, n = 15, p = 0.2, iter = 100)
  res4 <- adjust_lambda(design = design, n = 15, p1 = 0.2, alpha = 0.05,
    design_params = list(tune_a = 1, tune_b = 1), iter = 100, prec_digits = 3,
    data = simdata)
  res5 <- adjust_lambda(design = design, n = 15, p1 = 0.2, alpha = 0.05,
    design_params = list(tune_a = 1, tune_b = 1), iter = 100, prec_digits = 3,
    data = simdata)

  # Check if results are equal when adjust_lambda is called two times with
  # the same simulated data
  expect_equal(res4$lambda, res5$lambda)
  expect_equal(res4$toer, res5$toer)

  # Check if selected lambda controls the TOER
  toer4 <- toer(design, n = 15, p1 = 0.2, lambda = res4$lambda,
    design_params = list(tune_a = 1, tune_b = 1), iter = 100, data = simdata)
  toer5 <- toer(design, n = 15, p1 = 0.2, lambda = res4$lambda - 0.01,
    design_params = list(tune_a = 1, tune_b = 1), iter = 100, data = simdata)

  expect_equal(res4$toer, toer4)
  expect_gt(toer5, 0.05)
})

test_that("adjust_lambda works for exnex", {
  design <- setup_exnex(k = 3, p0 = 0.2)

  # Without simulated data
  set.seed(125)
  res1 <- adjust_lambda(design = design, n = 15,
    design_params = list(tau_scale = 1, w = 0.5), iter = 100, n_mcmc = 2500)

  set.seed(125)
  res2 <- toer(design = design, n = 15, lambda = res1$lambda,
    design_params = list(tau_scale = 1, w = 0.5), iter = 100, n_mcmc = 2500)

  expect_lte(res1$toer, 0.05)
  expect_equal(res1$toer, res2)

  skip_on_cran()
  # With simulated data
  set.seed(125)
  simdata <- get_data(k = 3, n = 15, p = 0.2, iter = 100, type = "bhmbasket")
  set.seed(126)
  res3 <- adjust_lambda(design = design, n = 15,
    design_params = list(tau_scale = 1, w = 0.5), iter = 100, n_mcmc = 2500,
    data = simdata)
  set.seed(126)
  res4 <- adjust_lambda(design = design, n = 15,
    design_params = list(tau_scale = 1, w = 0.5), iter = 100, n_mcmc = 2500,
    data = simdata)

  # Check if results are equal when adjust_lambda is called two times with
  # the same simulated data
  expect_equal(res3$lambda, res4$lambda)
  expect_equal(res3$toer, res4$toer)

  # Check if selected lambda controls the TOER
  set.seed(126)
  toer1 <- toer(design, n = 15, p1 = 0.2, lambda = res3$lambda,
    design_params = list(tau_scale = 1, w = 0.5), data = simdata, iter = 100,
    n_mcmc = 2500)
  toer2 <- toer(design, n = 15, p1 = 0.2, lambda = res3$lambda - 0.01,
    design_params = list(tau_scale = 1, w = 0.5), data = simdata, iter = 100,
    n_mcmc = 2500)

  expect_equal(toer1, 0.05)
  expect_gt(toer2, 0.05)
})

test_that("adjust_lambda works for bhm", {
  design <- setup_bhm(k = 3, p0 = 0.2, p_target = 0.5)

  # Without simulated data
  set.seed(125)
  res1 <- adjust_lambda(design = design, n = 15,
    design_params = list(tau_scale = 1), iter = 100, n_mcmc = 2500)

  set.seed(125)
  res2 <- toer(design = design, n = 15, lambda = res1$lambda,
    design_params = list(tau_scale = 1), iter = 100, n_mcmc = 2500)

  expect_lte(res1$toer, 0.05)
  expect_equal(res1$toer, res2)

  skip_on_cran()
  # With simulated data
  set.seed(125)
  simdata <- get_data(k = 3, n = 15, p = 0.2, iter = 100, type = "bhmbasket")
  set.seed(126)
  res3 <- adjust_lambda(design = design, n = 15,
    design_params = list(tau_scale = 1), iter = 100, n_mcmc = 2500,
    data = simdata)
  set.seed(126)
  res4 <- adjust_lambda(design = design, n = 15,
    design_params = list(tau_scale = 1), iter = 100, n_mcmc = 2500,
    data = simdata)

  # Check if results are equal when adjust_lambda is called two times with
  # the same simulated data
  expect_equal(res3$lambda, res4$lambda)
  expect_equal(res3$toer, res4$toer)

  # Check if selected lambda controls the TOER
  set.seed(126)
  toer1 <- toer(design, n = 15, p1 = 0.2, lambda = res1$lambda,
    design_params = list(tau_scale = 1), data = simdata, iter = 100,
    n_mcmc = 2500)
  toer2 <- toer(design, n = 15, p1 = 0.2, lambda = res1$lambda - 0.01,
    design_params = list(tau_scale = 1), data = simdata, iter = 100,
    n_mcmc = 2500)

  expect_equal(toer1, 0.05)
  expect_gt(toer2, 0.05)
})
