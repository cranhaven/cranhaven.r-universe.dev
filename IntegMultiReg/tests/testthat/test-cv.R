test_that("cv_imr returns labelled accuracy matrices", {
  cv <- cv_imr(fit_bin, k = 5, rounds = 2)
  expect_named(cv, c("total_cindex", "subset_cindex"))
  expect_equal(ncol(cv$total_cindex), length(fit_bin$model_bitstrings) + 1L)
  expect_equal(nrow(cv$total_cindex), 2L)
  expect_equal(tail(colnames(cv$total_cindex), 1), "all")
  expect_identical(attr(cv, "metric"), "AUC")
  expect_true(all(is.finite(cv$total_cindex)))
})

test_that("the cross-validated metric reflects the outcome type", {
  fc <- fit_demo("continuous", total = 200, burn = 100, seed = 4)
  expect_identical(attr(cv_imr(fc, k = 5, rounds = 1), "metric"), "MSE")
  fs <- fit_demo("right.censored", total = 200, burn = 100, seed = 4)
  expect_identical(attr(cv_imr(fs, k = 5, rounds = 1), "metric"), "C-index")
})

test_that("survival cross-validated C-index beats chance", {
  fs <- fit_demo("right.censored", total = 800, burn = 300, seed = 8)
  cv <- cv_imr(fs, k = 5, rounds = 3)
  expect_gt(mean(cv$total_cindex[, "all"]), 0.6)
})

test_that("cv_imr validates cross-validation controls", {
  expect_error(cv_imr(fit_bin, k = 1), "`k`")
  expect_error(cv_imr(fit_bin, k = max(fit_bin$sample_size) + 1), "sample size")
  expect_error(cv_imr(fit_bin, rounds = 0), "`rounds`")
  expect_error(cv_imr(fit_bin, max_models = 0), "`max_models`")
  expect_error(cv_imr(fit_bin, method = "lasso"), "should be one of")
  expect_error(cv_imr(fit_bin, method = "BMS"), "must match the fitted object")
})

test_that("cv_imr accepts an explicitly matching fitted method", {
  fit_bms <- imr(
    simIMR$platforms, simIMR$outcome.binary, cov = simIMR$covariates,
    type_outcome = "binary", method = "BMS", nu = c(-4, -3, -4),
    sample_mcmc = c(80, 40), ssize = 30, seed = 14
  )
  cv <- cv_imr(fit_bms, k = 5, rounds = 1, method = "BMS")
  expect_identical(attr(cv, "metric"), "AUC")
  expect_true(all(vapply(fit_bms$theta_sample, is.null, logical(1))))
})
