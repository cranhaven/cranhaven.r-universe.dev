# Tests of fit-related functions
# Arseniy Khvorov
# Created 2019/10/16
# Last edit 2019/10/16

library(sclr)

mf_one <- model.frame(status ~ logHI, one_titre_data)
x_one <- model.matrix(mf_one, data = one_titre_data)
y_one <- model.response(mf_one)

test_that("sclr_fit can be used directly", {
  fit_one <- sclr_fit(y_one, x_one)
  expect_named(
    fit_one, 
    c("parameters", "covariance_mat", "algorithm", "algorithm_return")
  )
})

test_that("error with unknown algorithm", {
  expect_error(
    sclr_fit(y_one, x_one, algorithm = "unknown"),
    "`algorithm` should be in:"
  )
})

test_that("Parameter matrix initalisation and resetting works", {
  init_mat <- get_init_pars_mat(
    y_one, x_one, conventional_names = FALSE, seed = 1
  )
  init_mat2 <- get_init_pars_mat(
    y_one, x_one, conventional_names = FALSE, seed = 1
  )
  init_mat3 <- get_init_pars_mat(
    y_one, x_one, conventional_names = FALSE
  )
  expect_equal(init_mat, init_mat2)
  expect_true(all(init_mat2 != init_mat3))
  expect_true(all(guess_again(init_mat) != guess_again(init_mat)))
})

test_that("Algorithms work", {
  x_coeffs_one <- get_x_coeffs(x_one)
  pars_mat_one <- get_init_pars_mat(y_one, x_one, FALSE)
  nr <- newton_raphson(
    y_one, x_one, pars_mat_one, x_coeffs_one, 
    max_iter = 1e4, tol = 10^(-7), seed = 20191101
  )
  ga <- gradient_ascent(
    y_one, x_one, pars_mat_one, x_coeffs_one, 
    max_iter = 1e4, tol = 10^(-7), seed = 20191101
  )
  nms <- c("init_mat", "found", "cov", "last_iter")
  expect_named(nr, nms)
  expect_named(ga, nms)
})

test_that("Warning when doesn't converge", {
  ss <- sclr_ideal_data(n = 50, seed = 20191101)
  x_ss <- model.matrix(status ~ logHI, ss)
  y_ss <- model.response(model.frame(status ~ logHI, ss))
  x_coeffs_ss <- get_x_coeffs(x_ss)
  expect_warning(
    sclr_fit(
      y_ss, x_ss, nr_iter = 100, algorithm = "newton-raphson", n_conv = 3,
      seed = 20191101
    ),
    regexp = "newton-raphson only converged 1 time\\(s\\) out of 3"
  )
  expect_warning(
    sclr_fit(
      y_ss, x_ss, nr_iter = 5, algorithm = "newton-raphson", n_conv = 3,
      seed = 20191101
    ),
    regexp = paste0(
      "newton-raphson did not converge,", 
      " check for boundary with check_baseline"
    )
  )
})

test_that("fallback works", {
  l1 <- sclr_ideal_data(theta = 1e6, n = 50, seed = 20191102)
  x_l1 <- model.matrix(status ~ logHI, l1)
  y_l1 <- model.response(model.frame(status ~ logHI, l1))
  fit_ga <- suppressWarnings(sclr_fit(
    y_l1, x_l1, algorithm = c("newton-raphson", "gradient-ascent"), n_conv = 3,
    seed = 20191101
  ))
  expect_named(
    fit_ga, 
    c("parameters", "covariance_mat", "algorithm", "algorithm_return")
  )
  expect_equal(fit_ga$algorithm, "gradient-ascent")
  expect_true(!is.null(fit_ga$parameters))
  expect_true(!is.null(fit_ga$covariance_mat))
})



