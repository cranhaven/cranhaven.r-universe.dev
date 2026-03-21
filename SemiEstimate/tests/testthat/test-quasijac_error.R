test_that("quansijac error", {
  # test the basic function of numDerv and use the iteration erro to determine the error
  # wrong input for the target equation function
  Phi_fn <- function(theta, lambda, alpha) 2 * theta + alpha * lambda
  Psi_fn <- function(theta, lambda, alpha) 2 * lambda + alpha * theta
  fake_fn1 <- function(theta) 1
  fake_fn2 <- function(x, y) 1
  res <- try(semislv(1, 1, Phi_fn, Psi_fn, alpha = 1))
  expect_equal(class(res), "savespace")
  expect_error(semislv(1, 1, fake_fn1, Psi_fn, alpha = 1))
  expect_error(semislv(1, 1, fake_fn2, Psi_fn, alpha = 1))
  # wrong input for the derivate function
  res <- try(semislv(1, 1, Phi_fn, Psi_fn, jac = list(
    Phi_der_theta_fn = function(theta, lambda, alpha) 2,
    Phi_der_lambda_fn = function(theta, lambda, alpha) alpha,
    Psi_der_theta_fn = function(theta, lambda, alpha) alpha,
    Psi_der_lambda_fn = function(theta, lambda, alpha) 2
  ), method = "implicit", alpha = 1))
  expect_equal(class(res), "savespace")
  res <- try(semislv(theta = 1, lambda = 1, Phi_fn = Phi_fn, Psi_fn = Psi_fn, alpha = 1, jac = list(
    Phi_der_theta_fn = function(theta, lambda, alpha) 2
  ), method = "implicit"))
  expect_equal(class(res), "savespace")
  expect_error(semislv(1, 1, Phi_fn, Psi_fn, jac = list(
    Phi_der_theta_fn = function(x, lambda, alpha) 2,
  ), method = "implicit", alpha = 1))
  # expect nearby result for numerical Jacobian and mathematical Jacobian
  ip1 <- semislv(1, 1, Phi_fn, Psi_fn, alpha = 1)
  ip2 <- semislv(1, 1, Phi_fn, Psi_fn, jac = list(
    Phi_der_theta_fn = function(theta, lambda, alpha) 2,
    Phi_der_lambda_fn = function(theta, lambda, alpha) alpha,
    Psi_der_theta_fn = function(theta, lambda, alpha) alpha,
    Psi_der_lambda_fn = function(theta, lambda, alpha) 2
  ), alpha = 1)
  expect_equal(ip1$theta, ip2$theta)
  expect_equal(ip1$lambda, ip2$lambda)
  it1 <- semislv(1, 1, Phi_fn, Psi_fn, method = "iterative", alpha = 1)
  it2 <- semislv(1, 1, Phi_fn, Psi_fn, method = "iterative", jac = list(
    Phi_der_theta_fn = function(theta, lambda, alpha) 2,
    Phi_der_lambda_fn = function(theta, lambda, alpha) alpha,
    Psi_der_theta_fn = function(theta, lambda, alpha) alpha,
    Psi_der_lambda_fn = function(theta, lambda, alpha) 2
  ), alpha = 1)
  expect_equal(it1$theta, it2$theta)
  expect_equal(it1$lambda, it2$lambda)
})