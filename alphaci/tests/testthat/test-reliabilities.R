test_that("bias term is correct", {
  set.seed(313)
  sigma <- runif(5)
  lambda <- runif(5)
  expect_equal(
    alpha_bias(sigma, lambda),
    omega(sigma, lambda) - alpha(sigma, lambda)
  )
  expect_equal(
    alpha_bias(sigma, lambda, w = sqrt(1 / (lambda^2 + sigma^2))),
    omega_std(sigma, lambda) - alpha_std(sigma, lambda)
  )
})
