context("mdm_ica")

# setup
set.seed(1)
num_obs <- 10
num_comp <- 2
X <- matrix(rnorm(num_obs * num_comp), num_obs, num_comp)

test_that("asym vs. sym", {
  set.seed(1)
  m1 <- EDMeasure::mdm_ica(X, type = "asym", algo = "par")
  set.seed(1)
  m2 <- EDMeasure::mdm_ica(X, type = "sym", algo = "par")

  expect_equal(m1$theta, m2$theta, tolerance = 3e-7)
  expect_equal(m1$W, m2$W, tolerance = 4e-7)
  expect_equal(m1$obj, m2$obj / 2, tolerance = 3e-7)
  expect_equal(m1$S, m2$S, tolerance = 4e-7)
})







