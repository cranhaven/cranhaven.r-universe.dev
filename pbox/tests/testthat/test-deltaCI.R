
library(testthat)

# Sample data for testing
cond_example <- list(
  c(P = 0.3597117, `2.5%` = 0.3074215, `97.5%` = 0.4075315),
  c(P = 0.5682882, `2.5%` = 0.4560553, `97.5%` = 0.6823438)
)


# Test correct functionality
test_that("Test correct computation", {
  result <- deltaCI(cond_example)

  # Check if the result is a numeric vector with correct names
  expect_type(result, "double")
  expect_length(result, 3)
  expect_named(result, c("P", "2.5%", "97.5%"))

  # Verify calculation correctness
  # Manually compute expected values and compare
  p <- c(0.3597117, 0.5682882)
  se <- c((0.4075315 - 0.3074215) / (2 * 1.96), (0.6823438 - 0.4560553) / (2 * 1.96))
  r_hat_expected <- p[1] / p[2]
  var_r_expected <- (1/p[2])^2 * se[1]^2 + (-p[1]/p[2]^2)^2 * se[2]^2
  z <- 1.96
  ci_expected <- c(r_hat_expected - z * sqrt(var_r_expected), r_hat_expected + z * sqrt(var_r_expected))

  expect_equal(as.vector(result["P"]), r_hat_expected, tolerance = 1e-5)
  expect_equal(as.vector(result["2.5%"]), ci_expected[1], tolerance = 1e-5)
  expect_equal(as.vector(result["97.5%"]), ci_expected[2], tolerance = 1e-5)
})

# Test with incorrect input
test_that("Test with incorrect input", {
  wrong_cond <- list(c(P = "not_a_number", `2.5%` = "low", `97.5%` = "high"))
  expect_error(deltaCI(wrong_cond))
})

# Test with incomplete input
test_that("Test with incomplete input", {
  incomplete_cond <- list(c(P = 0.3597117), c(P = 0.5682882, `2.5%` = 0.4560553))
  expect_error(deltaCI(incomplete_cond))
})

# Test with edge case inputs (extreme values)
test_that("Test with extreme values", {
  extreme_cond <- list(
    c(P = 1e-10, `2.5%` = 1e-10 - 1e-11, `97.5%` = 1e-10 + 1e-11),
    c(P = 1, `2.5%` = 0.999999, `97.5%` = 1.000001)
  )
  result_extreme <- deltaCI(extreme_cond)
  expect_type(result_extreme, "double")
  expect_length(result_extreme, 3)
})

