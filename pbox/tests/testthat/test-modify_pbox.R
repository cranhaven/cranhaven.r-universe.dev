library(testthat)

# Mock the param_dev function for testing
param_dev <- function(param, sigma, range) {
  return(param + sample(range, 1) * sigma)  # Apply a random deviation for testing
}

# Sample data for testing
some_distr <- list(
  A = list(mu = 31.07, sigma = 0.28),
  B = list(mu = 34.4, sigma = 0.98, nu = 1.7),
  C = list(mu = 31.4, sigma = 0.34),
  D = list(mu = 25.6, sigma = 0.24)
)
params_list <- list(A = "mu", B = c("mu", "sigma"), C = "nu")

# Test correct functionality with typical input
test_that("Test correct functionality with typical input", {
  result <- modify_pbox(some_distr, params_list)

  # Check if the result is a list
  expect_type(result, "list")

  # Verify that the parameters are modified
  expect_false(identical(result$A$mu, some_distr$A$mu))
  expect_false(identical(result$B$mu, some_distr$B$mu))
  expect_false(identical(result$B$sigma, some_distr$B$sigma))
  expect_true(identical(result$C$mu, some_distr$C$mu))  # `nu` not in `some_distr`
})

# Test handling of missing parameters in params_list
test_that("Test handling of missing parameters in params_list", {
  incomplete_params_list <- list(A = "mu", B = "nonexistent")
  result <- modify_pbox(some_distr, incomplete_params_list)

  # Verify that existing parameters are modified and nonexistent ones are ignored
  expect_false(identical(result$A$mu, some_distr$A$mu))
  expect_true(identical(result$B$sigma, some_distr$B$sigma))
})

# Test handling of non-list input for all_parms
test_that("Test handling of non-list input for all_parms", {
  invalid_all_parms <- "not_a_list"
  expect_error(modify_pbox(invalid_all_parms, params_list))
})

# Test handling of non-named list input for params_list
test_that("Test handling of non-named list input for params_list", {
  invalid_params_list <- list("mu", "sigma")
  expect_error(modify_pbox(some_distr, invalid_params_list))
})

# Test with empty params_list
test_that("Test with empty params_list", {
  empty_params_list <- list()
  expect_error(modify_pbox(some_distr, empty_params_list))
})


# Test with various sigma and range values
test_that("Test with various sigma and range values", {
  sigma <- 0.1
  range <- seq(-5, 5, 2)
  result <- modify_pbox(some_distr, params_list, sigma, range)

  # Check if the result is a list
  expect_type(result, "list")

  # Verify that the parameters are modified with the new sigma and range
  expect_false(identical(result$A$mu, some_distr$A$mu))
  expect_false(identical(result$B$mu, some_distr$B$mu))
  expect_false(identical(result$B$sigma, some_distr$B$sigma))
})
