library(testthat)
library(purrr)

# Sample data for testing
some_distr <- list(
  A = list(mu = 31.07, sigma = 0.28),
  B = list(mu = c(34.4, 31.4, 25.6), sigma = 0.98, nu = 1.7),
  C = list(mu = 31.4, sigma = 0.34),
  D = list(mu = 25.6, sigma = 0.24)
)

# Test correct functionality
test_that("Test correct functionality with typical input", {
  result <- gen_scenario(some_distr)

  # Check if the result is a list
  expect_type(result, "list")

  # Verify that the length of the result matches the length of the longest parameter list
  max_len <- max(unlist(map_depth(some_distr, 2, length)))
  expect_length(result, max_len)

  # Verify the structure of the first scenario
  expect_type(result[[1]], "list")
  expect_named(result[[1]], names(some_distr))

  # Check if each element is a list
  for (scenario in result) {
    expect_type(scenario, "list")
    expect_named(scenario, names(some_distr))
  }
})

# Test with single-length parameters
test_that("Test with single-length parameters", {
  single_length_params <- list(
    A = list(mu = 31.07, sigma = 0.28),
    B = list(mu = 34.4, sigma = 0.98)
  )

  result <- gen_scenario(single_length_params)

  # Check if the result is a list
  expect_type(result, "list")

  # Verify that there is only one scenario
  expect_length(result, 1)

  # Verify the structure of the scenario
  expect_type(result[[1]], "list")
  expect_named(result[[1]], names(single_length_params))
})

# Test with varying parameter lengths
test_that("Test with varying parameter lengths", {
  varying_length_params <- list(
    A = list(mu = c(31.07, 32.07), sigma = 0.28),
    B = list(mu = c(34.4, 31.4, 25.6), sigma = c(0.98, 0.99))
  )

  result <- gen_scenario(varying_length_params)

  # Check if the result is a list
  expect_type(result, "list")

  # Verify that the length of the result matches the length of the longest parameter list
  max_len <- max(unlist(map_depth(varying_length_params, 2, length)))
  expect_length(result, max_len)

  # Verify the structure of the first scenario
  expect_type(result[[1]], "list")
  expect_named(result[[1]], names(varying_length_params))
})

# Test with empty parameter list
test_that("Test with empty parameter list", {
  empty_params <- list()
  expect_error(gen_scenario(empty_params))
})

# Test with non-list input
test_that("Test with non-list input", {
  non_list_input <- "not_a_list"
  expect_error(gen_scenario(non_list_input))
})
