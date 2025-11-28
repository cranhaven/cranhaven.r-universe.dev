library(testthat)
library(stats)

# Test correct functionality with typical input
test_that("Test correct functionality with typical input", {
  paramMargins <- list(list(mu=0.2, sigma=0.3), list(mu=0.4, sigma=0.5))
  result <- perturbate_params(paramMargins)

  # Check if the result is a list
  expect_type(result, "list")

  # Check if the structure of the result matches the input
  expect_length(result, length(paramMargins))
  for (i in seq_along(result)) {
    expect_length(result[[i]], length(paramMargins[[i]]))
  }

})

# Test handling of single parameter input
test_that("Test handling of single parameter input", {
  paramMargins <- list(list(0.2))
  result <- perturbate_params(paramMargins)

  # Check if the result is a list
  expect_type(result, "list")

  # Check if the structure of the result matches the input
  expect_length(result, length(paramMargins))
  expect_length(result[[1]], length(paramMargins[[1]]))

})

# Test handling of empty list input
test_that("Test handling of empty list input", {
  paramMargins <- list()
  result <- perturbate_params(paramMargins)

  # Check if the result is a list
  expect_type(result, "list")

  # Check if the result is empty
  expect_length(result, 0)
})

# Test handling of non-numeric input within paramMargins
test_that("Test handling of non-numeric input within paramMargins", {
  paramMargins <- list(list("a", "b"))
  expect_error(perturbate_params(paramMargins), "non-numeric argument to binary operator")
})

# Test handling of nested list with varying lengths
test_that("Test handling of nested list with varying lengths", {
  paramMargins <- list(list(0.2, 0.3, 0.4), list(0.5))
  result <- perturbate_params(paramMargins)

  # Check if the result is a list
  expect_type(result, "list")

  # Check if the structure of the result matches the input
  expect_length(result, length(paramMargins))
  for (i in seq_along(result)) {
    expect_length(result[[i]], length(paramMargins[[i]]))
  }

})

# Test with edge case values
test_that("Test with edge case values", {
  paramMargins <- list(list(-Inf, Inf, NaN, NA))
  result <- suppressWarnings(perturbate_params(paramMargins))

  # Check if the result is a list
  expect_type(result, "list")

  # Check if the structure of the result matches the input
  expect_length(result, length(paramMargins))
  expect_length(result[[1]], length(paramMargins[[1]]))

  # Check if the parameters are perturbed correctly
  expect_true(is.nan(result[[1]][[3]]))
  expect_true(is.na(result[[1]][[4]]))
})
