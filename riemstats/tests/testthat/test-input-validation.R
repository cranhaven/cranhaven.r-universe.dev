# Input validation tests for riemstats functions

test_that("log_wilks_lambda input validation", {
  expect_error(
    log_wilks_lambda("not_a_supersample"),
    "Argument 'super_sample' must be an object of class 'CSuperSample'"
  )
  expect_error(
    log_wilks_lambda(123),
    "Argument 'super_sample' must be an object of class 'CSuperSample'"
  )
  expect_error(
    log_wilks_lambda(list()),
    "Argument 'super_sample' must be an object of class 'CSuperSample'"
  )
  expect_error(
    log_wilks_lambda(matrix(1:4, nrow = 2)),
    "Argument 'super_sample' must be an object of class 'CSuperSample'"
  )
  expect_error(
    log_wilks_lambda(NULL),
    "Argument 'super_sample' must be an object of class 'CSuperSample'"
  )
})

test_that("pillais_trace input validation", {
  expect_error(
    pillais_trace("not_a_supersample"),
    "Argument 'super_sample' must be an object of class 'CSuperSample'"
  )
  expect_error(
    pillais_trace(123),
    "Argument 'super_sample' must be an object of class 'CSuperSample'"
  )
  expect_error(
    pillais_trace(list()),
    "Argument 'super_sample' must be an object of class 'CSuperSample'"
  )
  expect_error(
    pillais_trace(matrix(1:4, nrow = 2)),
    "Argument 'super_sample' must be an object of class 'CSuperSample'"
  )
  expect_error(
    pillais_trace(NULL),
    "Argument 'super_sample' must be an object of class 'CSuperSample'"
  )
})

test_that("frechet_anova input validation", {
  library(riemtan)
  
  # Test with invalid data types
  expect_error(frechet_anova("not_a_supersample"))
  expect_error(frechet_anova(123))
  expect_error(frechet_anova(list()))
  expect_error(frechet_anova(matrix(1:4, nrow = 2)))
  expect_error(frechet_anova(NULL))
})

test_that("riem_anova input validation", {
  library(riemtan)
  
  # Test with invalid data types for super_sample
  expect_error(riem_anova("not_a_supersample"))
  expect_error(riem_anova(123))
  expect_error(riem_anova(list()))
  expect_error(riem_anova(matrix(1:4, nrow = 2)))
  expect_error(riem_anova(NULL))
})

test_that("harmonization functions input validation", {
  library(riemtan)
  data("airm")
  
  # Test combat_harmonization with invalid inputs
  expect_error(
    combat_harmonization("not_a_supersample"),
    "super_sample must be a CSuperSample object"
  )
  expect_error(
    combat_harmonization(123),
    "super_sample must be a CSuperSample object"
  )
  expect_error(
    combat_harmonization(list()),
    "super_sample must be a CSuperSample object"
  )
  expect_error(
    combat_harmonization(matrix(1:4, nrow = 2)),
    "super_sample must be a CSuperSample object"
  )
  expect_error(
    combat_harmonization(NULL),
    "super_sample must be a CSuperSample object"
  )
  
  # Test rigid_harmonization with invalid inputs
  expect_error(
    rigid_harmonization("not_a_supersample"),
    "super_sample must be a CSuperSample object"
  )
  expect_error(
    rigid_harmonization(123),
    "super_sample must be a CSuperSample object"
  )
  expect_error(
    rigid_harmonization(list()),
    "super_sample must be a CSuperSample object"
  )
  expect_error(
    rigid_harmonization(matrix(1:4, nrow = 2)),
    "super_sample must be a CSuperSample object"
  )
  expect_error(
    rigid_harmonization(NULL),
    "super_sample must be a CSuperSample object"
  )
  
})

test_that("normalization input validation", {
  # Test with invalid matrix inputs
  expect_error(
    normalization("not_a_matrix"),
    "non-numeric argument|invalid 'type'|argument is not a matrix"
  )
  expect_error(
    normalization(123),
    "non-numeric argument|invalid 'type'|argument is not a matrix"
  )
  expect_error(
    normalization(NULL),
    "object 'si' not found|subscript out of bounds"
  )
  expect_error(
    normalization(list(1, 2, 3)),
    "non-numeric argument|invalid 'type'"
  )
  
  # Test with edge cases
  expect_error(normalization(matrix(numeric(0))))  # Empty matrix
  expect_error(normalization(matrix(NA, nrow = 2, ncol = 2)))  # Matrix with NAs
  expect_error(normalization(matrix(Inf, nrow = 2, ncol = 2)))  # Matrix with Inf
  
  # Test with valid matrices (should not error)
  expect_no_error(normalization(matrix(1:6, nrow = 2)))
  expect_no_error(normalization(matrix(rnorm(12), nrow = 3)))
  expect_no_error(normalization(matrix(1, nrow = 1, ncol = 5)))  # Single row
})

test_that("format_matr input validation", {
  # Test with invalid inputs
  expect_error(
    format_matr("not_a_matrix"),
    "cannot coerce|invalid 'type'"
  )
  expect_error(
    format_matr(123),
    "cannot coerce|invalid 'type'"
  )
  expect_error(
    format_matr(NULL),
    "cannot coerce|invalid 'type'"
  )
  expect_error(
    format_matr(list(1, 2, 3)),
    "cannot coerce|invalid 'type'"
  )
  
  # Test with edge cases
  expect_error(format_matr(matrix(numeric(0))))  # Empty matrix
  expect_error(format_matr(matrix(NA, nrow = 2, ncol = 2)))  # Matrix with NAs
  expect_error(format_matr(matrix(c(1, 2, 3, 4), nrow = 2)))  # Non-positive definite
  
  # Test with valid positive definite matrix
  valid_matrix <- matrix(c(2, 1, 1, 2), nrow = 2)
  expect_no_error(format_matr(valid_matrix))
})

test_that("ts2corr input validation", {
  # Test with invalid inputs
  expect_error(
    ts2corr("not_a_matrix"),
    "non-numeric argument|invalid 'type'|argument is not a matrix"
  )
  expect_error(
    ts2corr(123),
    "non-numeric argument|invalid 'type'|argument is not a matrix"
  )
  expect_error(
    ts2corr(NULL),
    "object 'ts' not found|subscript out of bounds"
  )
  expect_error(
    ts2corr(list(1, 2, 3)),
    "non-numeric argument|invalid 'type'"
  )
  
  # Test with edge cases
  expect_error(ts2corr(matrix(numeric(0))))  # Empty matrix
  expect_error(ts2corr(matrix(NA, nrow = 2, ncol = 2)))  # Matrix with NAs
  expect_error(ts2corr(matrix(Inf, nrow = 2, ncol = 2)))  # Matrix with Inf
  
  # Test with valid time series matrix
  valid_ts <- matrix(rnorm(20), nrow = 4, ncol = 5)
  expect_no_error(ts2corr(valid_ts))
  
  # Test with minimal valid matrix
  minimal_ts <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  expect_no_error(ts2corr(minimal_ts))
})

test_that("empty and malformed CSuperSample objects", {
  library(riemtan)
  data("airm")
  
  # Helper for creating test data
  test_pd_mats <- list(
    Matrix::Matrix(c(2.0, 0.5, 0.5, 3.0), nrow = 2) |>
      Matrix::nearPD() |> _$mat |> Matrix::pack(),
    Matrix::Matrix(c(1.5, 0.3, 0.3, 2.5), nrow = 2) |>
      Matrix::nearPD() |> _$mat |> Matrix::pack()
  )
  
  # Test with single sample (should error due to insufficient groups)
  single_sample <- test_pd_mats |> CSample$new(metric_obj = airm)
  single_ss <- CSuperSample$new(list(single_sample))
  
  expect_error(log_wilks_lambda(single_ss))
  expect_error(pillais_trace(single_ss))
  expect_error(frechet_anova(single_ss))
  expect_error(riem_anova(single_ss))
  
  # Test harmonization functions with malformed CSuperSample
  expect_error(combat_harmonization(single_ss))
  expect_error(rigid_harmonization(single_ss))
})