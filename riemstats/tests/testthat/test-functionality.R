# Functionality tests for riemstats functions

# Test data helper
test_pd_mats <- list(
  Matrix::Matrix(c(2.0, 0.5, 0.5, 3.0), nrow = 2) |>
    Matrix::nearPD() |> _$mat |> Matrix::pack(),
  Matrix::Matrix(c(1.5, 0.3, 0.3, 2.5), nrow = 2) |>
    Matrix::nearPD() |> _$mat |> Matrix::pack()
)

test_that("ANOVA stats work", {
  library(riemtan)
  data("airm")
  sam1 <- test_pd_mats |>
    purrr::map(\(x) (2 * x) |> Matrix::unpack() |> as("dpoMatrix") |> Matrix::pack()) |>
    CSample$new(metric_obj = airm)
  sam2 <- test_pd_mats |> CSample$new(metric_obj = airm)
  ss <- list(sam1, sam2) |> CSuperSample$new()

  # ss$Log_Wilks_Lambda() |> (\(x) {
  ss |>
    log_wilks_lambda() |>
    (\(x){
      list(
        x |> is.null() |> expect_false(),
        x |> inherits("numeric") |> expect_true(),
        x |> expect_lt(0)
      )
    })()
  ss |>
    pillais_trace() |>
    (\(x) {
      list(
        x |> is.null() |> expect_false(),
        x |> inherits("numeric") |> expect_true(),
        x |> expect_gt(0)
      )
    })()
  ss |>
    frechet_anova() |>
    (\(x) {
      list(
        x |> is.null() |> expect_false(),
        x |> inherits("list") |> expect_true(),
        x$statistic |> is.null() |> expect_false(),
        x$statistic |> inherits("numeric") |> expect_true(),
        x$statistic |> expect_gt(0),
        x$p_value |> is.null() |> expect_false(),
        x$p_value |> inherits("numeric") |> expect_true(),
        x$p_value |> (\(p) p >= 0)() |> expect_true(),
        x$p_value |> (\(p) p <= 1)() |> expect_true()
      )
    })()
  ss |>
    riem_anova(nperm = 10) |>  # Use small number for faster testing
    (\(x) {
      list(
        x |> is.null() |> expect_false(),
        x |> inherits("numeric") |> expect_true(),
        x |> (\(x) x >= 0)() |> expect_true(),
        x |> (\(x) x <= 1)() |> expect_true()
      )
    })()
})

test_that("combat_harmonization functionality", {
  library(riemtan)
  data("airm")
  
  # Create test data with known batch effects
  sam1 <- test_pd_mats |>
    purrr::map(\(x) (2 * x) |> Matrix::unpack() |> as("dpoMatrix") |> Matrix::pack()) |>
    CSample$new(metric_obj = airm)
  sam2 <- test_pd_mats |> 
    purrr::map(\(x) (0.5 * x) |> Matrix::unpack() |> as("dpoMatrix") |> Matrix::pack()) |>
    CSample$new(metric_obj = airm)
  sam3 <- test_pd_mats |>
    CSample$new(metric_obj = airm)
  
  ss <- list(sam1, sam2, sam3) |> CSuperSample$new()
  
  # Test that combat_harmonization returns a CSuperSample
  harmonized_ss <- combat_harmonization(ss)
  expect_true(inherits(harmonized_ss, "CSuperSample"))
  
  # Test that output has same number of samples as input
  expect_equal(length(harmonized_ss$list_of_samples), length(ss$list_of_samples))
  
  # Test that each harmonized sample is a CSample
  for (i in seq_along(harmonized_ss$list_of_samples)) {
    expect_true(inherits(harmonized_ss$list_of_samples[[i]], "CSample"))
  }
  
  # Test that harmonized samples have same dimensions as original
  for (i in seq_along(harmonized_ss$list_of_samples)) {
    original_size <- ss$list_of_samples[[i]]$sample_size
    harmonized_size <- harmonized_ss$list_of_samples[[i]]$sample_size
    expect_equal(harmonized_size, original_size)
  }
})

test_that("rigid_harmonization functionality", {
  library(riemtan)
  data("airm")
  
  # Create test data with different batch effects
  sam1 <- test_pd_mats |>
    purrr::map(\(x) (3 * x) |> Matrix::unpack() |> as("dpoMatrix") |> Matrix::pack()) |>
    CSample$new(metric_obj = airm)
  sam2 <- test_pd_mats |> 
    purrr::map(\(x) (0.8 * x) |> Matrix::unpack() |> as("dpoMatrix") |> Matrix::pack()) |>
    CSample$new(metric_obj = airm)
  
  ss <- list(sam1, sam2) |> CSuperSample$new()
  
  # Test that rigid_harmonization returns a CSuperSample
  harmonized_ss <- rigid_harmonization(ss)
  expect_true(inherits(harmonized_ss, "CSuperSample"))
  
  # Test that output has same number of samples as input
  expect_equal(length(harmonized_ss$list_of_samples), length(ss$list_of_samples))
  
  # Test that each harmonized sample is a CSample
  for (i in seq_along(harmonized_ss$list_of_samples)) {
    expect_true(inherits(harmonized_ss$list_of_samples[[i]], "CSample"))
  }
  
  # Test that harmonized samples have same dimensions as original
  for (i in seq_along(harmonized_ss$list_of_samples)) {
    original_size <- ss$list_of_samples[[i]]$sample_size
    harmonized_size <- harmonized_ss$list_of_samples[[i]]$sample_size
    expect_equal(harmonized_size, original_size)
  }
})

test_that("normalization functionality", {
  # Test basic normalization properties
  test_matrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  normalized <- normalization(test_matrix)
  
  # Check that output is a matrix
  expect_true(is.matrix(normalized))
  
  # Check dimensions are preserved
  expect_equal(dim(normalized), dim(test_matrix))
  
  # Check that rows are centered (mean ≈ 0)
  row_means <- rowMeans(normalized)
  expect_true(all(abs(row_means) < 1e-10))
  
  # Check that rows have unit norm (approximately)
  row_norms <- sqrt(rowSums(normalized^2))
  expect_true(all(abs(row_norms - 1) < 1e-10))
  
  # Test with single row matrix
  single_row <- matrix(c(1, 2, 3), nrow = 1)
  normalized_single <- normalization(single_row)
  expect_equal(dim(normalized_single), dim(single_row))
  expect_true(abs(rowMeans(normalized_single)) < 1e-10)
  expect_true(abs(sqrt(rowSums(normalized_single^2)) - 1) < 1e-10)
  
  # Test with zero-variance row (should be handled by eps parameter)
  zero_var <- matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE)
  expect_no_error(normalization(zero_var))
})

test_that("format_matr functionality", {
  # Test with valid positive definite matrix
  valid_matrix <- matrix(c(2, 1, 1, 2), nrow = 2)
  formatted <- format_matr(valid_matrix)
  
  # Check that output is a packed dppMatrix
  expect_true(inherits(formatted, "dppMatrix"))
  
  # Test with larger positive definite matrix
  large_matrix <- diag(c(1, 2, 3)) + 0.1 * matrix(rnorm(9), 3, 3)
  large_matrix <- (large_matrix + t(large_matrix)) / 2  # Ensure symmetry
  large_matrix <- large_matrix + 3 * diag(3)  # Ensure positive definiteness
  formatted_large <- format_matr(large_matrix)
  expect_true(inherits(formatted_large, "dppMatrix"))
})

test_that("ts2corr functionality", {
  # Test basic functionality
  set.seed(123)
  ts_matrix <- matrix(rnorm(20), nrow = 4, ncol = 5)
  corr_result <- ts2corr(ts_matrix)
  
  # Check that output is a matrix
  expect_true(is.matrix(corr_result))
  
  # Check that output is square
  expect_equal(nrow(corr_result), ncol(corr_result))
  
  # Check that output dimensions match input rows
  expect_equal(nrow(corr_result), nrow(ts_matrix))
  
  # Check that result is symmetric (correlation matrix property)
  expect_true(all(abs(corr_result - t(corr_result)) < 1e-10))
  
  # Check that diagonal elements are approximately 1 (after normalization)
  expect_true(all(diag(corr_result) > 0))
  
  # Test with different sized input
  larger_ts <- matrix(rnorm(30), nrow = 6, ncol = 5)
  larger_corr <- ts2corr(larger_ts)
  expect_equal(dim(larger_corr), c(6, 6))
})

test_that("one_permutation functionality", {
  library(riemtan)
  data("airm")

  # Create test supersample
  sam1 <- test_pd_mats |> CSample$new(metric_obj = airm)
  sam2 <- test_pd_mats |>
    purrr::map(\(x) (1.1 * x) |> Matrix::unpack() |> as("dpoMatrix") |> Matrix::pack()) |>
    CSample$new(metric_obj = airm)
  ss <- list(sam1, sam2) |> CSuperSample$new()

  # Define a simple statistic function
  stat_fun <- function(x) x |> log_wilks_lambda()

  # Test one_permutation execution
  permutation_result <- one_permutation(ss, stat_fun)

  # Check that result is numeric
  expect_true(is.numeric(permutation_result))
  expect_length(permutation_result, 1)

  # Test with different statistic function
  stat_fun2 <- function(x) x |> pillais_trace()
  permutation_result2 <- one_permutation(ss, stat_fun2)

  expect_true(is.numeric(permutation_result2))
  expect_length(permutation_result2, 1)
  expect_true(permutation_result2 >= 0)  # Pillai's trace should be non-negative
})

test_that("harmonization functions preserve statistical properties", {
  library(riemtan)
  data("airm")
  
  # Create test data with measurable batch effects
  set.seed(42)
  sam1 <- test_pd_mats |>
    purrr::map(\(x) (2 * x + 0.1 * diag(2)) |> Matrix::unpack() |> as("dpoMatrix") |> Matrix::pack()) |>
    CSample$new(metric_obj = airm)
  sam2 <- test_pd_mats |>
    purrr::map(\(x) (0.5 * x + 0.05 * diag(2)) |> Matrix::unpack() |> as("dpoMatrix") |> Matrix::pack()) |>
    CSample$new(metric_obj = airm)
  
  ss <- list(sam1, sam2) |> CSuperSample$new()
  
  # Test that harmonization methods preserve sample sizes
  combat_result <- combat_harmonization(ss)
  rigid_result <- rigid_harmonization(ss)
  
  expect_equal(combat_result$sample_size, ss$sample_size)
  expect_equal(rigid_result$sample_size, ss$sample_size)
  
  # Test that both methods can be applied without errors
  expect_true(inherits(combat_result, "CSuperSample"))
  expect_true(inherits(rigid_result, "CSuperSample"))
  
  # Test that harmonized data can still be used for statistical analysis
  expect_no_error(log_wilks_lambda(combat_result))
  expect_no_error(log_wilks_lambda(rigid_result))
  expect_no_error(pillais_trace(combat_result))
  expect_no_error(pillais_trace(rigid_result))
})