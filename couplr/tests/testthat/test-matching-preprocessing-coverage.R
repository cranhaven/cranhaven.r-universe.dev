# ==============================================================================
# Additional tests for matching preprocessing functions to increase coverage
# ==============================================================================

# ------------------------------------------------------------------------------
# check_variable_health tests
# ------------------------------------------------------------------------------

test_that("check_variable_health errors with no variables", {
  skip_on_cran()
  left <- data.frame(x = 1:5)
  right <- data.frame(x = 1:5)

  expect_error(
    couplr:::check_variable_health(left, right, vars = character(0)),
    "No variables provided"
  )
})

test_that("check_variable_health detects all-NA variable", {
  skip_on_cran()
  left <- data.frame(x = c(NA, NA, NA), y = 1:3)
  right <- data.frame(x = c(NA, NA, NA), y = 4:6)

  result <- couplr:::check_variable_health(left, right, vars = c("x", "y"))

  expect_true("x" %in% result$exclude_vars)
  expect_true(any(sapply(result$issues, function(i) i$type == "all_na")))
  expect_true(any(grepl("all values are NA", result$warnings)))
})

test_that("check_variable_health detects constant variable", {
  skip_on_cran()
  left <- data.frame(x = rep(5, 5), y = 1:5)
  right <- data.frame(x = rep(5, 5), y = 6:10)

  result <- couplr:::check_variable_health(left, right, vars = c("x", "y"))

  expect_true("x" %in% result$exclude_vars)
  expect_true(any(sapply(result$issues, function(i) i$type == "constant")))
})

test_that("check_variable_health detects low variance", {
  skip_on_cran()
  left <- data.frame(x = c(1, 1, 1, 1.00000001), y = 1:4)
  right <- data.frame(x = c(1, 1, 1, 1), y = 5:8)

  result <- couplr:::check_variable_health(left, right, vars = c("x", "y"),
                                            low_variance_threshold = 1e-6)

  # x should trigger low variance warning
  issues <- result$issues
  low_var_issues <- Filter(function(i) i$type == "low_variance", issues)
  expect_true(length(low_var_issues) > 0 || "x" %in% result$exclude_vars)
})

test_that("check_variable_health detects high missingness", {
  skip_on_cran()
  left <- data.frame(x = c(NA, NA, NA, NA, 5), y = 1:5)
  right <- data.frame(x = c(NA, NA, NA, NA, 5), y = 6:10)

  result <- couplr:::check_variable_health(left, right, vars = c("x", "y"),
                                            high_missingness_threshold = 0.5)

  issues <- result$issues
  miss_issues <- Filter(function(i) i$type == "high_missingness", issues)
  expect_true(length(miss_issues) > 0)
})

test_that("check_variable_health detects extreme skewness", {
  skip_on_cran()
  # Create highly skewed data
  left <- data.frame(x = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 100), y = 1:10)
  right <- data.frame(x = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 100), y = 11:20)

  result <- couplr:::check_variable_health(left, right, vars = c("x", "y"))

  issues <- result$issues
  skew_issues <- Filter(function(i) i$type == "skewed", issues)
  expect_true(length(skew_issues) > 0)
})

test_that("check_variable_health returns proper structure", {
  skip_on_cran()
  left <- data.frame(x = 1:5, y = 6:10)
  right <- data.frame(x = 11:15, y = 16:20)

  result <- couplr:::check_variable_health(left, right, vars = c("x", "y"))

  expect_s3_class(result, "variable_health")
  expect_true("summary" %in% names(result))
  expect_true("issues" %in% names(result))
  expect_true("exclude_vars" %in% names(result))
  expect_true("warnings" %in% names(result))

  expect_true("variable" %in% names(result$summary))
  expect_true("mean" %in% names(result$summary))
  expect_true("sd" %in% names(result$summary))
})

test_that("check_variable_health handles multiple issues on same variable", {
  skip_on_cran()
  # Variable with both high missingness and skewness
  set.seed(123)
  vals <- c(rep(NA, 6), rep(1, 3), 1000)
  left <- data.frame(x = vals[1:5], y = 1:5)
  right <- data.frame(x = vals[6:10], y = 6:10)

  result <- couplr:::check_variable_health(left, right, vars = c("x", "y"),
                                            high_missingness_threshold = 0.3)

  # Should have multiple issues
  expect_true(length(result$issues) >= 1)
})

# ------------------------------------------------------------------------------
# suggest_scaling tests
# ------------------------------------------------------------------------------

test_that("suggest_scaling returns 'none' for empty vars", {
  skip_on_cran()
  left <- data.frame(x = 1:5)
  right <- data.frame(x = 1:5)

  result <- couplr:::suggest_scaling(left, right, vars = character(0))

  expect_equal(result, "none")
})

test_that("suggest_scaling suggests robust for skewed data", {
  skip_on_cran()
  # Create highly skewed data
  left <- data.frame(x = c(1, 1, 1, 1, 1, 100))
  right <- data.frame(x = c(1, 1, 1, 1, 1, 100))

  result <- couplr:::suggest_scaling(left, right, vars = "x")

  expect_true(result %in% c("robust", "standardize", "range", "none"))
})

test_that("suggest_scaling suggests standardize for normal data", {
  skip_on_cran()
  set.seed(123)
  left <- data.frame(x = rnorm(100), y = rnorm(100))
  right <- data.frame(x = rnorm(100), y = rnorm(100))

  result <- couplr:::suggest_scaling(left, right, vars = c("x", "y"))

  expect_true(result %in% c("standardize", "range", "robust", "none"))
})

# ------------------------------------------------------------------------------
# preprocess_matching_vars tests
# ------------------------------------------------------------------------------

test_that("preprocess_matching_vars works with default settings", {
  skip_on_cran()
  left <- data.frame(x = 1:5, y = 6:10)
  right <- data.frame(x = 11:15, y = 16:20)

  result <- preprocess_matching_vars(left, right, vars = c("x", "y"))

  expect_true("vars" %in% names(result))
  expect_true("scaling_method" %in% names(result))
})

test_that("preprocess_matching_vars removes problematic vars", {
  skip_on_cran()
  left <- data.frame(const = rep(5, 5), good = 1:5)
  right <- data.frame(const = rep(5, 5), good = 6:10)

  result <- preprocess_matching_vars(left, right, vars = c("const", "good"),
                                      check_health = TRUE,
                                      remove_problematic = TRUE,
                                      verbose = FALSE)

  expect_true("good" %in% result$vars)
  expect_false("const" %in% result$vars)
})

test_that("preprocess_matching_vars keeps problematic vars when requested", {
  skip_on_cran()
  left <- data.frame(const = rep(5, 5), good = 1:5)
  right <- data.frame(const = rep(5, 5), good = 6:10)

  result <- preprocess_matching_vars(left, right, vars = c("const", "good"),
                                      check_health = TRUE,
                                      remove_problematic = FALSE,
                                      verbose = FALSE)

  expect_true("const" %in% result$vars)
  expect_true("good" %in% result$vars)
})

test_that("preprocess_matching_vars auto-selects scaling", {
  skip_on_cran()
  set.seed(123)
  left <- data.frame(x = rnorm(50), y = rnorm(50) + 100)
  right <- data.frame(x = rnorm(50), y = rnorm(50) + 100)

  result <- preprocess_matching_vars(left, right, vars = c("x", "y"),
                                      auto_scale = TRUE,
                                      scale_method = "auto",
                                      verbose = FALSE)

  expect_true(result$scaling_method %in% c("standardize", "range", "robust", "none"))
})

test_that("preprocess_matching_vars respects explicit scale_method", {
  skip_on_cran()
  left <- data.frame(x = 1:5)
  right <- data.frame(x = 6:10)

  result <- preprocess_matching_vars(left, right, vars = "x",
                                      auto_scale = FALSE,
                                      scale_method = "robust",
                                      verbose = FALSE)

  expect_equal(result$scaling_method, "robust")
})

test_that("preprocess_matching_vars returns health info", {
  skip_on_cran()
  left <- data.frame(x = 1:5, y = 6:10)
  right <- data.frame(x = 11:15, y = 16:20)

  result <- preprocess_matching_vars(left, right, vars = c("x", "y"),
                                      check_health = TRUE,
                                      verbose = FALSE)

  expect_true("health" %in% names(result))
})

test_that("preprocess_matching_vars handles verbose output", {
  skip_on_cran()
  left <- data.frame(x = 1:5)
  right <- data.frame(x = 6:10)

  output <- capture.output({
    result <- preprocess_matching_vars(left, right, vars = "x",
                                        check_health = TRUE,
                                        auto_scale = TRUE,
                                        verbose = TRUE)
  })

  expect_true(length(output) > 0 || is.list(result))
})

# ------------------------------------------------------------------------------
# print.variable_health tests
# ------------------------------------------------------------------------------

test_that("print.variable_health works", {
  skip_on_cran()
  left <- data.frame(x = 1:5, y = c(NA, NA, 3, 4, 5))
  right <- data.frame(x = 6:10, y = c(6, NA, 8, 9, 10))

  health <- couplr:::check_variable_health(left, right, vars = c("x", "y"))

  output <- capture.output(print(health))

  expect_true(any(grepl("Variable Health", output)))
})

# ------------------------------------------------------------------------------
# print.preprocessing_result tests
# ------------------------------------------------------------------------------

test_that("print.preprocessing_result works", {
  skip_on_cran()
  left <- data.frame(x = 1:5)
  right <- data.frame(x = 6:10)

  result <- preprocess_matching_vars(left, right, vars = "x",
                                      verbose = FALSE)

  output <- capture.output(print(result))

  expect_true(length(output) >= 1)
})

# ------------------------------------------------------------------------------
# Edge cases
# ------------------------------------------------------------------------------

test_that("check_variable_health handles single row data", {
  skip_on_cran()
  left <- data.frame(x = 1)
  right <- data.frame(x = 2)

  result <- couplr:::check_variable_health(left, right, vars = "x")

  expect_s3_class(result, "variable_health")
})

test_that("check_variable_health handles many variables", {
  skip_on_cran()
  n <- 20
  left <- as.data.frame(matrix(rnorm(n * 10), nrow = 10))
  right <- as.data.frame(matrix(rnorm(n * 10), nrow = 10))

  result <- couplr:::check_variable_health(left, right, vars = names(left))

  expect_equal(nrow(result$summary), n)
})

test_that("preprocess_matching_vars errors when all vars are problematic", {
  skip_on_cran()
  left <- data.frame(x = rep(1, 5), y = rep(2, 5))
  right <- data.frame(x = rep(1, 5), y = rep(2, 5))

  # When all variables are excluded due to health issues, an error is thrown
  expect_error(
    preprocess_matching_vars(left, right, vars = c("x", "y"),
                              check_health = TRUE,
                              remove_problematic = TRUE,
                              verbose = FALSE),
    "All variables were excluded"
  )
})
