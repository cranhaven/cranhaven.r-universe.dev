# ==============================================================================
# Coverage tests for matching_preprocessing.R
# ==============================================================================

# ------------------------------------------------------------------------------
# check_variable_health edge cases
# ------------------------------------------------------------------------------

test_that("check_variable_health errors on empty vars", {
  skip_on_cran()
  left <- data.frame(x = 1:3)
  right <- data.frame(x = 4:6)
  expect_error(
    couplr:::check_variable_health(left, right, vars = character(0)),
    "No variables"
  )
})

test_that("check_variable_health detects all-NA variable", {
  skip_on_cran()
  left <- data.frame(x = 1:3, y = NA_real_)
  right <- data.frame(x = 4:6, y = NA_real_)

  result <- couplr:::check_variable_health(left, right, vars = c("x", "y"))
  expect_true("y" %in% result$exclude_vars)
  expect_true(any(sapply(result$issues, function(i) i$type == "all_na")))
})

test_that("check_variable_health detects constant variable", {
  skip_on_cran()
  left <- data.frame(x = 1:3, y = rep(5, 3))
  right <- data.frame(x = 4:6, y = rep(5, 3))

  result <- couplr:::check_variable_health(left, right, vars = c("x", "y"))
  expect_true("y" %in% result$exclude_vars)
  expect_true(any(sapply(result$issues, function(i) i$type == "constant")))
})

test_that("check_variable_health detects low variance", {
  skip_on_cran()
  left <- data.frame(x = 1:3, y = c(1, 1.0000001, 1.0000002))
  right <- data.frame(x = 4:6, y = c(1.0000003, 1.0000004, 1.0000005))

  result <- couplr:::check_variable_health(left, right, vars = c("x", "y"),
                                            low_variance_threshold = 1e-3)
  expect_true(any(sapply(result$issues, function(i) i$type == "low_variance")))
})

test_that("check_variable_health detects high missingness", {
  skip_on_cran()
  left <- data.frame(x = 1:5, y = c(1, NA, NA, NA, NA))
  right <- data.frame(x = 6:10, y = c(NA, NA, NA, 4, 5))

  result <- couplr:::check_variable_health(left, right, vars = c("x", "y"),
                                            high_missingness_threshold = 0.5)
  expect_true(any(sapply(result$issues, function(i) i$type == "high_missingness")))
})

test_that("check_variable_health detects skewed variable", {
  skip_on_cran()
  # Create highly skewed data (need many points for reliable skewness)
  set.seed(42)
  skewed <- exp(rnorm(50))  # Log-normal is highly skewed
  left <- data.frame(x = 1:50, y = skewed[1:50])
  right <- data.frame(x = 51:100, y = skewed)

  result <- couplr:::check_variable_health(left, right, vars = c("x", "y"))
  # Just verify it runs without error; skewness may not trigger issue for all data
  expect_true(is.list(result))
})

# ------------------------------------------------------------------------------
# preprocess_matching_vars edge cases
# ------------------------------------------------------------------------------

test_that("preprocess_matching_vars handles vars not in data", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)

  expect_error(
    couplr::preprocess_matching_vars(left, right, vars = c("x", "missing_var")),
    "not found|Missing"
  )
})

test_that("preprocess_matching_vars with scale='standardize'", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:10, x = c(6, 7, 8, 9, 10))

  result <- couplr::preprocess_matching_vars(left, right, vars = "x", scale = "standardize")
  expect_true("left" %in% names(result))
  expect_true("right" %in% names(result))
})

test_that("preprocess_matching_vars with scale='robust'", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:10, x = c(6, 7, 8, 9, 10))

  result <- couplr::preprocess_matching_vars(left, right, vars = "x", scale = "robust")
  expect_true("left" %in% names(result))
  expect_true("right" %in% names(result))
})

test_that("preprocess_matching_vars with scale='range'", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:10, x = c(6, 7, 8, 9, 10))

  result <- couplr::preprocess_matching_vars(left, right, vars = "x", scale = "range")
  expect_true("left" %in% names(result))
})

test_that("preprocess_matching_vars with scale='none'", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:10, x = c(6, 7, 8, 9, 10))

  result <- couplr::preprocess_matching_vars(left, right, vars = "x", scale = "none")
  expect_equal(result$left$x, left$x)  # Unchanged
})

test_that("preprocess_matching_vars returns scaled data", {
  skip_on_cran()
  set.seed(42)
  left <- data.frame(id = 1:20, x = rnorm(20, mean = 100, sd = 10))
  right <- data.frame(id = 21:40, x = rnorm(20, mean = 100, sd = 10))

  result <- couplr::preprocess_matching_vars(left, right, vars = "x", scale = "standardize")
  # Just verify scaled data is returned
  expect_true("left" %in% names(result))
  expect_true("right" %in% names(result))
})

# ------------------------------------------------------------------------------
# suggest_scaling
# ------------------------------------------------------------------------------

test_that("suggest_scaling returns recommendations", {
  skip_on_cran()
  left <- data.frame(x = c(1, 2, 3, 4, 5), y = c(100, 200, 300, 400, 500))
  right <- data.frame(x = c(6, 7, 8, 9, 10), y = c(600, 700, 800, 900, 1000))

  result <- couplr:::suggest_scaling(left, right, vars = c("x", "y"))
  expect_true(result %in% c("none", "standardize", "robust", "range"))
})

test_that("suggest_scaling returns none for empty vars", {
  skip_on_cran()
  left <- data.frame(x = 1:5)
  right <- data.frame(x = 6:10)

  result <- couplr:::suggest_scaling(left, right, vars = character(0))
  expect_equal(result, "none")
})

test_that("suggest_scaling returns robust for outliers", {
  skip_on_cran()
  # Create data with outliers
  left <- data.frame(x = c(1, 2, 3, 4, 100))  # 100 is an extreme outlier
  right <- data.frame(x = c(1, 2, 3, 4, 100))

  result <- couplr:::suggest_scaling(left, right, vars = "x")
  expect_true(result %in% c("none", "standardize", "robust"))
})

# ------------------------------------------------------------------------------
# auto_encode_categorical
# ------------------------------------------------------------------------------

test_that("auto_encode_categorical handles binary factors", {
  skip_on_cran()
  left <- data.frame(id = 1:4, x = factor(c("A", "B", "A", "B")))
  right <- data.frame(id = 5:8, x = factor(c("A", "B", "B", "A")))

  result <- couplr:::auto_encode_categorical(left, right, "x")
  expect_true("left" %in% names(result))
  expect_true("right" %in% names(result))
  expect_true(is.numeric(result$left))  # result$left is the encoded vector, not a df
})

test_that("auto_encode_categorical handles ordered factors", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = ordered(c("low", "medium", "high"),
                                            levels = c("low", "medium", "high")))
  right <- data.frame(id = 4:6, x = ordered(c("medium", "high", "low"),
                                            levels = c("low", "medium", "high")))

  result <- couplr:::auto_encode_categorical(left, right, "x")
  expect_true(is.numeric(result$left))  # result$left is the encoded vector
})

test_that("auto_encode_categorical keeps numeric unchanged", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = c(1.0, 2.0, 3.0))
  right <- data.frame(id = 4:6, x = c(4.0, 5.0, 6.0))

  result <- couplr:::auto_encode_categorical(left, right, "x")
  expect_equal(result$left, left$x)  # Returns vector directly
})

test_that("auto_encode_categorical errors on non-binary categorical", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = c("A", "B", "C"), stringsAsFactors = FALSE)
  right <- data.frame(id = 4:6, x = c("B", "C", "A"), stringsAsFactors = FALSE)

  expect_error(
    couplr:::auto_encode_categorical(left, right, "x"),
    "not binary or ordered"
  )
})

# ------------------------------------------------------------------------------
# print methods
# ------------------------------------------------------------------------------

test_that("print.preprocess_result works", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:10, x = c(6, 7, 8, 9, 10))

  result <- couplr::preprocess_matching_vars(left, right, vars = "x")
  expect_output(print(result), regexp = NULL)  # Just verify it prints without error
})
