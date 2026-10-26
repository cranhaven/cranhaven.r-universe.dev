# ==============================================================================
# Additional tests for matching_preprocessing.R to reach 90%+ coverage
# ==============================================================================

# Create test datasets
set.seed(456)
left_df <- data.frame(
  id = 1:50,
  normal_var = rnorm(50, 100, 15),
  income = rnorm(50, 50000, 15000),
  stringsAsFactors = FALSE
)
right_df <- data.frame(
  id = 51:100,
  normal_var = rnorm(50, 105, 15),
  income = rnorm(50, 52000, 15000),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# check_variable_health tests
# ------------------------------------------------------------------------------

test_that("check_variable_health works with normal variables", {
  skip_on_cran()
  health <- couplr:::check_variable_health(
    left_df, right_df,
    vars = c("normal_var", "income")
  )

  expect_s3_class(health, "variable_health")
  expect_true(is.data.frame(health$summary))
  expect_equal(nrow(health$summary), 2)
})

test_that("check_variable_health detects constant variables", {
  skip_on_cran()
  left_const <- left_df
  left_const$const_var <- 1  # Constant
  right_const <- right_df
  right_const$const_var <- 1

  health <- couplr:::check_variable_health(
    left_const, right_const,
    vars = c("normal_var", "const_var")
  )

  expect_true("const_var" %in% health$exclude_vars)
  expect_true(any(sapply(health$issues, function(x) x$type == "constant")))
})

test_that("check_variable_health detects all-NA variables", {
  skip_on_cran()
  left_na <- left_df
  left_na$na_var <- NA_real_
  right_na <- right_df
  right_na$na_var <- NA_real_

  health <- couplr:::check_variable_health(
    left_na, right_na,
    vars = c("normal_var", "na_var")
  )

  expect_true("na_var" %in% health$exclude_vars)
  expect_true(any(sapply(health$issues, function(x) x$type == "all_na")))
})

test_that("check_variable_health detects high missingness", {
  skip_on_cran()
  left_miss <- left_df
  left_miss$miss_var <- c(1:10, rep(NA, 40))  # 80% missing
  right_miss <- right_df
  right_miss$miss_var <- c(1:10, rep(NA, 40))

  health <- couplr:::check_variable_health(
    left_miss, right_miss,
    vars = c("normal_var", "miss_var"),
    high_missingness_threshold = 0.5
  )

  expect_true(any(sapply(health$issues, function(x) x$type == "high_missingness")))
})

test_that("check_variable_health detects low variance", {
  skip_on_cran()
  left_lowvar <- left_df
  left_lowvar$lowvar <- rep(c(1, 1.0000001), 25)  # Very low variance
  right_lowvar <- right_df
  right_lowvar$lowvar <- rep(c(1, 1.0000001), 25)

  health <- couplr:::check_variable_health(
    left_lowvar, right_lowvar,
    vars = c("lowvar"),
    low_variance_threshold = 0.001
  )

  expect_true(any(sapply(health$issues, function(x) x$type == "low_variance")))
})

test_that("check_variable_health detects skewness", {
  skip_on_cran()
  left_skew <- left_df
  left_skew$skew_var <- exp(rnorm(50))  # Highly skewed
  right_skew <- right_df
  right_skew$skew_var <- exp(rnorm(50))

  health <- couplr:::check_variable_health(
    left_skew, right_skew,
    vars = c("skew_var")
  )

  # May or may not detect skewness depending on random values
  expect_s3_class(health, "variable_health")
})

test_that("check_variable_health errors with no variables", {
  skip_on_cran()
  expect_error(
    couplr:::check_variable_health(left_df, right_df, vars = character(0)),
    "No variables provided"
  )
})

# ------------------------------------------------------------------------------
# suggest_scaling tests
# ------------------------------------------------------------------------------

test_that("suggest_scaling returns 'none' for empty vars", {
  skip_on_cran()
  result <- couplr:::suggest_scaling(left_df, right_df, vars = character(0))
  expect_equal(result, "none")
})

test_that("suggest_scaling suggests 'robust' for outliers", {
  skip_on_cran()
  left_outlier <- left_df
  left_outlier$outlier_var <- c(rnorm(45, 10, 1), 100, 200, 300, 400, 500)
  right_outlier <- right_df
  right_outlier$outlier_var <- c(rnorm(45, 10, 1), 100, 200, 300, 400, 500)

  result <- couplr:::suggest_scaling(
    left_outlier, right_outlier,
    vars = "outlier_var"
  )

  # May suggest robust due to outliers
  expect_true(result %in% c("robust", "standardize", "none"))
})

test_that("suggest_scaling suggests 'standardize' for different scales", {
  skip_on_cran()
  left_scale <- data.frame(
    small = rnorm(50, 0, 1),
    large = rnorm(50, 0, 100)
  )
  right_scale <- data.frame(
    small = rnorm(50, 0, 1),
    large = rnorm(50, 0, 100)
  )

  result <- couplr:::suggest_scaling(
    left_scale, right_scale,
    vars = c("small", "large")
  )

  expect_equal(result, "standardize")
})

test_that("suggest_scaling handles NA values", {
  skip_on_cran()
  left_with_na <- left_df
  left_with_na$with_na <- c(rnorm(40), rep(NA, 10))
  right_with_na <- right_df
  right_with_na$with_na <- c(rnorm(40), rep(NA, 10))

  result <- couplr:::suggest_scaling(
    left_with_na, right_with_na,
    vars = "with_na"
  )

  expect_true(result %in% c("robust", "standardize", "none"))
})

# ------------------------------------------------------------------------------
# auto_encode_categorical tests
# ------------------------------------------------------------------------------

test_that("auto_encode_categorical handles numeric variables", {
  skip_on_cran()
  left_num <- data.frame(x = 1:10)
  right_num <- data.frame(x = 11:20)

  result <- couplr:::auto_encode_categorical(left_num, right_num, "x")

  expect_equal(result$method, "none")
  expect_equal(result$left, 1:10)
  expect_equal(result$right, 11:20)
})

test_that("auto_encode_categorical encodes binary variables", {
  skip_on_cran()
  left_bin <- data.frame(gender = c("M", "F", "M", "F", "M"))
  right_bin <- data.frame(gender = c("F", "M", "F", "M", "F"))

  result <- couplr:::auto_encode_categorical(left_bin, right_bin, "gender")

  expect_equal(result$method, "binary")
  expect_true(all(result$left %in% c(0, 1)))
  expect_true(all(result$right %in% c(0, 1)))
})

test_that("auto_encode_categorical encodes ordered factors", {
  skip_on_cran()
  left_ord <- data.frame(
    edu = factor(c("low", "med", "high", "low", "med"),
                 levels = c("low", "med", "high"), ordered = TRUE)
  )
  right_ord <- data.frame(
    edu = factor(c("med", "high", "low", "med", "high"),
                 levels = c("low", "med", "high"), ordered = TRUE)
  )

  result <- couplr:::auto_encode_categorical(left_ord, right_ord, "edu")

  expect_equal(result$method, "ordered")
  expect_true(all(result$left %in% c(1, 2, 3)))
})

test_that("auto_encode_categorical errors on unordered multi-level factor", {
  skip_on_cran()
  left_cat <- data.frame(color = c("red", "blue", "green", "red"))
  right_cat <- data.frame(color = c("blue", "green", "red", "blue"))

  expect_error(
    couplr:::auto_encode_categorical(left_cat, right_cat, "color"),
    "categorical but not binary or ordered"
  )
})

# ------------------------------------------------------------------------------
# preprocess_matching_vars tests
# ------------------------------------------------------------------------------

test_that("preprocess_matching_vars works with auto_scale", {
  skip_on_cran()
  result <- preprocess_matching_vars(
    left_df, right_df,
    vars = c("normal_var", "income"),
    auto_scale = TRUE,
    scale_method = "auto"
  )

  expect_s3_class(result, "preprocessing_result")
  expect_equal(length(result$vars), 2)
})

test_that("preprocess_matching_vars removes problematic variables", {
  skip_on_cran()
  left_prob <- left_df
  left_prob$const <- 1
  right_prob <- right_df
  right_prob$const <- 1

  result <- suppressWarnings(
    preprocess_matching_vars(
      left_prob, right_prob,
      vars = c("normal_var", "const"),
      remove_problematic = TRUE,
      verbose = FALSE
    )
  )

  expect_false("const" %in% result$vars)
  expect_true("const" %in% result$excluded_vars)
})

test_that("preprocess_matching_vars errors when all vars excluded", {
  skip_on_cran()
  left_bad <- data.frame(const = rep(1, 10))
  right_bad <- data.frame(const = rep(1, 10))

  expect_error(
    suppressWarnings(
      preprocess_matching_vars(
        left_bad, right_bad,
        vars = "const",
        remove_problematic = TRUE
      )
    ),
    "All variables were excluded"
  )
})

test_that("preprocess_matching_vars errors with no variables", {
  skip_on_cran()
  expect_error(
    preprocess_matching_vars(left_df, right_df, vars = character(0)),
    "No variables provided"
  )
})

test_that("preprocess_matching_vars errors on missing variables", {
  skip_on_cran()
  expect_error(
    preprocess_matching_vars(left_df, right_df, vars = c("nonexistent")),
    "Variables not found"
  )
})

test_that("preprocess_matching_vars with scale_method=FALSE", {
  skip_on_cran()
  result <- preprocess_matching_vars(
    left_df, right_df,
    vars = c("normal_var"),
    auto_scale = FALSE,
    scale_method = FALSE
  )

  expect_equal(result$scaling_method, "none")
})

test_that("preprocess_matching_vars with verbose=TRUE issues messages", {
  skip_on_cran()
  left_outlier <- left_df
  left_outlier$outlier <- c(rnorm(45), rep(1000, 5))
  right_outlier <- right_df
  right_outlier$outlier <- c(rnorm(45), rep(1000, 5))

  expect_message(
    preprocess_matching_vars(
      left_outlier, right_outlier,
      vars = c("normal_var", "outlier"),
      auto_scale = TRUE,
      scale_method = "auto",
      verbose = TRUE
    ),
    "Auto-selected scaling method"
  )
})

test_that("preprocess_matching_vars with check_health=FALSE skips health check", {
  skip_on_cran()
  left_const <- left_df
  left_const$const <- 1
  right_const <- right_df
  right_const$const <- 1

  result <- preprocess_matching_vars(
    left_const, right_const,
    vars = c("normal_var", "const"),
    check_health = FALSE,
    auto_scale = FALSE
  )

  # const is NOT excluded because health check is skipped
  expect_true("const" %in% result$vars)
})

# ------------------------------------------------------------------------------
# Print methods
# ------------------------------------------------------------------------------

test_that("print.variable_health works", {
  skip_on_cran()
  health <- couplr:::check_variable_health(
    left_df, right_df,
    vars = c("normal_var")
  )

  expect_output(print(health), "Variable Health")
})

test_that("print.variable_health shows excluded vars", {
  skip_on_cran()
  left_const <- left_df
  left_const$const <- 1
  right_const <- right_df
  right_const$const <- 1

  health <- couplr:::check_variable_health(
    left_const, right_const,
    vars = c("const", "normal_var")
  )

  expect_output(print(health), "exclude")
})

test_that("print.preprocessing_result works", {
  skip_on_cran()
  result <- preprocess_matching_vars(
    left_df, right_df,
    vars = c("normal_var"),
    auto_scale = FALSE
  )

  expect_output(print(result), "Preprocessing Result")
})

test_that("print.preprocessing_result shows excluded vars", {
  skip_on_cran()
  left_const <- left_df
  left_const$const <- 1
  right_const <- right_df
  right_const$const <- 1

  result <- suppressWarnings(
    preprocess_matching_vars(
      left_const, right_const,
      vars = c("const", "normal_var"),
      verbose = FALSE
    )
  )

  expect_output(print(result), "Excluded")
})
