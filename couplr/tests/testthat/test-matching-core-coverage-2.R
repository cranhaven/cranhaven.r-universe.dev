# ==============================================================================
# Additional tests for matching_core.R coverage
# ==============================================================================

# Basic test data
test_left <- tibble::tibble(
  id = 1:10,
  age = c(25, 30, 35, 40, 45, 50, 55, 60, 65, 70),
  income = c(30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000, 110000, 120000)
)

test_right <- tibble::tibble(
  id = 101:110,
  age = c(27, 32, 37, 42, 47, 52, 57, 62, 67, 72),
  income = c(32000, 42000, 52000, 62000, 72000, 82000, 92000, 102000, 112000, 122000)
)

# ------------------------------------------------------------------------------
# match_couples edge cases
# ------------------------------------------------------------------------------

test_that("match_couples handles single row", {
  skip_on_cran()
  left <- test_left[1, ]
  right <- test_right[1, ]

  result <- suppressWarnings(match_couples(left, right, vars = c("age", "income")))

  expect_s3_class(result, "matching_result")
  expect_equal(nrow(result$pairs), 1)
})

test_that("match_couples handles different scaling methods", {
  skip_on_cran()
  for (scale in c("none", "standardize", "range", "robust")) {
    result <- match_couples(
      test_left, test_right,
      vars = c("age", "income"),
      scale = scale
    )

    expect_s3_class(result, "matching_result")
    expect_equal(nrow(result$pairs), nrow(test_left))
  }
})

test_that("match_couples handles max_distance constraint", {
  skip_on_cran()
  # Reasonable max_distance should work
  result <- match_couples(
    test_left, test_right,
    vars = c("age", "income"),
    auto_scale = TRUE,
    max_distance = 2.0
  )

  expect_s3_class(result, "matching_result")
  # Should have some matches
  expect_true(nrow(result$pairs) > 0)
})

test_that("match_couples handles method parameter", {
  skip_on_cran()
  for (method in c("jv", "hungarian", "auction")) {
    result <- match_couples(
      test_left[1:5, ], test_right[1:5, ],
      vars = c("age", "income"),
      method = method
    )

    expect_s3_class(result, "matching_result")
  }
})

test_that("match_couples errors on NA in data", {
  skip_on_cran()
  left_na <- test_left
  left_na$age[3] <- NA

  # Should error on NA values
  expect_error(
    match_couples(
      left_na, test_right,
      vars = c("age", "income"),
      auto_scale = TRUE
    ),
    "NA|missing"
  )
})

test_that("match_couples handles weights parameter", {
  skip_on_cran()
  result <- match_couples(
    test_left, test_right,
    vars = c("age", "income"),
    weights = c(age = 2, income = 1)
  )

  expect_s3_class(result, "matching_result")
})

test_that("match_couples with return_diagnostics = TRUE", {
  skip_on_cran()
  result <- match_couples(
    test_left, test_right,
    vars = c("age", "income"),
    return_diagnostics = TRUE
  )

  expect_s3_class(result, "matching_result")
  # Check that result contains expected fields
  expect_true("pairs" %in% names(result))
  expect_true("info" %in% names(result))
})

# ------------------------------------------------------------------------------
# greedy matching tests
# ------------------------------------------------------------------------------

test_that("greedy matching works with different strategies", {
  skip_on_cran()
  for (strategy in c("sorted", "row_best", "pq")) {
    result <- match_couples(
      test_left, test_right,
      vars = c("age", "income"),
      strategy = strategy
    , method = "greedy")

    expect_s3_class(result, "matching_result")
  }
})

test_that("greedy matching handles default settings", {
  skip_on_cran()
  result <- match_couples(
    test_left, test_right,
    vars = c("age", "income")
  , method = "greedy")

  expect_s3_class(result, "matching_result")
  expect_true(nrow(result$pairs) > 0)
})

test_that("greedy matching handles auto_scale", {
  skip_on_cran()
  result <- match_couples(
    test_left, test_right,
    vars = c("age", "income"),
    auto_scale = TRUE
  , method = "greedy")

  expect_s3_class(result, "matching_result")
})

# ------------------------------------------------------------------------------
# Distance object input
# ------------------------------------------------------------------------------

test_that("match_couples accepts distance_object input", {
  skip_on_cran()
  skip_if_not("compute_distances" %in% ls("package:couplr"))

  dist_obj <- compute_distances(test_left, test_right, vars = c("age", "income"))

  result <- match_couples(dist_obj)

  expect_s3_class(result, "matching_result")
})

# ------------------------------------------------------------------------------
# Print and summary methods
# ------------------------------------------------------------------------------

test_that("print.matchmaker_result works", {
  skip_on_cran()
  result <- match_couples(
    test_left, test_right,
    vars = c("age", "income")
  )

  output <- capture.output(print(result))
  expect_true(any(grepl("Matching Result", output)))
})

test_that("summary.matchmaker_result works", {
  skip_on_cran()
  result <- match_couples(
    test_left, test_right,
    vars = c("age", "income")
  )

  summary_result <- summary(result)
  expect_true(is.list(summary_result))
})
