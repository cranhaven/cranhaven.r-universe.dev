# ==============================================================================
# Tests for match_data() Output Layer
# ==============================================================================

test_that("match_data works for matching_result", {
  set.seed(42)
  left <- data.frame(id = 1:5, age = c(25, 35, 45, 55, 65))
  right <- data.frame(id = 6:15, age = runif(10, 20, 70))

  result <- match_couples(left, right, vars = "age")
  md <- match_data(result, left, right)

  expect_s3_class(md, "tbl_df")
  expect_true(all(c("treatment", "weights") %in% names(md)))
  expect_true(all(md$treatment %in% c(0L, 1L)))
  expect_true(all(md$weights > 0))
  # Should have both treated and control

  expect_true(any(md$treatment == 1))
  expect_true(any(md$treatment == 0))
})


test_that("match_data works for full_matching_result", {
  set.seed(42)
  left <- data.frame(id = 1:5, age = c(25, 35, 45, 55, 65))
  right <- data.frame(id = 6:15, age = runif(10, 20, 70))

  result <- full_match(left, right, vars = "age")
  md <- match_data(result, left, right)

  expect_s3_class(md, "tbl_df")
  expect_true(all(c("treatment", "weights", "subclass") %in% names(md)))
  expect_true(any(md$treatment == 1))
  expect_true(any(md$treatment == 0))
})


test_that("match_data works for cem_result", {
  set.seed(42)
  left <- data.frame(id = 1:20, age = rnorm(20, 40, 10))
  right <- data.frame(id = 21:40, age = rnorm(20, 42, 10))

  result <- cem_match(left, right, vars = "age")
  md <- match_data(result, left, right)

  expect_s3_class(md, "tbl_df")
  expect_true(all(c("treatment", "weights") %in% names(md)))
  # Only matched units (weight > 0)
  expect_true(all(md$weights > 0))
})


test_that("match_data works for subclass_result", {
  set.seed(42)
  n <- 200
  data <- data.frame(id = 1:n, x = rnorm(n))
  data$treatment <- rbinom(n, 1, plogis(data$x))

  result <- subclass_match(treatment ~ x, data, treatment = "treatment")
  md <- match_data(result, data = data)

  expect_s3_class(md, "tbl_df")
  expect_true(all(c("treatment", "weights") %in% names(md)))
  expect_true(all(md$weights > 0))
})


test_that("match_data returns empty tibble for empty matching_result", {
  set.seed(42)
  left <- data.frame(id = 1:5, age = c(25, 35, 45, 55, 65))
  right <- data.frame(id = 6:15, age = runif(10, 20, 70))

  # Create a result with no pairs by manually constructing one
  result <- match_couples(left, right, vars = "age")
  # Simulate empty pairs
  empty_result <- result
  empty_result$pairs <- result$pairs[0, ]
  md <- match_data(empty_result, left, right)

  expect_s3_class(md, "tbl_df")
  expect_equal(nrow(md), 0)
})
