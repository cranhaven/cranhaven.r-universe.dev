# ==============================================================================
# Tests for Coarsened Exact Matching
# ==============================================================================

test_that("cem_match basic usage works", {
  set.seed(42)
  left <- data.frame(
    id = 1:20,
    age = rnorm(20, 40, 10),
    income = rnorm(20, 50000, 10000)
  )
  right <- data.frame(
    id = 21:60,
    age = rnorm(40, 42, 10),
    income = rnorm(40, 52000, 10000)
  )

  result <- cem_match(left, right, vars = c("age", "income"))

  expect_s3_class(result, "cem_result")
  expect_s3_class(result, "couplr_result")
  expect_true(all(c("id", "side", "stratum", "weight") %in%
                    names(result$matched)))
  expect_true(result$info$n_matched_strata > 0)
})


test_that("cem_match with custom cutpoints works", {
  left <- data.frame(id = 1:10, age = c(20, 25, 30, 35, 40,
                                          45, 50, 55, 60, 65))
  right <- data.frame(id = 11:20, age = c(22, 27, 32, 37, 42,
                                            47, 52, 57, 62, 67))

  result <- cem_match(left, right, vars = "age",
                      cutpoints = list(age = c(30, 50)))

  expect_s3_class(result, "cem_result")
  expect_true(result$info$n_matched_strata > 0)
})


test_that("cem_match integer n_bins works", {
  set.seed(42)
  left <- data.frame(id = 1:20, x = rnorm(20))
  right <- data.frame(id = 21:40, x = rnorm(20))

  result <- cem_match(left, right, vars = "x", n_bins = 3)
  expect_s3_class(result, "cem_result")
})


test_that("cem_match with grouping variable works", {
  set.seed(42)
  left <- data.frame(
    id = 1:20,
    age = rnorm(20, 40, 10),
    site = rep(c("A", "B"), 10)
  )
  right <- data.frame(
    id = 21:60,
    age = rnorm(40, 42, 10),
    site = rep(c("A", "B"), 20)
  )

  result <- cem_match(left, right, vars = "age", grouping = "site")

  expect_s3_class(result, "cem_result")
  # Strata should incorporate the site variable
  expect_true(result$info$n_strata > 0)
})


test_that("cem_match weights are correct", {
  # Simple case: 2 left and 4 right in same stratum
  left <- data.frame(id = 1:2, x = c(1.0, 1.5))
  right <- data.frame(id = 3:6, x = c(1.1, 1.2, 1.3, 1.4))

  # Use wide bins to ensure all fall in same stratum
  result <- cem_match(left, right, vars = "x", n_bins = 1)

  matched <- result$matched
  left_matched <- matched[matched$side == "left" & matched$weight > 0, ]
  right_matched <- matched[matched$side == "right" & matched$weight > 0, ]

  # Left should have weight 1
  expect_true(all(left_matched$weight == 1))

  # Right should have weight n_left/n_right = 2/4 = 0.5
  expect_true(all(abs(right_matched$weight - 0.5) < 1e-10))
})


test_that("cem_match keep='matched' drops unmatched", {
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 100, 200))
  right <- data.frame(id = 6:10, x = c(1.5, 2.5, 3.5, 4.5, 5.5))

  result_all <- cem_match(left, right, vars = "x", keep = "all")
  result_matched <- cem_match(left, right, vars = "x", keep = "matched")

  # 'matched' should have fewer or equal rows

  expect_true(nrow(result_matched$matched) <= nrow(result_all$matched))
  # 'matched' should have no zero weights
  expect_true(all(result_matched$matched$weight > 0))
})


test_that("cem_match strata_summary is correct", {
  set.seed(42)
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))

  result <- cem_match(left, right, vars = "x")

  expect_true(all(c("stratum", "n_left", "n_right", "matched") %in%
                    names(result$strata_summary)))
  # Total counts should match
  expect_equal(sum(result$strata_summary$n_left), nrow(left))
  expect_equal(sum(result$strata_summary$n_right), nrow(right))
})


test_that("cem_match print method works", {
  set.seed(42)
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))

  result <- cem_match(left, right, vars = "x")
  expect_output(print(result), "Coarsened Exact Matching")
})


test_that("cem_match validates inputs", {
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)

  expect_error(cem_match("not_df", right, vars = "x"),
               "must be data frames")
  expect_error(cem_match(left, right, vars = NULL),
               "vars must be specified")
  expect_error(cem_match(left, right, vars = "x", keep = "invalid"),
               "keep must be")
  expect_error(cem_match(left, right, vars = "missing_var"),
               "not found in left")
})


test_that("cem_match with categorical variables works", {
  left <- data.frame(
    id = 1:10,
    x = factor(c("A", "A", "B", "B", "C", "A", "B", "C", "A", "B"))
  )
  right <- data.frame(
    id = 11:20,
    x = factor(c("A", "B", "C", "A", "B", "C", "A", "B", "A", "C"))
  )

  result <- cem_match(left, right, vars = "x")
  expect_s3_class(result, "cem_result")
  expect_true(result$info$n_matched_strata > 0)
})
