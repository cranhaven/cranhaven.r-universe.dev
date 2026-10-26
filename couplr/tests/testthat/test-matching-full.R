# ==============================================================================
# Tests for Full Matching
# ==============================================================================

test_that("full_match basic usage works", {
  set.seed(42)
  left <- data.frame(id = 1:5, age = c(25, 35, 45, 55, 65))
  right <- data.frame(id = 6:20, age = runif(15, 20, 70))

  result <- full_match(left, right, vars = "age")

  expect_s3_class(result, "full_matching_result")
  expect_s3_class(result, "couplr_result")
  expect_true(result$info$n_groups > 0)
  expect_true(result$info$n_groups <= nrow(left))
  expect_equal(result$info$n_left, nrow(left))
  expect_equal(result$info$n_right, nrow(right))
  expect_true(all(c("group_id", "id", "side", "weight") %in% names(result$groups)))
})


test_that("full_match assigns all units when no caliper", {
  set.seed(42)
  left <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:8, x = c(1.1, 2.1, 3.1, 4.1, 5.1))

  result <- full_match(left, right, vars = "x")

  # All left units should be matched

  expect_equal(result$info$n_unmatched_left, 0)
  # All right units should be matched (assigned to groups)
  expect_equal(result$info$n_unmatched_right, 0)
  # Total matched should equal n_left + n_right
  matched_total <- nrow(result$groups)
  expect_equal(matched_total, 8)
})


test_that("full_match respects caliper", {
  left <- data.frame(id = 1:3, x = c(1, 5, 100))
  right <- data.frame(id = 4:6, x = c(1.1, 5.1, 6))

  result <- full_match(left, right, vars = "x", caliper = 2)

  # Unit with x=100 should be unmatched
  expect_true(result$info$n_unmatched_left > 0)
})


test_that("full_match respects max_controls", {
  set.seed(42)
  left <- data.frame(id = 1:2, x = c(1, 10))
  right <- data.frame(id = 3:12, x = c(1.1, 1.2, 1.3, 1.4, 1.5,
                                        10.1, 10.2, 10.3, 10.4, 10.5))

  result <- full_match(left, right, vars = "x", max_controls = 3)

  # No group should have more than 3 right units
  right_per_group <- table(result$groups$group_id[result$groups$side == "right"])
  expect_true(all(right_per_group <= 3))
})


test_that("full_match weights are correct", {
  left <- data.frame(id = 1:2, x = c(1, 10))
  right <- data.frame(id = 3:6, x = c(1.1, 1.2, 10.1, 10.2))

  result <- full_match(left, right, vars = "x")

  # Left units always get weight 1
  left_weights <- result$groups$weight[result$groups$side == "left"]
  expect_true(all(left_weights == 1))

  # Right weights within a group should sum to 1 (equal to left weight)
  for (g in unique(result$groups$group_id)) {
    grp <- result$groups[result$groups$group_id == g, ]
    right_w <- sum(grp$weight[grp$side == "right"])
    left_w <- sum(grp$weight[grp$side == "left"])
    expect_equal(right_w, left_w, tolerance = 1e-10)
  }
})


test_that("full_match with caliper_sd works", {
  set.seed(42)
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:30, x = rnorm(20))

  result <- full_match(left, right, vars = "x", caliper_sd = 1)
  expect_s3_class(result, "full_matching_result")
})


test_that("full_match with multivariate works", {
  set.seed(42)
  left <- data.frame(id = 1:10, x = rnorm(10), y = rnorm(10))
  right <- data.frame(id = 11:30, x = rnorm(20), y = rnorm(20))

  result <- full_match(left, right, vars = c("x", "y"))
  expect_s3_class(result, "full_matching_result")
  expect_true(result$info$n_groups > 0)
})


test_that("full_match with auto_scale works", {
  set.seed(42)
  left <- data.frame(
    id = 1:10,
    x = rnorm(10, 0, 1),
    y = rnorm(10, 50000, 10000)
  )
  right <- data.frame(
    id = 11:30,
    x = rnorm(20, 0, 1),
    y = rnorm(20, 50000, 10000)
  )

  result <- full_match(left, right, vars = c("x", "y"), auto_scale = TRUE)
  expect_s3_class(result, "full_matching_result")
})


test_that("full_match print method works", {
  set.seed(42)
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:15, x = rnorm(10))

  result <- full_match(left, right, vars = "x")
  expect_output(print(result), "Full Matching Result")
})


test_that("full_match validates inputs", {
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)

  expect_error(full_match("not_df", right, vars = "x"),
               "must be data frames")
  expect_error(full_match(left, right, vars = NULL),
               "vars must be specified")
  expect_error(full_match(left, right, vars = "x", min_controls = 0),
               "positive integer")
  expect_error(full_match(left, right, vars = "x",
                          min_controls = 5, max_controls = 2),
               "max_controls must be >= min_controls")
})


test_that("full_match min_controls filters small groups", {
  left <- data.frame(id = 1:3, x = c(1, 50, 100))
  right <- data.frame(id = 4:6, x = c(1.1, 50.1, 50.2))

  result <- full_match(left, right, vars = "x", min_controls = 2)

  # Groups with only 1 right unit should be dropped
  if (result$info$n_groups > 0) {
    right_per_group <- table(
      result$groups$group_id[result$groups$side == "right"]
    )
    expect_true(all(right_per_group >= 2))
  }
})
