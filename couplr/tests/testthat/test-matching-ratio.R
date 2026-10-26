# Tests for k:1 matching (Feature 4)

test_that("ratio = 1 gives standard 1:1 matching", {
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:15, x = seq(1, 5, length.out = 10))
  result <- match_couples(left, right, vars = "x", ratio = 1L)

  # Each left matched exactly once
  left_counts <- table(result$pairs$left_id)
  expect_true(all(left_counts == 1))
  expect_equal(nrow(result$pairs), 5)
})

test_that("ratio = 2 produces more matches than ratio = 1", {
  set.seed(42)
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:25, x = seq(0.5, 5.5, length.out = 20))

  result_1 <- match_couples(left, right, vars = "x", ratio = 1L)
  result_2 <- match_couples(left, right, vars = "x", ratio = 2L)

  # k:1 should produce more pairs (up to 2x)
  expect_true(nrow(result_2$pairs) > nrow(result_1$pairs))

  # Left units can appear up to 2 times
  left_counts <- table(result_2$pairs$left_id)
  expect_true(all(left_counts <= 2))
})

test_that("ratio stores in info", {
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:9, x = c(1.1, 1.9, 2.1, 2.9, 3.1, 3.9))
  result <- match_couples(left, right, vars = "x", ratio = 2L,
                          return_diagnostics = TRUE)

  expect_equal(result$info$ratio, 2L)
})

test_that("ratio with greedy matching produces more pairs", {
  set.seed(42)
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:25, x = seq(0.5, 5.5, length.out = 20))

  result_1 <- match_couples(left, right, vars = "x", ratio = 1L, method = "greedy")
  result_2 <- match_couples(left, right, vars = "x", ratio = 2L, method = "greedy")

  expect_true(nrow(result_2$pairs) > nrow(result_1$pairs))
})

test_that("ratio without replacement: each right used at most once", {
  set.seed(42)
  left <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:9, x = c(1.1, 1.9, 2.1, 2.9, 3.1, 3.9))
  result <- match_couples(left, right, vars = "x", ratio = 2L)

  # In k:1 without replacement, each right unit used at most once
  right_counts <- table(result$pairs$right_id)
  expect_true(all(right_counts <= 1))
})

test_that("invalid ratio errors", {
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)
  expect_error(match_couples(left, right, vars = "x", ratio = 0),
               "ratio must be a positive integer")
  expect_error(match_couples(left, right, vars = "x", ratio = -1),
               "ratio must be a positive integer")
})

test_that("ratio = 3 with replacement", {
  left <- data.frame(id = 1:2, x = c(1, 10))
  right <- data.frame(id = 3:7, x = c(0.5, 1.0, 1.5, 9.5, 10.5))
  result <- match_couples(left, right, vars = "x",
                          replace = TRUE, ratio = 3L,
                          return_diagnostics = TRUE)

  # Each left can get up to 3 matches
  left_counts <- table(result$pairs$left_id)
  expect_true(all(left_counts <= 3))
  expect_true(nrow(result$pairs) >= 2)
})
