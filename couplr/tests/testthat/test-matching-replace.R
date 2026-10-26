# Tests for replacement matching (Feature 3)

test_that("replace = TRUE allows reuse of right units", {
  left <- data.frame(id = 1:5, x = c(1, 1.1, 1.2, 5, 6))
  right <- data.frame(id = 6:8, x = c(1, 5, 6))
  result <- match_couples(left, right, vars = "x", replace = TRUE,
                          return_diagnostics = TRUE, check_costs = FALSE)

  # Multiple left units can match to the same right unit
  expect_true(nrow(result$pairs) >= 3)
  # Right unit with x=1 should be matched by multiple left units
  right_counts <- table(result$pairs$right_id)
  expect_true(any(right_counts > 1))
})

test_that("replace = FALSE gives 1:1 matching (default)", {
  left <- data.frame(id = 1:5, x = c(1, 1.1, 1.2, 5, 6))
  right <- data.frame(id = 6:10, x = c(1, 2, 3, 5, 6))
  result <- match_couples(left, right, vars = "x", check_costs = FALSE)

  # Each right unit matched at most once
  right_counts <- table(result$pairs$right_id)
  expect_true(all(right_counts <= 1))
})

test_that("replace with max_distance still filters", {
  left <- data.frame(id = 1:3, x = c(1, 2, 100))
  right <- data.frame(id = 4:5, x = c(1.1, 2.1))
  result <- match_couples(left, right, vars = "x", replace = TRUE,
                          max_distance = 1, return_diagnostics = TRUE)

  # Unit with x=100 should be unmatched
  expect_true(nrow(result$pairs) == 2)
})

test_that("replace works with greedy matching", {
  left <- data.frame(id = 1:4, x = c(1, 1.1, 5, 6))
  right <- data.frame(id = 5:6, x = c(1, 6))
  result <- match_couples(left, right, vars = "x", replace = TRUE, method = "greedy")

  expect_true(nrow(result$pairs) >= 2)
})

test_that("replace = TRUE + ratio > 1 gives k best per left", {
  left <- data.frame(id = 1:2, x = c(1, 5))
  right <- data.frame(id = 3:7, x = c(0.9, 1.0, 1.1, 4.9, 5.1))
  result <- match_couples(left, right, vars = "x", replace = TRUE,
                          ratio = 2L, return_diagnostics = TRUE)

  # Each left unit should get up to 2 matches
  left_counts <- table(result$pairs$left_id)
  expect_true(all(left_counts <= 2))
  expect_true(nrow(result$pairs) >= 2)
})

test_that("replace stores info$replace = TRUE", {
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 1:3 + 0.1)
  result <- match_couples(left, right, vars = "x", replace = TRUE,
                          return_diagnostics = TRUE)

  expect_true(isTRUE(result$info$replace))
})

test_that("invalid replace parameter errors", {
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)
  expect_error(match_couples(left, right, vars = "x", replace = "yes"),
               "replace must be TRUE or FALSE")
})
