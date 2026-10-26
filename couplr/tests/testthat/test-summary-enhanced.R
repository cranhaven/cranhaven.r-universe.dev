# Tests for enhanced summary.matching_result (Feature 1)

test_that("summary includes match_rate", {
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:10, x = c(1.1, 2.2, 3.1, 4.2, 5.1))
  result <- match_couples(left, right, vars = "x", return_diagnostics = TRUE)

  s <- summary(result)
  expect_true(!is.null(s$match_rate))
  expect_true(s$match_rate > 0 && s$match_rate <= 1)
  # 5 matched from 5 left and 5 right => 100%
  expect_equal(s$match_rate, 1.0)
})

test_that("summary includes distance_percentiles", {
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))
  result <- match_couples(left, right, vars = "x", return_diagnostics = TRUE)

  s <- summary(result)
  expect_true(!is.null(s$distance_percentiles))
  expect_length(s$distance_percentiles, 7)  # 5%, 10%, 25%, 50%, 75%, 90%, 95%
  expect_named(s$distance_percentiles,
               c("5%", "10%", "25%", "50%", "75%", "90%", "95%"))
})

test_that("summary handles empty matches", {
  left <- data.frame(id = 1:3, x = c(100, 200, 300))
  right <- data.frame(id = 4:6, x = c(1, 2, 3))
  # greedy matching doesn't error on no valid pairs (strict_no_pairs = FALSE)
  expect_warning(
    result <- match_couples(left, right, vars = "x", max_distance = 0.001,
                            method = "greedy"),
    "No valid pairs"
  )

  s <- summary(result)
  expect_true(is.null(s$distance_percentiles))
  expect_true(is.na(s$match_rate) || s$match_rate == 0)
})

test_that("print.summary.matching_result displays match_rate", {
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:10, x = c(1.1, 2.2, 3.1, 4.2, 5.1))
  result <- match_couples(left, right, vars = "x", return_diagnostics = TRUE)

  s <- summary(result)
  out <- capture.output(print(s))
  expect_true(any(grepl("Match rate", out)))
  expect_true(any(grepl("100.0%", out)))
})

test_that("print.summary.matching_result displays percentiles", {
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))
  result <- match_couples(left, right, vars = "x", return_diagnostics = TRUE)

  s <- summary(result)
  out <- capture.output(print(s))
  expect_true(any(grepl("Percentile", out)))
})

test_that("summary includes replace and ratio fields", {
  left <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))
  result <- match_couples(left, right, vars = "x",
                          replace = TRUE, return_diagnostics = TRUE)

  s <- summary(result)
  expect_true(isTRUE(s$replace))
})
