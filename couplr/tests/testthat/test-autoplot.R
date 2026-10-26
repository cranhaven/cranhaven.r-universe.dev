# Tests for ggplot2 autoplot methods (Feature 2)

test_that("autoplot.matching_result returns ggplot for histogram", {
  skip_if_not_installed("ggplot2")

  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))
  result <- match_couples(left, right, vars = "x")

  p <- ggplot2::autoplot(result, type = "histogram")
  expect_s3_class(p, "gg")
})

test_that("autoplot.matching_result returns ggplot for density", {
  skip_if_not_installed("ggplot2")

  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))
  result <- match_couples(left, right, vars = "x")

  p <- ggplot2::autoplot(result, type = "density")
  expect_s3_class(p, "gg")
})

test_that("autoplot.matching_result returns ggplot for ecdf", {
  skip_if_not_installed("ggplot2")

  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))
  result <- match_couples(left, right, vars = "x")

  p <- ggplot2::autoplot(result, type = "ecdf")
  expect_s3_class(p, "gg")
})

test_that("autoplot.matching_result handles empty result", {
  skip_if_not_installed("ggplot2")

  left <- data.frame(id = 1:3, x = c(100, 200, 300))
  right <- data.frame(id = 4:6, x = c(1, 2, 3))
  # greedy matching returns empty result rather than erroring
  expect_warning(
    result <- match_couples(left, right, vars = "x", max_distance = 0.001,
                            method = "greedy"),
    "No valid pairs"
  )

  expect_message(ggplot2::autoplot(result), "No matched pairs")
})

test_that("autoplot.balance_diagnostics returns love plot", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  left <- data.frame(id = 1:20, x = rnorm(20, 0, 1), y = rnorm(20, 0, 1))
  right <- data.frame(id = 21:40, x = rnorm(20, 0.3, 1), y = rnorm(20, 0.2, 1))
  result <- match_couples(left, right, vars = c("x", "y"))
  bal <- balance_diagnostics(result, left, right, vars = c("x", "y"))

  p <- ggplot2::autoplot(bal, type = "love")
  expect_s3_class(p, "gg")
})

test_that("autoplot.balance_diagnostics returns histogram", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  left <- data.frame(id = 1:20, x = rnorm(20), y = rnorm(20))
  right <- data.frame(id = 21:40, x = rnorm(20), y = rnorm(20))
  result <- match_couples(left, right, vars = c("x", "y"))
  bal <- balance_diagnostics(result, left, right, vars = c("x", "y"))

  p <- ggplot2::autoplot(bal, type = "histogram")
  expect_s3_class(p, "gg")
})

test_that("autoplot.balance_diagnostics returns variance plot", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  left <- data.frame(id = 1:20, x = rnorm(20), y = rnorm(20))
  right <- data.frame(id = 21:40, x = rnorm(20), y = rnorm(20))
  result <- match_couples(left, right, vars = c("x", "y"))
  bal <- balance_diagnostics(result, left, right, vars = c("x", "y"))

  p <- ggplot2::autoplot(bal, type = "variance")
  expect_s3_class(p, "gg")
})

test_that("autoplot.sensitivity_analysis returns ggplot", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  left <- data.frame(id = 1:20, x = rnorm(20), outcome = rnorm(20, 1, 1))
  right <- data.frame(id = 21:40, x = rnorm(20), outcome = rnorm(20, 0, 1))
  result <- match_couples(left, right, vars = "x")
  sens <- sensitivity_analysis(result, left, right, outcome_var = "outcome")

  p <- ggplot2::autoplot(sens)
  expect_s3_class(p, "gg")
})
