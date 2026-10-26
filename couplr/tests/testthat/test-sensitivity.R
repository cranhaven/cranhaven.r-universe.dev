# Tests for Rosenbaum sensitivity analysis (Feature 7)

test_that("sensitivity_analysis returns correct structure", {
  set.seed(42)
  left <- data.frame(id = 1:20, x = rnorm(20), outcome = rnorm(20, 1, 1))
  right <- data.frame(id = 21:40, x = rnorm(20), outcome = rnorm(20, 0, 1))
  result <- match_couples(left, right, vars = "x")
  sens <- sensitivity_analysis(result, left, right, outcome_var = "outcome")

  expect_s3_class(sens, "sensitivity_analysis")
  expect_true(is.data.frame(sens$results))
  expect_true("gamma" %in% names(sens$results))
  expect_true("p_upper" %in% names(sens$results))
  expect_true("p_lower" %in% names(sens$results))
  expect_true(is.numeric(sens$n_pairs))
  expect_true(is.numeric(sens$critical_gamma))
})

test_that("sensitivity_analysis at gamma=1 gives standard Wilcoxon p-value", {
  set.seed(42)
  left <- data.frame(id = 1:30, x = rnorm(30),
                     outcome = rnorm(30, 2, 1))  # Strong effect
  right <- data.frame(id = 31:60, x = rnorm(30),
                      outcome = rnorm(30, 0, 1))
  result <- match_couples(left, right, vars = "x")
  sens <- sensitivity_analysis(result, left, right, outcome_var = "outcome",
                               gamma = 1)

  # At gamma=1, p_upper should equal p_lower (no hidden bias)
  expect_equal(sens$results$p_upper[1], sens$results$p_lower[1],
               tolerance = 0.01)
})

test_that("p_upper increases with gamma", {
  set.seed(42)
  left <- data.frame(id = 1:30, x = rnorm(30), outcome = rnorm(30, 1, 1))
  right <- data.frame(id = 31:60, x = rnorm(30), outcome = rnorm(30, 0, 1))
  result <- match_couples(left, right, vars = "x")
  sens <- sensitivity_analysis(result, left, right, outcome_var = "outcome",
                               gamma = c(1, 1.5, 2, 2.5, 3))

  # p_upper should be non-decreasing in gamma
  p_upper <- sens$results$p_upper
  for (i in 2:length(p_upper)) {
    expect_true(p_upper[i] >= p_upper[i - 1] - 1e-10)
  }
})

test_that("sensitivity_analysis validates inputs", {
  set.seed(42)
  left <- data.frame(id = 1:5, x = rnorm(5), outcome = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5), outcome = rnorm(5))
  result <- match_couples(left, right, vars = "x")

  expect_error(sensitivity_analysis(list(), left, right,
                                    outcome_var = "outcome"),
               "matching_result")
  expect_error(sensitivity_analysis(result, left, right,
                                    outcome_var = "nonexistent"),
               "not found")
  expect_error(sensitivity_analysis(result, left, right,
                                    outcome_var = "outcome",
                                    gamma = c(0.5, 1)),
               "gamma must be numeric with all values >= 1")
})

test_that("print.sensitivity_analysis produces output", {
  set.seed(42)
  left <- data.frame(id = 1:20, x = rnorm(20), outcome = rnorm(20, 1, 1))
  right <- data.frame(id = 21:40, x = rnorm(20), outcome = rnorm(20, 0, 1))
  result <- match_couples(left, right, vars = "x")
  sens <- sensitivity_analysis(result, left, right, outcome_var = "outcome")

  out <- capture.output(print(sens))
  expect_true(any(grepl("Rosenbaum", out)))
  expect_true(any(grepl("Gamma", out)))
})

test_that("summary.sensitivity_analysis works", {
  set.seed(42)
  left <- data.frame(id = 1:20, x = rnorm(20), outcome = rnorm(20, 1, 1))
  right <- data.frame(id = 21:40, x = rnorm(20), outcome = rnorm(20, 0, 1))
  result <- match_couples(left, right, vars = "x")
  sens <- sensitivity_analysis(result, left, right, outcome_var = "outcome")

  s <- summary(sens)
  expect_s3_class(s, "summary.sensitivity_analysis")
  expect_true(!is.null(s$critical_gamma))

  out <- capture.output(print(s))
  expect_true(any(grepl("Sensitivity", out)))
})

test_that("plot.sensitivity_analysis works", {
  set.seed(42)
  left <- data.frame(id = 1:20, x = rnorm(20), outcome = rnorm(20, 1, 1))
  right <- data.frame(id = 21:40, x = rnorm(20), outcome = rnorm(20, 0, 1))
  result <- match_couples(left, right, vars = "x")
  sens <- sensitivity_analysis(result, left, right, outcome_var = "outcome")

  expect_invisible(plot(sens))
})

test_that("alternative = 'less' works", {
  set.seed(42)
  left <- data.frame(id = 1:20, x = rnorm(20), outcome = rnorm(20, -1, 1))
  right <- data.frame(id = 21:40, x = rnorm(20), outcome = rnorm(20, 0, 1))
  result <- match_couples(left, right, vars = "x")
  sens <- sensitivity_analysis(result, left, right, outcome_var = "outcome",
                               alternative = "less")

  expect_equal(sens$alternative, "less")
})

test_that("alternative = 'two.sided' works", {
  set.seed(42)
  left <- data.frame(id = 1:20, x = rnorm(20), outcome = rnorm(20, 1, 1))
  right <- data.frame(id = 21:40, x = rnorm(20), outcome = rnorm(20, 0, 1))
  result <- match_couples(left, right, vars = "x")
  sens <- sensitivity_analysis(result, left, right, outcome_var = "outcome",
                               alternative = "two.sided")

  expect_equal(sens$alternative, "two.sided")
})
