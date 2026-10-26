# Tests for propensity score matching (Feature 5)

test_that("ps_match works with formula", {
  set.seed(42)
  n <- 100
  data <- data.frame(
    id = seq_len(n),
    treated = rbinom(n, 1, 0.4),
    age = rnorm(n, 50, 10),
    income = rnorm(n, 50000, 15000)
  )
  result <- ps_match(treated ~ age + income, data = data, treatment = "treated",
                     check_costs = FALSE)

  expect_s3_class(result, "matching_result")
  expect_true(nrow(result$pairs) > 0)
  expect_true(!is.null(result$info$caliper_value))
  expect_true(!is.null(result$info$ps_model))
})

test_that("ps_match works with pre-fitted model", {
  set.seed(42)
  n <- 100
  data <- data.frame(
    id = seq_len(n),
    treated = rbinom(n, 1, 0.4),
    age = rnorm(n, 50, 10)
  )
  model <- glm(treated ~ age, data = data, family = binomial())
  result <- ps_match(data = data, treatment = "treated", ps_model = model,
                     check_costs = FALSE)

  expect_s3_class(result, "matching_result")
  expect_true(nrow(result$pairs) > 0)
})

test_that("ps_match validates inputs", {
  expect_error(ps_match(formula = y ~ x), "data must be provided")
  expect_error(ps_match(formula = y ~ x, data = data.frame(x = 1)),
               "treatment column name must be specified")
  expect_error(ps_match(formula = y ~ x, data = data.frame(x = 1),
                        treatment = "z"),
               "not found in data")
})

test_that("ps_match validates treatment column", {
  data <- data.frame(id = 1:5, trt = c(0, 1, 2, 0, 1), x = rnorm(5))
  expect_error(ps_match(trt ~ x, data = data, treatment = "trt"),
               "binary")
})

test_that("ps_match validates caliper_sd", {
  data <- data.frame(id = 1:10, trt = c(rep(0, 5), rep(1, 5)), x = rnorm(10))
  expect_error(ps_match(trt ~ x, data = data, treatment = "trt",
                        caliper_sd = -1),
               "caliper_sd must be a positive number")
})

test_that("ps_match stores caliper info", {
  set.seed(123)
  n <- 50
  data <- data.frame(
    id = seq_len(n),
    treated = rbinom(n, 1, 0.5),
    x = rnorm(n)
  )
  result <- ps_match(treated ~ x, data = data, treatment = "treated",
                     caliper_sd = 0.3, check_costs = FALSE)

  expect_equal(result$info$caliper_sd, 0.3)
  expect_true(result$info$caliper_value > 0)
})

test_that("ps_match works with logical treatment", {
  set.seed(42)
  n <- 60
  data <- data.frame(
    id = seq_len(n),
    treated = sample(c(TRUE, FALSE), n, replace = TRUE),
    x = rnorm(n)
  )
  result <- ps_match(treated ~ x, data = data, treatment = "treated",
                     check_costs = FALSE)

  expect_s3_class(result, "matching_result")
})

test_that("ps_match works with replace and ratio", {
  set.seed(42)
  n <- 100
  data <- data.frame(
    id = seq_len(n),
    treated = rbinom(n, 1, 0.3),
    x = rnorm(n)
  )
  result <- ps_match(treated ~ x, data = data, treatment = "treated",
                     replace = TRUE, ratio = 2L, check_costs = FALSE)

  expect_s3_class(result, "matching_result")
})
