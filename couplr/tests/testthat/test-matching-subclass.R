# ==============================================================================
# Tests for Subclassification
# ==============================================================================

test_that("subclass_match basic usage works", {
  set.seed(42)
  n <- 200
  data <- data.frame(
    id = 1:n,
    age = rnorm(n, 40, 10),
    income = rnorm(n, 50000, 15000)
  )
  data$treatment <- rbinom(n, 1, plogis(-2 + 0.05 * data$age))

  result <- subclass_match(
    treatment ~ age + income, data, treatment = "treatment"
  )

  expect_s3_class(result, "subclass_result")
  expect_s3_class(result, "couplr_result")
  expect_true(all(c("id", "side", "subclass", "ps", "weight") %in%
                    names(result$matched)))
  expect_equal(result$info$n_subclasses, 5)
  expect_equal(result$info$estimand, "ATT")
})


test_that("subclass_match with custom n_subclasses works", {
  set.seed(42)
  n <- 200
  data <- data.frame(
    id = 1:n,
    x = rnorm(n)
  )
  data$treatment <- rbinom(n, 1, plogis(data$x))

  result <- subclass_match(
    treatment ~ x, data, treatment = "treatment",
    n_subclasses = 10
  )

  expect_true(result$info$n_subclasses <= 10)
})


test_that("subclass_match with pre-computed PS works", {
  set.seed(42)
  n <- 100
  data <- data.frame(
    id = 1:n,
    x = rnorm(n)
  )
  data$treatment <- rbinom(n, 1, plogis(data$x))

  # Pre-compute PS
  ps <- plogis(data$x + rnorm(n, 0, 0.1))

  result <- subclass_match(
    data = data, treatment = "treatment", ps = ps
  )

  expect_s3_class(result, "subclass_result")
  expect_true(all(result$matched$ps > 0))
})


test_that("subclass_match estimand affects weights", {
  set.seed(42)
  n <- 200
  data <- data.frame(
    id = 1:n,
    x = rnorm(n)
  )
  data$treatment <- rbinom(n, 1, plogis(data$x))

  result_att <- subclass_match(
    treatment ~ x, data, treatment = "treatment", estimand = "ATT"
  )
  result_ate <- subclass_match(
    treatment ~ x, data, treatment = "treatment", estimand = "ATE"
  )
  result_atc <- subclass_match(
    treatment ~ x, data, treatment = "treatment", estimand = "ATC"
  )

  expect_equal(result_att$info$estimand, "ATT")
  expect_equal(result_ate$info$estimand, "ATE")
  expect_equal(result_atc$info$estimand, "ATC")

  # ATT: treated units should have weight 1
  att_treated <- result_att$matched[result_att$matched$side == "left" &
                                      result_att$matched$weight > 0, ]
  expect_true(all(att_treated$weight == 1))

  # ATC: control units should have weight 1
  atc_control <- result_atc$matched[result_atc$matched$side == "right" &
                                      result_atc$matched$weight > 0, ]
  expect_true(all(atc_control$weight == 1))
})


test_that("subclass_match subclass_summary is correct", {
  set.seed(42)
  n <- 200
  data <- data.frame(id = 1:n, x = rnorm(n))
  data$treatment <- rbinom(n, 1, plogis(data$x))

  result <- subclass_match(
    treatment ~ x, data, treatment = "treatment"
  )

  ss <- result$subclass_summary
  expect_true(all(c("subclass", "n_treated", "n_control", "mean_ps",
                     "has_overlap") %in% names(ss)))
  expect_equal(sum(ss$n_treated) + sum(ss$n_control), n)
})


test_that("subclass_match print method works", {
  set.seed(42)
  n <- 100
  data <- data.frame(id = 1:n, x = rnorm(n))
  data$treatment <- rbinom(n, 1, plogis(data$x))

  result <- subclass_match(
    treatment ~ x, data, treatment = "treatment"
  )
  expect_output(print(result), "Subclassification Result")
})


test_that("subclass_match validates inputs", {
  data <- data.frame(id = 1:10, x = rnorm(10), trt = rbinom(10, 1, 0.5))

  expect_error(subclass_match(data = NULL, treatment = "trt"),
               "data must be provided")
  expect_error(subclass_match(data = data, treatment = "missing"),
               "not found in data")
  expect_error(subclass_match(data = data, treatment = "trt",
                               n_subclasses = 1),
               "n_subclasses must be")
  expect_error(subclass_match(data = data, treatment = "trt",
                               estimand = "invalid"),
               "estimand must be")
  expect_error(subclass_match(data = data, treatment = "trt"),
               "One of formula, ps, or ps_model")
})


test_that("subclass_match handles non-binary treatment", {
  data <- data.frame(id = 1:10, x = rnorm(10), trt = rep(c(0, 1, 2), length.out = 10))

  expect_error(
    subclass_match(trt ~ x, data = data, treatment = "trt"),
    "binary"
  )
})


test_that("subclass_match with pre-fitted model works", {
  set.seed(42)
  n <- 100
  data <- data.frame(id = 1:n, x = rnorm(n))
  data$treatment <- rbinom(n, 1, plogis(data$x))

  model <- glm(treatment ~ x, data = data, family = binomial())
  result <- subclass_match(
    data = data, treatment = "treatment", ps_model = model
  )

  expect_s3_class(result, "subclass_result")
})
