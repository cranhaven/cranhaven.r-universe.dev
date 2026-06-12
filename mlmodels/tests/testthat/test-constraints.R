# tests/testthat/test-constraints.R
library(testthat)

# =============================================================================
# Tests for constraint parsing and constrained estimation in ml_lm
# 
# Covers:
# - Basic equality and inequality parsing
# - Mixed constraint rejection
# - Scaled coefficients (e.g. `* 2`, `/ 5`)
# - Proper storage in the model object
# - Enforcement of constraints during estimation
# =============================================================================

test_that("parse_constraints works with simple equality constraints", {
  data(mtcars)
  fit <- ml_lm(mpg ~ wt + hp + qsec, scale = ~ wt, data = mtcars)
  coef_names <- names(coef(fit))
  
  cons <- c("value::wt = 0",
            "scale::wt = -0.5",
            "value::hp + value::wt = 1")
  
  result <- .parse_constraints(cons, coef_names)
  
  expect_type(result, "list")
  expect_named(result, c("names", "strings", "maxLik"))
  expect_true(!is.null(result$maxLik$eqA))
  expect_true(!is.null(result$maxLik$eqB))
  expect_equal(nrow(result$maxLik$eqA), 3)
})

test_that("parse_constraints correctly handles inequality constraints", {
  data(mtcars)
  fit <- ml_lm(mpg ~ wt + hp, scale = ~ wt, data = mtcars)
  coef_names <- names(coef(fit))
  
  cons <- c("value::wt <= -2",
            "scale::wt >= 0.1")
  
  result <- .parse_constraints(cons, coef_names)
  
  expect_true(!is.null(result$maxLik$ineqA))
  expect_true(!is.null(result$maxLik$ineqB))
  expect_null(result$maxLik$eqA)
})

test_that("parse_constraints rejects mixed equality and inequality constraints", {
  data(mtcars)
  fit <- ml_lm(mpg ~ wt + hp, data = mtcars)
  coef_names <- names(coef(fit))
  
  cons <- c("value::wt = 0",
            "value::hp >= 0.1")
  
  expect_error(
    .parse_constraints(cons, coef_names),
    class = "rlang_error"   # or just check for part of the message
  )
  
  # Or more specifically:
  expect_error(
    .parse_constraints(cons, coef_names),
    regexp = "Cannot mix equality and inequality constraints",
    class = "rlang_error"
  )
})

test_that("parse_constraints correctly parses equality constraints with scale coefficients", {
  data(mtcars)
  
  fit <- ml_lm(mpg ~ wt + hp, 
               scale = ~ wt, 
               data = mtcars)
  
  coef_names <- names(coef(fit))
  
  cons <- c("value::wt = -4",
            "scale::wt = -0.1")
  
  result <- .parse_constraints(cons, coef_names)
  
  # Check structure
  expect_type(result, "list")
  expect_named(result, c("names", "strings", "maxLik"))
  
  # Check that eqA and eqB were created
  expect_true(!is.null(result$maxLik$eqA))
  expect_true(!is.null(result$maxLik$eqB))
  
  # Check that eqA is a matrix
  expect_true(is.matrix(result$maxLik$eqA))
  expect_equal(nrow(result$maxLik$eqA), 2)        # two constraints
  expect_equal(ncol(result$maxLik$eqA), length(coef_names))
})

test_that("constrained estimation works and stores constraints correctly", {
  data(mtcars)
  fit <- ml_lm(mpg ~ wt + hp + qsec,
               scale = ~ wt,
               data = mtcars,
               constraints = c("value::wt = -4",
                               "scale::wt = -0.1"),
               start = c(27, -3.05, 0, 0.5, 1, 0))
  
  expect_true(!is.null(fit$model$constraints))
  expect_equal(length(fit$model$constraints$strings), 2)
  expect_true(!is.null(fit$model$constraints$maxLik$eqA))
  
  # Check that constraints were actually respected (approximately)
  coefs <- coef(fit)
  expect_true(abs(coefs[["value::wt"]] + 4)   < 1e-4)
  expect_true(abs(coefs[["scale::wt"]] + 0.1) < 1e-4)
})


test_that("invalid constraints throw clear errors", {
  data(mtcars)
  
  expect_error(
    ml_lm(mpg ~ wt + hp, data = mtcars,
          constraints = c("value::wt = abc"),
          start = c(25, -3, 0, 1)),
    "Right-hand side must be numeric"
  )
  
  expect_error(
    ml_lm(mpg ~ wt + hp, data = mtcars,
          constraints = "value::wt = 0"),
    "Constrained optimization requires a vector of initial values"
  )
})