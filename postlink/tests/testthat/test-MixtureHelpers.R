local_edition(3)

# ------------------------------------------------------------------------------
# Tests for constrained_logistic_regression
# ------------------------------------------------------------------------------

test_that("constrained_logistic_regression respects the intercept bound", {
 set.seed(123)
 n <- 100
 x <- cbind(1, rnorm(n))
 # Generate y such that the unconstrained intercept would be high
 y <- rbinom(n, 1, 0.8)

 # Set a restrictive bound (e.g., -1)
 bound <- -1
 fit <- postlink:::constrained_logistic_regression(x, y, bound = bound, cmax.iter = 100)

 # The mean of linear predictors (X %*% beta) should be constrained to the bound
 lp_mean <- mean(x %*% fit$beta)
 expect_equal(lp_mean, bound, tolerance = 1e-5)
})

test_that("constrained_logistic_regression converges for standard data", {
 set.seed(456)
 n <- 200
 x <- cbind(1, rnorm(n), rbinom(n, 1, 0.5))
 true_beta <- c(0.5, 1.2, -0.8)
 y <- rbinom(n, 1, plogis(x %*% true_beta))

 # Set a loose bound that should not be active
 bound <- 5

 # Use suppressWarnings here to ignore the 0/1 boundary warnings
 fit <- suppressWarnings(
  postlink:::constrained_logistic_regression(x, y, bound = bound, cmax.iter = 500)
 )

 expect_length(fit$beta, 3)
 expect_true(all(!is.na(fit$beta)))

 # Objective function (negative log-likelihood) should generally decrease
 valid_objs <- fit$objs[fit$objs > 0]
 expect_true(valid_objs[length(valid_objs)] <= valid_objs[1])
})

test_that("constrained_logistic_regression handles fitted probabilities near 0 or 1", {
 # Force the initial probability to be below the 1e-10 cutoff
 # plogis(-25) is approx 1.3e-11, which triggers 'flag0'
 x <- matrix(c(1, 0), nrow = 1)
 y <- 1 # Mismatch: The response is 1, but predicted prob is near 0

 # The warning should fire on the very first log-likelihood evaluation
 expect_warning(
  postlink:::constrained_logistic_regression(x, y, bound = -25, cmax.iter = 1),
  "Fitted probabilities"
 )
})

# ------------------------------------------------------------------------------
# Tests for calc_breslow
# ------------------------------------------------------------------------------

test_that("calc_breslow computes correct baseline hazard increments", {
 # Small controlled example
 y <- c(1, 2, 3, 4)
 status <- c(1, 0, 1, 1) # events at 1, 3, 4
 weights <- rep(1, 4)
 lp <- rep(0, 4) # risk scores = exp(0) = 1

 fit <- postlink:::calc_breslow(y, status, weights, lp)

 # Check components
 expect_equal(fit$times, 1:4)
 expect_length(fit$cumhaz, 4)

 # At t=1: 1 event / 4 at risk = 0.25
 # At t=2: 0 events / 3 at risk = 0 increment
 # At t=3: 1 event / 2 at risk = 0.5 increment (cum = 0.75)
 # At t=4: 1 event / 1 at risk = 1.0 increment (cum = 1.75)
 expect_equal(fit$cumhaz, c(0.25, 0.25, 0.75, 1.75))
})

test_that("calc_breslow handles observation weights", {
 y <- c(1, 1, 2)
 status <- c(1, 1, 1)
 # Weighting second observation less
 weights <- c(1, 0.5, 1)
 lp <- rep(0, 3)

 fit <- postlink:::calc_breslow(y, status, weights, lp)

 # Unique times should be 1, 2
 expect_equal(fit$times, c(1, 2))

 # At t=1: events = 1+0.5=1.5; at risk = 1+0.5+1=2.5. Haz = 1.5/2.5 = 0.6
 # At t=2: events = 1; at risk = 1. Haz increment = 1/1 = 1.0 (cum = 1.6)
 expect_equal(fit$cumhaz, c(0.6, 1.6))
})

test_that("calc_breslow calculates density increments", {
 y <- c(10, 20, 30)
 status <- c(1, 1, 1)
 weights <- rep(1, 3)
 lp <- rep(0, 3)

 fit <- postlink:::calc_breslow(y, status, weights, lp)

 # Hazards: 1/3, 1/2, 1/1
 # CumHaz: 0.333, 0.833, 1.833
 # Densities (diff_haz / diff_times):
 # t=10: 0.333
 # t=20: (0.833 - 0.333) / 10 = 0.05
 # t=30: (1.833 - 0.833) / 10 = 0.10

 expect_equal(fit$dens[2], 0.05, tolerance = 1e-3)
 expect_equal(fit$dens[3], 0.10, tolerance = 1e-3)
})
