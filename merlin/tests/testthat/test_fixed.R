testthat::context("ghq_fixed")
#############################################

# method: gauss-hermite quadrature
# models: no-random effects
# covariance: no-random effects

# GAUSSIAN
###############################################################################
# basic model with no covariates
testthat::test_that("Test 1.1", {
  data("iris", package = "datasets")
  lmmodel <- stats::lm(Sepal.Length ~ 1, data = iris)
  lmres <- c(lmmodel$coeff, log(summary(lmmodel)$sigma), logLik(lmmodel))
  names(lmres) <- NULL
  comp <- rep(0, length(lmres))
  mod <- merlin::merlin(
    model = list(Sepal.Length ~ 1),
    family = "gaussian",
    data = iris
  )
  modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
  testthat::expect_equal(object = (abs(modres - lmres) / (abs(lmres) + 1)), expected = comp, tolerance = 1e-3)
})

# add in covariates
testthat::test_that("Test 1.2", {
  data("iris", package = "datasets")
  lmmodel <- stats::lm(Sepal.Length ~ Petal.Length, data = iris)
  lmres <- c(lmmodel$coeff[c(2, 1)], log(summary(lmmodel)$sigma), logLik(lmmodel))
  names(lmres) <- NULL
  comp <- rep(0, length(lmres))
  mod <- merlin::merlin(
    model = list(Sepal.Length ~ Petal.Length),
    family = "gaussian",
    data = iris
  )
  modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
  testthat::expect_equal(object = (abs(modres - lmres) / (abs(lmres) + 1)), expected = comp, tolerance = 1e-3)
})

# use log of variable
testthat::test_that("Test 1.2", {
  data("iris", package = "datasets")
  lmmodel <- stats::lm(Sepal.Length ~ log(Petal.Length), data = iris)
  lmres <- c(lmmodel$coeff[c(2, 1)], log(summary(lmmodel)$sigma), logLik(lmmodel))
  names(lmres) <- NULL
  comp <- rep(0, length(lmres))
  mod <- merlin::merlin(
    model = list(Sepal.Length ~ fp(Petal.Length, powers = c(0))),
    family = "gaussian",
    data = iris
  )
  modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
  testthat::expect_equal(object = (abs(modres - lmres) / (abs(lmres) + 1)), expected = comp, tolerance = 1e-3)
})

# BERNOULLI
###############################################################################
testthat::test_that("Test 2.1", {
  data("iris", package = "datasets")
  iris$y <- as.numeric(iris$Sepal.Length > mean(iris$Sepal.Length))
  glmmod <- stats::glm(y ~ 1, family = "binomial", data = iris)
  glmres <- c(glmmod$coefficients, logLik(glmmod))
  names(glmres) <- NULL
  comp <- rep(0, length(glmres))
  mod <- suppressWarnings({
    merlin::merlin(
      model = list(y ~ 1),
      family = "bernoulli",
      data = iris
    )
  })
  modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
  testthat::expect_equal(object = (abs(modres - glmres) / (abs(glmres) + 1)), expected = comp, tolerance = 1e-4)
})


# EXPONENTIAL
###############################################################################
testthat::test_that("Test 8.1", {
  data("ovarian", package = "survival")
  survmod <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps, data = ovarian, dist = "exponential")
  survres <- c(-survmod$coefficients[c(2, 1)], logLik(survmod))
  names(survres) <- NULL
  comp <- rep(0, length(survres))
  mod <- merlin(
    model = list(Surv(futime, fustat) ~ ecog.ps),
    family = "exponential",
    data = ovarian
  )
  modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
  testthat::expect_equal(object = (abs(modres - survres) / (abs(survres) + 1)), expected = comp, tolerance = 1e-3)
})


# EXPONENTIAL
###############################################################################
testthat::test_that("Test 11.1", {
  data("ovarian", package = "survival")
  survmod <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps, data = ovarian, dist = "weibull")
  survres <- c((-survmod$coefficient * (1 / (survmod$scale)))[c(2, 1)], -log(survmod$scale), logLik(survmod))
  names(survres) <- NULL
  comp <- rep(0, length(survres))
  mod <- merlin(
    model = list(Surv(futime, fustat) ~ ecog.ps),
    family = "weibull",
    data = ovarian
  )
  modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
  testthat::expect_equal(object = (abs(modres - survres) / (abs(survres) + 1)), expected = comp, tolerance = 1e-3)
})
