# Tests for Bayesian GLM mixture (real Stan MCMC)
# Part 1: glmMixBayes() basic object validity on a simple synthetic mixture dataset.
#
# NOTE:
# - skipped on CRAN
# - opt-in by setting RUN_STAN_TESTS=true
#
# Supported families (glmMixBayes): gaussian, poisson, binomial, gamma
#
# Example:
#   Sys.setenv(RUN_STAN_TESTS = "true")
#   devtools::test()

local_edition(3)

# ------------------------------------------------------------------------------
# Helper: generate simple synthetic 2-component mixture GLM data
# (in the spirit of generate_synthetic_mixture_data.R)
# ------------------------------------------------------------------------------
generate_bayesglm_mixture_data <- function(
  family = "gaussian",
  seed = 123,
  n = 120,
  theta = 0.6
) {
 family <- tolower(trimws(as.character(family)[1]))
 if (!family %in% c("gaussian", "poisson", "binomial", "gamma")) {
  stop("family must be one of gaussian/poisson/binomial/gamma", call. = FALSE)
 }

 set.seed(seed)

 # Design: intercept + one continuous + one binary predictor
 x1 <- stats::runif(n, -2, 2)
 x2 <- stats::rbinom(n, 1, 0.5)
 X  <- cbind(1, x1, x2)
 colnames(X) <- c("(Intercept)", "x1", "x2")

 # Mixture membership: 1 or 2
 z <- stats::rbinom(n, 1, theta) + 1L

 # Choose component-specific parameters with decent separation
 if (family == "gaussian") {
  beta1 <- c(0.2,  1.0, -0.6)
  beta2 <- c(1.5, -0.8,  0.9)
  sigma1 <- 0.7
  sigma2 <- 1.1

  eta1 <- drop(X %*% beta1)
  eta2 <- drop(X %*% beta2)
  mu   <- ifelse(z == 1L, eta1, eta2)
  sd   <- ifelse(z == 1L, sigma1, sigma2)
  y    <- stats::rnorm(n, mean = mu, sd = sd)

  truth <- list(beta1 = beta1, beta2 = beta2,
                dispersion1 = sigma1^2, dispersion2 = sigma2^2, theta = theta)

 } else if (family == "poisson") {
  beta1 <- c(-0.2, 0.4, -0.3)
  beta2 <- c( 0.8, 0.2,  0.5)

  eta1 <- drop(X %*% beta1)
  eta2 <- drop(X %*% beta2)
  lam  <- ifelse(z == 1L, exp(eta1), exp(eta2))
  y    <- stats::rpois(n, lambda = lam)

  truth <- list(beta1 = beta1, beta2 = beta2, theta = theta)

 } else if (family == "binomial") {
  beta1 <- c(-0.5, 0.8, -0.6)
  beta2 <- c( 0.7,-0.4,  0.9)

  eta1 <- drop(X %*% beta1)
  eta2 <- drop(X %*% beta2)
  p    <- ifelse(z == 1L, stats::plogis(eta1), stats::plogis(eta2))
  y    <- stats::rbinom(n, size = 1, prob = p)

  truth <- list(beta1 = beta1, beta2 = beta2, theta = theta)

 } else if (family == "gamma") {
  # Use mean = exp(eta), with component-specific shape (phi)
  beta1 <- c(0.1, 0.3, -0.2)
  beta2 <- c(0.6,-0.2,  0.4)
  phi1  <- 3.0
  phi2  <- 2.0

  eta1  <- drop(X %*% beta1)
  eta2  <- drop(X %*% beta2)
  mu    <- ifelse(z == 1L, exp(eta1), exp(eta2))

  shape <- ifelse(z == 1L, phi1, phi2)
  rate  <- shape / mu
  y     <- stats::rgamma(n, shape = shape, rate = rate)

  truth <- list(beta1 = beta1, beta2 = beta2,
                dispersion1 = 1/phi1, dispersion2 = 1/phi2, theta = theta)
 }

 list(X = X, y = y, truth = truth)
}

# ------------------------------------------------------------------------------
# Test 1: glmMixBayes basic validity
# ------------------------------------------------------------------------------
test_that("glmMixBayes runs real MCMC and returns a valid object", {
 skip_on_cran()
 skip_if_not_installed("rstan")
 skip_if_not_installed("label.switching")

 if (!identical(Sys.getenv("RUN_STAN_TESTS"), "true")) {
  skip("Set RUN_STAN_TESTS=true to run real Stan MCMC tests.")
 }

 rstan::rstan_options(auto_write = TRUE)
 options(mc.cores = 1L)

 # Document supported family types explicitly
 supported_families <- c("gaussian", "poisson", "binomial", "gamma")
 expect_error(glmMixBayes(X = matrix(1, 3, 1), y = rnorm(3), family = "badfamily"))
 expect_true(all(supported_families %in% c("gaussian","poisson","binomial","gamma")))

 # Keep this test simple + stable: run ONE family (gaussian) on mixture-like data
 dat <- generate_bayesglm_mixture_data(family = "gaussian", seed = 123, n = 100, theta = 0.6)
 X <- dat$X
 y <- dat$y

 custom_priors <- list(
  beta1 = "normal(0,3)",
  beta2 = "normal(0,4)",
  sigma1 = "cauchy(0,1.5)",
  sigma2 = "cauchy(0,2)",
  theta = "beta(2,2)"
 )

 fit <- glmMixBayes(
  X = X,
  y = y,
  family = "gaussian",
  priors = custom_priors,
  control = list(
   iterations = 2000,
   burnin.iterations = 1000,
   seed = 123,
   cores = 1,
   adapt_delta = 0.99,
   max_treedepth = 12
  )
 )

 expect_s3_class(fit, "glmMixBayes")

 # Basic structure checks (match your existing conventions)
 expect_true(is.matrix(fit$m_samples))
 expect_equal(ncol(fit$m_samples), nrow(X))

 expect_true(is.list(fit$estimates))
 expect_true(is.matrix(fit$estimates$coefficients))
 expect_true(is.matrix(fit$estimates$m.coefficients))
 expect_equal(ncol(fit$estimates$coefficients), ncol(X))
 expect_equal(ncol(fit$estimates$m.coefficients), ncol(X))

 if (!is.null(colnames(X))) {
  expect_equal(colnames(fit$estimates$coefficients), colnames(X))
  expect_equal(colnames(fit$estimates$m.coefficients), colnames(X))
 }

 # Dispersion present for gaussian (and gamma)
 expect_true("dispersion" %in% names(fit$estimates))
 expect_true("m.dispersion" %in% names(fit$estimates))
})



