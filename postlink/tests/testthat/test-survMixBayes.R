# Tests for Bayesian survreg mixture (real Stan MCMC)
# Part 1: survregMixBayes() basic object validity on a simple synthetic survival dataset.
#
# NOTE:
# - skipped on CRAN
# - opt-in by setting RUN_STAN_TESTS=true
#
# Example:
#   Sys.setenv(RUN_STAN_TESTS = "true")
#   devtools::test()

local_edition(3)

# ------------------------------------------------------------------------------
# Helper: simulate synthetic 2-component survival mixture with censoring
# ------------------------------------------------------------------------------
generate_bayessurv_mixture_data <- function(family = c("gamma", "weibull"),
                                            seed = 321,
                                            N = 120) {
 family <- match.arg(family)
 set.seed(seed)

 # Design: intercept + one covariate
 X <- cbind(1, stats::runif(N, -2, 2))
 colnames(X) <- c("X1", "X2")

 # True coefficients
 beta1 <- c(0.5, 1.2)
 beta2 <- c(1.5, -0.8)

 # True component labels: 60% in component 1
 z_true <- 1 + stats::rbinom(N, size = 1, prob = 0.4)  # P(z=1)=0.6, P(z=2)=0.4

 y <- numeric(N)

 if (family == "gamma") {
  phi1 <- 3.0
  phi2 <- 2.0

  eta1 <- as.vector(X %*% beta1)
  eta2 <- as.vector(X %*% beta2)
  mu1 <- exp(eta1)
  mu2 <- exp(eta2)

  shape_vec <- ifelse(z_true == 1, phi1, phi2)
  rate_vec  <- ifelse(z_true == 1, phi1 / mu1, phi2 / mu2)

  y <- stats::rgamma(N, shape = shape_vec, rate = rate_vec)

  true_par <- list(beta1 = beta1, beta2 = beta2, phi1 = phi1, phi2 = phi2)

 } else if (family == "weibull") {
  shape1 <- 2.0
  shape2 <- 1.5

  eta1 <- as.vector(X %*% beta1)
  eta2 <- as.vector(X %*% beta2)

  # Common choice: log(scale) depends on linear predictor
  scale_vec <- ifelse(z_true == 1, exp(-eta1), exp(-eta2))
  shape_vec <- ifelse(z_true == 1, shape1, shape2)

  y <- stats::rweibull(N, shape = shape_vec, scale = scale_vec)

  true_par <- list(beta1 = beta1, beta2 = beta2,
                   shape1 = shape1, shape2 = shape2,
                   scale1 = 1, scale2 = 1)
 }

 # Random right-censoring (keep censor rate moderate & stable)
 cmin <- as.numeric(stats::quantile(y, 0.4))
 cmax <- as.numeric(stats::quantile(y, 0.9))
 censoring_time <- stats::runif(N, min = cmin, max = cmax)

 status <- as.integer(y <= censoring_time)   # 1=event observed
 y_obs  <- pmin(y, censoring_time)

 dat <- data.frame(X1 = X[, 1], X2 = X[, 2], y = y_obs, status = status)

 list(
  dat = dat,
  X = X,
  y = survival::Surv(dat$y, dat$status),
  z_true = z_true,
  family = family,
  true = true_par
 )
}

# ------------------------------------------------------------------------------
# Test 1: survregMixBayes basic validity
# ------------------------------------------------------------------------------
test_that("survregMixBayes runs real MCMC and returns a valid object", {
 skip_on_cran()
 skip_if_not_installed("rstan")
 skip_if_not_installed("label.switching")
 skip_if_not_installed("survival")

 if (!identical(Sys.getenv("RUN_STAN_TESTS"), "true")) {
  skip("Set RUN_STAN_TESTS=true to run real Stan MCMC tests.")
 }

 rstan::rstan_options(auto_write = TRUE)
 options(mc.cores = 1L)

 d <- generate_bayessurv_mixture_data(family = "gamma", seed = 321, N = 100)
 X <- d$X
 y <- d$y

 fit <- survregMixBayes(
  X = X,
  y = y,
  dist = "gamma",
  control = list(
   iterations = 2000,
   burnin.iterations = 1000,
   seed = 321,
   cores = 1,
   adapt_delta = 0.99,
   max_treedepth = 12
  )
 )

 expect_s3_class(fit, "survMixBayes")

 expect_true(is.matrix(fit$m_samples))
 expect_equal(ncol(fit$m_samples), nrow(X))

 expect_true(is.list(fit$estimates))
 expect_true(is.matrix(fit$estimates$coefficients))
 expect_true(is.matrix(fit$estimates$m.coefficients))

 expect_equal(ncol(fit$estimates$coefficients), ncol(X))
 expect_equal(ncol(fit$estimates$m.coefficients), ncol(X))

 if (!is.null(colnames(X))) {
  cnX <- colnames(X)

  cn1 <- colnames(fit$estimates$coefficients)
  expect_true(is.null(cn1) || identical(cn1, cnX))

  cnm <- colnames(fit$estimates$m.coefficients)
  expect_true(is.null(cnm) || identical(cnm, cnX))
 }
})
