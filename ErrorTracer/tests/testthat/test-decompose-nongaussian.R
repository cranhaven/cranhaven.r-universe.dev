# tests/testthat/test-decompose-nongaussian.R
#
# Tests for B1.1: response-scale decomposition for non-Gaussian families.
# These tests exercise .decompose_from_arrays() directly with mock mu_draws
# that simulate what posterior_linpred(transform=TRUE) returns for each family.
# No real Stan fit is needed.

# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------

# Build a mock disp_draws list from named numeric vectors.
.mock_disp <- function(...) {
  args <- list(...)
  base <- list(sigma = NULL, phi = NULL, shape = NULL, nu = NULL)
  base[names(args)] <- args
  base
}

# Call .decompose_from_arrays() with a given family and mu_draws,
# returning the decomposition data.frame.
.decomp_with_family <- function(family, mu_draws, disp_draws,
                                 n_obs = NULL, n_draws = NULL) {
  if (is.null(n_obs))    n_obs    <- ncol(mu_draws)
  if (is.null(n_draws))  n_draws  <- nrow(mu_draws)
  set.seed(7)
  pp <- matrix(rnorm(n_draws * n_obs), nrow = n_draws)
  # mu_perturbed: slightly perturbed version of mu_draws for env_var > 0
  mu_perturbed <- mu_draws + matrix(rnorm(n_draws * n_obs, sd = 0.02),
                                    nrow = n_draws)
  # For non-Gaussian families, clip mu_perturbed to the valid range
  fname <- tolower(family$family)
  if (fname %in% c("binomial", "bernoulli", "beta")) {
    mu_perturbed <- pmin(pmax(mu_perturbed, 1e-6), 1 - 1e-6)
  }
  if (fname %in% c("poisson", "negbinomial", "gamma")) {
    mu_perturbed <- pmax(mu_perturbed, 1e-6)
  }

  ErrorTracer:::.decompose_from_arrays(
    pp           = pp,
    mu_draws     = mu_draws,
    mu_perturbed = mu_perturbed,
    mu_draws_sub = mu_draws,
    family       = family,
    disp_draws   = disp_draws
  )
}

# ---------------------------------------------------------------------------
# Gaussian (identity link) — baseline, should match legacy behaviour
# ---------------------------------------------------------------------------

test_that("Gaussian identity: residual_var equals mean(sigma^2) for all obs", {
  set.seed(1)
  n_draws <- 200; n_obs <- 4
  mu_draws    <- matrix(rnorm(n_draws * n_obs), nrow = n_draws)
  sigma_draws <- abs(rnorm(n_draws, mean = 1, sd = 0.1))
  family <- list(family = "gaussian", link = "identity", linkinv = identity)

  d <- .decomp_with_family(family, mu_draws, .mock_disp(sigma = sigma_draws))

  expected_resid <- mean(sigma_draws^2)
  expect_equal(unique(round(d$residual_var, 8)), round(expected_resid, 8))
})

# ---------------------------------------------------------------------------
# Binomial (logit link) — key non-Gaussian target family
# ---------------------------------------------------------------------------

test_that("Binomial: residual_var = colMeans(mu*(1-mu)) varies per observation", {
  set.seed(2)
  n_draws <- 300; n_obs <- 5
  # mu_draws on response scale (probabilities): simulate logit^{-1}(eta)
  mu_draws <- matrix(plogis(rnorm(n_draws * n_obs, sd = 1.5)), nrow = n_draws)
  family   <- list(family = "binomial", link = "logit", linkinv = plogis)

  d <- .decomp_with_family(family, mu_draws, .mock_disp())

  expected <- colMeans(mu_draws * (1 - mu_draws))
  expect_equal(d$residual_var, expected, tolerance = 1e-10)
  # residual_var should differ across observations (not constant)
  expect_gt(diff(range(d$residual_var)), 0)
})

test_that("Binomial: param_var is on response (probability) scale, not logit scale", {
  set.seed(3)
  n_draws <- 200; n_obs <- 3
  eta      <- matrix(rnorm(n_draws * n_obs, sd = 2), nrow = n_draws)
  mu_draws <- matrix(plogis(as.vector(eta)), nrow = n_draws)
  family   <- list(family = "binomial", link = "logit", linkinv = plogis)

  d <- .decomp_with_family(family, mu_draws, .mock_disp())
  expected_param_var <- apply(mu_draws, 2, var)

  expect_equal(d$param_var, expected_param_var, tolerance = 1e-10)
  # param_var on probability scale must be much smaller than on logit scale
  link_scale_var <- apply(eta, 2, var)
  expect_true(all(d$param_var < link_scale_var))
})

test_that("Binomial: all decomposition components are non-negative", {
  set.seed(4)
  n_draws <- 150; n_obs <- 6
  mu_draws <- matrix(plogis(rnorm(n_draws * n_obs)), nrow = n_draws)
  family   <- list(family = "binomial", link = "logit", linkinv = plogis)
  d <- .decomp_with_family(family, mu_draws, .mock_disp())

  expect_true(all(d$param_var    >= 0))
  expect_true(all(d$env_var      >= 0))
  expect_true(all(d$v_env_mcse   >= 0))
  expect_true(all(d$residual_var >= 0))
  expect_true(all(d$total_var    >= 0))
})

# ---------------------------------------------------------------------------
# Poisson (log link)
# ---------------------------------------------------------------------------

test_that("Poisson: residual_var = colMeans(mu) per observation", {
  set.seed(5)
  n_draws <- 200; n_obs <- 4
  mu_draws <- matrix(exp(rnorm(n_draws * n_obs, mean = 1, sd = 0.5)),
                     nrow = n_draws)
  family   <- list(family = "poisson", link = "log", linkinv = exp)

  d <- .decomp_with_family(family, mu_draws, .mock_disp())

  expected <- colMeans(mu_draws)
  expect_equal(d$residual_var, expected, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Student-t
# ---------------------------------------------------------------------------

test_that("Student-t: residual_var = mean(sigma^2 * nu / (nu - 2))", {
  set.seed(6)
  n_draws     <- 200; n_obs <- 3
  mu_draws    <- matrix(rnorm(n_draws * n_obs), nrow = n_draws)
  sigma_draws <- abs(rnorm(n_draws, mean = 1, sd = 0.1))
  nu_draws    <- abs(rnorm(n_draws, mean = 10, sd = 1)) + 2 + 0.1  # nu > 2
  family      <- list(family = "student", link = "identity", linkinv = identity)

  d <- .decomp_with_family(family, mu_draws,
                            .mock_disp(sigma = sigma_draws, nu = nu_draws))

  expected <- mean(sigma_draws^2 * nu_draws / (nu_draws - 2))
  expect_equal(unique(round(d$residual_var, 6)), round(expected, 6))
})

# ---------------------------------------------------------------------------
# Unknown family — should warn and return NA
# ---------------------------------------------------------------------------

test_that("Unknown family warns and returns NA for residual_var", {
  set.seed(8)
  n_draws <- 50; n_obs <- 3
  mu_draws <- matrix(rnorm(n_draws * n_obs), nrow = n_draws)
  family   <- list(family = "xyzzy_unknown", link = "identity",
                   linkinv = identity)

  expect_message(
    d <- .decomp_with_family(family, mu_draws, .mock_disp()),
    regexp = "not supported"
  )
  expect_true(all(is.na(d$residual_var)))
})

# ---------------------------------------------------------------------------
# .is_gaussian_identity() helper
# ---------------------------------------------------------------------------

test_that(".is_gaussian_identity returns TRUE only for Gaussian identity", {
  expect_true(ErrorTracer:::.is_gaussian_identity(
    list(family = "gaussian", link = "identity")))
  expect_true(ErrorTracer:::.is_gaussian_identity(
    list(family = "normal", link = "identity")))
  expect_false(ErrorTracer:::.is_gaussian_identity(
    list(family = "gaussian", link = "log")))
  expect_false(ErrorTracer:::.is_gaussian_identity(
    list(family = "binomial", link = "logit")))
  expect_true(ErrorTracer:::.is_gaussian_identity(NULL))
})

# ---------------------------------------------------------------------------
# .apply_inv_link() helper
# ---------------------------------------------------------------------------

test_that(".apply_inv_link identity passes matrix through unchanged", {
  family <- list(family = "gaussian", link = "identity", linkinv = identity)
  m <- matrix(rnorm(20), 4, 5)
  expect_identical(ErrorTracer:::.apply_inv_link(m, family), m)
})

test_that(".apply_inv_link logit applies plogis element-wise", {
  family <- list(family = "binomial", link = "logit", linkinv = plogis)
  m <- matrix(c(-2, 0, 2, 1), 2, 2)
  result <- ErrorTracer:::.apply_inv_link(m, family)
  expect_equal(result, matrix(plogis(c(-2, 0, 2, 1)), 2, 2), tolerance = 1e-10)
  expect_true(all(result > 0 & result < 1))
})

test_that(".apply_inv_link log applies exp element-wise", {
  family <- list(family = "poisson", link = "log", linkinv = exp)
  m <- matrix(c(0, 1, -1, 2), 2, 2)
  result <- ErrorTracer:::.apply_inv_link(m, family)
  expect_equal(result, matrix(exp(c(0, 1, -1, 2)), 2, 2), tolerance = 1e-10)
  expect_true(all(result > 0))
})
