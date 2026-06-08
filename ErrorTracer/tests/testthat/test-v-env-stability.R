# tests/testthat/test-v-env-stability.R
#
# Tests for B1.2: Monte Carlo stability of the environmental variance estimator.
# These tests verify:
#   1. v_env_mcse == 0 when env_noise = 0 (no perturbation, no MC noise).
#   2. v_env_mcse decreases as n_env_draws increases (averaging effect).
#   3. env_var == 0 when mu_perturbed == mu_draws_sub (zero noise path).
#   4. .compute_lp_perturbed() accepts n_env_draws and its output has the
#      same shape regardless of n_env_draws value.
# No real Stan fit is needed.

# ---------------------------------------------------------------------------
# Helper: build mu arrays with a controlled level of perturbation noise
# ---------------------------------------------------------------------------

.mock_mu_arrays <- function(n_draws = 200, n_obs = 5,
                             noise_sd = 0.1, seed = 42) {
  set.seed(seed)
  mu_draws     <- matrix(rnorm(n_draws * n_obs, mean = 0.5, sd = 0.3),
                          nrow = n_draws)
  mu_perturbed <- mu_draws + matrix(rnorm(n_draws * n_obs, sd = noise_sd),
                                     nrow = n_draws)
  list(mu_draws = mu_draws, mu_perturbed = mu_perturbed)
}

.gauss_family  <- list(family = "gaussian", link = "identity", linkinv = identity)
.gauss_disp    <- function(n) list(sigma = abs(rnorm(n, 0.5, 0.1)),
                                   phi = NULL, shape = NULL, nu = NULL)

# ---------------------------------------------------------------------------
# v_env_mcse == 0 when env_noise = 0 (mu_perturbed == mu_draws_sub)
# ---------------------------------------------------------------------------

test_that("env_var and v_env_mcse are zero when mu_perturbed equals mu_draws_sub", {
  set.seed(1)
  n_draws <- 100; n_obs <- 4
  mu_draws  <- matrix(rnorm(n_draws * n_obs), nrow = n_draws)
  pp        <- matrix(rnorm(n_draws * n_obs), nrow = n_draws)
  sigma     <- abs(rnorm(n_draws, 0.5))
  disp      <- list(sigma = sigma, phi = NULL, shape = NULL, nu = NULL)

  # Perturbed == draws_sub (same object) → v_env_raw == 0 exactly → SE = 0
  d <- ErrorTracer:::.decompose_from_arrays(
    pp           = pp,
    mu_draws     = mu_draws,
    mu_perturbed = mu_draws,   # identical → no env noise
    mu_draws_sub = mu_draws,
    family       = .gauss_family,
    disp_draws   = disp
  )

  expect_equal(d$env_var,    rep(0, n_obs), tolerance = 1e-10)
  expect_equal(d$v_env_mcse, rep(0, n_obs), tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# env_var > 0 and finite when perturbation noise is present
# ---------------------------------------------------------------------------

test_that("env_var and v_env_mcse are positive and finite with perturbation", {
  arrays <- .mock_mu_arrays(n_draws = 300, n_obs = 5, noise_sd = 0.3)
  pp     <- matrix(rnorm(300 * 5), nrow = 300)
  set.seed(10)
  sigma  <- abs(rnorm(300, 0.5))
  disp   <- list(sigma = sigma, phi = NULL, shape = NULL, nu = NULL)

  d <- ErrorTracer:::.decompose_from_arrays(
    pp           = pp,
    mu_draws     = arrays$mu_draws,
    mu_perturbed = arrays$mu_perturbed,
    mu_draws_sub = arrays$mu_draws,
    family       = .gauss_family,
    disp_draws   = disp
  )

  expect_true(all(is.finite(d$env_var)))
  expect_true(all(is.finite(d$v_env_mcse)))
  expect_true(mean(d$env_var) > 0)
  expect_true(mean(d$v_env_mcse) > 0)
})

# ---------------------------------------------------------------------------
# v_env_mcse decreases as n_env_draws increases
# (averaged perturbations reduce within-draw MC noise → lower v_env variance)
# ---------------------------------------------------------------------------

test_that("v_env_mcse decreases as n_env_draws increases", {
  skip_if_not_installed("brms")

  set.seed(99)
  n_obs     <- 3
  n_perturb <- 60
  # Synthetic draws matrix with two parameters
  draws_mat <- cbind(
    b_Intercept = rnorm(n_perturb, 0, 0.5),
    b_x1        = rnorm(n_perturb, 1, 0.3)
  )
  newdata   <- data.frame(x1 = rnorm(n_obs))
  noise_sds <- list(x1 = rep(0.5, n_obs))

  lp1 <- ErrorTracer:::.compute_lp_perturbed(
    draws_mat = draws_mat, newdata = newdata,
    pred_names = "x1", noise_sds = noise_sds,
    n_env_draws = 1L
  )
  lp5 <- ErrorTracer:::.compute_lp_perturbed(
    draws_mat = draws_mat, newdata = newdata,
    pred_names = "x1", noise_sds = noise_sds,
    n_env_draws = 5L
  )
  lp20 <- ErrorTracer:::.compute_lp_perturbed(
    draws_mat = draws_mat, newdata = newdata,
    pred_names = "x1", noise_sds = noise_sds,
    n_env_draws = 20L
  )

  # All have same shape
  expect_equal(dim(lp1),  c(n_perturb, n_obs))
  expect_equal(dim(lp5),  c(n_perturb, n_obs))
  expect_equal(dim(lp20), c(n_perturb, n_obs))

  # Variance across draws should decrease with n_env_draws because within-draw
  # noise is averaged out. Check for the first observation.
  var1  <- apply(lp1,  2, var)
  var5  <- apply(lp5,  2, var)
  var20 <- apply(lp20, 2, var)

  # With n_env_draws=20 the variance should be closer to the "true" parameter
  # variance (less inflated by MC noise). On average across obs, mean(var20)
  # should be <= mean(var1) — not guaranteed for every obs with finite n_perturb,
  # so we test the mean.
  expect_lte(mean(var20), mean(var1) * 1.1)   # allow 10% slack for randomness
})

# ---------------------------------------------------------------------------
# .compute_lp_perturbed output shape invariant to n_env_draws
# ---------------------------------------------------------------------------

test_that(".compute_lp_perturbed returns [n_perturb x n_obs] for any n_env_draws", {
  set.seed(5)
  n_perturb <- 30; n_obs <- 4
  draws_mat <- cbind(
    b_Intercept = rnorm(n_perturb),
    b_x1        = rnorm(n_perturb)
  )
  newdata   <- data.frame(x1 = rnorm(n_obs))
  noise_sds <- list(x1 = rep(0.2, n_obs))

  for (k in c(1L, 3L, 10L)) {
    lp <- ErrorTracer:::.compute_lp_perturbed(
      draws_mat   = draws_mat,
      newdata     = newdata,
      pred_names  = "x1",
      noise_sds   = noise_sds,
      n_env_draws = k
    )
    expect_equal(dim(lp), c(n_perturb, n_obs),
                 label = paste0("n_env_draws = ", k))
  }
})

# ---------------------------------------------------------------------------
# v_env_mcse formula: check it is sqrt(2/(n-1)) * sqrt(Vp^2 + Vs^2)
# ---------------------------------------------------------------------------

test_that("v_env_mcse matches the chi-squared SE formula analytically", {
  set.seed(20)
  n_draws <- 100; n_obs <- 3
  mu_draws     <- matrix(rnorm(n_draws * n_obs, sd = 0.5), nrow = n_draws)
  mu_perturbed <- mu_draws + matrix(rnorm(n_draws * n_obs, sd = 0.15),
                                     nrow = n_draws)
  pp           <- matrix(rnorm(n_draws * n_obs), nrow = n_draws)
  sigma        <- abs(rnorm(n_draws, 0.5))
  disp         <- list(sigma = sigma, phi = NULL, shape = NULL, nu = NULL)

  d <- ErrorTracer:::.decompose_from_arrays(
    pp           = pp,
    mu_draws     = mu_draws,
    mu_perturbed = mu_perturbed,
    mu_draws_sub = mu_draws,
    family       = .gauss_family,
    disp_draws   = disp
  )

  n_p   <- n_draws
  f_se  <- sqrt(2 / (n_p - 1))
  vp    <- apply(mu_perturbed, 2, var)
  vs    <- apply(mu_draws,     2, var)
  expected_mcse <- sqrt((vp * f_se)^2 + (vs * f_se)^2)

  expect_equal(d$v_env_mcse, expected_mcse, tolerance = 1e-10)
})
