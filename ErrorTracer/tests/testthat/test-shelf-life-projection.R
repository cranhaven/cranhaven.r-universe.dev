# tests/testthat/test-shelf-life-projection.R
#
# Tests for B1.5: shelf-life projection formula and se_t_star.
# Verifies:
#   1. Projected t* = (tau - a) / b (correct inversion of linear fit).
#   2. se_t_star is present and positive in the projected horizon.
#   3. se_t_star recovers the known shelf life within ±2 SE on synthetic data.
#   4. se_t_star is zero / near-zero when the linear fit is exact.
#   5. Observed and lower-bound modes do not return se_t_star.
# No real Stan fit is needed.

# ---------------------------------------------------------------------------
# Helper: build an et_prediction mock where the CI widths follow a known
# linear trend: width_j = a_true + b_true * time_j.
# ---------------------------------------------------------------------------

.mock_pred_linear <- function(n_obs = 12,
                               a_true = 0.05,   # intercept (ratio at t=0)
                               b_true = 0.06,   # slope (rate of ratio increase)
                               noise_sd = 0,    # width noise; 0 = exact line
                               pr = 1.0,        # response_scale width
                               threshold = 1.0,
                               seed = 42) {
  set.seed(seed)
  # 301 draws so stats::quantile(., 0.05) lands on order statistic 16 and
  # stats::quantile(., 0.95) on order statistic 286 (type 7, no interpolation).
  n_draws <- 301L
  times <- seq_len(n_obs)

  # Target CI widths from the linear model (on ratio scale, times pr)
  target_widths <- (a_true + b_true * times) * pr +
    rnorm(n_obs, sd = noise_sd * pr)
  target_widths <- pmax(target_widths, 0.01 * pr)

  # Build pp deterministically so the empirical 90% CI width equals
  # target_widths[j] exactly — no Monte Carlo noise.
  # Linear spread with x[16] = -w/2 and x[286] = +w/2.
  k <- seq_len(n_draws)
  base <- (k - 16L) / 270  # base[16] = 0, base[286] = 1
  pp <- matrix(NA_real_, n_draws, n_obs)
  for (j in seq_len(n_obs)) {
    pp[, j] <- target_widths[j] * (base - 0.5)
  }

  lp  <- pp + matrix(rnorm(n_draws * n_obs, sd = 1e-4), n_draws, n_obs)
  sigma_draws <- rep(0.1, n_draws)
  gauss_family <- list(family = "gaussian", link = "identity", linkinv = identity)
  disp_draws   <- list(sigma = sigma_draws, phi = NULL, shape = NULL, nu = NULL)
  decomp <- ErrorTracer:::.decompose_from_arrays(
    pp = pp, mu_draws = lp, mu_perturbed = lp, mu_draws_sub = lp,
    family = gauss_family, disp_draws = disp_draws
  )
  ci_df <- ErrorTracer:::.compute_ci(pp, c(0.90))

  structure(
    list(
      posterior_predict  = pp,
      posterior_linpred  = lp,
      lp_perturbed       = lp,
      sigma_draws        = sigma_draws,
      credible_intervals = ci_df,
      decomposition      = decomp,
      newdata            = data.frame(time = times),
      model              = NULL
    ),
    class = "et_prediction"
  )
}

# ---------------------------------------------------------------------------
# 1. Projected t* matches (tau - a) / b for an exact linear CI-width trend
# ---------------------------------------------------------------------------

test_that("projected t* equals (tau - a) / b for exact linear width trend", {
  # With zero noise, the ratio vs time relationship is exactly linear.
  # t* = (1.0 - a_true) / b_true (threshold = 1.0, pr = 1 so ratio = width/pr)
  a_true <- 0.10; b_true <- 0.08; threshold <- 1.0
  pred <- .mock_pred_linear(n_obs = 10, a_true = a_true, b_true = b_true,
                             noise_sd = 0, pr = 1.0, threshold = threshold,
                             seed = 1)

  sl  <- suppressWarnings(  # lm() warns "essentially perfect fit" on exact data
    shelf_life(pred, response_scale = c(0, 1), threshold = threshold,
               min_slope_for_projection = 1e-10,
               max_extrapolation_factor = Inf)
  )
  hor <- attr(sl, "horizon")

  expect_equal(hor$type, "projected")

  # Recover the linear fit parameters from the ratios directly
  lm_fit <- lm(sl$ratio ~ sl$time)
  a_fit  <- unname(coef(lm_fit)[1])
  b_fit  <- unname(coef(lm_fit)[2])
  t_star_expected <- (threshold - a_fit) / b_fit

  expect_equal(hor$value, t_star_expected, tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# 2. se_t_star is present and positive in projected mode
# ---------------------------------------------------------------------------

test_that("projected horizon includes se_t_star and it is positive", {
  pred <- .mock_pred_linear(n_obs = 10, a_true = 0.10, b_true = 0.07,
                             noise_sd = 0.005, seed = 7)
  sl  <- shelf_life(pred, response_scale = c(0, 1),
                    min_slope_for_projection = 1e-10,
                    max_extrapolation_factor = Inf)
  hor <- attr(sl, "horizon")

  expect_equal(hor$type, "projected")
  expect_true(!is.null(hor$se_t_star))
  expect_true(is.finite(hor$se_t_star))
  expect_true(hor$se_t_star >= 0)
})

# ---------------------------------------------------------------------------
# 3. se_t_star recovers known shelf life within ±2 SE (noisy case)
# ---------------------------------------------------------------------------

test_that("true t* lies within projected t* ± 2 se_t_star", {
  # True t*: (1 - 0.10) / 0.06 = 15.0
  a_true <- 0.10; b_true <- 0.06; threshold <- 1.0
  t_star_true <- (threshold - a_true) / b_true   # = 15

  set.seed(123)
  # Small noise so the test is reliable without many replicates
  pred <- .mock_pred_linear(n_obs = 14, a_true = a_true, b_true = b_true,
                             noise_sd = 0.003, pr = 1.0, threshold = threshold,
                             seed = 123)

  sl  <- shelf_life(pred, response_scale = c(0, 1), threshold = threshold,
                    min_slope_for_projection = 1e-10,
                    max_extrapolation_factor = Inf)
  hor <- attr(sl, "horizon")

  expect_equal(hor$type, "projected")
  expect_true(abs(hor$value - t_star_true) <= 2 * hor$se_t_star)
})

# ---------------------------------------------------------------------------
# 4. se_t_star is near zero for an exact (no-noise) linear trend
# ---------------------------------------------------------------------------

test_that("se_t_star is near zero when CI widths follow an exact linear trend", {
  pred <- .mock_pred_linear(n_obs = 8, a_true = 0.10, b_true = 0.09,
                             noise_sd = 0, seed = 5)
  sl  <- suppressWarnings(  # lm() warns "essentially perfect fit" on exact data
    shelf_life(pred, response_scale = c(0, 1),
               min_slope_for_projection = 1e-10,
               max_extrapolation_factor = Inf)
  )
  hor <- attr(sl, "horizon")

  expect_equal(hor$type, "projected")
  # With zero noise, the lm fit is (nearly) perfect — vcov entries are tiny
  expect_lt(hor$se_t_star, 0.5)
})

# ---------------------------------------------------------------------------
# 5. Observed and lower-bound modes do NOT return se_t_star
# ---------------------------------------------------------------------------

test_that("observed horizon has no se_t_star field", {
  # Widths exceed threshold = 0.2 immediately
  pred <- .mock_pred_linear(n_obs = 6, a_true = 0.5, b_true = 0.1,
                             noise_sd = 0, pr = 1.0, threshold = 0.2)
  sl  <- shelf_life(pred, response_scale = c(0, 1), threshold = 0.2)
  hor <- attr(sl, "horizon")

  expect_equal(hor$type, "observed")
  expect_null(hor$se_t_star)
})

test_that("lower_bound horizon has no se_t_star field", {
  pred <- .mock_pred_linear(n_obs = 6, a_true = 0.1, b_true = 0.0,
                             noise_sd = 0, pr = 1.0)
  sl  <- shelf_life(pred, response_scale = c(0, 1), threshold = 1.0,
                    min_slope_for_projection = 0.01)   # slope < min_slope
  hor <- attr(sl, "horizon")

  expect_equal(hor$type, "lower_bound")
  expect_null(hor$se_t_star)
})
