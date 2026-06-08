# tests/testthat/test-shelf_life.R

# Build a minimal mock et_prediction for shelf life tests
.mock_pred_for_sl <- function(n_obs = 6, ci_widths = NULL) {
  set.seed(42)
  n_draws <- 200
  # Widths grow with obs_id (simulating increasing uncertainty over forecast horizon)
  if (is.null(ci_widths)) ci_widths <- seq(0.2, 1.4, length.out = n_obs)

  # Construct a pp matrix whose quantile widths approximate ci_widths
  pp <- matrix(NA_real_, n_draws, n_obs)
  for (j in seq_len(n_obs)) {
    pp[, j] <- stats::rnorm(n_draws, sd = ci_widths[j] / (2 * 1.645))
  }

  lp  <- pp + matrix(rnorm(n_draws * n_obs, sd = 0.01), n_draws, n_obs)
  lp_p <- lp
  sigma_draws <- rep(0.1, n_draws)

  gauss_family <- list(family = "gaussian", link = "identity", linkinv = identity)
  disp_draws   <- list(sigma = sigma_draws, phi = NULL, shape = NULL, nu = NULL)
  decomp <- ErrorTracer:::.decompose_from_arrays(
    pp = pp, mu_draws = lp, mu_perturbed = lp_p, mu_draws_sub = lp,
    family = gauss_family, disp_draws = disp_draws
  )
  ci_df  <- ErrorTracer:::.compute_ci(pp, c(0.90, 0.95))

  nd <- data.frame(year = 2020 + seq_len(n_obs) - 1L)

  structure(
    list(
      posterior_predict  = pp,
      posterior_linpred  = lp,
      lp_perturbed       = lp_p,
      sigma_draws        = sigma_draws,
      credible_intervals = ci_df,
      decomposition      = decomp,
      newdata            = nd,
      model              = NULL
    ),
    class = "et_prediction"
  )
}

test_that("shelf_life returns et_shelf_life with correct columns", {
  pred <- .mock_pred_for_sl()
  sl   <- shelf_life(pred, response_scale = c(-1, 1), ci_level = 0.90)

  expect_s3_class(sl, c("et_shelf_life", "data.frame"))
  expect_true(all(c("obs_id", "time", "ci_width", "plausible_range",
                    "ratio", "informative") %in% colnames(sl)))
})

test_that("shelf_life row count matches n_obs", {
  n <- 8
  pred <- .mock_pred_for_sl(n_obs = n)
  sl   <- shelf_life(pred, response_scale = c(-2, 2))
  expect_equal(nrow(sl), n)
})

test_that("shelf_life ratio = ci_width / response_scale", {
  pred <- .mock_pred_for_sl()
  sl   <- shelf_life(pred, response_scale = c(-1, 1))
  expect_equal(sl$ratio, sl$ci_width / 2, tolerance = 1e-10)
})

test_that("shelf_life informative flag respects threshold", {
  pred <- .mock_pred_for_sl()
  sl   <- shelf_life(pred, response_scale = c(-1, 1), threshold = 0.5)
  # informative == TRUE when ratio < 0.5
  expect_equal(sl$informative, sl$ratio < 0.5)
})

test_that("shelf_life uses time_col from newdata", {
  pred <- .mock_pred_for_sl(n_obs = 4)
  sl   <- shelf_life(pred, response_scale = c(-1, 1), time_col = "year")
  expect_equal(sl$time, 2020:2023)
})

test_that("shelf_life errors if ci_level not in predictions", {
  pred <- .mock_pred_for_sl()
  expect_error(
    shelf_life(pred, response_scale = c(-1, 1), ci_level = 0.99),
    "ci_level"
  )
})

test_that("shelf_life errors if response_scale has length != 2", {
  pred <- .mock_pred_for_sl()
  expect_error(shelf_life(pred, response_scale = c(1, 2, 3)), "length 2")
})

test_that("shelf_life errors if response_scale min == max", {
  pred <- .mock_pred_for_sl()
  expect_error(shelf_life(pred, response_scale = c(1, 1)), "equal")
})

test_that("shelf_life print method runs without error", {
  pred <- .mock_pred_for_sl()
  sl   <- shelf_life(pred, response_scale = c(-1, 1))
  expect_output(print(sl), "ErrorTracer shelf life")
  expect_output(print(sl), "Informative")
})

test_that("shelf_life plausible_range output column is constant (scalar diff)", {
  pred <- .mock_pred_for_sl(n_obs = 5)
  sl   <- shelf_life(pred, response_scale = c(-2, 2))
  expect_true(all(sl$plausible_range == 4))
})

# ── Deprecated plausible_range alias ────────────────────────────────────────

test_that("shelf_life warns when deprecated plausible_range is used", {
  pred <- .mock_pred_for_sl(n_obs = 4)
  expect_warning(
    shelf_life(pred, plausible_range = c(-1, 1)),
    "deprecated"
  )
})

test_that("shelf_life plausible_range alias produces same result as response_scale", {
  pred <- .mock_pred_for_sl(n_obs = 4)
  sl_new  <- shelf_life(pred, response_scale = c(-1, 1))
  sl_old  <- suppressWarnings(shelf_life(pred, plausible_range = c(-1, 1)))
  expect_equal(sl_new$ratio, sl_old$ratio, tolerance = 1e-10)
})

# ── Horizon attribute tests ──────────────────────────────────────────────────

test_that("horizon type is 'observed' when threshold crossed in window", {
  # widths grow 0.2 → 1.4 over 6 obs; threshold = 0.5 => crossed early
  pred <- .mock_pred_for_sl(n_obs = 6)
  sl   <- shelf_life(pred, response_scale = c(-1, 1), threshold = 0.5)
  hor  <- attr(sl, "horizon")
  expect_equal(hor$type, "observed")
  expect_true(is.numeric(hor$value) && !is.na(hor$value))
})

test_that("horizon type is 'lower_bound' when all informative and trend flat", {
  # Constant widths => zero slope => lower bound
  pred <- .mock_pred_for_sl(n_obs = 6, ci_widths = rep(0.2, 6))
  sl   <- shelf_life(pred, response_scale = c(-1, 1), threshold = 1.0)
  hor  <- attr(sl, "horizon")
  expect_equal(hor$type, "lower_bound")
  expect_true(is.na(hor$value))
})

test_that("horizon type is 'projected' when positive slope within cap", {
  # Use many obs and a steep, clear trend so quantile-based widths reliably
  # increase; large max_extrapolation_factor ensures projection is not capped.
  pred <- .mock_pred_for_sl(n_obs = 10,
                            ci_widths = seq(0.20, 0.90, length.out = 10))
  sl   <- shelf_life(pred, response_scale = c(-1, 1), threshold = 1.0,
                     min_slope_for_projection = 1e-10,
                     max_extrapolation_factor = Inf)
  hor  <- attr(sl, "horizon")
  expect_equal(hor$type, "projected")
  # Projected value must be a finite number beyond the last observed time
  expect_true(is.numeric(hor$value) && !is.na(hor$value) &&
              hor$value > hor$last_informative)
})

test_that("max_extrapolation_factor cap converts projected to lower_bound", {
  # Same gentle slope as above but cap = 0 => always lower bound
  pred <- .mock_pred_for_sl(n_obs = 6, ci_widths = seq(0.1, 0.35, length.out = 6))
  sl_cap <- shelf_life(pred, response_scale = c(-1, 1), threshold = 1.0,
                       min_slope_for_projection = 1e-10,
                       max_extrapolation_factor = 0)
  expect_equal(attr(sl_cap, "horizon")$type, "lower_bound")
})

test_that("horizon attribute has required fields", {
  pred <- .mock_pred_for_sl(n_obs = 6)
  sl   <- shelf_life(pred, response_scale = c(-1, 1))
  hor  <- attr(sl, "horizon")
  expect_true(all(c("type", "value", "last_informative", "description") %in% names(hor)))
})
