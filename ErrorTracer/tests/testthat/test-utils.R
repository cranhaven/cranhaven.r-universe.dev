# tests/testthat/test-utils.R
# Tests for exported and internal utilities in utils.R

# ── standardize / unstandardize ─────────────────────────────────────────────

test_that("standardize produces zero-mean unit-variance output", {
  x   <- c(1, 2, 3, 4, 5)
  z   <- standardize(x)
  expect_equal(mean(z), 0, tolerance = 1e-10)
  expect_equal(sd(z),   1, tolerance = 1e-10)
})

test_that("standardize returns zeros for constant input", {
  x <- rep(3, 5)
  expect_equal(standardize(x), rep(0, 5))
})

test_that("unstandardize inverts standardize", {
  x    <- rnorm(20, mean = 10, sd = 3)
  mu   <- mean(x)
  s    <- sd(x)
  z    <- (x - mu) / s
  back <- unstandardize(z, mu, s)
  expect_equal(back, x, tolerance = 1e-10)
})

# ── et_theme ────────────────────────────────────────────────────────────────

test_that("et_theme returns a ggplot2 theme object", {
  th <- et_theme()
  expect_s3_class(th, "theme")
})

test_that("et_theme respects base_size", {
  th <- et_theme(base_size = 14)
  expect_s3_class(th, "theme")
})

# ── .resolve_env_noise edge cases ───────────────────────────────────────────

test_that("resolve_env_noise scalar: missing predictor uses sd = 1 fallback", {
  nd  <- data.frame(Tmean = rnorm(4), PPT = rnorm(4))
  # SWE is in pred_names but NOT in newdata — fallback sd = 1, noise = 0.1 * 1
  res <- ErrorTracer:::.resolve_env_noise(0.1, c("Tmean", "PPT", "SWE"), nd)
  expect_equal(res[["SWE"]], rep(0.1, 4), tolerance = 1e-10)
})

test_that("resolve_env_noise named list: omitted predictor stays zero", {
  nd  <- data.frame(Tmean = rnorm(4), PPT = rnorm(4), SWE = rnorm(4))
  # Only Tmean and PPT supplied — SWE defaults to zero
  res <- ErrorTracer:::.resolve_env_noise(
    list(Tmean = 0.5, PPT = 0.2), c("Tmean", "PPT", "SWE"), nd
  )
  expect_true(all(res[["SWE"]] == 0))
})

test_that("resolve_env_noise scalar SD near-zero becomes 1", {
  # If sd of predictor is near zero, fall back to sd = 1
  nd  <- data.frame(X = rep(5, 6))  # constant predictor
  res <- ErrorTracer:::.resolve_env_noise(0.2, "X", nd)
  # sd(rep(5,6)) == 0 => fallback sd = 1, so noise = 0.2 * 1 = 0.2
  expect_equal(res[["X"]], rep(0.2, 6), tolerance = 1e-10)
})

# ── .compute_lp_perturbed edge case: no beta columns ────────────────────────

test_that("compute_lp_perturbed returns zero matrix when no beta columns found", {
  n_p  <- 50L
  n_obs <- 4L
  draws <- matrix(0.5, nrow = n_p, ncol = 1L,
                  dimnames = list(NULL, "b_Intercept"))
  nd    <- data.frame(X = rnorm(n_obs))
  noise <- list(X = rep(0.1, n_obs))
  # "X" maps to "b_X" which is not in draws — triggers the no-beta-columns branch
  result <- ErrorTracer:::.compute_lp_perturbed(
    draws_mat  = draws,
    newdata    = nd,
    pred_names = "X",
    noise_sds  = noise
  )
  expect_equal(dim(result), c(n_p, n_obs))
  expect_true(all(result == 0))
})

# ── print methods for et_prediction ─────────────────────────────────────────

test_that("print.et_prediction runs without error", {
  set.seed(1)
  n_d <- 50L; n_o <- 3L
  pp  <- matrix(rnorm(n_d * n_o), n_d, n_o)
  lp  <- pp
  lp_p <- pp
  sigma <- abs(rnorm(n_d, 0.5, 0.1))
  gauss_family <- list(family = "gaussian", link = "identity",
                       linkinv = identity)
  disp_draws   <- list(sigma = sigma, phi = NULL, shape = NULL, nu = NULL)
  decomp <- ErrorTracer:::.decompose_from_arrays(
    pp = pp, mu_draws = lp, mu_perturbed = lp_p, mu_draws_sub = lp,
    family = gauss_family, disp_draws = disp_draws
  )
  ci_df  <- ErrorTracer:::.compute_ci(pp, c(0.9))
  pred <- structure(
    list(posterior_predict = pp, posterior_linpred = lp,
         lp_perturbed = lp_p, sigma_draws = sigma,
         credible_intervals = ci_df, decomposition = decomp,
         newdata = data.frame(x = seq_len(n_o)),
         model = NULL, env_noise = NULL, n_draws = n_d),
    class = "et_prediction"
  )
  expect_output(print(pred), "ErrorTracer prediction")
})

test_that("print.et_prediction_list runs without error", {
  pred_list <- structure(
    list(predictions = list(A = NULL, B = NULL),
         grouping = "grp", newdata = data.frame()),
    class = "et_prediction_list"
  )
  expect_output(print(pred_list), "grouped predictions")
})
