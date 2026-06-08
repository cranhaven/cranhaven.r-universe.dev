# tests/testthat/test-env-noise.R
# Unit tests for .resolve_env_noise() — covers NULL, scalar, named scalar,
# and the new per-row (time-varying) vector form.

n_obs      <- 6L
pred_names <- c("Tmean", "PPT", "SWE")
newdata    <- data.frame(
  Tmean = rnorm(n_obs),
  PPT   = rnorm(n_obs),
  SWE   = rnorm(n_obs)
)

# ── NULL ────────────────────────────────────────────────────────────────────

test_that("resolve_env_noise(NULL) returns all-zero list", {
  res <- ErrorTracer:::.resolve_env_noise(NULL, pred_names, newdata)
  expect_type(res, "list")
  expect_named(res, pred_names)
  for (p in pred_names) expect_true(all(res[[p]] == 0))
  for (p in pred_names) expect_length(res[[p]], n_obs)
})

# ── Scalar fraction ──────────────────────────────────────────────────────────

test_that("resolve_env_noise(scalar) scales by predictor SD", {
  res <- ErrorTracer:::.resolve_env_noise(0.1, pred_names, newdata)
  expect_type(res, "list")
  for (p in pred_names) {
    expect_length(res[[p]], n_obs)
    expected_sd <- 0.1 * sd(newdata[[p]])
    expect_equal(res[[p]], rep(expected_sd, n_obs), tolerance = 1e-10)
  }
})

# ── Named constant list ──────────────────────────────────────────────────────

test_that("resolve_env_noise(named list of scalars) replicates to n_obs", {
  en  <- list(Tmean = 0.5, PPT = 0.2)
  res <- ErrorTracer:::.resolve_env_noise(en, pred_names, newdata)
  expect_length(res[["Tmean"]], n_obs)
  expect_true(all(res[["Tmean"]] == 0.5))
  expect_true(all(res[["PPT"]]   == 0.2))
  expect_true(all(res[["SWE"]]   == 0.0))  # not supplied => zero
})

test_that("resolve_env_noise silently ignores unknown predictor names", {
  # .et_warn() uses an internal logger, not warning() — unknown names are
  # logged but the call still completes, returning zero noise for that pred.
  res <- ErrorTracer:::.resolve_env_noise(
    list(Tmean = 0.5, WIND = 0.1), pred_names, newdata
  )
  # WIND is not a model predictor; result should still have all pred_names
  expect_named(res, pred_names)
  expect_true(all(res[["Tmean"]] == 0.5))
})

# ── Per-row vector (time-varying noise) ─────────────────────────────────────

test_that("resolve_env_noise accepts per-row vector and preserves it", {
  growing <- seq(0.30, 0.80, length.out = n_obs)
  en  <- list(Tmean = growing, PPT = 0.20)
  res <- ErrorTracer:::.resolve_env_noise(en, pred_names, newdata)
  expect_equal(res[["Tmean"]], growing, tolerance = 1e-10)
  expect_true(all(res[["PPT"]] == 0.20))
})

test_that("resolve_env_noise errors when vector length != 1 and != n_obs", {
  en <- list(Tmean = c(0.3, 0.4))   # length 2, but n_obs = 6
  expect_error(
    ErrorTracer:::.resolve_env_noise(en, pred_names, newdata),
    "length"
  )
})

# ── Integration: per-row noise produces larger env_var for noisier obs ───────

test_that("time-varying noise inflates env_var for high-noise observations", {
  # Build a tiny mock draws matrix
  set.seed(7L)
  n_perturb  <- 300L
  draws_mat  <- matrix(
    c(rep(0.5, n_perturb),     # b_Intercept
      rep(1.0, n_perturb),     # b_Tmean
      rep(0.0, n_perturb),     # b_PPT
      rep(0.0, n_perturb)),    # b_SWE
    nrow = n_perturb,
    dimnames = list(NULL, c("b_Intercept", "b_Tmean", "b_PPT", "b_SWE"))
  )

  # Noise grows: first half of obs have low noise, second half high noise
  noise_sds <- list(
    Tmean = c(rep(0.01, n_obs %/% 2), rep(1.00, n_obs - n_obs %/% 2)),
    PPT   = rep(0, n_obs),
    SWE   = rep(0, n_obs)
  )

  lp_p <- ErrorTracer:::.compute_lp_perturbed(
    draws_mat  = draws_mat,
    newdata    = newdata,
    pred_names = pred_names,
    noise_sds  = noise_sds
  )

  # Variance across perturbation draws should be higher for high-noise obs
  lp_var <- apply(lp_p, 2, var)
  low_noise_mean  <- mean(lp_var[seq_len(n_obs %/% 2)])
  high_noise_mean <- mean(lp_var[(n_obs %/% 2 + 1):n_obs])
  expect_gt(high_noise_mean, low_noise_mean * 10)
})
