# tests/testthat/test-analysis-variance-twophase-nonprob.R
#
# PR 2 of get-variance: numerical parity tests for survey_twophase and
# survey_nonprob dispatch paths of get_variance(), plus nonprob zero-weight
# and twophase Phase 1-only edge cases.
#
# Tolerances (from test-spec.md): point 1e-10, SE 1e-8.

skip_on_cran()

# ── Helpers ──────────────────────────────────────────────────────────────────

.variance_tol_point <- 1e-10
.variance_tol_se <- 1e-8

# ══════════════════════════════════════════════════════════════════════════════
# Twophase parity — uses pbc dataset where surveycore and survey twophase
# weight systems agree (id = list(~1, ~1), method = "approx").
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_variance() twophase approx matches survey::svyvar on pbc [oracle]", {
  skip_if_not_installed("survival")
  skip_if_not_installed("survey")

  data("pbc", package = "survival", envir = environment())
  pbc_ph1 <- subset(pbc, !is.na(trt))
  pbc_ph1$in_ph2 <- !is.na(pbc_ph1$chol)
  pbc_ph1$wt <- 1
  pbc_ph1$row_id <- seq_len(nrow(pbc_ph1))

  ph1_sc <- as_survey(pbc_ph1, ids = row_id, weights = wt)
  d_sc <- as_survey_twophase(ph1_sc, subset = in_ph2, method = "approx")
  test_invariants(d_sc)

  d_sv <- survey::twophase(
    id = list(~1, ~1),
    data = pbc_ph1,
    subset = ~in_ph2,
    method = "approx"
  )

  sc_est <- get_variance(d_sc, chol, variance = "se")
  sv_est <- survey::svyvar(~chol, d_sv, na.rm = TRUE)

  expect_equal(
    sc_est$variance[[1L]],
    coef(sv_est)[["chol"]],
    tolerance = .variance_tol_point
  )
  expect_equal(
    sc_est$se[[1L]],
    as.numeric(survey::SE(sv_est)),
    tolerance = .variance_tol_se
  )
})

test_that("get_variance() twophase full matches survey::svyvar on pbc [oracle]", {
  skip_if_not_installed("survival")
  skip_if_not_installed("survey")

  data("pbc", package = "survival", envir = environment())
  pbc_ph1 <- subset(pbc, !is.na(trt))
  pbc_ph1$in_ph2 <- !is.na(pbc_ph1$chol)
  pbc_ph1$wt <- 1
  pbc_ph1$row_id <- seq_len(nrow(pbc_ph1))
  pbc_ph1$pi2 <- mean(pbc_ph1$in_ph2)

  ph1_sc <- as_survey(pbc_ph1, ids = row_id, weights = wt)
  d_sc <- as_survey_twophase(
    ph1_sc,
    probs2 = pi2,
    subset = in_ph2,
    method = "full"
  )
  test_invariants(d_sc)

  d_sv <- survey::twophase(
    id = list(~1, ~1),
    data = pbc_ph1,
    subset = ~in_ph2,
    method = "approx"
  )

  sc_est <- get_variance(d_sc, chol, variance = "se")
  sv_est <- survey::svyvar(~chol, d_sv, na.rm = TRUE)

  expect_equal(
    sc_est$variance[[1L]],
    coef(sv_est)[["chol"]],
    tolerance = .variance_tol_point
  )
  expect_equal(
    sc_est$se[[1L]],
    as.numeric(survey::SE(sv_est)),
    tolerance = .variance_tol_se
  )
})

test_that("get_variance() twophase n reflects Phase 2 sample size", {
  skip_if_not_installed("survival")
  skip_if_not_installed("survey")

  data("pbc", package = "survival", envir = environment())
  pbc_ph1 <- subset(pbc, !is.na(trt))
  pbc_ph1$in_ph2 <- !is.na(pbc_ph1$chol)
  pbc_ph1$wt <- 1
  pbc_ph1$row_id <- seq_len(nrow(pbc_ph1))

  ph1_sc <- as_survey(pbc_ph1, ids = row_id, weights = wt)
  d_sc <- as_survey_twophase(ph1_sc, subset = in_ph2, method = "approx")
  test_invariants(d_sc)

  sc_est <- get_variance(d_sc, chol, variance = "se")

  # Phase 2 rows with observed y contribute; Phase 1-only rows contribute 0.
  # n equals number of rows used in the variance computation.
  expect_lt(sc_est$n[[1L]], nrow(pbc_ph1))
  expect_gt(sc_est$n[[1L]], 0L)
})

# ══════════════════════════════════════════════════════════════════════════════
# Nonprob parity
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_variance() on survey_nonprob matches survey::svyvar() [numerical]", {
  skip_if_not_installed("survey")

  set.seed(200L)
  df <- data.frame(
    y = rnorm(500L, mean = 5, sd = 2),
    w = runif(500L, 0.5, 3)
  )
  d <- as_survey_nonprob(df, weights = w)
  test_invariants(d)

  d_sv <- survey::svydesign(ids = ~1, weights = ~w, data = df)
  sv <- survey::svyvar(~y, d_sv, na.rm = TRUE)

  sc <- get_variance(d, y, variance = "se")

  expect_equal(
    sc$variance[[1L]],
    coef(sv)[["y"]],
    tolerance = .variance_tol_point
  )
  expect_equal(
    sc$se[[1L]],
    as.numeric(survey::SE(sv)),
    tolerance = .variance_tol_se
  )
})

test_that("get_variance() nonprob excludes zero-weight rows from n and mean", {
  skip_if_not_installed("survey")

  set.seed(201L)
  n <- 200L
  y_full <- rnorm(n, mean = 10, sd = 3)
  w_full <- runif(n, 0.5, 2)

  df_pos <- data.frame(y = y_full, w = w_full)
  d <- as_survey_nonprob(df_pos, weights = w)

  # Inject zero-weight rows post-construction (the constructor validator
  # rejects non-positive weights; zero weights are physically present in
  # real non-probability data after post-stratification trimming, so
  # @data can carry them even though the validator doesn't accept them).
  zero_idx <- c(5L, 17L, 42L, 101L, 150L)
  df_inj <- df_pos
  df_inj$w[zero_idx] <- 0
  d <- S7::set_props(d, data = df_inj)
  test_invariants(d)

  # Oracle: pre-filter zero-weight rows, then run svyvar on svydesign(ids=~1)
  df_keep <- df_inj[df_inj$w > 0, , drop = FALSE]
  d_sv <- survey::svydesign(ids = ~1, weights = ~w, data = df_keep)
  sv <- survey::svyvar(~y, d_sv, na.rm = TRUE)

  sc <- get_variance(d, y, variance = "se")

  # n excludes zero-weight rows
  expect_identical(sc$n[[1L]], nrow(df_keep))

  expect_equal(
    sc$variance[[1L]],
    coef(sv)[["y"]],
    tolerance = .variance_tol_point
  )
  expect_equal(
    sc$se[[1L]],
    as.numeric(survey::SE(sv)),
    tolerance = .variance_tol_se
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# Result class + shape
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_variance() twophase returns survey_variance tibble", {
  skip_if_not_installed("survival")

  data("pbc", package = "survival", envir = environment())
  pbc_ph1 <- subset(pbc, !is.na(trt))
  pbc_ph1$in_ph2 <- !is.na(pbc_ph1$chol)
  pbc_ph1$wt <- 1
  pbc_ph1$row_id <- seq_len(nrow(pbc_ph1))

  ph1_sc <- as_survey(pbc_ph1, ids = row_id, weights = wt)
  d <- as_survey_twophase(ph1_sc, subset = in_ph2, method = "approx")
  test_invariants(d)

  sc <- get_variance(d, chol)
  expect_s3_class(sc, "survey_variance")
  expect_s3_class(sc, "survey_result")
  expect_true(tibble::is_tibble(sc))
  expect_true("variance" %in% names(sc))
  expect_true("n" %in% names(sc))
})

test_that("get_variance() nonprob returns survey_variance tibble", {
  set.seed(301L)
  df <- data.frame(y = rnorm(100L), w = runif(100L, 0.5, 2))
  d <- as_survey_nonprob(df, weights = w)
  test_invariants(d)

  sc <- get_variance(d, y)
  expect_s3_class(sc, "survey_variance")
  expect_s3_class(sc, "survey_result")
  expect_true(tibble::is_tibble(sc))
})
