# test-variance-twophase.R
# Tests for the two-phase variance engine: .twophasevar(), .twophase_mean(),
# .twophase_total(), .twophase_df(), and associated helpers.

skip_on_cran()

# ── Section 1: Engine unit tests (synthetic data) ─────────────────────────────

test_that(".twophasevar() returns a finite non-negative scalar for valid input (approx)", {
  df  <- make_survey_data(design = "twophase", n = 200L, n_psu = 20L,
                           n_strata = 2L, seed = 1L)
  ph1 <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  d   <- as_survey_twophase(ph1, subset = subset, method = "approx")

  inp <- .twophase_build_inputs(d, "y1", na.rm = TRUE)
  v   <- .twophasevar(inp$influence_mean, d)

  expect_true(is.numeric(v))
  expect_length(v, 1L)
  expect_true(is.finite(v))
  expect_gte(v, 0)
})

test_that(".twophasevar() phase 1 variance component is nonzero for clustered phase 1", {
  df  <- make_survey_data(design = "twophase", n = 200L, n_psu = 20L,
                           n_strata = 2L, seed = 2L)
  ph1 <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  d   <- as_survey_twophase(ph1, subset = subset, method = "approx")

  inp    <- .twophase_build_inputs(d, "y1", na.rm = TRUE)
  v1     <- .twophase_phase1_var(inp$influence_mean, d, "approx", "remove")

  expect_true(is.finite(v1))
  expect_gt(v1, 0)
})

test_that(".twophasevar() full variance > phase 1 variance alone (phase 2 adds uncertainty)", {
  df  <- make_survey_data(design = "twophase", n = 200L, n_psu = 20L,
                           n_strata = 2L, seed = 3L)
  ph1 <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  d   <- as_survey_twophase(ph1, subset = subset, method = "approx")

  inp    <- .twophase_build_inputs(d, "y1", na.rm = TRUE)
  subset <- d@data[[d@variables$subset]]

  v_total <- .twophasevar(inp$influence_mean, d)
  v1_only <- .twophase_phase1_var(inp$influence_mean, d, "approx", "remove")
  v2_only <- .twophase_phase2_var(inp$influence_mean, d, subset, "remove")

  expect_equal(v_total, v1_only + v2_only, tolerance = 1e-15)
  expect_gt(v2_only, 0)
  expect_gt(v_total, v1_only)
})

test_that('.twophasevar() with method = "simple" equals phase 1 component only', {
  df  <- make_survey_data(design = "twophase", n = 200L, n_psu = 20L,
                           n_strata = 2L, seed = 4L)
  ph1 <- suppressWarnings(
    as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  )
  d_simple <- suppressWarnings(
    as_survey_twophase(ph1, subset = subset, method = "simple")
  )

  inp    <- .twophase_build_inputs(d_simple, "y1", na.rm = TRUE)

  v_simple  <- .twophasevar(inp$influence_mean, d_simple)
  v1_simple <- .twophase_phase1_var(inp$influence_mean, d_simple,
                                     "simple", "remove")

  expect_equal(v_simple, v1_simple, tolerance = 1e-15)
})

test_that(".twophase_mean() returns list with mean, var, se of correct types", {
  df  <- make_survey_data(design = "twophase", n = 200L, n_psu = 20L,
                           n_strata = 2L, seed = 5L)
  ph1 <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  d   <- as_survey_twophase(ph1, subset = subset, method = "approx")

  result <- .twophase_mean(d, "y1")

  expect_type(result, "list")
  expect_named(result, c("mean", "var", "se"))
  expect_true(is.numeric(result$mean) && is.finite(result$mean))
  expect_true(is.numeric(result$var)  && result$var >= 0)
  expect_true(is.numeric(result$se)   && result$se >= 0)
  expect_equal(result$se, sqrt(result$var), tolerance = 1e-15)
})

test_that(".twophase_total() returns list with total, var, se of correct types", {
  df  <- make_survey_data(design = "twophase", n = 200L, n_psu = 20L,
                           n_strata = 2L, seed = 6L)
  ph1 <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  d   <- as_survey_twophase(ph1, subset = subset, method = "approx")

  result <- .twophase_total(d, "y1")

  expect_type(result, "list")
  expect_named(result, c("total", "var", "se"))
  expect_true(is.numeric(result$total) && is.finite(result$total))
  expect_true(is.numeric(result$var)   && result$var >= 0)
  expect_true(is.numeric(result$se)    && result$se >= 0)
  expect_equal(result$se, sqrt(result$var), tolerance = 1e-15)
})

test_that(".twophase_df() returns a nonnegative integer (class integer)", {
  df  <- make_survey_data(design = "twophase", n = 200L, n_psu = 20L,
                           n_strata = 2L, seed = 7L)
  ph1 <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  d   <- as_survey_twophase(ph1, subset = subset, method = "approx")

  df_val <- .twophase_df(d)

  expect_type(df_val, "integer")
  expect_length(df_val, 1L)
  expect_gte(df_val, 0L)
})

test_that(".twophase_df() returns correct df for nest = TRUE Phase 1 design", {
  # PSU IDs 1–5 reused within each stratum; nest = TRUE makes them globally unique.
  # 2 strata × 5 PSUs each = 10 distinct PSUs; df = 10 - 2 = 8.
  df_nest <- data.frame(
    psu    = c(rep(1:5, each = 2), rep(1:5, each = 2)),
    strata = c(rep(1L, 10), rep(2L, 10)),
    weight = rep(1, 20),
    y1     = rnorm(20),
    subset = rep(c(TRUE, FALSE), 10)
  )
  ph1 <- as_survey(df_nest, ids = psu, weights = weight, strata = strata,
                   nest = TRUE)
  d   <- as_survey_twophase(ph1, subset = subset, method = "approx")

  expect_equal(.twophase_df(d), 8L)
})

# ── Section 2: Error paths ────────────────────────────────────────────────────

test_that('.twophase_mean() errors for method = "full" with no phase 2 design info', {
  df  <- make_survey_data(design = "twophase", n = 100L, n_psu = 10L,
                           n_strata = 2L, seed = 10L)
  ph1 <- as_survey(df, ids = psu, weights = wt, strata = strata)
  # method = "full" with no ids2/strata2/probs2 → error at estimation time
  d   <- as_survey_twophase(ph1, subset = subset, method = "full")

  expect_error(
    .twophase_mean(d, "y1"),
    class = "surveycore_error_full_requires_phase2"
  )
  expect_snapshot(
    error = TRUE,
    .twophase_mean(d, "y1")
  )
})

# ── Section 3: Oracle — survival::pbc ─────────────────────────────────────────

test_that("get_means() full method matches survey::svymean on pbc [oracle]", {
  skip_if_not_installed("survival")
  skip_if_not_installed("survey")

  data("pbc", package = "survival", envir = environment())
  pbc_ph1        <- subset(pbc, !is.na(trt))
  pbc_ph1$in_ph2 <- !is.na(pbc_ph1$chol)
  pbc_ph1$wt     <- 1
  pbc_ph1$row_id <- seq_len(nrow(pbc_ph1))
  pbc_ph1$pi2    <- mean(pbc_ph1$in_ph2)

  ph1_sc   <- as_survey(pbc_ph1, ids = row_id, weights = wt)
  d_sc     <- as_survey_twophase(ph1_sc, probs2 = pi2, subset = in_ph2,
                                 method = "full")

  d_sv     <- survey::twophase(
    id     = list(~1, ~1),
    data   = pbc_ph1,
    subset = ~in_ph2,
    method = "approx"
  )

  sc_est <- get_means(d_sc, chol, variance = c("se", "ci"))
  sv_est <- survey::svymean(~chol, d_sv, na.rm = TRUE)

  expect_equal(sc_est$mean,    coef(sv_est)[["chol"]], tolerance = 1e-10)
  expect_equal(sc_est$se,      as.numeric(survey::SE(sv_est)),   tolerance = 1e-8)
  expect_equal(sc_est$ci_low,  confint(sv_est)[1], tolerance = 1e-6)
  expect_equal(sc_est$ci_high, confint(sv_est)[2], tolerance = 1e-6)
})

test_that("get_means() approx method matches survey::svymean on pbc [oracle]", {
  skip_if_not_installed("survival")
  skip_if_not_installed("survey")

  data("pbc", package = "survival", envir = environment())
  pbc_ph1        <- subset(pbc, !is.na(trt))
  pbc_ph1$in_ph2 <- !is.na(pbc_ph1$chol)
  pbc_ph1$wt     <- 1
  pbc_ph1$row_id <- seq_len(nrow(pbc_ph1))

  ph1_sc   <- as_survey(pbc_ph1, ids = row_id, weights = wt)
  d_sc     <- as_survey_twophase(ph1_sc, subset = in_ph2, method = "approx")

  d_sv     <- survey::twophase(
    id     = list(~1, ~1),
    data   = pbc_ph1,
    subset = ~in_ph2,
    method = "approx"
  )

  sc_est <- get_means(d_sc, chol, variance = c("se", "ci"))
  sv_est <- survey::svymean(~chol, d_sv, na.rm = TRUE)

  expect_equal(sc_est$mean,    coef(sv_est)[["chol"]], tolerance = 1e-10)
  expect_equal(sc_est$se,      as.numeric(survey::SE(sv_est)),   tolerance = 1e-8)
  expect_equal(sc_est$ci_low,  confint(sv_est)[1], tolerance = 1e-6)
  expect_equal(sc_est$ci_high, confint(sv_est)[2], tolerance = 1e-6)
})

test_that("get_totals() full method matches survey::svytotal on pbc [oracle]", {
  skip_if_not_installed("survival")
  skip_if_not_installed("survey")

  data("pbc", package = "survival", envir = environment())
  pbc_ph1        <- subset(pbc, !is.na(trt))
  pbc_ph1$in_ph2 <- !is.na(pbc_ph1$chol)
  pbc_ph1$wt     <- 1
  pbc_ph1$row_id <- seq_len(nrow(pbc_ph1))
  pbc_ph1$pi2    <- mean(pbc_ph1$in_ph2)

  ph1_sc   <- as_survey(pbc_ph1, ids = row_id, weights = wt)
  d_sc     <- as_survey_twophase(ph1_sc, probs2 = pi2, subset = in_ph2,
                                 method = "full")

  d_sv     <- survey::twophase(
    id     = list(~1, ~1),
    data   = pbc_ph1,
    subset = ~in_ph2,
    method = "approx"
  )

  sc_est <- get_totals(d_sc, chol, variance = c("se", "ci"))
  sv_est <- survey::svytotal(~chol, d_sv, na.rm = TRUE)

  expect_equal(sc_est$total,   coef(sv_est)[["chol"]], tolerance = 1e-10)
  expect_equal(sc_est$se,      as.numeric(survey::SE(sv_est)),    tolerance = 1e-8)
  expect_equal(sc_est$ci_low,  confint(sv_est)[1], tolerance = 1e-6)
  expect_equal(sc_est$ci_high, confint(sv_est)[2], tolerance = 1e-6)
})

test_that("get_totals() approx method matches survey::svytotal on pbc [oracle]", {
  skip_if_not_installed("survival")
  skip_if_not_installed("survey")

  data("pbc", package = "survival", envir = environment())
  pbc_ph1        <- subset(pbc, !is.na(trt))
  pbc_ph1$in_ph2 <- !is.na(pbc_ph1$chol)
  pbc_ph1$wt     <- 1
  pbc_ph1$row_id <- seq_len(nrow(pbc_ph1))

  ph1_sc   <- as_survey(pbc_ph1, ids = row_id, weights = wt)
  d_sc     <- as_survey_twophase(ph1_sc, subset = in_ph2, method = "approx")

  d_sv     <- survey::twophase(
    id     = list(~1, ~1),
    data   = pbc_ph1,
    subset = ~in_ph2,
    method = "approx"
  )

  sc_est <- get_totals(d_sc, chol, variance = c("se", "ci"))
  sv_est <- survey::svytotal(~chol, d_sv, na.rm = TRUE)

  expect_equal(sc_est$total,   coef(sv_est)[["chol"]], tolerance = 1e-10)
  expect_equal(sc_est$se,      as.numeric(survey::SE(sv_est)),    tolerance = 1e-8)
  expect_equal(sc_est$ci_low,  confint(sv_est)[1], tolerance = 1e-6)
  expect_equal(sc_est$ci_high, confint(sv_est)[2], tolerance = 1e-6)
})

# ── Section 4: Oracle — survival::nwtco ───────────────────────────────────────

test_that("get_means() full method matches survey::svymean on nwtco [oracle]", {
  skip_if_not_installed("survival")
  skip_if_not_installed("survey")

  data("nwtco", package = "survival", envir = environment())
  nwtco$in_ph2 <- nwtco$rel == 1 | nwtco$in.subcohort == 1
  nwtco$wt     <- 1

  ph1_sc <- as_survey(nwtco, ids = seqno, weights = wt)
  d_sc   <- as_survey_twophase(ph1_sc, strata2 = rel, subset = in_ph2,
                                method = "full")

  d_sv   <- survey::twophase(
    id     = list(~seqno, ~seqno),
    strata = list(NULL, ~rel),
    data   = nwtco,
    subset = ~in_ph2,
    method = "approx"
  )

  sc_est <- get_means(d_sc, edrel, variance = c("se", "ci"))
  sv_est <- survey::svymean(~edrel, d_sv, na.rm = TRUE)

  expect_equal(sc_est$mean,    coef(sv_est)[["edrel"]], tolerance = 1e-10)
  expect_equal(sc_est$se,      as.numeric(survey::SE(sv_est)),   tolerance = 1e-8)
  expect_equal(sc_est$ci_low,  confint(sv_est)[1], tolerance = 1e-6)
  expect_equal(sc_est$ci_high, confint(sv_est)[2], tolerance = 1e-6)
})

test_that("get_means() approx method matches survey::svymean on nwtco [oracle]", {
  skip_if_not_installed("survival")
  skip_if_not_installed("survey")

  data("nwtco", package = "survival", envir = environment())
  nwtco$in_ph2 <- nwtco$rel == 1 | nwtco$in.subcohort == 1
  nwtco$wt     <- 1

  ph1_sc <- as_survey(nwtco, ids = seqno, weights = wt)
  d_sc   <- as_survey_twophase(ph1_sc, strata2 = rel, subset = in_ph2,
                                method = "approx")

  d_sv   <- survey::twophase(
    id     = list(~seqno, ~seqno),
    strata = list(NULL, ~rel),
    data   = nwtco,
    subset = ~in_ph2,
    method = "approx"
  )

  sc_est <- get_means(d_sc, edrel, variance = c("se", "ci"))
  sv_est <- survey::svymean(~edrel, d_sv, na.rm = TRUE)

  expect_equal(sc_est$mean,    coef(sv_est)[["edrel"]], tolerance = 1e-10)
  expect_equal(sc_est$se,      as.numeric(survey::SE(sv_est)),   tolerance = 1e-8)
  expect_equal(sc_est$ci_low,  confint(sv_est)[1], tolerance = 1e-6)
  expect_equal(sc_est$ci_high, confint(sv_est)[2], tolerance = 1e-6)
})

test_that("get_totals() full method matches survey::svytotal on nwtco [oracle]", {
  skip_if_not_installed("survival")
  skip_if_not_installed("survey")

  data("nwtco", package = "survival", envir = environment())
  nwtco$in_ph2 <- nwtco$rel == 1 | nwtco$in.subcohort == 1
  nwtco$wt     <- 1

  ph1_sc <- as_survey(nwtco, ids = seqno, weights = wt)
  d_sc   <- as_survey_twophase(ph1_sc, strata2 = rel, subset = in_ph2,
                                method = "full")

  d_sv   <- survey::twophase(
    id     = list(~seqno, ~seqno),
    strata = list(NULL, ~rel),
    data   = nwtco,
    subset = ~in_ph2,
    method = "approx"
  )

  sc_est <- get_totals(d_sc, edrel, variance = c("se", "ci"))
  sv_est <- survey::svytotal(~edrel, d_sv, na.rm = TRUE)

  expect_equal(sc_est$total,   coef(sv_est)[["edrel"]], tolerance = 1e-10)
  expect_equal(sc_est$se,      as.numeric(survey::SE(sv_est)),    tolerance = 1e-8)
  expect_equal(sc_est$ci_low,  confint(sv_est)[1], tolerance = 1e-6)
  expect_equal(sc_est$ci_high, confint(sv_est)[2], tolerance = 1e-6)
})

test_that("get_totals() approx method matches survey::svytotal on nwtco [oracle]", {
  skip_if_not_installed("survival")
  skip_if_not_installed("survey")

  data("nwtco", package = "survival", envir = environment())
  nwtco$in_ph2 <- nwtco$rel == 1 | nwtco$in.subcohort == 1
  nwtco$wt     <- 1

  ph1_sc <- as_survey(nwtco, ids = seqno, weights = wt)
  d_sc   <- as_survey_twophase(ph1_sc, strata2 = rel, subset = in_ph2,
                                method = "approx")

  d_sv   <- survey::twophase(
    id     = list(~seqno, ~seqno),
    strata = list(NULL, ~rel),
    data   = nwtco,
    subset = ~in_ph2,
    method = "approx"
  )

  sc_est <- get_totals(d_sc, edrel, variance = c("se", "ci"))
  sv_est <- survey::svytotal(~edrel, d_sv, na.rm = TRUE)

  expect_equal(sc_est$total,   coef(sv_est)[["edrel"]], tolerance = 1e-10)
  expect_equal(sc_est$se,      as.numeric(survey::SE(sv_est)),    tolerance = 1e-8)
  expect_equal(sc_est$ci_low,  confint(sv_est)[1], tolerance = 1e-6)
  expect_equal(sc_est$ci_high, confint(sv_est)[2], tolerance = 1e-6)
})

# ── Section 5: Edge cases ──────────────────────────────────────────────────────

test_that(".twophase_mean() returns NA estimate when all Phase 2 y values are NA and na.rm = TRUE", {
  df      <- make_survey_data(design = "twophase", n = 100L, n_psu = 10L,
                               n_strata = 2L, seed = 20L)
  ph1     <- as_survey(df, ids = psu, weights = wt, strata = strata)
  d       <- as_survey_twophase(ph1, subset = subset, method = "approx")

  # Replace all Phase 2 y values with NA
  d2      <- d
  d2@data[d2@data[[d2@variables$subset]], "y1"] <- NA

  result <- .twophase_mean(d2, "y1", na.rm = TRUE)

  expect_true(is.na(result$mean))
  expect_true(is.na(result$var))
  expect_true(is.na(result$se))
})

test_that(".twophase_mean() propagates NA when na.rm = FALSE and y has NA", {
  df  <- make_survey_data(design = "twophase", n = 100L, n_psu = 10L,
                           n_strata = 2L, seed = 21L)
  ph1 <- as_survey(df, ids = psu, weights = wt, strata = strata)
  d   <- as_survey_twophase(ph1, subset = subset, method = "approx")

  # Introduce one NA in y1 for a Phase 2 row
  d2 <- d
  ph2_rows <- which(d2@data[[d2@variables$subset]])
  d2@data[ph2_rows[1L], "y1"] <- NA

  result <- .twophase_mean(d2, "y1", na.rm = FALSE)

  expect_true(is.na(result$mean))
  expect_true(is.na(result$se))
})

test_that(".twophase_mean() returns finite estimate for high Phase 2 fraction", {
  df  <- make_survey_data(design = "twophase", n = 100L, n_psu = 10L,
                           n_strata = 2L, phase2_frac = 0.95, seed = 22L)
  ph1 <- as_survey(df, ids = psu, weights = wt, strata = strata)
  d   <- as_survey_twophase(ph1, subset = subset, method = "approx")

  result <- .twophase_mean(d, "y1")

  expect_true(is.numeric(result$mean) && is.finite(result$mean))
  expect_true(is.numeric(result$se)   && result$se >= 0)
})

test_that(".twophase_mean() returns finite SE with small Phase 2 sample", {
  # Single Phase 2 row edge case: variance may be 0 or small but should be finite
  df  <- make_survey_data(design = "twophase", n = 50L, n_psu = 10L,
                           n_strata = 2L, phase2_frac = 0.1, seed = 23L)
  ph1 <- as_survey(df, ids = psu, weights = wt, strata = strata)
  d   <- as_survey_twophase(ph1, subset = subset, method = "approx")

  result <- .twophase_mean(d, "y1")

  # With small phase 2, mean and SE should still be numeric (possibly 0 SE)
  expect_true(is.numeric(result$mean))
  expect_true(is.numeric(result$se) && result$se >= 0)
})

# ---------------------------------------------------------------------------
# Block: get_corr() and edge cases for variance-twophase.R coverage
# ---------------------------------------------------------------------------

test_that("get_corr() works for survey_twophase design", {
  d <- make_survey_data(n = 100, n_psu = 10, n_strata = 2,
                        design = "twophase", seed = 70)
  phase1 <- as_survey(d, ids = psu, weights = wt, strata = strata,
                      fpc = fpc, nest = TRUE)
  sc <- as_survey_twophase(phase1, subset = subset,
                           ids2 = psu, strata2 = strata)

  result <- get_corr(sc, x = c(y1, y2), variance = "se")
  test_result_invariants(result, "survey_corr")
  expect_true(is.finite(result$r[[1L]]))
  expect_gte(result$se[[1L]], 0)
})

test_that("get_corr() twophase with small domain returns NA r", {
  d <- make_survey_data(n = 80, n_psu = 10, n_strata = 2,
                        design = "twophase", seed = 71)
  phase1 <- as_survey(d, ids = psu, weights = wt, strata = strata,
                      fpc = fpc, nest = TRUE)
  sc <- as_survey_twophase(phase1, subset = subset,
                           ids2 = psu, strata2 = strata)
  # Keep only 1 phase-2 row in domain via domain column
  ph2_rows <- which(d$subset)
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <-
    seq_len(nrow(d)) == ph2_rows[[1L]]

  result <- get_corr(sc, x = c(y1, y2), variance = "se")
  expect_true(is.na(result$r[[1L]]))
})

test_that("twophase design with nested phase1 PSUs covers nest branch in .twophasevar()", {
  # Use nest = TRUE in phase1 to trigger the PSU-uniqueness branch
  d <- make_survey_data(n = 100, n_psu = 10, n_strata = 2,
                        design = "twophase", seed = 72)
  phase1 <- as_survey(d, ids = psu, weights = wt, strata = strata,
                      fpc = fpc, nest = TRUE)
  sc <- as_survey_twophase(phase1, subset = subset,
                           ids2 = psu, strata2 = strata)

  result <- get_means(sc, y1, variance = "se")
  expect_true(is.finite(result$mean[[1L]]))
  expect_gte(result$se[[1L]], 0)
})

test_that(".twophase_total() via get_totals() returns finite values", {
  d <- make_survey_data(n = 80, n_psu = 10, n_strata = 2,
                        design = "twophase", seed = 73)
  phase1 <- as_survey(d, ids = psu, weights = wt, strata = strata, nest = TRUE)
  sc <- as_survey_twophase(phase1, subset = subset,
                           ids2 = psu, strata2 = strata)

  result <- get_totals(sc, y1, variance = "se")
  expect_true(is.finite(result$total[[1L]]))
  expect_gte(result$se[[1L]], 0)
})

# ---------------------------------------------------------------------------
# Additional coverage: "simple" method, .twophase_df() no-ids/no-strata
# ---------------------------------------------------------------------------

test_that("get_means() twophase with method='simple' covers .twophasevar() simple path", {
  # 'simple' method with no explicit ids2/strata2 requires method='simple'
  d <- make_survey_data(n = 100, n_psu = 10, n_strata = 2, design = "twophase", seed = 901)
  phase1 <- as_survey(d, ids = psu, weights = wt, strata = strata, fpc = fpc, nest = TRUE)
  sc <- as_survey_twophase(phase1, subset = subset, method = "simple")
  result <- get_means(sc, y1, variance = "se")
  test_result_invariants(result, "survey_means")
  expect_true(is.finite(result$mean[[1L]]))
})

test_that(".twophase_total() NA path fires when all domain values are NA", {
  # All y1 values set to NA with na.rm=FALSE → total is NA → early return in .twophase_total()
  d <- make_survey_data(n = 100, n_psu = 10, n_strata = 2, design = "twophase", seed = 902)
  phase1 <- as_survey(d, ids = psu, weights = wt, strata = strata, fpc = fpc, nest = TRUE)
  sc <- as_survey_twophase(phase1, subset = subset, method = "approx")
  sc@data$y1 <- NA_real_
  result <- surveycore:::.twophase_total(sc, "y1", na.rm = FALSE)
  expect_true(is.na(result$total))
})

# ── Section 6: SRS / replicate phase-1 designs ──────────────────────────────

test_that("two-phase with SRS phase-1 matches survey [oracle]", {
  skip_if_not_installed("survival")
  skip_if_not_installed("survey")

  # Use the pbc dataset: SRS phase-1 (no clusters, no strata)
  data("pbc", package = "survival", envir = environment())
  pbc_ph1        <- subset(pbc, !is.na(trt))
  pbc_ph1$in_ph2 <- !is.na(pbc_ph1$chol)
  pbc_ph1$wt     <- 1

  ph1_sc <- as_survey(pbc_ph1, weights = wt)
  d_sc   <- as_survey_twophase(ph1_sc, subset = in_ph2, method = "approx")

  d_sv <- survey::twophase(
    id     = list(~1, ~1),
    data   = pbc_ph1,
    subset = ~in_ph2,
    method = "approx"
  )

  sc_est <- get_means(d_sc, chol, variance = c("se", "ci"))
  sv_est <- survey::svymean(~chol, d_sv, na.rm = TRUE)

  expect_equal(sc_est$mean,    coef(sv_est)[["chol"]], tolerance = 1e-10)
  expect_equal(sc_est$se,      as.numeric(survey::SE(sv_est)), tolerance = 1e-8)
  expect_equal(sc_est$ci_low,  confint(sv_est)[1], tolerance = 1e-6)
  expect_equal(sc_est$ci_high, confint(sv_est)[2], tolerance = 1e-6)
})

test_that("two-phase with SRS phase-1 get_totals matches survey [oracle]", {
  skip_if_not_installed("survival")
  skip_if_not_installed("survey")

  data("pbc", package = "survival", envir = environment())
  pbc_ph1        <- subset(pbc, !is.na(trt))
  pbc_ph1$in_ph2 <- !is.na(pbc_ph1$chol)
  pbc_ph1$wt     <- 1

  ph1_sc <- as_survey(pbc_ph1, weights = wt)
  d_sc   <- as_survey_twophase(ph1_sc, subset = in_ph2, method = "approx")

  d_sv <- survey::twophase(
    id     = list(~1, ~1),
    data   = pbc_ph1,
    subset = ~in_ph2,
    method = "approx"
  )

  sc_est <- get_totals(d_sc, chol, variance = c("se", "ci"))
  sv_est <- survey::svytotal(~chol, d_sv, na.rm = TRUE)

  expect_equal(sc_est$total,   coef(sv_est)[["chol"]], tolerance = 1e-10)
  expect_equal(sc_est$se,      as.numeric(survey::SE(sv_est)), tolerance = 1e-8)
  expect_equal(sc_est$ci_low,  confint(sv_est)[1], tolerance = 1e-6)
  expect_equal(sc_est$ci_high, confint(sv_est)[2], tolerance = 1e-6)
})

test_that("two-phase with survey_replicate phase-1 constructs and estimates", {
  # No direct survey:: oracle for replicate phase-1 twophase, but verify

  # construction succeeds and estimation produces finite results
  df <- make_survey_data(
    n = 100, n_psu = 10L, design = "replicate", seed = 801L
  )
  df$in_phase2 <- c(rep(TRUE, 50), rep(FALSE, 50))

  phase_rep <- as_survey_replicate(
    df,
    weights = wt,
    repweights = starts_with("repwt_"),
    type = "JK1"
  )
  tp <- as_survey_twophase(phase_rep, subset = in_phase2, method = "approx")

  expect_true(S7::S7_inherits(tp, survey_twophase))

  result <- get_means(tp, y1, variance = "se")
  expect_true(is.finite(result$mean[[1L]]))
  expect_gte(result$se[[1L]], 0)
})
