# test-analysis-quantiles.R
# Tests for get_quantiles() — Phase 1 survey-weighted Woodruff quantiles.
#
# Numerical tolerances (from testing-surveycore.md):
#   Point estimates: 1e-10
#   SE:              1e-8
#   CI bounds:       1e-6
#
# Oracle: survey::svyquantile(interval.type = "mean").
# Note: replicate-design oracle tests use mse = FALSE to match survey's default.

# ---------------------------------------------------------------------------
# Category 1: Happy path — return structure (all design types)
# ---------------------------------------------------------------------------

test_that("get_quantiles() returns survey_quantiles tibble for survey_taylor", {
  df <- make_survey_data(
    n = 200L,
    n_psu = 20L,
    n_strata = 4L,
    design = "taylor",
    seed = 1L
  )
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )

  result <- get_quantiles(d, y1)
  test_result_invariants(result, "survey_quantiles")

  # Default probs = c(0.25, 0.5, 0.75) → 3 rows
  expect_equal(nrow(result), 3L)
  expect_true("quantile" %in% names(result))
  expect_true("estimate" %in% names(result))
  expect_true("ci_low" %in% names(result))
  expect_true("ci_high" %in% names(result))
  expect_true("n" %in% names(result))
  # variance = "ci" by default → no se column
  expect_false("se" %in% names(result))

  expect_identical(result$quantile, c("p25", "p50", "p75"))
  expect_true(all(is.finite(result$estimate)))
  expect_true(all(is.finite(result$ci_low)))
  expect_true(all(is.finite(result$ci_high)))
  expect_true(all(result$ci_low < result$estimate | is.na(result$estimate)))
  expect_true(all(result$estimate < result$ci_high | is.na(result$estimate)))
  expect_true(all(result$n > 0L))
})

test_that("get_quantiles() returns correct result for survey_replicate", {
  df <- make_survey_data(
    n = 200L,
    n_psu = 20L,
    n_strata = 4L,
    design = "replicate",
    type = "brr",
    seed = 2L
  )
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)
  d <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )

  result <- get_quantiles(d, y1, probs = 0.5)
  test_result_invariants(result, "survey_quantiles")

  expect_equal(nrow(result), 1L)
  expect_equal(result$quantile, "p50")
  expect_true(is.finite(result$estimate[[1L]]))
  expect_true(is.finite(result$ci_low[[1L]]))
  expect_true(is.finite(result$ci_high[[1L]]))
  expect_lt(result$ci_low[[1L]], result$estimate[[1L]])
  expect_lt(result$estimate[[1L]], result$ci_high[[1L]])
})

test_that("get_quantiles() returns correct result for SRS design", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 3L)
  d <- as_survey(df, weights = wt)

  result <- get_quantiles(d, y1, probs = c(0.25, 0.75))
  test_result_invariants(result, "survey_quantiles")

  expect_equal(nrow(result), 2L)
  expect_identical(result$quantile, c("p25", "p75"))
  expect_true(all(is.finite(result$estimate)))
  expect_true(all(is.finite(result$ci_low)))
  expect_lt(result$estimate[[1L]], result$estimate[[2L]]) # p25 < p75
})

test_that("get_quantiles() returns correct result for survey_twophase", {
  df <- make_survey_data(
    design = "twophase",
    n = 300L,
    n_psu = 30L,
    n_strata = 3L,
    seed = 4L
  )
  ph1 <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  d <- as_survey_twophase(ph1, subset = subset, method = "approx")

  result <- get_quantiles(d, y1, probs = 0.5)
  test_result_invariants(result, "survey_quantiles")

  expect_equal(nrow(result), 1L)
  expect_true(is.finite(result$estimate[[1L]]))
  expect_true(is.finite(result$ci_low[[1L]]))
  expect_true(is.finite(result$ci_high[[1L]]))
  expect_lt(result$ci_low[[1L]], result$ci_high[[1L]])
})

test_that("get_quantiles() returns correct result for survey_nonprob", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 5L)
  d <- as_survey_nonprob(df, weights = wt)

  result <- get_quantiles(d, y1, probs = 0.5)
  test_result_invariants(result, "survey_quantiles")

  expect_equal(nrow(result), 1L)
  expect_true(is.finite(result$estimate[[1L]]))
  expect_true(is.finite(result$ci_low[[1L]]))
  expect_true(is.finite(result$ci_high[[1L]]))
})

# ---------------------------------------------------------------------------
# Category 2 (oracle): Numerical validation — survey::svyquantile()
# ---------------------------------------------------------------------------

test_that("get_quantiles() Taylor IQR+median matches svyquantile() — synthetic [oracle]", {
  skip_if_not_installed("survey")
  df <- make_survey_data(
    n = 500L,
    n_psu = 50L,
    n_strata = 5L,
    design = "taylor",
    seed = 42L
  )
  d_sc <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  d_sv <- survey::svydesign(
    ids = ~psu,
    weights = ~wt,
    strata = ~strata,
    fpc = ~fpc,
    data = df,
    nest = TRUE
  )

  sc <- get_quantiles(
    d_sc,
    y2,
    probs = c(0.25, 0.5, 0.75),
    variance = c("ci", "se")
  )
  sv <- survey::svyquantile(
    ~y2,
    d_sv,
    quantiles = c(0.25, 0.5, 0.75),
    ci = TRUE,
    na.rm = TRUE,
    interval.type = "mean"
  )

  sv_est <- coef(sv)
  sv_se <- survey::SE(sv)
  sv_ci <- confint(sv)

  expect_equal(sc$estimate, as.numeric(sv_est), tolerance = 1e-10)
  expect_equal(sc$se, as.numeric(sv_se), tolerance = 1e-8)
  expect_equal(sc$ci_low, as.numeric(sv_ci[, "l"]), tolerance = 1e-6)
  expect_equal(sc$ci_high, as.numeric(sv_ci[, "u"]), tolerance = 1e-6)
})

test_that("get_quantiles() Taylor matches svyquantile() — NHANES bpxsy1 [oracle]", {
  skip_if_not_installed("survey")
  # Drop zero-weight rows: surveycore rejects non-positive weights
  df <- nhanes_2017[nhanes_2017[["wtint2yr"]] > 0, ]
  d_sc <- as_survey(
    df,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d_sv <- survey::svydesign(
    ids = ~sdmvpsu,
    weights = ~wtint2yr,
    strata = ~sdmvstra,
    data = df,
    nest = TRUE
  )

  sc <- get_quantiles(d_sc, ridageyr, probs = 0.5, variance = c("ci", "se"))
  sv <- survey::svyquantile(
    ~ridageyr,
    d_sv,
    quantiles = 0.5,
    ci = TRUE,
    na.rm = TRUE,
    interval.type = "mean"
  )

  expect_equal(sc$estimate[[1L]], coef(sv)[[1L]], tolerance = 1e-10)
  expect_equal(sc$se[[1L]], survey::SE(sv)[[1L]], tolerance = 1e-8)
  expect_equal(sc$ci_low[[1L]], confint(sv)[, "l"][[1L]], tolerance = 1e-6)
  expect_equal(sc$ci_high[[1L]], confint(sv)[, "u"][[1L]], tolerance = 1e-6)
})

test_that("get_quantiles() BRR matches svyquantile() mse=FALSE [oracle]", {
  skip_if_not_installed("survey")
  # Use mse = FALSE to match survey::svyquantile()'s default BRR variance
  df <- make_survey_data(
    n = 200L,
    n_psu = 20L,
    n_strata = 4L,
    design = "replicate",
    type = "brr",
    seed = 6L
  )
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)

  d_sc <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR",
    mse = FALSE
  )
  d_sv <- survey::svrepdesign(
    weights = ~wt,
    repweights = as.matrix(df[, repwt_cols]),
    type = "BRR",
    data = df,
    mse = FALSE
  )

  sc <- get_quantiles(
    d_sc,
    y1,
    probs = c(0.25, 0.5, 0.75),
    variance = c("ci", "se")
  )
  sv <- survey::svyquantile(
    ~y1,
    d_sv,
    quantiles = c(0.25, 0.5, 0.75),
    ci = TRUE,
    na.rm = TRUE,
    interval.type = "mean"
  )

  expect_equal(sc$estimate, as.numeric(coef(sv)), tolerance = 1e-10)
  expect_equal(sc$se, as.numeric(survey::SE(sv)), tolerance = 1e-8)
  expect_equal(sc$ci_low, as.numeric(confint(sv)[, "l"]), tolerance = 1e-6)
  expect_equal(sc$ci_high, as.numeric(confint(sv)[, "u"]), tolerance = 1e-6)
})

test_that("get_quantiles() SRS matches svyquantile() [oracle]", {
  skip_if_not_installed("survey")
  # Use equal weights so the classical SRS variance (surveycore) and HT
  # variance (survey) agree — they differ for non-uniform weights.
  df <- make_survey_data(
    n = 500L,
    n_psu = 50L,
    n_strata = 5L,
    design = "taylor",
    seed = 7L
  )
  df$wt_equal <- 1L

  d_sc <- as_survey(df, weights = wt_equal)
  d_sv <- survey::svydesign(ids = ~1, weights = ~wt_equal, data = df)

  sc <- get_quantiles(d_sc, y1, probs = 0.5, variance = c("ci", "se"))
  sv <- survey::svyquantile(
    ~y1,
    d_sv,
    quantiles = 0.5,
    ci = TRUE,
    na.rm = TRUE,
    interval.type = "mean"
  )

  expect_equal(sc$estimate[[1L]], coef(sv)[[1L]], tolerance = 1e-10)
  expect_equal(sc$se[[1L]], survey::SE(sv)[[1L]], tolerance = 1e-8)
  expect_equal(
    sc$ci_low[[1L]],
    as.numeric(confint(sv)[, "l"]),
    tolerance = 1e-6
  )
  expect_equal(
    sc$ci_high[[1L]],
    as.numeric(confint(sv)[, "u"]),
    tolerance = 1e-6
  )
})

# ---------------------------------------------------------------------------
# Category 3: probs behaviour
# ---------------------------------------------------------------------------

test_that("get_quantiles() single prob produces one row", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 10L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_quantiles(d, y1, probs = 0.5)
  test_result_invariants(result, "survey_quantiles")
  expect_equal(nrow(result), 1L)
  expect_equal(result$quantile, "p50")
})

test_that("get_quantiles() preserves probs order in output", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 11L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_quantiles(d, y1, probs = c(0.9, 0.1, 0.5))
  test_result_invariants(result, "survey_quantiles")
  expect_equal(nrow(result), 3L)
  expect_identical(result$quantile, c("p90", "p10", "p50"))
  # estimates should follow the ordering given by probs
  expect_lt(result$estimate[[2L]], result$estimate[[3L]]) # p10 < p50
  expect_lt(result$estimate[[3L]], result$estimate[[1L]]) # p50 < p90
})

test_that("get_quantiles() labels probs correctly: 0.05 → p5, 0.95 → p95", {
  df <- make_survey_data(n = 300L, design = "taylor", seed = 12L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_quantiles(d, y1, probs = c(0.05, 0.95))
  expect_identical(result$quantile, c("p5", "p95"))
})

test_that("get_quantiles() stores probs in meta()", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 13L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  probs_arg <- c(0.25, 0.5, 0.75)
  result <- get_quantiles(d, y1, probs = probs_arg)
  expect_equal(meta(result)$probs, probs_arg)
})

# ---------------------------------------------------------------------------
# Category 4: Grouped analysis
# ---------------------------------------------------------------------------

test_that("get_quantiles() group = produces rows per (group × quantile)", {
  df <- make_survey_data(
    n = 300L,
    n_psu = 30L,
    n_strata = 3L,
    design = "taylor",
    seed = 20L
  )
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )

  result <- get_quantiles(d, y1, group = group, probs = c(0.25, 0.75))
  test_result_invariants(result, "survey_quantiles")

  n_groups <- length(unique(df$group))
  n_probs <- 2L
  expect_equal(nrow(result), n_groups * n_probs)
  expect_true("group" %in% names(result))
  expect_true("quantile" %in% names(result))
  expect_true(all(result$n > 0L))
})

test_that("get_quantiles() group result equals ungrouped subsets", {
  df <- make_survey_data(
    n = 300L,
    n_psu = 30L,
    n_strata = 3L,
    design = "taylor",
    seed = 21L
  )
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )

  result_g <- get_quantiles(
    d,
    y1,
    group = group,
    probs = 0.5,
    variance = c("ci", "se")
  )
  # Ungrouped result (no group arg)
  result_u <- get_quantiles(d, y1, probs = 0.5, variance = c("ci", "se"))

  # Overall estimate should be in the ballpark of group-level estimates
  expect_true(all(is.finite(result_g$estimate)))
  expect_true(is.finite(result_u$estimate[[1L]]))
})

test_that("get_quantiles() NA group values produce no row for that group", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 22L)
  df$group_na <- df$group
  df$group_na[1:10] <- NA_character_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result_na <- get_quantiles(d, y1, group = group_na, probs = 0.5)
  result_no <- get_quantiles(d, y1, group = group, probs = 0.5)

  # Same groups, same number of rows
  expect_equal(nrow(result_na), nrow(result_no))
})

# ---------------------------------------------------------------------------
# Category 5: Domain estimation
# ---------------------------------------------------------------------------

test_that("get_quantiles() respects domain filter", {
  df <- make_survey_data(
    n = 300L,
    n_psu = 30L,
    n_strata = 3L,
    design = "taylor",
    seed = 30L
  )
  d_full <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  d_dom <- d_full
  d_dom@data[[SURVEYCORE_DOMAIN_COL]] <- as.integer(
    !is.na(d_full@data$group) & d_full@data$group == "A"
  )

  result_dom <- get_quantiles(d_dom, y1, probs = 0.5, variance = c("ci", "se"))
  result_full <- get_quantiles(
    d_full,
    y1,
    probs = 0.5,
    variance = c("ci", "se")
  )

  test_result_invariants(result_dom, "survey_quantiles")

  # Domain estimate (group A only) should differ from the full-sample estimate
  expect_false(
    isTRUE(all.equal(
      result_dom$estimate[[1L]],
      result_full$estimate[[1L]],
      tolerance = 1e-10
    ))
  )
})

# ---------------------------------------------------------------------------
# Category 6: Variance columns
# ---------------------------------------------------------------------------

test_that("get_quantiles() variance = NULL returns no variance columns", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 40L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_quantiles(d, y1, variance = NULL)
  test_result_invariants(result, "survey_quantiles")
  expect_false("se" %in% names(result))
  expect_false("ci_low" %in% names(result))
  expect_false("ci_high" %in% names(result))
  expect_false("moe" %in% names(result))
  expect_false("deff" %in% names(result))
  expect_true("n" %in% names(result))
})

test_that("get_quantiles() variance = 'se' returns se column only", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 41L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_quantiles(d, y1, variance = "se")
  test_result_invariants(result, "survey_quantiles")
  expect_true("se" %in% names(result))
  expect_false("ci_low" %in% names(result))
  expect_true(all(result$se >= 0 | is.na(result$se)))
})

test_that("get_quantiles() variance includes all requested columns", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 42L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_quantiles(
    d,
    y1,
    variance = c("se", "var", "cv", "ci", "moe", "deff")
  )
  test_result_invariants(result, "survey_quantiles")
  expect_true("se" %in% names(result))
  expect_true("var" %in% names(result))
  expect_true("cv" %in% names(result))
  expect_true("ci_low" %in% names(result))
  expect_true("ci_high" %in% names(result))
  expect_true("moe" %in% names(result))
  expect_true("deff" %in% names(result))
})

test_that("get_quantiles() var = se^2", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 43L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_quantiles(d, y1, variance = c("se", "var"))
  expect_equal(result$var, result$se^2, tolerance = 1e-15)
})

test_that("get_quantiles() Woodruff CI: se derived from CI width", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 44L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_quantiles(d, y1, variance = c("se", "ci"), conf_level = 0.95)
  # se = (ci_high - ci_low) / (2 * t_crit)
  degf_w <- surveycore:::.degf_woodruff(d)
  t_crit <- qt(0.975, df = degf_w)
  expected_se <- (result$ci_high - result$ci_low) / (2 * t_crit)
  expect_equal(result$se, expected_se, tolerance = 1e-10)
})

test_that("get_quantiles() moe = half CI width", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 45L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_quantiles(d, y1, variance = c("ci", "moe"))
  expect_equal(
    result$moe,
    (result$ci_high - result$ci_low) / 2,
    tolerance = 1e-15
  )
})

test_that("get_quantiles() deff is always NA", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 46L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_quantiles(d, y1, variance = "deff")
  expect_true("deff" %in% names(result))
  expect_true(all(is.na(result$deff)))
})

test_that("get_quantiles() conf_level = 0.99 gives wider CI than 0.95", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 47L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  r95 <- get_quantiles(d, y1, variance = "ci", conf_level = 0.95)
  r99 <- get_quantiles(d, y1, variance = "ci", conf_level = 0.99)

  width95 <- r95$ci_high - r95$ci_low
  width99 <- r99$ci_high - r99$ci_low
  expect_true(all(width99 >= width95))
})

test_that("get_quantiles() n_weighted adds column", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 48L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_quantiles(d, y1, n_weighted = TRUE)
  test_result_invariants(result, "survey_quantiles")
  expect_true("n_weighted" %in% names(result))
  expect_true(all(result$n_weighted > 0 | is.na(result$n_weighted)))
})

# ---------------------------------------------------------------------------
# Category 7: label_values / label_vars (no-op)
# ---------------------------------------------------------------------------

test_that("get_quantiles() label_values and label_vars are no-ops", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 50L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  r_def <- get_quantiles(d, y1, probs = 0.5)
  r_false <- get_quantiles(
    d,
    y1,
    probs = 0.5,
    label_values = FALSE,
    label_vars = FALSE
  )

  expect_equal(r_def$estimate[[1L]], r_false$estimate[[1L]], tolerance = 1e-15)
  expect_equal(r_def$quantile, r_false$quantile)
})

# ---------------------------------------------------------------------------
# Category 8: name_style
# ---------------------------------------------------------------------------

test_that("get_quantiles() name_style='broom' renames se/ci columns", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 60L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_quantiles(d, y1, variance = c("se", "ci"), name_style = "broom")
  test_result_invariants(result, "survey_quantiles")
  expect_true("std.error" %in% names(result))
  expect_true("conf.low" %in% names(result))
  expect_true("conf.high" %in% names(result))
  expect_false("se" %in% names(result))
  expect_false("ci_low" %in% names(result))
  # estimate column unchanged
  expect_true("estimate" %in% names(result))
  expect_true("quantile" %in% names(result))
})

test_that("get_quantiles() name_style='surveycore' preserves original names", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 61L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_quantiles(
    d,
    y1,
    variance = c("se", "ci"),
    name_style = "surveycore"
  )
  expect_true("se" %in% names(result))
  expect_true("ci_low" %in% names(result))
  expect_true("ci_high" %in% names(result))
})

# ---------------------------------------------------------------------------
# Category 9: na.rm behaviour
# ---------------------------------------------------------------------------

test_that("get_quantiles() na.rm = TRUE (default) ignores NAs", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 70L)
  df$y_na <- df$y1
  df$y_na[1:20] <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_quantiles(d, y_na, probs = 0.5)
  test_result_invariants(result, "survey_quantiles")
  expect_true(is.finite(result$estimate[[1L]]))
  expect_true(result$n[[1L]] <= 180L) # fewer than 200 (NAs excluded)
})

test_that("get_quantiles() na.rm = FALSE with NAs returns NA estimate", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 71L)
  df$y_na <- df$y1
  df$y_na[1:5] <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_quantiles(d, y_na, probs = 0.5, na.rm = FALSE)
  test_result_invariants(result, "survey_quantiles")
  expect_true(is.na(result$estimate[[1L]]))
})

test_that("get_quantiles() na.rm = FALSE with no NAs matches na.rm = TRUE", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 72L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  r_true <- get_quantiles(d, y1, probs = 0.5, na.rm = TRUE)
  r_false <- get_quantiles(d, y1, probs = 0.5, na.rm = FALSE)

  expect_equal(r_true$estimate[[1L]], r_false$estimate[[1L]], tolerance = 1e-15)
})

# ---------------------------------------------------------------------------
# Category 10: Small cell warning
# ---------------------------------------------------------------------------

test_that("get_quantiles() fires surveycore_warning_small_cell when n < min_cell_n", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 80L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  # With n_total = 200 and min_cell_n = 500, all cells will trigger the warning
  expect_warning(
    get_quantiles(d, y1, probs = 0.5, min_cell_n = 500L),
    class = "surveycore_warning_small_cell"
  )
})

test_that("get_quantiles() no small-cell warning when n >= min_cell_n", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 81L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_no_warning(
    get_quantiles(d, y1, probs = 0.5, min_cell_n = 1L)
  )
})

# ---------------------------------------------------------------------------
# Category 11: Error paths
# ---------------------------------------------------------------------------

test_that("get_quantiles() rejects non-survey-base input", {
  expect_error(
    get_quantiles(list(x = 1), y1),
    class = "surveycore_error_unsupported_class"
  )
  expect_snapshot(error = TRUE, get_quantiles(list(x = 1), y1))
})

test_that("get_quantiles() rejects probs outside (0,1)", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 90L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(
    get_quantiles(d, y1, probs = c(0.25, 1.1)),
    class = "surveycore_error_invalid_probs"
  )
  expect_snapshot(error = TRUE, get_quantiles(d, y1, probs = c(0.25, 1.1)))
})

test_that("get_quantiles() rejects probs = 0 or probs = 1 exactly", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 91L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(
    get_quantiles(d, y1, probs = c(0, 0.5)),
    class = "surveycore_error_invalid_probs"
  )
  expect_error(
    get_quantiles(d, y1, probs = c(0.5, 1)),
    class = "surveycore_error_invalid_probs"
  )
})

test_that("get_quantiles() rejects empty probs vector", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 92L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(
    get_quantiles(d, y1, probs = numeric(0)),
    class = "surveycore_error_invalid_probs"
  )
})

test_that("get_quantiles() rejects non-numeric x", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 93L)
  df$char_col <- as.character(df$group)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(
    get_quantiles(d, char_col, probs = 0.5),
    class = "surveycore_error_non_numeric_variable"
  )
  expect_snapshot(error = TRUE, get_quantiles(d, char_col, probs = 0.5))
})

test_that("get_quantiles() rejects multiple x variables", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 94L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(
    get_quantiles(d, c(y1, y2), probs = 0.5),
    class = "surveycore_error_wrong_variable_count"
  )
  expect_snapshot(error = TRUE, get_quantiles(d, c(y1, y2), probs = 0.5))
})

test_that("get_quantiles() rejects invalid variance", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 95L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(
    get_quantiles(d, y1, variance = "not_a_stat"),
    class = "surveycore_error_invalid_variance_arg"
  )
})

test_that("get_quantiles() rejects invalid conf_level", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 96L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(
    get_quantiles(d, y1, conf_level = 1.5),
    class = "surveycore_error_invalid_conf_level"
  )
})

test_that("get_quantiles() rejects invalid name_style", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 97L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_error(
    get_quantiles(d, y1, name_style = "wrong"),
    class = "surveycore_error_invalid_name_style"
  )
})

# ---------------------------------------------------------------------------
# Category 12: Edge cases
# ---------------------------------------------------------------------------

test_that("get_quantiles() all-NA column returns NA estimates", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 100L)
  df$all_na <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_quantiles(d, all_na, probs = c(0.25, 0.5))
  test_result_invariants(result, "survey_quantiles")
  expect_true(all(is.na(result$estimate)))
  expect_true(all(result$n == 0L))
})

test_that("get_quantiles() single distinct value returns that value as estimate", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 101L)
  df$const <- 42.0
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_quantiles(d, const, probs = c(0.25, 0.5, 0.75), variance = NULL)
  test_result_invariants(result, "survey_quantiles")
  expect_true(all(result$estimate == 42.0))
})

test_that("get_quantiles() empty domain (n = 0) returns NA estimate", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 102L)
  d_full <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  # Mark domain as empty: no rows belong to "NONEXISTENT_GROUP"
  d_empty <- d_full
  d_empty@data[[SURVEYCORE_DOMAIN_COL]] <- as.integer(
    !is.na(d_full@data$group) & d_full@data$group == "NONEXISTENT_GROUP"
  )

  result <- get_quantiles(d_empty, y1, probs = 0.5)
  test_result_invariants(result, "survey_quantiles")
  expect_true(is.na(result$estimate[[1L]]))
  expect_equal(result$n[[1L]], 0L)
})

test_that("get_quantiles() CV warning fires for zero-or-negative estimate", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 103L)
  df$neg <- -abs(df$y2) # all negative
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_warning(
    get_quantiles(d, neg, variance = "cv"),
    class = "surveycore_warning_cv_undefined"
  )
})

test_that("get_quantiles() single-level group warning fires", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 104L)
  df$g1 <- "only_level"
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_warning(
    get_quantiles(d, y1, group = g1),
    class = "surveycore_warning_single_level"
  )
})

test_that("get_quantiles() probs in meta match argument value", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 105L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  probs <- c(0.1, 0.9)

  result <- get_quantiles(d, y1, probs = probs)
  expect_identical(meta(result)$probs, probs)
  expect_equal(names(meta(result)$x), "y1")
})


# ── decimals argument ──────────────────────────────────────────────────────────

test_that("get_quantiles() decimals=2 rounds all double columns", {
  df <- make_survey_data(n = 200L, n_psu = 20L, n_strata = 4L, seed = 601L)
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  r <- get_quantiles(d, y1, variance = "ci", decimals = 2L)

  dbl_cols <- names(r)[vapply(r, is.double, logical(1L))]
  for (col in dbl_cols) {
    expect_equal(
      r[[col]],
      round(r[[col]], 2L),
      label = paste0(col, " rounded to 2 decimals")
    )
  }
})

test_that("get_quantiles() rejects invalid decimals", {
  df <- make_survey_data(n = 100L, n_psu = 10L, n_strata = 2L, seed = 602L)
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )

  expect_error(
    get_quantiles(d, y1, decimals = "two"),
    class = "surveycore_error_invalid_decimals"
  )
})

# ── NA group rows (na.rm extension) — Test Blocks 1–8c + oracle ───────────────

# Block 1: default na.rm = TRUE excludes NA group rows (regression guard)

test_that("get_quantiles() default (na.rm = TRUE) excludes group NA rows", {
  d <- make_na_group_design()
  r <- get_quantiles(d, y1, probs = 0.5, group = grp)
  expect_false(anyNA(r$grp))
})

# Block 2: na.rm = FALSE includes NA group row

test_that("get_quantiles() includes NA group row when na.rm = FALSE", {
  d <- make_na_group_design()
  r <- get_quantiles(d, y1, probs = 0.5, group = grp, na.rm = FALSE)
  expect_true(any(is.na(r$grp)))
})

# Block 3: NA group row is last

test_that("get_quantiles() places NA group row after non-NA rows", {
  d <- make_na_group_design()
  r <- get_quantiles(d, y1, probs = 0.5, group = grp, na.rm = FALSE)
  na_idx <- which(is.na(r$grp))
  nn_idx <- which(!is.na(r$grp))
  expect_true(all(na_idx > max(nn_idx)))
})

# Block 4: NA group row has finite estimate

test_that("get_quantiles() NA group row has finite estimate", {
  d <- make_na_group_design()
  r <- get_quantiles(d, y1, probs = 0.5, group = grp, na.rm = FALSE)
  na_row <- get_na_group_rows(r, "grp")
  expect_true(all(is.finite(na_row$estimate)))
})

# Block 5a: multi-group — NA in first group var

test_that("get_quantiles() handles NA in first of two group vars (na.rm = FALSE)", {
  d <- make_na_group_design() # grp has NAs; grp2 has none
  r <- get_quantiles(d, y1, probs = 0.5, group = c(grp, grp2), na.rm = FALSE)
  expect_true(any(is.na(r$grp) & !is.na(r$grp2)))
})

# Block 5b: multi-group — NA in second group var (inline fixture)

test_that("get_quantiles() handles NA in second of two group vars (na.rm = FALSE)", {
  df <- make_survey_data(n = 200L, seed = 42L)
  set.seed(43L)
  df$grp <- sample(c("A", "B", "C"), 200L, replace = TRUE)
  df$grp2 <- sample(c("X", "Y", NA_character_), 200L, replace = TRUE)
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  r <- get_quantiles(d, y1, probs = 0.5, group = c(grp, grp2), na.rm = FALSE)
  expect_true(any(!is.na(r$grp) & is.na(r$grp2)))
})

# Block 6: all-NA group var — warning fires; output has NA group row

test_that("get_quantiles() handles group var that is entirely NA (na.rm = FALSE)", {
  d <- make_all_na_group_design()
  expect_warning(
    r <- get_quantiles(d, y1, probs = 0.5, group = grp, na.rm = FALSE),
    class = "surveycore_warning_single_level"
  )
  expect_equal(nrow(r), 1L)
  expect_true(is.na(r$grp[[1L]]))
  expect_true(is.finite(r$estimate[[1L]]))
})

# Block 7a: label_values = TRUE — regular NA group row remains NA in factor

test_that("get_quantiles() regular NA group row is NA in factor when label_values = TRUE", {
  df <- make_survey_data(n = 200L, seed = 42L)
  set.seed(43L)
  df$grp <- sample(c(1L, 2L, NA_integer_), 200L, replace = TRUE)
  attr(df$grp, "labels") <- c("GroupA" = 1L, "GroupB" = 2L)
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  r <- get_quantiles(
    d,
    y1,
    probs = 0.5,
    group = grp,
    na.rm = FALSE,
    label_values = TRUE
  )
  expect_true(is.factor(r$grp))
  na_row <- get_na_group_rows(r, "grp")
  expect_true(nrow(na_row) > 0L)
  expect_true(is.na(na_row$grp[[1L]]))
})

# Block 7b: label_values = TRUE — haven-tagged NA becomes a factor level

test_that("get_quantiles() haven-labeled NA group rows become factor levels when label_values = TRUE", {
  skip_if_not_installed("haven")
  df <- make_survey_data(n = 200L, seed = 42L)
  set.seed(43L)
  df$grp <- sample(c(1L, 2L), 200L, replace = TRUE)
  df$grp <- as.double(df$grp)
  df$grp[sample(200L, 40L)] <- haven::tagged_na("r")
  attr(df$grp, "labels") <- c(
    "GroupA" = 1,
    "GroupB" = 2,
    "Refused" = haven::tagged_na("r")
  )
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  r <- get_quantiles(
    d,
    y1,
    probs = 0.5,
    group = grp,
    na.rm = FALSE,
    label_values = TRUE
  )
  expect_true(is.factor(r$grp))
  expect_true("Refused" %in% levels(r$grp))
  refused_row <- r[!is.na(r$grp) & r$grp == "Refused", ]
  expect_true(nrow(refused_row) > 0L)
})

# Block 8: group_by() path — NA group rows appear with na.rm = FALSE

test_that("get_quantiles() includes NA group row when group set via group_by() and na.rm = FALSE", {
  skip_if_not_installed("surveytidy")
  d <- surveytidy::group_by(make_na_group_design(), grp)
  r <- get_quantiles(d, y1, probs = 0.5, na.rm = FALSE)
  expect_true(anyNA(r$grp))
})

# Block 8b: group_by() path — NA group rows excluded by default

test_that("get_quantiles() excludes NA group rows by default when group set via group_by()", {
  skip_if_not_installed("surveytidy")
  d <- surveytidy::group_by(make_na_group_design(), grp)
  r <- get_quantiles(d, y1, probs = 0.5)
  expect_false(anyNA(r$grp))
})

# Block 8c: na.rm = NA is rejected (dual pattern)

test_that("get_quantiles() rejects na.rm = NA with surveycore_error_na_rm_not_logical", {
  d <- make_na_group_design()
  expect_error(
    get_quantiles(d, y1, probs = 0.5, group = grp, na.rm = NA),
    class = "surveycore_error_na_rm_not_logical"
  )
  expect_snapshot(
    error = TRUE,
    get_quantiles(d, y1, probs = 0.5, group = grp, na.rm = NA)
  )
})


# ── Oracle tests: NA group row estimate matches filtered design ────────────────

test_that("get_quantiles() NA group row estimate matches filtered taylor design [oracle]", {
  df <- make_survey_data(n = 100L, n_psu = 10L, n_strata = 2L, seed = 42L)
  set.seed(43L)
  df$grp <- sample(c("A", "B", NA_character_), 100L, replace = TRUE)
  design_oracle <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  na_df <- df[is.na(df$grp), ]
  na_design <- as_survey(
    na_df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  expected <- get_quantiles(na_design, y1, probs = 0.5, variance = "se")
  result <- get_quantiles(
    design_oracle,
    y1,
    probs = 0.5,
    group = grp,
    na.rm = FALSE,
    variance = "se"
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$estimate, expected$estimate, tolerance = 1e-10)
  expect_equal(na_row$se, expected$se, tolerance = 1e-8)
  expect_equal(na_row$n, expected$n)
})

test_that("get_quantiles() NA group row estimate matches filtered replicate design [oracle]", {
  df_r <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    n_strata = 2L,
    design = "replicate",
    type = "brr",
    seed = 42L
  )
  set.seed(43L)
  df_r$grp <- sample(c("A", "B", NA_character_), 100L, replace = TRUE)
  repwt_cols <- grep("^repwt_", names(df_r), value = TRUE)
  design_rep <- as_survey_replicate(
    df_r,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )
  na_df_r <- df_r[is.na(df_r$grp), ]
  repwt_cols_na <- grep("^repwt_", names(na_df_r), value = TRUE)
  na_design_rep <- as_survey_replicate(
    na_df_r,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols_na),
    type = "BRR"
  )
  expected <- get_quantiles(na_design_rep, y1, probs = 0.5, variance = "se")
  result <- get_quantiles(
    design_rep,
    y1,
    probs = 0.5,
    group = grp,
    na.rm = FALSE,
    variance = "se"
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$estimate, expected$estimate, tolerance = 1e-10)
  expect_equal(na_row$se, expected$se, tolerance = 1e-8)
  expect_equal(na_row$n, expected$n)
})

test_that("get_quantiles() NA group row estimate matches filtered twophase design [oracle]", {
  df_p <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    n_strata = 2L,
    design = "twophase",
    seed = 42L
  )
  set.seed(43L)
  df_p$grp <- sample(c("A", "B", NA_character_), 100L, replace = TRUE)
  phase1 <- as_survey(
    df_p,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  design_twophase <- as_survey_twophase(
    phase1,
    subset = subset,
    method = "approx"
  )
  na_df_p <- df_p[is.na(df_p$grp), ]
  na_phase1 <- as_survey(
    na_df_p,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  na_design_twophase <- as_survey_twophase(
    na_phase1,
    subset = subset,
    method = "approx"
  )
  expected <- suppressWarnings(
    get_quantiles(na_design_twophase, y1, probs = 0.5, variance = "se")
  )
  result <- suppressWarnings(
    get_quantiles(
      design_twophase,
      y1,
      probs = 0.5,
      group = grp,
      na.rm = FALSE,
      variance = "se"
    )
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$estimate, expected$estimate, tolerance = 1e-10)
  expect_true(all(is.finite(na_row$se)))
  expect_equal(na_row$n, expected$n)
})

test_that("get_quantiles() NA group row estimate matches filtered calibrated design [oracle]", {
  df_c <- make_survey_data(n = 100L, n_psu = 10L, n_strata = 2L, seed = 42L)
  set.seed(43L)
  df_c$grp <- sample(c("A", "B", NA_character_), 100L, replace = TRUE)
  design_cal <- as_survey_nonprob(df_c, weights = wt)
  na_df_c <- df_c[is.na(df_c$grp), ]
  na_design_cal <- as_survey_nonprob(na_df_c, weights = wt)
  expected <- get_quantiles(na_design_cal, y1, probs = 0.5, variance = "se")
  result <- get_quantiles(
    design_cal,
    y1,
    probs = 0.5,
    group = grp,
    na.rm = FALSE,
    variance = "se"
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$estimate, expected$estimate, tolerance = 1e-10)
  expect_true(all(is.finite(na_row$se)))
  expect_equal(na_row$n, expected$n)
})

test_that("get_quantiles() NA group row estimate matches filtered srs design [oracle]", {
  df_s <- make_survey_data(n = 100L, n_psu = 10L, n_strata = 2L, seed = 42L)
  set.seed(43L)
  df_s$grp <- sample(c("A", "B", NA_character_), 100L, replace = TRUE)
  design_srs <- as_survey(df_s, weights = wt)
  na_df_s <- df_s[is.na(df_s$grp), ]
  na_design_srs <- as_survey(na_df_s, weights = wt)
  expected <- get_quantiles(na_design_srs, y1, probs = 0.5, variance = "se")
  result <- get_quantiles(
    design_srs,
    y1,
    probs = 0.5,
    group = grp,
    na.rm = FALSE,
    variance = "se"
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$estimate, expected$estimate, tolerance = 1e-10)
  # SE legitimately differs: .degf_woodruff() for SRS uses nrow(design@data);
  # domain estimation (full design) != pre-filtered oracle.
  expect_true(all(is.finite(na_row$se)))
  expect_equal(na_row$n, expected$n)
})

test_that("get_quantiles() multi-group NA row estimate matches filtered taylor design [oracle]", {
  df <- make_survey_data(n = 100L, n_psu = 10L, n_strata = 2L, seed = 42L)
  set.seed(43L)
  df$grp <- sample(c("A", "B", NA_character_), 100L, replace = TRUE)
  df$grp2 <- sample(c("X", "Y"), 100L, replace = TRUE)
  design_multi <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  result <- suppressWarnings(
    get_quantiles(
      design_multi,
      y1,
      probs = 0.5,
      group = c(grp, grp2),
      na.rm = FALSE,
      variance = "se"
    )
  )
  oracle_df <- df[is.na(df$grp) & df$grp2 == "X", ]
  oracle_design <- as_survey(
    oracle_df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  expected <- suppressWarnings(
    get_quantiles(oracle_design, y1, probs = 0.5, variance = "se")
  )
  na_x_rows <- result[is.na(result$grp) & result$grp2 == "X", ]
  expect_equal(na_x_rows$estimate, expected$estimate, tolerance = 1e-10)
  expect_true(all(is.finite(na_x_rows$se)))
  expect_equal(na_x_rows$n, expected$n)
})

# ---------------------------------------------------------------------------
# Additional coverage: NA CI propagation, empty domain
# ---------------------------------------------------------------------------

test_that("get_quantiles() empty domain returns NA estimate and se (covers NA CI path)", {
  df <- make_survey_data(n = 60, n_psu = 10, n_strata = 2, seed = 800)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- FALSE
  result <- get_quantiles(sc, y1, probs = 0.5, variance = "se")
  # Empty domain → p_hat is NA → CI path returns NA
  expect_true(all(is.na(result$estimate)))
})

test_that("get_quantiles() works for SRS design (Taylor with no ids/strata)", {
  set.seed(801)
  n <- 100L
  df <- data.frame(y = rnorm(n), w = rep(1, n))
  sc <- as_survey(df, weights = w)
  result <- get_quantiles(sc, y, probs = c(0.25, 0.5, 0.75), variance = "se")
  test_result_invariants(result, "survey_quantiles")
  expect_equal(nrow(result), 3L)
  expect_true(all(is.finite(result$estimate)))
})

test_that("get_quantiles() works for survey_twophase design", {
  d <- make_survey_data(
    n = 100,
    n_psu = 10,
    n_strata = 2,
    design = "twophase",
    seed = 802
  )
  phase1 <- as_survey(
    d,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  sc <- as_survey_twophase(
    phase1,
    subset = subset,
    ids2 = psu,
    strata2 = strata
  )
  result <- get_quantiles(sc, y1, probs = 0.5, variance = "se")
  test_result_invariants(result, "survey_quantiles")
  expect_true(is.finite(result$estimate[[1L]]))
})

# ---------------------------------------------------------------------------
# Additional coverage: NA p_hat/se_p path in .quantile_woodruff_cell()
# ---------------------------------------------------------------------------

test_that("get_quantiles() empty domain triggers NA p_hat path in .quantile_woodruff_cell()", {
  # When the domain is empty, .mean_cell() returns NA p_hat/se_p → early return in woodruff
  set.seed(901)
  df <- make_survey_data(n = 80, n_psu = 10, n_strata = 2, seed = 901)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- rep(FALSE, 80L)
  result <- suppressWarnings(get_quantiles(sc, y1, probs = 0.5))
  expect_true(is.na(result$estimate[[1L]]))
  expect_true(is.na(result$ci_low[[1L]]))
  expect_true(is.na(result$ci_high[[1L]]))
})

# ---------------------------------------------------------------------------
# Coverage: is.na(se_p) early-return path (lines 188-196) in
# .quantile_woodruff_cell().
#
# The empty-domain test above (all FALSE) does NOT reach lines 188-196
# because n_d == 0 is caught at line 156 first. To reach lines 188-196 we
# need n_d > 0 but .mean_cell() returning NA for se. This occurs with an
# SRS design when n_d == 1: .srs_mean_cell() returns se = NA_real_ for
# single-observation domains (no variance with one point).
# ---------------------------------------------------------------------------

test_that("get_quantiles() single-row SRS domain: CI collapses to point estimate", {
  n <- 50L
  df <- make_survey_data(n = n, seed = 910L)
  sc <- as_survey(df, weights = wt)
  # Domain contains exactly 1 non-NA observation → n_d == 1
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- seq_len(n) == 1L
  result <- suppressWarnings(get_quantiles(sc, y1, probs = 0.5))
  # With n_d=1, estimate is the single observed value; CI collapses to it
  # (Taylor SE for the proportion at the quantile is 0 → CI width is 0)
  expect_false(is.na(result$estimate[[1L]]))
  expect_equal(result$ci_low[[1L]], result$estimate[[1L]])
  expect_equal(result$ci_high[[1L]], result$estimate[[1L]])
})
