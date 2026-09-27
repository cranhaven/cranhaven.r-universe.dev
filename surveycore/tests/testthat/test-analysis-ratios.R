# test-analysis-ratios.R
# Tests for get_ratios() — Phase 1 survey-weighted ratio estimation.
#
# Numerical tolerances (from testing-surveycore.md):
#   Point estimates: 1e-10
#   SE:              1e-8
#   CI bounds:       1e-6
#
# Oracle: survey::svyratio().
# Note: replicate-design oracle tests use mse = FALSE to match survey's default.

# ---------------------------------------------------------------------------
# Category 1: Happy path — return structure (all design types)
# ---------------------------------------------------------------------------

test_that("get_ratios() returns survey_ratios tibble for survey_taylor", {
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

  result <- get_ratios(d, numerator = y1, denominator = y2)
  test_result_invariants(result, "survey_ratios")

  expect_equal(nrow(result), 1L)
  expect_true("ratio" %in% names(result))
  expect_true("ci_low" %in% names(result))
  expect_true("ci_high" %in% names(result))
  expect_true("n" %in% names(result))
  # variance = "ci" by default → no se column
  expect_false("se" %in% names(result))

  expect_true(is.finite(result$ratio[[1L]]))
  expect_true(is.finite(result$ci_low[[1L]]))
  expect_true(is.finite(result$ci_high[[1L]]))
  expect_lt(result$ci_low[[1L]], result$ratio[[1L]])
  expect_lt(result$ratio[[1L]], result$ci_high[[1L]])
  expect_true(result$n[[1L]] > 0L)
})

test_that("get_ratios() returns correct result for survey_replicate", {
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

  result <- get_ratios(d, y1, y2)
  test_result_invariants(result, "survey_ratios")

  expect_equal(nrow(result), 1L)
  expect_true(is.finite(result$ratio[[1L]]))
  expect_true(is.finite(result$ci_low[[1L]]))
  expect_true(is.finite(result$ci_high[[1L]]))
  expect_lt(result$ci_low[[1L]], result$ci_high[[1L]])
})

test_that("get_ratios() returns correct result for SRS design", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 3L)
  d <- as_survey(df, weights = wt)

  result <- get_ratios(d, y1, y2)
  test_result_invariants(result, "survey_ratios")

  expect_equal(nrow(result), 1L)
  expect_true(is.finite(result$ratio[[1L]]))
  expect_true(result$n[[1L]] > 0L)
})

test_that("get_ratios() returns correct result for survey_twophase", {
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

  result <- get_ratios(d, y1, y2)
  test_result_invariants(result, "survey_ratios")

  expect_equal(nrow(result), 1L)
  expect_true(is.finite(result$ratio[[1L]]))
  expect_true(is.finite(result$ci_low[[1L]]))
  expect_true(is.finite(result$ci_high[[1L]]))
  expect_lt(result$ci_low[[1L]], result$ci_high[[1L]])
})

test_that("get_ratios() returns correct result for survey_nonprob", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 5L)
  d <- as_survey_nonprob(df, weights = wt)

  result <- get_ratios(d, y1, y2)
  test_result_invariants(result, "survey_ratios")

  expect_equal(nrow(result), 1L)
  expect_true(is.finite(result$ratio[[1L]]))
  expect_true(is.finite(result$ci_low[[1L]]))
  expect_true(is.finite(result$ci_high[[1L]]))
})

# ---------------------------------------------------------------------------
# Category 2 (oracle): Numerical validation — survey::svyratio()
# ---------------------------------------------------------------------------

test_that("get_ratios() Taylor matches svyratio() — synthetic [oracle]", {
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

  sc <- get_ratios(d_sc, y1, y2, variance = c("se", "ci"))
  sv <- survey::svyratio(~y1, ~y2, d_sv, na.rm = TRUE)

  expect_equal(sc$ratio[[1L]], as.numeric(coef(sv)), tolerance = 1e-10)
  expect_equal(sc$se[[1L]], as.numeric(sqrt(diag(vcov(sv)))), tolerance = 1e-8)
  expect_equal(sc$ci_low[[1L]], confint(sv)[1L], tolerance = 1e-6)
  expect_equal(sc$ci_high[[1L]], confint(sv)[2L], tolerance = 1e-6)
})

test_that("get_ratios() Taylor matches svyratio() — NHANES bpxsy1/ridageyr [oracle]", {
  skip_if_not_installed("survey")
  df <- nhanes_2017[nhanes_2017[["wtmec2yr"]] > 0, ]
  d_sc <- as_survey(
    df,
    ids = sdmvpsu,
    weights = wtmec2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d_sv <- survey::svydesign(
    ids = ~sdmvpsu,
    weights = ~wtmec2yr,
    strata = ~sdmvstra,
    data = df,
    nest = TRUE
  )

  sc <- get_ratios(d_sc, bpxsy1, ridageyr, variance = c("se", "ci"))
  sv <- survey::svyratio(~bpxsy1, ~ridageyr, d_sv, na.rm = TRUE)

  expect_equal(sc$ratio[[1L]], as.numeric(coef(sv)), tolerance = 1e-10)
  expect_equal(sc$se[[1L]], as.numeric(sqrt(diag(vcov(sv)))), tolerance = 1e-8)
  expect_equal(sc$ci_low[[1L]], confint(sv)[1L], tolerance = 1e-6)
  expect_equal(sc$ci_high[[1L]], confint(sv)[2L], tolerance = 1e-6)
})

test_that("get_ratios() BRR matches svyratio() mse=FALSE [oracle]", {
  skip_if_not_installed("survey")
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

  sc <- get_ratios(d_sc, y1, y2, variance = c("se", "ci"))
  sv <- survey::svyratio(~y1, ~y2, d_sv, na.rm = TRUE)

  expect_equal(sc$ratio[[1L]], as.numeric(coef(sv)), tolerance = 1e-10)
  expect_equal(sc$se[[1L]], as.numeric(sqrt(diag(vcov(sv)))), tolerance = 1e-8)
  expect_equal(sc$ci_low[[1L]], confint(sv)[1L], tolerance = 1e-6)
  expect_equal(sc$ci_high[[1L]], confint(sv)[2L], tolerance = 1e-6)
})

test_that("get_ratios() SRS matches svyratio() — equal weights [oracle]", {
  skip_if_not_installed("survey")
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

  sc <- get_ratios(d_sc, y1, y2, variance = c("se", "ci"))
  sv <- survey::svyratio(~y1, ~y2, d_sv, na.rm = TRUE)

  expect_equal(sc$ratio[[1L]], as.numeric(coef(sv)), tolerance = 1e-10)
  expect_equal(sc$se[[1L]], as.numeric(sqrt(diag(vcov(sv)))), tolerance = 1e-8)
  expect_equal(sc$ci_low[[1L]], confint(sv)[1L], tolerance = 1e-6)
  expect_equal(sc$ci_high[[1L]], confint(sv)[2L], tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# Category 3: Variance argument
# ---------------------------------------------------------------------------

test_that("get_ratios() variance = NULL produces no uncertainty columns", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 10L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_ratios(d, y1, y2, variance = NULL)
  test_result_invariants(result, "survey_ratios")
  expect_false("se" %in% names(result))
  expect_false("ci_low" %in% names(result))
  expect_true("ratio" %in% names(result))
  expect_true("n" %in% names(result))
})

test_that("get_ratios() variance = 'se' produces only se column", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 11L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_ratios(d, y1, y2, variance = "se")
  expect_true("se" %in% names(result))
  expect_false("ci_low" %in% names(result))
  expect_false("var" %in% names(result))
  expect_true(is.finite(result$se[[1L]]))
})

test_that("get_ratios() variance = c('se', 'ci', 'moe') produces all three", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 12L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_ratios(d, y1, y2, variance = c("se", "ci", "moe"))
  expect_true("se" %in% names(result))
  expect_true("ci_low" %in% names(result))
  expect_true("ci_high" %in% names(result))
  expect_true("moe" %in% names(result))
  # moe = (ci_high - ci_low) / 2
  expect_equal(
    result$moe[[1L]],
    (result$ci_high[[1L]] - result$ci_low[[1L]]) / 2,
    tolerance = 1e-12
  )
})

test_that("get_ratios() deff column produced when requested", {
  df <- make_survey_data(
    n = 300L,
    n_psu = 30L,
    n_strata = 3L,
    design = "taylor",
    seed = 13L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_ratios(d, y1, y2, variance = c("se", "deff"))
  expect_true("deff" %in% names(result))
  expect_true(is.finite(result$deff[[1L]]))
  expect_gt(result$deff[[1L]], 0)
})

test_that("get_ratios() var column = se^2", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 14L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_ratios(d, y1, y2, variance = c("se", "var"))
  expect_equal(result$var[[1L]], result$se[[1L]]^2, tolerance = 1e-15)
})

# ---------------------------------------------------------------------------
# Category 4: Grouped analysis
# ---------------------------------------------------------------------------

test_that("get_ratios() group = produces one row per group", {
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

  result <- get_ratios(d, y1, y2, group = group)
  test_result_invariants(result, "survey_ratios")

  n_groups <- length(unique(df$group))
  expect_equal(nrow(result), n_groups)
  expect_true("group" %in% names(result))
  expect_true("ratio" %in% names(result))
  expect_true(all(result$n > 0L))
})

test_that("get_ratios() NA group values produce no row for that group", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 21L)
  df$group_na <- df$group
  df$group_na[1:10] <- NA_character_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result_na <- get_ratios(d, y1, y2, group = group_na)
  result_no <- get_ratios(d, y1, y2, group = group)

  expect_equal(nrow(result_na), nrow(result_no))
})

test_that("get_ratios() single-level group fires warning", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 22L)
  df$one_level <- "A"
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  expect_warning(
    get_ratios(d, y1, y2, group = one_level),
    class = "surveycore_warning_single_level"
  )
})

# ---------------------------------------------------------------------------
# Category 5: Domain estimation
# ---------------------------------------------------------------------------

test_that("get_ratios() respects domain filter", {
  skip_if_not_installed("surveytidy")
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
  d_domain <- surveytidy::filter(d_full, group == "A")

  result_domain <- get_ratios(d_domain, y1, y2, variance = "se")
  result_full <- get_ratios(d_full, y1, y2, variance = "se")

  # Domain estimate != full-sample estimate
  expect_false(
    isTRUE(all.equal(result_domain$ratio, result_full$ratio, tolerance = 1e-6))
  )
  # Domain n <= full-sample n
  expect_lte(result_domain$n[[1L]], result_full$n[[1L]])
  # Domain SE >= full-sample SE (domain estimation uses full design)
  expect_gte(result_domain$se[[1L]], result_full$se[[1L]] * 0.5)
})

test_that("get_ratios() domain + group combination works", {
  skip_if_not_installed("surveytidy")
  df <- make_survey_data(
    n = 300L,
    n_psu = 30L,
    n_strata = 3L,
    design = "taylor",
    seed = 31L
  )
  d_full <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  d_domain <- surveytidy::filter(d_full, group == "A")

  result <- get_ratios(d_domain, y1, y2, group = strata)
  test_result_invariants(result, "survey_ratios")
  expect_gte(nrow(result), 1L)
  expect_true("strata" %in% names(result))
})

# ---------------------------------------------------------------------------
# Category 6: NA handling
# ---------------------------------------------------------------------------

test_that("get_ratios() na.rm = TRUE excludes NA rows from estimate and n", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 40L)
  df$y1[1:10] <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_ratios(d, y1, y2, variance = "se", na.rm = TRUE)
  expect_true(is.finite(result$ratio[[1L]]))
  expect_equal(result$n[[1L]], 90L)
})

test_that("get_ratios() n counts pairs where BOTH are non-NA", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 41L)
  df$y1[1:5] <- NA_real_ # 5 numerator NAs
  df$y2[6:12] <- NA_real_ # 7 denominator NAs (no overlap with above)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_ratios(d, y1, y2, na.rm = TRUE)
  # 12 rows have at least one NA; 88 rows are non-NA in both
  expect_equal(result$n[[1L]], 88L)
})

test_that("get_ratios() na.rm = FALSE propagates NA to estimate when NAs present", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 42L)
  df$y1[1:5] <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_ratios(d, y1, y2, na.rm = FALSE)
  expect_true(is.na(result$ratio[[1L]]))
})

test_that("get_ratios() na.rm = FALSE with no NAs gives same estimate as na.rm = TRUE", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 43L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  r_true <- get_ratios(d, y1, y2, variance = "se", na.rm = TRUE)
  r_false <- get_ratios(d, y1, y2, variance = "se", na.rm = FALSE)

  expect_equal(r_true$ratio[[1L]], r_false$ratio[[1L]], tolerance = 1e-15)
  expect_equal(r_true$se[[1L]], r_false$se[[1L]], tolerance = 1e-15)
})

# ---------------------------------------------------------------------------
# Category 7: n and n_weighted columns
# ---------------------------------------------------------------------------

test_that("get_ratios() n_weighted column added when requested", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 50L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result_nw <- get_ratios(d, y1, y2, n_weighted = TRUE)
  result_no <- get_ratios(d, y1, y2, n_weighted = FALSE)

  expect_true("n_weighted" %in% names(result_nw))
  expect_false("n_weighted" %in% names(result_no))
  expect_true(is.finite(result_nw$n_weighted[[1L]]))
  expect_gt(result_nw$n_weighted[[1L]], 0)
})

test_that("get_ratios() n is always last before n_weighted", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 51L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_ratios(d, y1, y2, variance = c("se", "ci"), n_weighted = TRUE)
  col_names <- names(result)
  n_pos <- which(col_names == "n")
  nw_pos <- which(col_names == "n_weighted")
  expect_true(n_pos == length(col_names) - 1L)
  expect_true(nw_pos == length(col_names))
})

# ---------------------------------------------------------------------------
# Category 8: meta() output
# ---------------------------------------------------------------------------

test_that("get_ratios() stores numerator and denominator names in meta()", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 60L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_ratios(d, y1, y2)
  m <- meta(result)

  expect_identical(m$numerator$name, "y1")
  expect_identical(m$denominator$name, "y2")
})

test_that("get_ratios() meta() stores design_type", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 61L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_ratios(d, y1, y2)
  expect_identical(meta(result)$design_type, "taylor")
})

test_that("get_ratios() meta() stores variable labels when present", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 62L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  d <- set_var_label(d, y1 = "Numerator variable")
  d <- set_var_label(d, y2 = "Denominator variable")

  result <- get_ratios(d, y1, y2)
  expect_identical(meta(result)$numerator$variable_label, "Numerator variable")
  expect_identical(
    meta(result)$denominator$variable_label,
    "Denominator variable"
  )
})

test_that("get_ratios() meta() n_respondents equals nrow(design@data)", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 63L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_ratios(d, y1, y2)
  expect_identical(meta(result)$n_respondents, nrow(df))
})

test_that("get_ratios() meta() group_names populated when group used", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 64L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_ratios(d, y1, y2, group = group)
  expect_identical(names(meta(result)$group), "group")
})

# ---------------------------------------------------------------------------
# Category 9: Error paths
# ---------------------------------------------------------------------------

test_that("get_ratios() errors for non-survey-design object", {
  expect_error(
    get_ratios(data.frame(x = 1:5, y = rnorm(5)), x, y),
    class = "surveycore_error_unsupported_class"
  )
})

test_that("get_ratios() errors for non-numeric numerator", {
  df <- data.frame(y = letters[1:10], x = 1:10, w = rep(1, 10))
  d <- as_survey(df, weights = w)
  expect_error(
    get_ratios(d, y, x),
    class = "surveycore_error_non_numeric_variable"
  )
  expect_snapshot(error = TRUE, get_ratios(d, y, x))
})

test_that("get_ratios() errors for non-numeric denominator", {
  df <- data.frame(y = 1:10, x = letters[1:10], w = rep(1, 10))
  d <- as_survey(df, weights = w)
  expect_error(
    get_ratios(d, y, x),
    class = "surveycore_error_non_numeric_variable"
  )
  expect_snapshot(error = TRUE, get_ratios(d, y, x))
})

test_that("get_ratios() errors when all denominator values are zero", {
  df <- data.frame(y = 1:10, x = rep(0, 10), w = rep(1, 10))
  d <- as_survey(df, weights = w)
  expect_error(
    get_ratios(d, y, x),
    class = "surveycore_error_ratio_zero_denominator"
  )
  expect_snapshot(error = TRUE, get_ratios(d, y, x))
})

test_that("get_ratios() errors for invalid variance value", {
  df <- data.frame(y = 1:10, x = 1:10, w = rep(1, 10))
  d <- as_survey(df, weights = w)
  expect_error(
    get_ratios(d, y, x, variance = "bogus"),
    class = "surveycore_error_invalid_variance_arg"
  )
  expect_snapshot(error = TRUE, get_ratios(d, y, x, variance = "bogus"))
})

test_that("get_ratios() errors for invalid conf_level", {
  df <- data.frame(y = 1:10, x = 1:10, w = rep(1, 10))
  d <- as_survey(df, weights = w)
  expect_error(
    get_ratios(d, y, x, conf_level = 1.5),
    class = "surveycore_error_invalid_conf_level"
  )
})

test_that("get_ratios() errors when numerator resolves to multiple variables", {
  df <- data.frame(y1 = 1:10, y2 = 1:10, x = 1:10, w = rep(1, 10))
  d <- as_survey(df, weights = w)
  expect_error(
    get_ratios(d, starts_with("y"), x),
    class = "surveycore_error_wrong_variable_count"
  )
})

test_that("get_ratios() errors when denominator resolves to zero variables", {
  df <- data.frame(y = 1:10, x = 1:10, w = rep(1, 10))
  d <- as_survey(df, weights = w)
  expect_error(
    get_ratios(d, y, starts_with("zzz")),
    class = "surveycore_error_wrong_variable_count"
  )
})

# ---------------------------------------------------------------------------
# Category 10: Edge cases
# ---------------------------------------------------------------------------

test_that("get_ratios() warns for small cell (n < min_cell_n)", {
  df <- data.frame(y = 1:5, x = 5:1, w = rep(1, 5))
  d <- as_survey(df, weights = w)

  expect_warning(
    get_ratios(d, y, x, min_cell_n = 30L),
    class = "surveycore_warning_small_cell"
  )
})

test_that("get_ratios() no small-cell warning when min_cell_n = 0L", {
  df <- data.frame(y = 1:5, x = 5:1, w = rep(1, 5))
  d <- as_survey(df, weights = w)

  expect_no_warning(
    get_ratios(d, y, x, min_cell_n = 0L)
  )
})

test_that("get_ratios() cv = NA + warning when ratio estimate is 0", {
  # ratio = 0: numerator is all zeros; use n >= 30 to avoid small-cell warning
  df <- data.frame(y = rep(0, 50), x = seq_len(50), w = rep(1, 50))
  d <- as_survey(df, weights = w)

  expect_warning(
    result <- get_ratios(d, y, x, variance = "cv"),
    class = "surveycore_warning_cv_undefined"
  )
  expect_true(is.na(result$cv[[1L]]))
})

test_that("get_ratios() ratio column is first after group columns", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 70L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  # No groups: ratio is the first column
  result_ng <- get_ratios(d, y1, y2, variance = NULL)
  expect_identical(names(result_ng)[[1L]], "ratio")

  # With groups: group column is first, ratio is second
  result_g <- get_ratios(d, y1, y2, group = strata, variance = NULL)
  expect_identical(names(result_g)[[1L]], "strata")
  expect_identical(names(result_g)[[2L]], "ratio")
})

# ---------------------------------------------------------------------------
# Category 11: conf_level argument
# ---------------------------------------------------------------------------

test_that("get_ratios() wider CI for higher conf_level", {
  df <- make_survey_data(n = 200L, n_psu = 20L, design = "taylor", seed = 80L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  r95 <- get_ratios(d, y1, y2, variance = "ci", conf_level = 0.95)
  r99 <- get_ratios(d, y1, y2, variance = "ci", conf_level = 0.99)

  width95 <- r95$ci_high[[1L]] - r95$ci_low[[1L]]
  width99 <- r99$ci_high[[1L]] - r99$ci_low[[1L]]
  expect_gt(width99, width95)
})

# ---------------------------------------------------------------------------
# Category 12: name_style = "broom"
# ---------------------------------------------------------------------------

test_that("get_ratios() name_style = 'broom' renames ratio → estimate", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 90L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_ratios(
    d,
    y1,
    y2,
    variance = c("se", "ci"),
    name_style = "broom"
  )

  expect_true("estimate" %in% names(result))
  expect_true("std.error" %in% names(result))
  expect_true("conf.low" %in% names(result))
  expect_true("conf.high" %in% names(result))
  expect_false("ratio" %in% names(result))
  expect_false("se" %in% names(result))

  # class and meta preserved
  test_result_invariants(result, "survey_ratios")
})

# ---------------------------------------------------------------------------
# Category 13: All 5 designs via make_all_designs()
# ---------------------------------------------------------------------------

test_that("get_ratios() returns finite ratio for all 5 design types", {
  designs <- make_all_designs(seed = 99L)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- get_ratios(d, y1, y2)
    test_result_invariants(result, "survey_ratios")
    expect_true(
      is.finite(result$ratio[[1L]]),
      label = paste0("get_ratios() finite ratio for design type: ", nm)
    )
  }
})

# ---------------------------------------------------------------------------
# Category 14: New meta structure — nested numerator/denominator/group
# ---------------------------------------------------------------------------

test_that("get_ratios() meta$numerator has nested list structure", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 80L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  d <- set_var_label(d, y1 = "Income")

  result <- get_ratios(d, y1, y2)
  m <- meta(result)

  expect_identical(m$numerator$name, "y1")
  expect_identical(m$numerator$variable_label, "Income")
  expect_true(all(
    c("name", "variable_label", "question_preface", "value_labels") %in%
      names(m$numerator)
  ))
})

test_that("get_ratios() meta$denominator has nested list structure", {
  df <- make_survey_data(n = 200L, design = "taylor", seed = 81L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_ratios(d, y1, y2)
  m <- meta(result)

  expect_identical(m$denominator$name, "y2")
  expect_true(all(
    c("name", "variable_label", "question_preface", "value_labels") %in%
      names(m$denominator)
  ))
})

test_that("get_ratios() group column is <fct> when group var has haven labels", {
  df <- data.frame(
    y1 = rnorm(100),
    y2 = abs(rnorm(100)) + 0.5,
    gender = structure(
      c(1L, 2L)[rep(1:2, 50)],
      labels = c(Male = 1L, Female = 2L)
    ),
    w = rep(1, 100)
  )
  d <- as_survey(df, weights = w)

  result <- get_ratios(d, y1, y2, group = gender)
  expect_true(is.factor(result$gender))
  expect_identical(levels(result$gender), c("Male", "Female"))
})

test_that("get_ratios() meta$group stores labels regardless of label_values", {
  df <- data.frame(
    y1 = rnorm(100),
    y2 = abs(rnorm(100)) + 0.5,
    gender = structure(
      c(1L, 2L)[rep(1:2, 50)],
      labels = c(Male = 1L, Female = 2L)
    ),
    w = rep(1, 100)
  )
  d <- as_survey(df, weights = w)

  r_true <- get_ratios(d, y1, y2, group = gender, label_values = TRUE)
  r_false <- get_ratios(d, y1, y2, group = gender, label_values = FALSE)

  expect_equal(
    meta(r_true)$group$gender$value_labels,
    c(Male = 1L, Female = 2L)
  )
  expect_equal(
    meta(r_false)$group$gender$value_labels,
    c(Male = 1L, Female = 2L)
  )
})

# ── decimals argument ──────────────────────────────────────────────────────────

test_that("get_ratios() decimals=2 rounds all double columns", {
  df <- make_survey_data(n = 200L, n_psu = 20L, n_strata = 4L, seed = 701L)
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  r <- get_ratios(d, y1, y2, variance = "ci", decimals = 2L)

  dbl_cols <- names(r)[vapply(r, is.double, logical(1L))]
  for (col in dbl_cols) {
    expect_equal(
      r[[col]],
      round(r[[col]], 2L),
      label = paste0(col, " rounded to 2 decimals")
    )
  }
})

test_that("get_ratios() rejects invalid decimals", {
  df <- make_survey_data(n = 100L, n_psu = 10L, n_strata = 2L, seed = 702L)
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )

  expect_error(
    get_ratios(d, y1, y2, decimals = -1),
    class = "surveycore_error_invalid_decimals"
  )
})

# ── NA group rows (na.rm extension) — Test Blocks 1–8c + oracle ───────────────

# Block 1: default na.rm = TRUE excludes NA group rows (regression guard)

test_that("get_ratios() default (na.rm = TRUE) excludes group NA rows", {
  d <- make_na_group_design()
  r <- get_ratios(d, y1, y2, group = grp, label_values = FALSE)
  expect_false(anyNA(r$grp))
})

# Block 2: na.rm = FALSE includes NA group row

test_that("get_ratios() includes NA group row when na.rm = FALSE", {
  d <- make_na_group_design()
  r <- get_ratios(d, y1, y2, group = grp, na.rm = FALSE, label_values = FALSE)
  expect_true(any(is.na(r$grp)))
})

# Block 3: NA group row is last

test_that("get_ratios() places NA group row after non-NA rows", {
  d <- make_na_group_design()
  r <- get_ratios(d, y1, y2, group = grp, na.rm = FALSE, label_values = FALSE)
  na_idx <- which(is.na(r$grp))
  nn_idx <- which(!is.na(r$grp))
  expect_true(all(na_idx > max(nn_idx)))
})

# Block 4: NA group row has finite ratio estimate

test_that("get_ratios() NA group row has finite ratio estimate", {
  d <- make_na_group_design()
  r <- get_ratios(d, y1, y2, group = grp, na.rm = FALSE, label_values = FALSE)
  na_row <- get_na_group_rows(r, "grp")
  expect_true(all(is.finite(na_row$ratio)))
})

# Block 5a: multi-group — NA in first group var

test_that("get_ratios() handles NA in first of two group vars (na.rm = FALSE)", {
  d <- make_na_group_design() # grp has NAs; grp2 has none
  r <- get_ratios(
    d,
    y1,
    y2,
    group = c(grp, grp2),
    na.rm = FALSE,
    label_values = FALSE
  )
  expect_true(any(is.na(r$grp) & !is.na(r$grp2)))
})

# Block 5b: multi-group — NA in second group var (inline fixture)

test_that("get_ratios() handles NA in second of two group vars (na.rm = FALSE)", {
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
  r <- get_ratios(
    d,
    y1,
    y2,
    group = c(grp, grp2),
    na.rm = FALSE,
    label_values = FALSE
  )
  expect_true(any(!is.na(r$grp) & is.na(r$grp2)))
})

# Block 6: all-NA group var — warning fires; output has NA group row

test_that("get_ratios() handles group var that is entirely NA (na.rm = FALSE)", {
  d <- make_all_na_group_design()
  expect_warning(
    r <- get_ratios(
      d,
      y1,
      y2,
      group = grp,
      na.rm = FALSE,
      label_values = FALSE
    ),
    class = "surveycore_warning_single_level"
  )
  expect_equal(nrow(r), 1L)
  expect_true(is.na(r$grp[[1L]]))
  expect_true(is.finite(r$ratio[[1L]]))
})

# Block 7a: label_values = TRUE — regular NA group row remains NA in factor

test_that("get_ratios() regular NA group row is NA in factor when label_values = TRUE", {
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
  r <- get_ratios(d, y1, y2, group = grp, na.rm = FALSE, label_values = TRUE)
  expect_true(is.factor(r$grp))
  na_row <- get_na_group_rows(r, "grp")
  expect_true(nrow(na_row) > 0L)
  expect_true(is.na(na_row$grp[[1L]]))
})

# Block 7b: label_values = TRUE — haven-tagged NA becomes a factor level

test_that("get_ratios() haven-labeled NA group rows become factor levels when label_values = TRUE", {
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
  r <- get_ratios(d, y1, y2, group = grp, na.rm = FALSE, label_values = TRUE)
  expect_true(is.factor(r$grp))
  expect_true("Refused" %in% levels(r$grp))
  refused_row <- r[!is.na(r$grp) & r$grp == "Refused", ]
  expect_true(nrow(refused_row) > 0L)
})

# Block 8: group_by() path — NA group rows appear with na.rm = FALSE

test_that("get_ratios() includes NA group row when group set via group_by() and na.rm = FALSE", {
  skip_if_not_installed("surveytidy")
  d <- surveytidy::group_by(make_na_group_design(), grp)
  r <- get_ratios(d, y1, y2, na.rm = FALSE)
  expect_true(anyNA(r$grp))
})

# Block 8b: group_by() path — NA group rows excluded by default

test_that("get_ratios() excludes NA group rows by default when group set via group_by()", {
  skip_if_not_installed("surveytidy")
  d <- surveytidy::group_by(make_na_group_design(), grp)
  r <- get_ratios(d, y1, y2)
  expect_false(anyNA(r$grp))
})

# Block 8c: na.rm = NA is rejected (dual pattern)

test_that("get_ratios() rejects na.rm = NA with surveycore_error_na_rm_not_logical", {
  d <- make_na_group_design()
  expect_error(
    get_ratios(d, y1, y2, group = grp, na.rm = NA),
    class = "surveycore_error_na_rm_not_logical"
  )
  expect_snapshot(
    error = TRUE,
    get_ratios(d, y1, y2, group = grp, na.rm = NA)
  )
})

# ── Oracle tests: NA group row estimate matches filtered design ────────────────

test_that("get_ratios() NA group row ratio matches filtered taylor design [oracle]", {
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
  expected <- get_ratios(na_design, y1, y2, variance = "se")
  result <- get_ratios(
    design_oracle,
    y1,
    y2,
    group = grp,
    na.rm = FALSE,
    variance = "se",
    label_values = FALSE
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$ratio, expected$ratio, tolerance = 1e-10)
  expect_equal(na_row$se, expected$se, tolerance = 1e-8)
  expect_equal(na_row$n, expected$n)
})

test_that("get_ratios() NA group row ratio matches filtered replicate design [oracle]", {
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
  expected <- get_ratios(na_design_rep, y1, y2, variance = "se")
  result <- get_ratios(
    design_rep,
    y1,
    y2,
    group = grp,
    na.rm = FALSE,
    variance = "se",
    label_values = FALSE
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$ratio, expected$ratio, tolerance = 1e-10)
  expect_equal(na_row$se, expected$se, tolerance = 1e-8)
  expect_equal(na_row$n, expected$n)
})

test_that("get_ratios() NA group row ratio matches filtered twophase design [oracle]", {
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
    get_ratios(na_design_twophase, y1, y2, variance = "se")
  )
  result <- suppressWarnings(
    get_ratios(
      design_twophase,
      y1,
      y2,
      group = grp,
      na.rm = FALSE,
      variance = "se",
      label_values = FALSE
    )
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$ratio, expected$ratio, tolerance = 1e-10)
  expect_true(all(is.finite(na_row$se)))
  expect_equal(na_row$n, expected$n)
})

test_that("get_ratios() NA group row ratio matches filtered calibrated design [oracle]", {
  df_c <- make_survey_data(n = 100L, n_psu = 10L, n_strata = 2L, seed = 42L)
  set.seed(43L)
  df_c$grp <- sample(c("A", "B", NA_character_), 100L, replace = TRUE)
  design_cal <- as_survey_nonprob(df_c, weights = wt)
  na_df_c <- df_c[is.na(df_c$grp), ]
  na_design_cal <- as_survey_nonprob(na_df_c, weights = wt)
  expected <- get_ratios(na_design_cal, y1, y2, variance = "se")
  result <- get_ratios(
    design_cal,
    y1,
    y2,
    group = grp,
    na.rm = FALSE,
    variance = "se",
    label_values = FALSE
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$ratio, expected$ratio, tolerance = 1e-10)
  expect_true(all(is.finite(na_row$se)))
  expect_equal(na_row$n, expected$n)
})

test_that("get_ratios() NA group row ratio matches filtered srs design [oracle]", {
  df_s <- make_survey_data(n = 100L, n_psu = 10L, n_strata = 2L, seed = 42L)
  set.seed(43L)
  df_s$grp <- sample(c("A", "B", NA_character_), 100L, replace = TRUE)
  design_srs <- as_survey(df_s, weights = wt)
  na_df_s <- df_s[is.na(df_s$grp), ]
  na_design_srs <- as_survey(na_df_s, weights = wt)
  expected <- get_ratios(na_design_srs, y1, y2, variance = "se")
  result <- get_ratios(
    design_srs,
    y1,
    y2,
    group = grp,
    na.rm = FALSE,
    variance = "se",
    label_values = FALSE
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$ratio, expected$ratio, tolerance = 1e-10)
  # SE legitimately differs: SRS variance uses nrow(design@data) for n_full;
  # domain estimation (full design) != pre-filtered oracle.
  expect_true(all(is.finite(na_row$se)))
  expect_equal(na_row$n, expected$n)
})

test_that("get_ratios() multi-group NA row ratio matches filtered taylor design [oracle]", {
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
    get_ratios(
      design_multi,
      y1,
      y2,
      group = c(grp, grp2),
      na.rm = FALSE,
      variance = "se",
      label_values = FALSE
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
    get_ratios(oracle_design, y1, y2, variance = "se")
  )
  na_x_rows <- result[is.na(result$grp) & result$grp2 == "X", ]
  expect_equal(na_x_rows$ratio, expected$ratio, tolerance = 1e-10)
  expect_true(all(is.finite(na_x_rows$se)))
  expect_equal(na_x_rows$n, expected$n)
})

# ---------------------------------------------------------------------------
# Additional coverage: NA propagation path, replicate design
# ---------------------------------------------------------------------------

test_that("get_ratios() na.rm=FALSE with NA denominator propagates NA (covers is.na(total_x) path)", {
  df <- make_survey_data(n = 60, n_psu = 10, n_strata = 2, seed = 900)
  # Introduce NA in denominator; na.rm=FALSE means total_x becomes NA
  df$denom_na <- c(NA_real_, df$y2[-1L])
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  result <- get_ratios(sc, y1, denom_na, variance = "se", na.rm = FALSE)
  test_result_invariants(result, "survey_ratios")
  expect_true(is.na(result$ratio[[1L]]))
  expect_true(is.na(result$se[[1L]]))
})

test_that("get_ratios() works for survey_replicate design (covers .replicate_ratio_cell())", {
  d <- make_survey_data(
    n = 100,
    n_psu = 10,
    n_strata = 2,
    design = "replicate",
    type = "brr",
    seed = 901
  )
  repwt_cols <- grep("^repwt_", names(d), value = TRUE)
  sc <- as_survey_replicate(
    d,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )
  result <- get_ratios(sc, y1, y2, variance = "se")
  test_result_invariants(result, "survey_ratios")
  expect_true(is.finite(result$ratio[[1L]]))
  expect_gte(result$se[[1L]], 0)
})

test_that("get_ratios() works for survey_twophase design", {
  d <- make_survey_data(
    n = 100,
    n_psu = 10,
    n_strata = 2,
    design = "twophase",
    seed = 902
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
  result <- get_ratios(sc, y1, y2, variance = "se")
  test_result_invariants(result, "survey_ratios")
  expect_true(is.finite(result$ratio[[1L]]))
})

test_that("get_ratios() empty domain for twophase returns NA", {
  d <- make_survey_data(
    n = 80,
    n_psu = 10,
    n_strata = 2,
    design = "twophase",
    seed = 903
  )
  phase1 <- as_survey(d, ids = psu, weights = wt, strata = strata, nest = TRUE)
  sc <- as_survey_twophase(
    phase1,
    subset = subset,
    ids2 = psu,
    strata2 = strata
  )
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- FALSE
  result <- get_ratios(sc, y1, y2, variance = "se")
  expect_true(all(is.na(result$ratio)))
})

# ---------------------------------------------------------------------------
# Additional coverage: empty domain for .replicate_ratio_cell(), se_srs=0 path
# ---------------------------------------------------------------------------

test_that("get_ratios() replicate empty domain returns NA (covers .replicate_ratio_cell() n_d=0)", {
  d <- make_survey_data(
    n = 80,
    n_psu = 10,
    n_strata = 2,
    design = "replicate",
    type = "brr",
    seed = 901
  )
  repwt_cols <- grep("^repwt_", names(d), value = TRUE)
  sc <- as_survey_replicate(
    d,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- rep(FALSE, 80L)
  result <- suppressWarnings(get_ratios(sc, numerator = y1, denominator = y2))
  expect_true(is.na(result$ratio[[1L]]))
})

test_that("get_ratios() replicate single-row domain hits se_srs=0 path in .replicate_ratio_cell()", {
  d <- make_survey_data(
    n = 80,
    n_psu = 10,
    n_strata = 2,
    design = "replicate",
    type = "brr",
    seed = 902
  )
  repwt_cols <- grep("^repwt_", names(d), value = TRUE)
  sc <- as_survey_replicate(
    d,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- seq_len(80L) == 1L
  result <- suppressWarnings(get_ratios(sc, numerator = y1, denominator = y2))
  expect_true(is.finite(result$ratio[[1L]]) || is.na(result$ratio[[1L]]))
})
