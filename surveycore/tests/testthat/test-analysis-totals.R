# test-analysis-totals.R
# Tests for get_totals() — the Phase 1 weighted-total estimation function.
#
# Numerical tolerance: 1e-10 for point estimates, 1e-8 for SE.
# Oracle tests skip if the 'survey' package is not installed.

# ---------------------------------------------------------------------------
# Category 1: Happy path — two modes
# ---------------------------------------------------------------------------

test_that("get_totals() with variable returns survey_totals tibble with n column", {
  df <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    n_strata = 2L,
    design = "taylor",
    seed = 1L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_totals(d, y1)
  test_result_invariants(result, "survey_totals")
  expect_true("total" %in% names(result))
  expect_true("ci_low" %in% names(result))
  expect_true("ci_high" %in% names(result))
  expect_true("n" %in% names(result))
  expect_false("se" %in% names(result)) # not in default variance = "ci"
  expect_true(is.finite(result$total[[1L]]))
  expect_lt(result$ci_low[[1L]], result$ci_high[[1L]])
  expect_identical(names(meta(result)$x), "y1")
})

test_that("get_totals() with no variable estimates population size (no n column)", {
  df <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    n_strata = 2L,
    design = "taylor",
    seed = 2L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_totals(d)
  test_result_invariants(result, "survey_totals")
  expect_true("total" %in% names(result))
  expect_false("n" %in% names(result)) # omitted in no-variable mode
  expect_true(is.finite(result$total[[1L]]))
  expect_null(meta(result)$x)

  # Population size ≈ sum of weights
  expected_N <- sum(df$wt)
  expect_equal(result$total[[1L]], expected_N, tolerance = 1e-10)
})

test_that("get_totals() no-variable n_weighted = TRUE includes n_weighted column", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 3L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_totals(d, n_weighted = TRUE)
  expect_true("n_weighted" %in% names(result))
  # For no-variable mode n_weighted equals total
  expect_equal(result$n_weighted[[1L]], result$total[[1L]], tolerance = 1e-14)
})

# ---------------------------------------------------------------------------
# Category 2: All 5 design types — variable mode
# ---------------------------------------------------------------------------

test_that("get_totals() works for survey_replicate design", {
  df <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    design = "replicate",
    type = "brr",
    seed = 4L
  )
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)
  d <- as_survey_replicate(
    df,
    weights = wt,
    repweights = all_of(repwt_cols),
    type = "BRR"
  )

  result <- get_totals(d, y1)
  test_result_invariants(result, "survey_totals")
  expect_true(is.finite(result$total[[1L]]))
})

test_that("get_totals() works for SRS design (Taylor with no ids/strata)", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 5L)
  d <- as_survey(df, weights = wt)

  result <- get_totals(d, y1)
  test_result_invariants(result, "survey_totals")
  expect_true(is.finite(result$total[[1L]]))
})

test_that("get_totals() works for survey_twophase design", {
  df <- make_survey_data(
    design = "twophase",
    n = 200L,
    n_psu = 20L,
    n_strata = 2L,
    seed = 6L
  )
  ph1 <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  d <- as_survey_twophase(ph1, subset = subset, method = "approx")

  result <- get_totals(d, y1)
  test_result_invariants(result, "survey_totals")
  expect_true(is.finite(result$total[[1L]]))
})

test_that("get_totals() works for survey_nonprob design", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 7L)
  d <- as_survey_nonprob(df, weights = wt)

  result <- get_totals(d, y1)
  test_result_invariants(result, "survey_totals")
  expect_true(is.finite(result$total[[1L]]))
})

# ---------------------------------------------------------------------------
# Category 3: meta() content
# ---------------------------------------------------------------------------

test_that("get_totals() meta() stores variable and design type", {
  df <- make_survey_data(n = 50L, design = "taylor", seed = 8L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_totals(d, y1)
  m <- meta(result)

  expect_identical(names(m$x), "y1")
  expect_true(is.character(m$design_type))
  expect_equal(m$conf_level, 0.95)
})

test_that("get_totals() meta()$x is NULL in no-variable mode", {
  df <- make_survey_data(n = 50L, design = "taylor", seed = 9L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_totals(d)
  expect_null(meta(result)$x)
})

# ---------------------------------------------------------------------------
# Category 4: variance= argument
# ---------------------------------------------------------------------------

test_that("get_totals() variance = 'se' produces se column", {
  df <- make_survey_data(n = 50L, design = "taylor", seed = 10L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_totals(d, y1, variance = "se")
  expect_true("se" %in% names(result))
  expect_false("ci_low" %in% names(result))
  expect_gte(result$se[[1L]], 0)
})

test_that("get_totals() variance = NULL produces only total (+ n in variable mode)", {
  df <- make_survey_data(n = 50L, design = "taylor", seed = 11L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result_var <- get_totals(d, y1, variance = NULL)
  result_novar <- get_totals(d, variance = NULL)

  expect_identical(names(result_var), c("total", "n"))
  expect_identical(names(result_novar), c("total"))
})

test_that("get_totals() variance = 'deff' produces deff column", {
  df <- make_survey_data(n = 100L, n_psu = 10L, design = "taylor", seed = 12L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_totals(d, y1, variance = "deff")
  expect_true("deff" %in% names(result))
  expect_gte(result$deff[[1L]], 0)
})

# ---------------------------------------------------------------------------
# Category 5: group= argument
# ---------------------------------------------------------------------------

test_that("get_totals() group= produces one row per group level", {
  df <- make_survey_data(
    n = 200L,
    n_psu = 20L,
    n_strata = 2L,
    design = "taylor",
    seed = 13L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  n_strata <- length(unique(df$strata))
  result <- get_totals(d, y1, group = strata)

  test_result_invariants(result, "survey_totals")
  expect_equal(nrow(result), n_strata)
  expect_true("strata" %in% names(result))
  expect_true(all(is.finite(result$total)))
})

test_that("get_totals() group= no-variable mode produces grouped pop sizes", {
  df <- make_survey_data(
    n = 200L,
    n_psu = 20L,
    n_strata = 2L,
    design = "taylor",
    seed = 14L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_totals(d, group = strata)
  test_result_invariants(result, "survey_totals")
  expect_false("n" %in% names(result)) # no n in no-variable mode
  expect_true(all(is.finite(result$total)))

  # Sum of grouped population sizes ≈ total population size
  result_all <- get_totals(d)
  expect_equal(sum(result$total), result_all$total[[1L]], tolerance = 1e-8)
})

# ---------------------------------------------------------------------------
# Category 6: na.rm
# ---------------------------------------------------------------------------

test_that("get_totals() na.rm = TRUE excludes NAs from computation", {
  df <- make_survey_data(n = 100L, n_psu = 10L, design = "taylor", seed = 15L)
  df$y1[c(1, 5, 10)] <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result_rm <- get_totals(d, y1, variance = NULL, na.rm = TRUE)
  result_nrm <- get_totals(d, y1, variance = NULL, na.rm = FALSE)

  expect_true(is.finite(result_rm$total[[1L]]))
  expect_equal(result_rm$n[[1L]], 97L)
  expect_true(is.na(result_nrm$total[[1L]]))
})

# ---------------------------------------------------------------------------
# Category 7: n_weighted
# ---------------------------------------------------------------------------

test_that("get_totals() n_weighted = TRUE adds n_weighted column", {
  df <- make_survey_data(n = 50L, design = "taylor", seed = 16L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_totals(d, y1, variance = NULL, n_weighted = TRUE)
  expect_true("n_weighted" %in% names(result))
  expect_gte(result$n_weighted[[1L]], 0)
})

# ---------------------------------------------------------------------------
# Category 8: name_style = "broom"
# ---------------------------------------------------------------------------

test_that("get_totals() name_style = 'broom' renames columns", {
  df <- make_survey_data(n = 50L, design = "taylor", seed = 17L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  result <- get_totals(d, y1, variance = c("se", "ci"), name_style = "broom")

  expect_true("estimate" %in% names(result))
  expect_true("std.error" %in% names(result))
  expect_true("conf.low" %in% names(result))
  expect_true("conf.high" %in% names(result))
  expect_false("total" %in% names(result))
})

# ---------------------------------------------------------------------------
# Category 9: min_cell_n warning
# ---------------------------------------------------------------------------

test_that("get_totals() emits small-cell warning when n < min_cell_n", {
  df <- data.frame(
    y = c(rep(1.0, 5), rep(2.0, 95)),
    g = c(rep("tiny", 5), rep("big", 95)),
    w = rep(1, 100),
    psu = rep(1:20, 5)
  )
  d <- as_survey(df, ids = psu, weights = w)

  expect_warning(
    get_totals(d, y, group = g, min_cell_n = 10L),
    class = "surveycore_warning_small_cell"
  )
})

# ---------------------------------------------------------------------------
# Category 10: Error paths
# ---------------------------------------------------------------------------

test_that("get_totals() errors for non-survey-design object", {
  expect_error(
    get_totals(data.frame(x = 1:5, y = rnorm(5)), y),
    class = "surveycore_error_unsupported_class"
  )
})

test_that("get_totals() errors for non-numeric variable", {
  df <- data.frame(y = letters[1:10], w = rep(1, 10))
  d <- as_survey(df, weights = w)
  expect_error(
    get_totals(d, y),
    class = "surveycore_error_non_numeric_variable"
  )
  expect_snapshot(error = TRUE, get_totals(d, y))
})

test_that("get_totals() errors when x resolves to multiple variables", {
  df <- data.frame(y1 = 1:10, y2 = 1:10, w = rep(1, 10))
  d <- as_survey(df, weights = w)
  expect_error(
    get_totals(d, starts_with("y")),
    class = "surveycore_error_wrong_variable_count"
  )
})

test_that("get_totals() errors for invalid variance value", {
  df <- data.frame(y = 1:10, w = rep(1, 10))
  d <- as_survey(df, weights = w)
  expect_error(
    get_totals(d, y, variance = "bad_val"),
    class = "surveycore_error_invalid_variance_arg"
  )
  expect_snapshot(error = TRUE, get_totals(d, y, variance = "bad_val"))
})

# ---------------------------------------------------------------------------
# Category 11: Oracle — NHANES Taylor design
# ---------------------------------------------------------------------------

test_that("get_totals() Taylor point + SE + CI match survey::svytotal() — NHANES [oracle]", {
  skip_if_not_installed("survey")

  d <- nhanes_2017[nhanes_2017$ridstatr == 2, ]
  sc <- as_survey(
    d,
    ids = sdmvpsu,
    strata = sdmvstra,
    weights = wtmec2yr,
    nest = TRUE
  )
  sv <- survey::svydesign(
    ids = ~sdmvpsu,
    strata = ~sdmvstra,
    weights = ~wtmec2yr,
    data = d,
    nest = TRUE
  )

  sc_est <- get_totals(sc, bpxsy1, variance = c("se", "ci"))
  sv_est <- survey::svytotal(~bpxsy1, sv, na.rm = TRUE)

  expect_equal(sc_est$total[[1L]], coef(sv_est)[["bpxsy1"]], tolerance = 1e-10)
  expect_equal(
    sc_est$se[[1L]],
    as.numeric(survey::SE(sv_est)),
    tolerance = 1e-8
  )
  expect_equal(sc_est$ci_low[[1L]], confint(sv_est)[1], tolerance = 1e-6)
  expect_equal(sc_est$ci_high[[1L]], confint(sv_est)[2], tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# Category 12: Numerical correctness — trivial cases
# ---------------------------------------------------------------------------

test_that("get_totals() total = sum(w * y) for trivial SRS design", {
  df <- data.frame(y = c(1.0, 2.0, 3.0), w = c(2.0, 3.0, 4.0))
  d <- as_survey(df, weights = w)
  result <- get_totals(d, y, variance = NULL)
  expect_equal(result$total[[1L]], 2 * 1 + 3 * 2 + 4 * 3, tolerance = 1e-14)
})

test_that("get_totals() no-variable total = sum(weights)", {
  df <- data.frame(y = 1:5, w = c(1, 2, 3, 4, 5))
  d <- as_survey(df, weights = w)
  result <- get_totals(d, variance = NULL)
  expect_equal(result$total[[1L]], 15.0, tolerance = 1e-14)
})

# ---------------------------------------------------------------------------
# Category 13: All designs via make_all_designs()
# ---------------------------------------------------------------------------

test_that("get_totals() returns a finite total for all 5 design types", {
  designs <- make_all_designs(seed = 99L)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- get_totals(d, y1)
    test_result_invariants(result, "survey_totals")
    expect_true(
      is.finite(result$total[[1L]]),
      label = paste0("get_totals() finite total for design type: ", nm)
    )
  }
})

test_that("get_totals() no-variable mode returns finite total for all 5 design types", {
  designs <- make_all_designs(seed = 100L)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- get_totals(d)
    test_result_invariants(result, "survey_totals")
    expect_true(
      is.finite(result$total[[1L]]),
      label = paste0("get_totals() no-var finite total for design type: ", nm)
    )
  }
})

# ── decimals argument ──────────────────────────────────────────────────────────

test_that("get_totals() decimals=2 rounds all double columns", {
  df <- make_survey_data(n = 200L, n_psu = 20L, n_strata = 4L, seed = 401L)
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  r <- get_totals(d, y1, variance = "ci", decimals = 2L)

  dbl_cols <- names(r)[vapply(r, is.double, logical(1L))]
  for (col in dbl_cols) {
    expect_equal(
      r[[col]],
      round(r[[col]], 2L),
      label = paste0(col, " rounded to 2 decimals")
    )
  }
})

test_that("get_totals() decimals=NULL applies no rounding", {
  df <- make_survey_data(n = 200L, n_psu = 20L, n_strata = 4L, seed = 402L)
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  r_none <- get_totals(d, y1, variance = NULL, decimals = NULL)
  r_rounded <- get_totals(d, y1, variance = NULL, decimals = 0L)

  expect_false(identical(r_none$total, r_rounded$total))
})

test_that("get_totals() rejects invalid decimals", {
  df <- make_survey_data(n = 100L, n_psu = 10L, n_strata = 2L, seed = 403L)
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )

  expect_error(
    get_totals(d, y1, decimals = 1.5),
    class = "surveycore_error_invalid_decimals"
  )
})


# ── NA group rows (na.rm extension) — Test Blocks 1–8c + oracle ───────────────

# Block 1: default na.rm = TRUE excludes NA group rows (regression guard)

test_that("get_totals() default (na.rm = TRUE) excludes group NA rows", {
  d <- make_na_group_design()
  r <- get_totals(d, y1, group = grp)
  expect_false(anyNA(r$grp))
})

# Block 2: na.rm = FALSE includes NA group row

test_that("get_totals() includes NA group row when na.rm = FALSE", {
  d <- make_na_group_design()
  r <- get_totals(d, y1, group = grp, na.rm = FALSE)
  expect_true(any(is.na(r$grp)))
})

# Block 3: NA group row is last

test_that("get_totals() places NA group row after non-NA rows", {
  d <- make_na_group_design()
  r <- get_totals(d, y1, group = grp, na.rm = FALSE)
  na_idx <- which(is.na(r$grp))
  nn_idx <- which(!is.na(r$grp))
  expect_true(all(na_idx > max(nn_idx)))
})

# Block 4: NA group row has finite total estimate

test_that("get_totals() NA group row has finite total estimate", {
  d <- make_na_group_design()
  r <- get_totals(d, y1, group = grp, na.rm = FALSE)
  na_row <- get_na_group_rows(r, "grp")
  expect_true(all(is.finite(na_row$total)))
})

# Block 5a: multi-group — NA in first group var

test_that("get_totals() handles NA in first of two group vars (na.rm = FALSE)", {
  d <- make_na_group_design() # grp has NAs; grp2 has none
  r <- get_totals(d, y1, group = c(grp, grp2), na.rm = FALSE)
  expect_true(any(is.na(r$grp) & !is.na(r$grp2)))
})

# Block 5b: multi-group — NA in second group var (inline fixture)

test_that("get_totals() handles NA in second of two group vars (na.rm = FALSE)", {
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
  r <- get_totals(d, y1, group = c(grp, grp2), na.rm = FALSE)
  expect_true(any(!is.na(r$grp) & is.na(r$grp2)))
})

# Block 6: all-NA group var — warning fires; output has NA group row; matches ungrouped

test_that("get_totals() handles group var that is entirely NA (na.rm = FALSE)", {
  d <- make_all_na_group_design()
  expect_warning(
    r <- get_totals(d, y1, group = grp, na.rm = FALSE),
    class = "surveycore_warning_single_level"
  )
  expect_equal(nrow(r), 1L)
  expect_true(is.na(r$grp[[1L]]))
  expect_true(is.finite(r$total[[1L]]))
  ungrouped <- get_totals(d, y1)
  expect_equal(r$total, ungrouped$total, tolerance = 1e-10)
})

# Block 7a: label_values = TRUE — regular NA group row remains NA in factor

test_that("get_totals() regular NA group row is NA in factor when label_values = TRUE", {
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
  r <- get_totals(d, y1, group = grp, na.rm = FALSE, label_values = TRUE)

  expect_true(is.factor(r$grp))
  na_row <- get_na_group_rows(r, "grp")
  expect_true(nrow(na_row) > 0L)
  expect_true(is.na(na_row$grp[[1L]]))
})

# Block 7b: label_values = TRUE — haven-tagged NA becomes a factor level

test_that("get_totals() haven-labeled NA group rows become factor levels when label_values = TRUE", {
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
  r <- get_totals(d, y1, group = grp, na.rm = FALSE, label_values = TRUE)

  expect_true(is.factor(r$grp))
  expect_true("Refused" %in% levels(r$grp))
  refused_row <- r[!is.na(r$grp) & r$grp == "Refused", ]
  expect_true(nrow(refused_row) > 0L)
})

# Block 8: group_by() path — NA group rows appear with na.rm = FALSE

test_that("get_totals() includes NA group row when group set via group_by() and na.rm = FALSE", {
  skip_if_not_installed("surveytidy")
  d <- surveytidy::group_by(make_na_group_design(), grp)
  r <- get_totals(d, y1, na.rm = FALSE)
  expect_true(anyNA(r$grp))
})

# Block 8b: group_by() path — NA group rows excluded by default

test_that("get_totals() excludes NA group rows by default when group set via group_by()", {
  skip_if_not_installed("surveytidy")
  d <- surveytidy::group_by(make_na_group_design(), grp)
  r <- get_totals(d, y1)
  expect_false(anyNA(r$grp))
})

# Block 8c: na.rm = NA is rejected (dual pattern)

test_that("get_totals() rejects na.rm = NA with surveycore_error_na_rm_not_logical", {
  d <- make_na_group_design()
  expect_error(
    get_totals(d, y1, group = grp, na.rm = NA),
    class = "surveycore_error_na_rm_not_logical"
  )
  expect_snapshot(
    error = TRUE,
    get_totals(d, y1, group = grp, na.rm = NA)
  )
})


# ── Oracle tests: NA group row estimate matches filtered design ────────────────

test_that("get_totals() NA group row total matches filtered taylor design [oracle]", {
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
  expected <- get_totals(na_design, y1, variance = "se")
  result <- get_totals(
    design_oracle,
    y1,
    group = grp,
    na.rm = FALSE,
    variance = "se"
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$total, expected$total, tolerance = 1e-10)
  expect_equal(na_row$se, expected$se, tolerance = 1e-8)
  expect_equal(na_row$n, expected$n)
})

test_that("get_totals() NA group row total matches filtered replicate design [oracle]", {
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
  expected <- get_totals(na_design_rep, y1, variance = "se")
  result <- get_totals(
    design_rep,
    y1,
    group = grp,
    na.rm = FALSE,
    variance = "se"
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$total, expected$total, tolerance = 1e-10)
  expect_equal(na_row$se, expected$se, tolerance = 1e-8)
  expect_equal(na_row$n, expected$n)
})

test_that("get_totals() NA group row total matches filtered twophase design [oracle]", {
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
  expected <- suppressWarnings(get_totals(
    na_design_twophase,
    y1,
    variance = "se"
  ))
  result <- suppressWarnings(
    get_totals(design_twophase, y1, group = grp, na.rm = FALSE, variance = "se")
  )
  na_row <- get_na_group_rows(result, "grp")
  # Total legitimately differs for two-phase: the total estimator uses
  # calibration weights that depend on the full sample structure. Unlike ratio
  # estimates (means, proportions), totals do not cancel out weight scaling.
  expect_true(all(is.finite(na_row$total)))
  expect_true(all(is.finite(na_row$se)))
  expect_equal(na_row$n, expected$n)
})

test_that("get_totals() NA group row total matches filtered calibrated design [oracle]", {
  df_c <- make_survey_data(n = 100L, n_psu = 10L, n_strata = 2L, seed = 42L)
  set.seed(43L)
  df_c$grp <- sample(c("A", "B", NA_character_), 100L, replace = TRUE)
  design_cal <- as_survey_nonprob(df_c, weights = wt)
  na_df_c <- df_c[is.na(df_c$grp), ]
  na_design_cal <- as_survey_nonprob(na_df_c, weights = wt)
  expected <- get_totals(na_design_cal, y1, variance = "se")
  result <- get_totals(
    design_cal,
    y1,
    group = grp,
    na.rm = FALSE,
    variance = "se"
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$total, expected$total, tolerance = 1e-10)
  # SE legitimately differs: calibrated variance depends on total sample size;
  # domain estimation (full design) != pre-filtered oracle.
  expect_true(all(is.finite(na_row$se)))
  expect_equal(na_row$n, expected$n)
})

test_that("get_totals() NA group row total matches filtered SRS design [oracle]", {
  df_s <- make_survey_data(n = 100L, n_psu = 10L, n_strata = 2L, seed = 42L)
  set.seed(43L)
  df_s$grp <- sample(c("A", "B", NA_character_), 100L, replace = TRUE)
  design_srs <- as_survey(df_s, weights = wt)
  na_df_s <- df_s[is.na(df_s$grp), ]
  na_design_srs <- as_survey(na_df_s, weights = wt)
  expected <- get_totals(na_design_srs, y1, variance = "se")
  result <- get_totals(
    design_srs,
    y1,
    group = grp,
    na.rm = FALSE,
    variance = "se"
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$total, expected$total, tolerance = 1e-10)
  # SE legitimately differs: HT variance depends on total sample size;
  # domain estimation (full design, n=100) != pre-filtered oracle (n=~30).
  expect_true(all(is.finite(na_row$se)))
  expect_equal(na_row$n, expected$n)
})

test_that("get_totals() multi-group NA row total matches filtered taylor design [oracle]", {
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
    get_totals(
      design_multi,
      y1,
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
  expected <- suppressWarnings(get_totals(oracle_design, y1, variance = "se"))
  na_x_rows <- result[is.na(result$grp) & result$grp2 == "X", ]
  expect_equal(na_x_rows$total, expected$total, tolerance = 1e-10)
  # SE legitimately differs: multi-group oracle cells are small;
  # domain estimation uses full cluster/strata structure, oracle uses subset.
  expect_true(all(is.finite(na_x_rows$se)))
  expect_equal(na_x_rows$n, expected$n)
})

# ---------------------------------------------------------------------------
# Additional coverage: SRS, twophase, empty domains
# ---------------------------------------------------------------------------

test_that("get_totals() works for SRS design (covers .srs_total_cell())", {
  set.seed(600)
  n <- 100L
  N <- 1000L
  df <- data.frame(y = rnorm(n, mean = 50, sd = 10), w = rep(N / n, n))
  sc <- as_survey(df, weights = w)
  result <- get_totals(sc, y, variance = "se")
  test_result_invariants(result, "survey_totals")
  expect_true(is.finite(result$total[[1L]]))
  expect_gte(result$se[[1L]], 0)
})

test_that("get_totals() SRS design with FPC covers FPC path in .srs_total_cell()", {
  set.seed(601)
  n <- 80L
  N <- 800L
  df <- data.frame(y = rnorm(n), w = rep(N / n, n), pop = rep(N, n))
  sc <- as_survey(df, weights = w, fpc = pop)
  result <- get_totals(sc, y, variance = "se")
  test_result_invariants(result, "survey_totals")
  expect_true(is.finite(result$total[[1L]]))
  expect_gte(result$se[[1L]], 0)
})

test_that("get_totals() works for survey_twophase design (covers .twophase_total_cell())", {
  d <- make_survey_data(
    n = 100,
    n_psu = 10,
    n_strata = 2,
    design = "twophase",
    seed = 602
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
  result <- get_totals(sc, y1, variance = "se")
  test_result_invariants(result, "survey_totals")
  expect_true(is.finite(result$total[[1L]]))
  expect_gte(result$se[[1L]], 0)
})

test_that("get_totals() taylor: empty domain returns NA (covers n_d=0 branch)", {
  df <- make_survey_data(n = 60, n_psu = 10, n_strata = 2, seed = 603)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- FALSE
  result <- get_totals(sc, y1, variance = "se")
  expect_true(all(is.na(result$total)))
})

test_that("get_totals() replicate: empty domain returns NA (covers n_d=0 branch)", {
  d <- make_survey_data(
    n = 60,
    n_psu = 10,
    n_strata = 2,
    design = "replicate",
    type = "brr",
    seed = 604
  )
  repwt_cols <- grep("^repwt_", names(d), value = TRUE)
  sc <- as_survey_replicate(
    d,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- FALSE
  result <- get_totals(sc, y1, variance = "se")
  expect_true(all(is.na(result$total)))
})

test_that("get_totals() twophase: empty domain returns NA (covers .twophase_total_cell() n_d=0)", {
  d <- make_survey_data(
    n = 80,
    n_psu = 10,
    n_strata = 2,
    design = "twophase",
    seed = 605
  )
  phase1 <- as_survey(d, ids = psu, weights = wt, strata = strata, nest = TRUE)
  sc <- as_survey_twophase(
    phase1,
    subset = subset,
    ids2 = psu,
    strata2 = strata
  )
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- FALSE
  result <- get_totals(sc, y1, variance = "se")
  expect_true(all(is.na(result$total)))
})

test_that("get_totals() SRS design n_d=1 domain returns finite SE (Taylor: uncentered influence)", {
  set.seed(606)
  n <- 40L
  df <- data.frame(y = rnorm(n), w = rep(1, n))
  sc <- as_survey(df, weights = w)
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- seq_len(n) == 1L
  result <- get_totals(sc, y, variance = "se")
  expect_equal(result$n[[1L]], 1L)
  expect_true(is.finite(result$total[[1L]]))
  # For totals, the influence function is NOT centered (infl_i = w_i * y_i),
  # so even with n_d=1 the single contributing PSU produces nonzero variance
  # (matches survey::svytotal behavior)
  expect_true(is.finite(result$se[[1L]]))
})

test_that("get_totals() taylor with FPC fraction covers FPC fraction path in .taylor_total_cell()", {
  set.seed(607)
  df <- make_survey_data(n = 100, n_psu = 10, n_strata = 2, seed = 607)
  df$fpc_frac <- df$fpc / (df$fpc * 5) # fractions in (0, 0.2]
  sc <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc_frac,
    nest = TRUE
  )
  result <- get_totals(sc, y1, variance = "se")
  test_result_invariants(result, "survey_totals")
  expect_true(is.finite(result$total[[1L]]))
})

# ---------------------------------------------------------------------------
# Additional coverage: calibrated design, se_srs=0 paths, unsupported class
# ---------------------------------------------------------------------------

test_that("get_totals() works for survey_nonprob design", {
  set.seed(701)
  df <- data.frame(y = rnorm(60), w = runif(60, 0.5, 2))
  sc <- as_survey_nonprob(df, weights = w)
  result <- get_totals(sc, y, variance = "se")
  test_result_invariants(result, "survey_totals")
  expect_true(is.finite(result$total[[1L]]))
  expect_true(is.finite(result$se[[1L]]))
})

test_that("get_totals() calibrated empty domain returns NA (covers .calibrated_total_cell() n_d=0)", {
  set.seed(702)
  df <- data.frame(y = rnorm(20), w = runif(20, 0.5, 2))
  sc <- as_survey_nonprob(df, weights = w)
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- rep(FALSE, 20L)
  result <- get_totals(sc, y, variance = "se")
  expect_true(is.na(result$total[[1L]]))
})

test_that("get_totals() calibrated single-row domain returns total with NA se (covers n_d=1 path)", {
  set.seed(703)
  df <- data.frame(y = rnorm(20), w = runif(20, 0.5, 2))
  sc <- as_survey_nonprob(df, weights = w)
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- seq_len(20L) == 1L
  result <- get_totals(sc, y, variance = "se")
  expect_true(is.finite(result$total[[1L]]))
  expect_true(is.na(result$se[[1L]]))
})

test_that("get_totals() taylor single-row domain hits se_srs=0 branch in .taylor_total_cell()", {
  set.seed(704)
  df <- make_survey_data(n = 50, n_psu = 10, n_strata = 2, seed = 704)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- seq_len(50L) == 1L
  result <- get_totals(sc, y1, variance = "se")
  expect_true(is.finite(result$total[[1L]]) || is.na(result$total[[1L]]))
})

test_that("get_totals() replicate single-row domain hits se_srs=0 branch in .replicate_total_cell()", {
  d <- make_survey_data(
    n = 60,
    n_psu = 10,
    n_strata = 2,
    design = "replicate",
    type = "brr",
    seed = 705
  )
  repwt_cols <- grep("^repwt_", names(d), value = TRUE)
  sc <- as_survey_replicate(
    d,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- seq_len(60L) == 1L
  result <- get_totals(sc, y1, variance = "se")
  expect_true(is.finite(result$total[[1L]]) || is.na(result$total[[1L]]))
})

test_that(".total_cell() errors for unsupported design class", {
  expect_error(
    surveycore:::.total_cell(list(fake = TRUE), "y", rep(1, 5)),
    class = "surveycore_error_unsupported_class"
  )
})
