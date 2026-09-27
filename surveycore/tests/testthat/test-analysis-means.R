# test-analysis-means.R
# Tests for get_means() — the Phase 1 weighted-mean estimation function.
#
# Numerical tolerance: 1e-10 for point estimates, 1e-8 for SE.
# Oracle tests skip if the 'survey' package is not installed.

# ---------------------------------------------------------------------------
# Category 1: Happy path — return structure (all design types)
# ---------------------------------------------------------------------------

test_that("get_means() returns survey_means tibble for survey_taylor", {
  df <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    n_strata = 2L,
    design = "taylor",
    seed = 1L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_means(d, y1)
  test_result_invariants(result, "survey_means")
  expect_true("mean" %in% names(result))
  expect_true("ci_low" %in% names(result))
  expect_true("ci_high" %in% names(result))
  expect_true("n" %in% names(result))
  expect_false("se" %in% names(result)) # not in default variance = "ci"
  expect_true(is.finite(result$mean[[1L]]))
  expect_true(is.finite(result$ci_low[[1L]]))
  expect_true(is.finite(result$ci_high[[1L]]))
  expect_lt(result$ci_low[[1L]], result$ci_high[[1L]])
})

test_that("get_means() works for survey_replicate design", {
  df <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    design = "replicate",
    type = "brr",
    seed = 2L
  )
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)
  d <- as_survey_replicate(
    df,
    weights = wt,
    repweights = all_of(repwt_cols),
    type = "BRR"
  )

  result <- get_means(d, y1)
  test_result_invariants(result, "survey_means")
  expect_true(is.finite(result$mean[[1L]]))
  expect_true(is.finite(result$ci_low[[1L]]))
})

test_that("get_means() works for SRS design (Taylor with no ids/strata)", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 3L)
  d <- as_survey(df, weights = wt)

  result <- get_means(d, y1)
  test_result_invariants(result, "survey_means")
  expect_true(is.finite(result$mean[[1L]]))
})

test_that("get_means() works for survey_twophase design", {
  df <- make_survey_data(
    design = "twophase",
    n = 200L,
    n_psu = 20L,
    n_strata = 2L,
    seed = 4L
  )
  ph1 <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  d <- as_survey_twophase(ph1, subset = subset, method = "approx")

  result <- get_means(d, y1)
  test_result_invariants(result, "survey_means")
  expect_true(is.finite(result$mean[[1L]]))
})

test_that("get_means() works for survey_nonprob design", {
  df <- make_survey_data(n = 100L, design = "taylor", seed = 5L)
  d <- as_survey_nonprob(df, weights = wt)

  result <- get_means(d, y1)
  test_result_invariants(result, "survey_means")
  expect_true(is.finite(result$mean[[1L]]))
})

# ---------------------------------------------------------------------------
# Category 2: meta() content
# ---------------------------------------------------------------------------

test_that("get_means() meta() stores variable name and design type", {
  df <- make_survey_data(n = 50L, design = "taylor", seed = 6L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_means(d, y1)
  m <- meta(result)

  expect_identical(names(m$x), "y1")
  expect_true(is.character(m$design_type))
  expect_equal(m$conf_level, 0.95)
  expect_type(m$group, "list")
  expect_length(m$group, 0L)
})

# ---------------------------------------------------------------------------
# Category 3: variance= argument — column presence
# ---------------------------------------------------------------------------

test_that("get_means() variance = 'se' produces se column", {
  df <- make_survey_data(n = 50L, design = "taylor", seed = 7L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_means(d, y1, variance = "se")
  expect_true("se" %in% names(result))
  expect_false("ci_low" %in% names(result))
  expect_false("ci_high" %in% names(result))
  expect_true(is.finite(result$se[[1L]]))
  expect_gte(result$se[[1L]], 0)
})

test_that("get_means() variance = c('se', 'ci') produces se + CI columns", {
  df <- make_survey_data(n = 50L, design = "taylor", seed = 8L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_means(d, y1, variance = c("se", "ci"))
  expect_true("se" %in% names(result))
  expect_true("ci_low" %in% names(result))
  expect_true("ci_high" %in% names(result))
})

test_that("get_means() variance = 'var' produces var column", {
  df <- make_survey_data(n = 50L, design = "taylor", seed = 9L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_means(d, y1, variance = "var")
  expect_true("var" %in% names(result))
  expect_gte(result$var[[1L]], 0)
  expect_equal(
    sqrt(result$var[[1L]]),
    get_means(d, y1, variance = "se")$se[[1L]],
    tolerance = 1e-14
  )
})

test_that("get_means() variance = 'cv' produces cv column", {
  df <- make_survey_data(n = 50L, design = "taylor", seed = 10L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_means(d, y1, variance = "cv")
  expect_true("cv" %in% names(result))
  # cv = se / |mean|
  r2 <- get_means(d, y1, variance = "se")
  expect_equal(
    result$cv[[1L]],
    r2$se[[1L]] / abs(r2$mean[[1L]]),
    tolerance = 1e-14
  )
})

test_that("get_means() variance = 'moe' produces moe column", {
  df <- make_survey_data(n = 50L, design = "taylor", seed = 11L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_means(d, y1, variance = "moe")
  expect_true("moe" %in% names(result))
  expect_gte(result$moe[[1L]], 0)
})

test_that("get_means() variance = 'deff' produces deff column", {
  df <- make_survey_data(n = 100L, n_psu = 10L, design = "taylor", seed = 12L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_means(d, y1, variance = "deff")
  expect_true("deff" %in% names(result))
  expect_gte(result$deff[[1L]], 0)
})

test_that("get_means() variance = NULL produces only mean + n columns", {
  df <- make_survey_data(n = 50L, design = "taylor", seed = 13L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result <- get_means(d, y1, variance = NULL)
  expect_identical(names(result), c("mean", "n"))
})

# ---------------------------------------------------------------------------
# Category 4: n_weighted
# ---------------------------------------------------------------------------

test_that("get_means() n_weighted = TRUE adds n_weighted column", {
  df <- make_survey_data(n = 50L, design = "taylor", seed = 14L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result_no <- get_means(d, y1, variance = NULL)
  result_nw <- get_means(d, y1, variance = NULL, n_weighted = TRUE)

  expect_false("n_weighted" %in% names(result_no))
  expect_true("n_weighted" %in% names(result_nw))
  expect_gte(result_nw$n_weighted[[1L]], 0)
})

# ---------------------------------------------------------------------------
# Category 5: group= argument
# ---------------------------------------------------------------------------

test_that("get_means() group= produces one row per group level", {
  df <- make_survey_data(
    n = 200L,
    n_psu = 20L,
    n_strata = 2L,
    design = "taylor",
    seed = 15L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  n_strata <- length(unique(df$strata))
  result <- get_means(d, y1, group = strata)

  test_result_invariants(result, "survey_means")
  expect_equal(nrow(result), n_strata)
  expect_true("strata" %in% names(result))
  expect_true(all(is.finite(result$mean)))
  expect_identical(names(meta(result)$group), "strata")
})

test_that("get_means() group= with multiple group vars produces correct row count", {
  set.seed(1)
  df <- data.frame(
    g1 = rep(c("A", "B"), each = 50),
    g2 = rep(c("X", "Y"), 50),
    y = rnorm(100),
    wt = runif(100, 0.5, 2),
    psu = rep(1:20, 5)
  )
  d <- as_survey(df, ids = psu, weights = wt)

  result <- get_means(d, y, group = c(g1, g2))

  # 2 levels of g1 × 2 levels of g2 = 4 rows
  expect_equal(nrow(result), 4L)
  expect_true("g1" %in% names(result))
  expect_true("g2" %in% names(result))
})

# ---------------------------------------------------------------------------
# Category 6: na.rm
# ---------------------------------------------------------------------------

test_that("get_means() na.rm = TRUE excludes NAs from computation", {
  df <- make_survey_data(n = 100L, n_psu = 10L, design = "taylor", seed = 16L)
  df$y1[c(1, 5, 10)] <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  result_rm <- get_means(d, y1, variance = "se", na.rm = TRUE)
  result_nrm <- get_means(d, y1, variance = "se", na.rm = FALSE)

  expect_true(is.finite(result_rm$mean[[1L]]))
  expect_equal(result_rm$n[[1L]], 97L) # 100 - 3 NAs
  expect_true(is.na(result_nrm$mean[[1L]]))
})

test_that("get_means() na.rm = FALSE returns NA when any x is NA", {
  df <- data.frame(y = c(1.0, 2.0, NA_real_), w = rep(1, 3))
  d <- as_survey(df, weights = w)
  result <- get_means(d, y, variance = NULL, na.rm = FALSE)
  expect_true(is.na(result$mean[[1L]]))
})

# ---------------------------------------------------------------------------
# Category 7: name_style = "broom"
# ---------------------------------------------------------------------------

test_that("get_means() name_style = 'broom' renames estimate columns", {
  df <- make_survey_data(n = 50L, design = "taylor", seed = 17L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  result <- get_means(d, y1, variance = c("se", "ci"), name_style = "broom")

  expect_true("estimate" %in% names(result))
  expect_true("std.error" %in% names(result))
  expect_true("conf.low" %in% names(result))
  expect_true("conf.high" %in% names(result))
  expect_false("mean" %in% names(result))
  expect_false("se" %in% names(result))
})

# ---------------------------------------------------------------------------
# Category 8: min_cell_n warning
# ---------------------------------------------------------------------------

test_that("get_means() emits small-cell warning when n < min_cell_n", {
  # Use a group that produces a very small cell
  df <- data.frame(
    y = c(rep(1.0, 5), rep(2.0, 95)),
    g = c(rep("tiny", 5), rep("big", 95)),
    w = rep(1, 100),
    psu = rep(1:20, 5)
  )
  d <- as_survey(df, ids = psu, weights = w)

  expect_warning(
    get_means(d, y, group = g, min_cell_n = 10L),
    class = "surveycore_warning_small_cell"
  )
})

# ---------------------------------------------------------------------------
# Category 9: Error paths
# ---------------------------------------------------------------------------

test_that("get_means() errors for non-survey-design object", {
  expect_error(
    get_means(data.frame(x = 1:5, y = rnorm(5)), y),
    class = "surveycore_error_unsupported_class"
  )
})

test_that("get_means() errors for non-numeric variable", {
  df <- data.frame(y = letters[1:10], w = rep(1, 10))
  d <- as_survey(df, weights = w)
  expect_error(
    get_means(d, y),
    class = "surveycore_error_non_numeric_variable"
  )
  expect_snapshot(error = TRUE, get_means(d, y))
})

test_that("get_means() errors when x resolves to zero variables", {
  df <- data.frame(y1 = 1:10, w = rep(1, 10))
  d <- as_survey(df, weights = w)
  # Select an empty set
  expect_error(
    get_means(d, starts_with("zzz")),
    class = "surveycore_error_wrong_variable_count"
  )
})

test_that("get_means() errors when x resolves to multiple variables", {
  df <- data.frame(y1 = 1:10, y2 = 1:10, w = rep(1, 10))
  d <- as_survey(df, weights = w)
  expect_error(
    get_means(d, starts_with("y")),
    class = "surveycore_error_wrong_variable_count"
  )
})

test_that("get_means() errors for invalid variance value", {
  df <- data.frame(y = 1:10, w = rep(1, 10))
  d <- as_survey(df, weights = w)
  expect_error(
    get_means(d, y, variance = "bogus"),
    class = "surveycore_error_invalid_variance_arg"
  )
  expect_snapshot(error = TRUE, get_means(d, y, variance = "bogus"))
})

test_that("get_means() errors for invalid conf_level", {
  df <- data.frame(y = 1:10, w = rep(1, 10))
  d <- as_survey(df, weights = w)
  expect_error(
    get_means(d, y, conf_level = 1.5),
    class = "surveycore_error_invalid_conf_level"
  )
})

# ---------------------------------------------------------------------------
# Category 10: n column and domain estimation
# ---------------------------------------------------------------------------

test_that("get_means() n column equals unweighted count of non-NA observations", {
  df <- data.frame(y = c(1:8, NA_real_, NA_real_), w = rep(1, 10))
  d <- as_survey(df, weights = w)
  result <- get_means(d, y, variance = NULL, na.rm = TRUE)
  expect_equal(result$n[[1L]], 8L)
})

test_that("get_means() domain SE is not smaller than physical subset SE (Taylor)", {
  # Domain estimation retains full cluster/strata structure → SE >= subset SE
  df <- make_survey_data(
    n = 200L,
    n_psu = 20L,
    n_strata = 2L,
    design = "taylor",
    seed = 20L
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  # Full design grouped by strata (domain estimation)
  result_domain <- get_means(d, y1, variance = "se", group = strata)
  se_domain <- result_domain$se[[1L]]

  # Physical subset
  strata_levels <- sort(unique(df$strata))
  df_sub <- df[df$strata == strata_levels[[1L]], ]
  d_sub <- as_survey(
    df_sub,
    ids = psu,
    weights = wt,
    strata = strata,
    nest = TRUE
  )
  result_sub <- get_means(d_sub, y1, variance = "se")
  se_sub <- result_sub$se[[1L]]

  expect_gte(se_domain, se_sub * 0.99) # domain SE >= subset SE (allow tiny float gap)
})

# ---------------------------------------------------------------------------
# Category 11: conf_level argument
# ---------------------------------------------------------------------------

test_that("get_means() wider CI for higher conf_level", {
  df <- make_survey_data(n = 100L, n_psu = 10L, design = "taylor", seed = 21L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)

  r95 <- get_means(d, y1, variance = "ci", conf_level = 0.95)
  r99 <- get_means(d, y1, variance = "ci", conf_level = 0.99)

  width95 <- r95$ci_high[[1L]] - r95$ci_low[[1L]]
  width99 <- r99$ci_high[[1L]] - r99$ci_low[[1L]]
  expect_gt(width99, width95)
})

# ---------------------------------------------------------------------------
# Category 12: Oracle — NHANES Taylor design
# ---------------------------------------------------------------------------

test_that("get_means() Taylor point + SE + CI match survey::svymean() — NHANES [oracle]", {
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

  sc_est <- get_means(sc, bpxsy1, variance = c("se", "ci"))
  sv_est <- survey::svymean(~bpxsy1, sv, na.rm = TRUE)

  expect_equal(sc_est$mean[[1L]], coef(sv_est)[["bpxsy1"]], tolerance = 1e-10)
  expect_equal(
    sc_est$se[[1L]],
    as.numeric(survey::SE(sv_est)),
    tolerance = 1e-8
  )
  expect_equal(sc_est$ci_low[[1L]], confint(sv_est)[1], tolerance = 1e-6)
  expect_equal(sc_est$ci_high[[1L]], confint(sv_est)[2], tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# Category 13: All designs via make_all_designs()
# ---------------------------------------------------------------------------

test_that("get_means() returns a finite estimate for all 5 design types", {
  designs <- make_all_designs(seed = 99L)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- get_means(d, y1)
    test_result_invariants(result, "survey_means")
    expect_true(
      is.finite(result$mean[[1L]]),
      label = paste0("get_means() finite mean for design type: ", nm)
    )
  }
})

# ---------------------------------------------------------------------------
# Category 14: New meta structure & group label conversion
# ---------------------------------------------------------------------------

test_that("get_means() group column is <fct> when group var has haven labels", {
  df <- data.frame(
    y = rnorm(100),
    gender = structure(
      c(1L, 2L, 1L, 2L)[rep(1:4, 25)],
      labels = c(Male = 1L, Female = 2L)
    ),
    w = rep(1, 100)
  )
  d <- as_survey(df, weights = w)

  result <- get_means(d, y, group = gender, variance = NULL)
  expect_true(is.factor(result$gender))
  expect_identical(levels(result$gender), c("Male", "Female"))
  expect_identical(as.character(result$gender), c("Male", "Female"))
})

test_that("get_means() group column is <fct> when group var is a plain R factor", {
  df <- data.frame(
    y = rnorm(100),
    grp = factor(rep(c("B", "A"), 50), levels = c("B", "A")),
    w = rep(1, 100)
  )
  d <- as_survey(df, weights = w)

  result <- get_means(d, y, group = grp, variance = NULL)
  expect_true(is.factor(result$grp))
  expect_identical(levels(result$grp), c("B", "A"))
})

test_that("get_means() group column retains raw codes when label_values = FALSE", {
  df <- data.frame(
    y = rnorm(100),
    gender = structure(
      c(1L, 2L)[rep(1:2, 50)],
      labels = c(Male = 1L, Female = 2L)
    ),
    w = rep(1, 100)
  )
  d <- as_survey(df, weights = w)

  result <- get_means(
    d,
    y,
    group = gender,
    variance = NULL,
    label_values = FALSE
  )
  expect_false(is.factor(result$gender))
  expect_true(is.integer(result$gender))
})

test_that("get_means() meta$group stores value_labels regardless of label_values", {
  df <- data.frame(
    y = rnorm(100),
    gender = structure(
      c(1L, 2L)[rep(1:2, 50)],
      labels = c(Male = 1L, Female = 2L)
    ),
    w = rep(1, 100)
  )
  d <- as_survey(df, weights = w)

  r_true <- get_means(
    d,
    y,
    group = gender,
    variance = NULL,
    label_values = TRUE
  )
  r_false <- get_means(
    d,
    y,
    group = gender,
    variance = NULL,
    label_values = FALSE
  )

  # .meta always stores labels regardless of label_values argument
  expect_equal(
    meta(r_true)$group$gender$value_labels,
    c(Male = 1L, Female = 2L)
  )
  expect_equal(
    meta(r_false)$group$gender$value_labels,
    c(Male = 1L, Female = 2L)
  )
})

test_that("get_means() group column name is always raw variable name (not label)", {
  df <- data.frame(y = rnorm(50), grp = rep(1:2, 25), w = rep(1, 50))
  d <- as_survey(df, weights = w)
  d <- set_var_label(d, grp = "Group variable")

  result <- get_means(d, y, group = grp, variance = NULL, label_vars = TRUE)
  # Column is named "grp", not "Group variable"
  expect_true("grp" %in% names(result))
  expect_false("Group variable" %in% names(result))
})

test_that("get_means() meta$x has correct nested structure", {
  df <- data.frame(y = rnorm(50), w = rep(1, 50))
  d <- as_survey(df, weights = w)
  d <- set_var_label(d, y = "Outcome variable")

  result <- get_means(d, y, variance = NULL)
  m <- meta(result)

  expect_identical(names(m$x), "y")
  expect_identical(m$x$y$variable_label, "Outcome variable")
  expect_type(m$x$y, "list")
  expect_true(all(
    c("variable_label", "question_preface", "value_labels") %in%
      names(m$x$y)
  ))
})


# ── decimals argument ──────────────────────────────────────────────────────────

test_that("get_means() decimals=2 rounds all double columns", {
  df <- make_survey_data(n = 200L, n_psu = 20L, n_strata = 4L, seed = 301L)
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  r <- get_means(d, y1, variance = "ci", decimals = 2L)

  dbl_cols <- names(r)[vapply(r, is.double, logical(1L))]
  for (col in dbl_cols) {
    expect_equal(
      r[[col]],
      round(r[[col]], 2L),
      label = paste0(col, " rounded to 2 decimals")
    )
  }
})

test_that("get_means() decimals=NULL applies no rounding", {
  df <- make_survey_data(n = 200L, n_psu = 20L, n_strata = 4L, seed = 302L)
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  r_none <- get_means(d, y1, variance = "se", decimals = NULL)
  r_rounded <- get_means(d, y1, variance = "se", decimals = 0L)

  # Rounding to 0 places changes at least one value
  expect_false(identical(r_none$mean, r_rounded$mean))
})

test_that("get_means() rejects invalid decimals", {
  df <- make_survey_data(n = 100L, n_psu = 10L, n_strata = 2L, seed = 303L)
  d <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )

  expect_error(
    get_means(d, y1, decimals = -1L),
    class = "surveycore_error_invalid_decimals"
  )
})


# ── NA group rows (na.rm extension) — Test Blocks 1–8c + oracle ───────────────

# Block 1: default na.rm = TRUE excludes NA group rows (regression guard)

test_that("get_means() default (na.rm = TRUE) excludes group NA rows", {
  d <- make_na_group_design()
  r <- get_means(d, y1, group = grp)
  expect_false(anyNA(r$grp))
})

# Block 2: na.rm = FALSE includes NA group row

test_that("get_means() includes NA group row when na.rm = FALSE", {
  d <- make_na_group_design()
  r <- get_means(d, y1, group = grp, na.rm = FALSE)
  expect_true(any(is.na(r$grp)))
})

# Block 3: NA group row is last

test_that("get_means() places NA group row after non-NA rows", {
  d <- make_na_group_design()
  r <- get_means(d, y1, group = grp, na.rm = FALSE)
  na_idx <- which(is.na(r$grp))
  nn_idx <- which(!is.na(r$grp))
  expect_true(all(na_idx > max(nn_idx)))
})

# Block 4: NA group row has finite mean estimate

test_that("get_means() NA group row has finite mean estimate", {
  d <- make_na_group_design()
  r <- get_means(d, y1, group = grp, na.rm = FALSE)
  na_row <- get_na_group_rows(r, "grp")
  expect_true(all(is.finite(na_row$mean)))
})

# Block 5a: multi-group — NA in first group var

test_that("get_means() handles NA in first of two group vars (na.rm = FALSE)", {
  d <- make_na_group_design() # grp has NAs; grp2 has none
  r <- get_means(d, y1, group = c(grp, grp2), na.rm = FALSE)
  expect_true(any(is.na(r$grp) & !is.na(r$grp2)))
})

# Block 5b: multi-group — NA in second group var (inline fixture)

test_that("get_means() handles NA in second of two group vars (na.rm = FALSE)", {
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
  r <- get_means(d, y1, group = c(grp, grp2), na.rm = FALSE)
  expect_true(any(!is.na(r$grp) & is.na(r$grp2)))
})

# Block 6: all-NA group var — warning fires; output has NA group row; matches ungrouped

test_that("get_means() handles group var that is entirely NA (na.rm = FALSE)", {
  d <- make_all_na_group_design()
  expect_warning(
    r <- get_means(d, y1, group = grp, na.rm = FALSE),
    class = "surveycore_warning_single_level"
  )
  expect_equal(nrow(r), 1L)
  expect_true(is.na(r$grp[[1L]]))
  expect_true(is.finite(r$mean[[1L]]))
  ungrouped <- get_means(d, y1)
  expect_equal(r$mean, ungrouped$mean, tolerance = 1e-10)
})

# Block 7a: label_values = TRUE — regular NA group row remains NA in factor

test_that("get_means() regular NA group row is NA in factor when label_values = TRUE", {
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
  r <- get_means(d, y1, group = grp, na.rm = FALSE, label_values = TRUE)

  expect_true(is.factor(r$grp))
  na_row <- get_na_group_rows(r, "grp")
  expect_true(nrow(na_row) > 0L)
  expect_true(is.na(na_row$grp[[1L]]))
})

# Block 7b: label_values = TRUE — haven-tagged NA becomes a factor level

test_that("get_means() haven-labeled NA group rows become factor levels when label_values = TRUE", {
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
  r <- get_means(d, y1, group = grp, na.rm = FALSE, label_values = TRUE)

  expect_true(is.factor(r$grp))
  expect_true("Refused" %in% levels(r$grp))
  refused_row <- r[!is.na(r$grp) & r$grp == "Refused", ]
  expect_true(nrow(refused_row) > 0L)
})

# Block 8: group_by() path — NA group rows appear with na.rm = FALSE

test_that("get_means() includes NA group row when group set via group_by() and na.rm = FALSE", {
  skip_if_not_installed("surveytidy")
  d <- surveytidy::group_by(make_na_group_design(), grp)
  r <- get_means(d, y1, na.rm = FALSE)
  expect_true(anyNA(r$grp))
})

# Block 8b: group_by() path — NA group rows excluded by default

test_that("get_means() excludes NA group rows by default when group set via group_by()", {
  skip_if_not_installed("surveytidy")
  d <- surveytidy::group_by(make_na_group_design(), grp)
  r <- get_means(d, y1)
  expect_false(anyNA(r$grp))
})

# Block 8c: na.rm = NA is rejected (dual pattern)

test_that("get_means() rejects na.rm = NA with surveycore_error_na_rm_not_logical", {
  d <- make_na_group_design()
  expect_error(
    get_means(d, y1, group = grp, na.rm = NA),
    class = "surveycore_error_na_rm_not_logical"
  )
  expect_snapshot(
    error = TRUE,
    get_means(d, y1, group = grp, na.rm = NA)
  )
})


# ── Oracle tests: NA group row estimate matches filtered design ────────────────

test_that("get_means() NA group row mean matches filtered taylor design [oracle]", {
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
  expected <- get_means(na_design, y1, variance = "se")
  result <- get_means(
    design_oracle,
    y1,
    group = grp,
    na.rm = FALSE,
    variance = "se"
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$mean, expected$mean, tolerance = 1e-10)
  expect_equal(na_row$se, expected$se, tolerance = 1e-8)
  expect_equal(na_row$n, expected$n)
})

test_that("get_means() NA group row mean matches filtered replicate design [oracle]", {
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
  expected <- get_means(na_design_rep, y1, variance = "se")
  result <- get_means(
    design_rep,
    y1,
    group = grp,
    na.rm = FALSE,
    variance = "se"
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$mean, expected$mean, tolerance = 1e-10)
  expect_equal(na_row$se, expected$se, tolerance = 1e-8)
  expect_equal(na_row$n, expected$n)
})

test_that("get_means() NA group row mean matches filtered twophase design [oracle]", {
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
  expected <- suppressWarnings(get_means(
    na_design_twophase,
    y1,
    variance = "se"
  ))
  result <- suppressWarnings(
    get_means(design_twophase, y1, group = grp, na.rm = FALSE, variance = "se")
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$mean, expected$mean, tolerance = 1e-10)
  # SE legitimately differs: two-phase variance correction depends on total
  # sample structure; domain estimation (full design) != pre-filtered oracle.
  expect_true(all(is.finite(na_row$se)))
  expect_equal(na_row$n, expected$n)
})

test_that("get_means() NA group row mean matches filtered calibrated design [oracle]", {
  df_c <- make_survey_data(n = 100L, n_psu = 10L, n_strata = 2L, seed = 42L)
  set.seed(43L)
  df_c$grp <- sample(c("A", "B", NA_character_), 100L, replace = TRUE)
  design_cal <- as_survey_nonprob(df_c, weights = wt)
  na_df_c <- df_c[is.na(df_c$grp), ]
  na_design_cal <- as_survey_nonprob(na_df_c, weights = wt)
  expected <- get_means(na_design_cal, y1, variance = "se")
  result <- get_means(
    design_cal,
    y1,
    group = grp,
    na.rm = FALSE,
    variance = "se"
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$mean, expected$mean, tolerance = 1e-10)
  # SE legitimately differs: calibrated variance depends on total sample size;
  # domain estimation (full design) != pre-filtered oracle.
  expect_true(all(is.finite(na_row$se)))
  expect_equal(na_row$n, expected$n)
})

test_that("get_means() NA group row mean matches filtered SRS design [oracle]", {
  df_s <- make_survey_data(n = 100L, n_psu = 10L, n_strata = 2L, seed = 42L)
  set.seed(43L)
  df_s$grp <- sample(c("A", "B", NA_character_), 100L, replace = TRUE)
  design_srs <- as_survey(df_s, weights = wt)
  na_df_s <- df_s[is.na(df_s$grp), ]
  na_design_srs <- as_survey(na_df_s, weights = wt)
  expected <- get_means(na_design_srs, y1, variance = "se")
  result <- get_means(
    design_srs,
    y1,
    group = grp,
    na.rm = FALSE,
    variance = "se"
  )
  na_row <- get_na_group_rows(result, "grp")
  expect_equal(na_row$mean, expected$mean, tolerance = 1e-10)
  # SE legitimately differs: HT variance depends on total sample size;
  # domain estimation (full design, n=100) != pre-filtered oracle (n=~30).
  expect_true(all(is.finite(na_row$se)))
  expect_equal(na_row$n, expected$n)
})

test_that("get_means() multi-group NA row mean matches filtered taylor design [oracle]", {
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
    get_means(
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
  expected <- suppressWarnings(get_means(oracle_design, y1, variance = "se"))
  na_x_rows <- result[is.na(result$grp) & result$grp2 == "X", ]
  expect_equal(na_x_rows$mean, expected$mean, tolerance = 1e-10)
  # SE legitimately differs: multi-group oracle cells are small;
  # domain estimation uses full cluster/strata structure, oracle uses subset.
  expect_true(all(is.finite(na_x_rows$se)))
  expect_equal(na_x_rows$n, expected$n)
})

# ---------------------------------------------------------------------------
# Additional coverage: twophase, empty domains, SRS edge cases
# ---------------------------------------------------------------------------

test_that("get_means() works for survey_twophase design (covers .twophase_mean_cell())", {
  d <- make_survey_data(
    n = 100,
    n_psu = 10,
    n_strata = 2,
    design = "twophase",
    seed = 500
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
  result <- get_means(sc, y1, variance = "se")
  test_result_invariants(result, "survey_means")
  expect_true(is.finite(result$mean[[1L]]))
  expect_gte(result$se[[1L]], 0)
})

test_that("get_means() taylor: empty domain returns NA (covers n_d=0 branch)", {
  df <- make_survey_data(n = 60, n_psu = 10, n_strata = 2, seed = 501)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- FALSE
  result <- get_means(sc, y1, variance = "se")
  expect_true(all(is.na(result$mean)))
})

test_that("get_means() twophase: empty domain returns NA (covers .twophase_mean_cell() n_d=0)", {
  d <- make_survey_data(
    n = 80,
    n_psu = 10,
    n_strata = 2,
    design = "twophase",
    seed = 502
  )
  phase1 <- as_survey(d, ids = psu, weights = wt, strata = strata, nest = TRUE)
  sc <- as_survey_twophase(
    phase1,
    subset = subset,
    ids2 = psu,
    strata2 = strata
  )
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- FALSE
  result <- get_means(sc, y1, variance = "se")
  expect_true(all(is.na(result$mean)))
})

test_that("get_means() SRS design n_d=1 domain returns se = 0 (Taylor: centered influence is zero)", {
  set.seed(503)
  n <- 30L
  df <- data.frame(y = rnorm(n), w = rep(1, n))
  sc <- as_survey(df, weights = w)
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- seq_len(n) == 1L
  result <- get_means(sc, y, variance = "se")
  expect_equal(result$n[[1L]], 1L)
  # With n_d=1 the single observation equals the domain mean, so the centered

  # influence function is identically 0 → SE = 0 (matches survey::svymean)
  expect_equal(result$se[[1L]], 0)
})

test_that("get_means() replicate: empty domain returns NA (covers n_d=0 in .replicate_mean_cell())", {
  d <- make_survey_data(
    n = 60,
    n_psu = 10,
    n_strata = 2,
    design = "replicate",
    type = "brr",
    seed = 504
  )
  repwt_cols <- grep("^repwt_", names(d), value = TRUE)
  sc <- as_survey_replicate(
    d,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- FALSE
  result <- get_means(sc, y1, variance = "se")
  expect_true(all(is.na(result$mean)))
})

test_that("get_means() taylor with FPC fraction covers FPC fraction path in .taylor_mean_cell()", {
  set.seed(505)
  df <- make_survey_data(n = 100, n_psu = 10, n_strata = 2, seed = 505)
  df$fpc_frac <- df$fpc / (df$fpc * 5) # fractions in (0, 0.2]
  sc <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc_frac,
    nest = TRUE
  )
  result <- get_means(sc, y1, variance = "se")
  test_result_invariants(result, "survey_means")
  expect_true(is.finite(result$mean[[1L]]))
})

# ---------------------------------------------------------------------------
# Additional coverage: calibrated design, se_srs=0 paths, unsupported class
# ---------------------------------------------------------------------------

test_that("get_means() works for survey_nonprob design", {
  set.seed(601)
  df <- data.frame(y = rnorm(60), w = runif(60, 0.5, 2))
  sc <- as_survey_nonprob(df, weights = w)
  result <- get_means(sc, y, variance = "se")
  test_result_invariants(result, "survey_means")
  expect_true(is.finite(result$mean[[1L]]))
  expect_true(is.finite(result$se[[1L]]))
})

test_that("get_means() calibrated empty domain returns NA (covers .calibrated_mean_cell() n_d=0 path)", {
  set.seed(602)
  df <- data.frame(y = rnorm(20), w = runif(20, 0.5, 2))
  sc <- as_survey_nonprob(df, weights = w)
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- rep(FALSE, 20L)
  result <- get_means(sc, y, variance = "se")
  expect_true(is.na(result$mean[[1L]]))
})

test_that("get_means() calibrated single-row domain returns mean with NA se (covers n_d=1 path)", {
  set.seed(603)
  df <- data.frame(y = rnorm(20), w = runif(20, 0.5, 2))
  sc <- as_survey_nonprob(df, weights = w)
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- seq_len(20L) == 1L
  result <- get_means(sc, y, variance = "se")
  expect_true(is.finite(result$mean[[1L]]))
  expect_true(is.na(result$se[[1L]]))
})

test_that("get_means() taylor single-row domain hits se_srs=0 branch in .taylor_mean_cell()", {
  set.seed(604)
  df <- make_survey_data(n = 50, n_psu = 10, n_strata = 2, seed = 604)
  sc <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- seq_len(50L) == 1L
  result <- get_means(sc, y1, variance = "se")
  expect_true(is.finite(result$mean[[1L]]) || is.na(result$mean[[1L]]))
})

test_that("get_means() replicate single-row domain hits se_srs=0 branch in .replicate_mean_cell()", {
  d <- make_survey_data(
    n = 60,
    n_psu = 10,
    n_strata = 2,
    design = "replicate",
    type = "brr",
    seed = 605
  )
  repwt_cols <- grep("^repwt_", names(d), value = TRUE)
  sc <- as_survey_replicate(
    d,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )
  sc@data[[surveycore::SURVEYCORE_DOMAIN_COL]] <- seq_len(60L) == 1L
  result <- get_means(sc, y1, variance = "se")
  expect_true(is.finite(result$mean[[1L]]) || is.na(result$mean[[1L]]))
})

test_that(".mean_cell() errors for unsupported design class", {
  df <- data.frame(y = 1:5, wt = rep(1, 5))
  expect_error(
    surveycore:::.mean_cell(list(fake = TRUE), "y", rep(1, 5)),
    class = "surveycore_error_unsupported_class"
  )
})
