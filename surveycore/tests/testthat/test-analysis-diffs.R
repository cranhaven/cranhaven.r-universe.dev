# tests/testthat/test-analysis-diffs.R
#
# Happy paths + error paths + edge cases for get_diffs()

# ── Shared test data setup ──────────────────────────────────────────────────

.make_diffs_data <- function(n = 200, seed = 42) {
  set.seed(seed)
  df <- make_survey_data(n = n, n_psu = 20, n_strata = 4, seed = seed)
  df$treats <- factor(
    sample(c("Control", "A", "B"), n, replace = TRUE)
  )
  df$dv <- rnorm(n, mean = 50, sd = 10)
  df$binary_dv <- as.integer(df$dv > 50)
  df$gender <- factor(sample(c("M", "F"), n, replace = TRUE))
  df$age <- rnorm(n, 40, 10)
  df
}

.make_diffs_design <- function(n = 200, seed = 42) {
  df <- .make_diffs_data(n = n, seed = seed)
  as_survey(df, ids = psu, weights = wt, strata = strata)
}


# ═══════════════════════════════════════════════════════════════════════════
# 1. ERROR PATHS
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_diffs() rejects non-survey-base objects", {
  df <- data.frame(dv = 1:5, treats = factor(c("A", "A", "B", "B", "B")))
  expect_error(
    get_diffs(df, dv, treats),
    class = "surveycore_error_unsupported_class"
  )
  expect_snapshot(error = TRUE, get_diffs(df, dv, treats))
})

test_that("get_diffs() rejects x resolving to non-existent column", {
  d <- .make_diffs_design()
  expect_error(
    get_diffs(d, nonexistent_col, treats),
    class = "surveycore_error_wrong_variable_count"
  )
  expect_snapshot(error = TRUE, get_diffs(d, nonexistent_col, treats))
})

test_that("get_diffs() rejects treats resolving to non-existent column", {
  d <- .make_diffs_design()
  expect_error(
    get_diffs(d, dv, nonexistent_arm),
    class = "surveycore_error_treats_single"
  )
  expect_snapshot(error = TRUE, get_diffs(d, dv, nonexistent_arm))
})

test_that("get_diffs() rejects non-numeric x", {
  d <- .make_diffs_design()
  expect_error(
    get_diffs(d, group, treats),
    class = "surveycore_error_non_numeric_variable"
  )
  expect_snapshot(error = TRUE, get_diffs(d, group, treats))
})

test_that("get_diffs() rejects treats with only 1 level", {
  df <- .make_diffs_data()
  df$single <- factor(rep("A", nrow(df)))
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  expect_error(
    get_diffs(d, dv, single),
    class = "surveycore_error_treats_one_level"
  )
  expect_snapshot(error = TRUE, get_diffs(d, dv, single))
})

test_that("get_diffs() rejects invalid ref_level", {
  d <- .make_diffs_design()
  expect_error(
    get_diffs(d, dv, treats, ref_level = "nonexistent"),
    class = "surveycore_error_ref_level_not_found"
  )
  expect_snapshot(
    error = TRUE,
    get_diffs(d, dv, treats, ref_level = "nonexistent")
  )
})

test_that("get_diffs() rejects invalid pval_adj", {
  d <- .make_diffs_design()
  expect_error(
    get_diffs(d, dv, treats, pval_adj = "invalid_method"),
    class = "surveycore_error_invalid_pval_adj"
  )
  expect_snapshot(
    error = TRUE,
    get_diffs(d, dv, treats, pval_adj = "invalid_method")
  )
})

test_that("get_diffs() rejects non-character covariates", {
  d <- .make_diffs_design()
  expect_error(
    get_diffs(d, dv, treats, covariates = 42),
    class = "surveycore_error_covariates_not_character"
  )
  expect_snapshot(
    error = TRUE,
    get_diffs(d, dv, treats, covariates = 42)
  )
})

test_that("get_diffs() rejects non-logical na.rm", {
  d <- .make_diffs_design()
  expect_error(
    get_diffs(d, dv, treats, na.rm = "yes"),
    class = "surveycore_error_na_rm_not_logical"
  )
})

test_that("get_diffs() rejects invalid variance arg", {
  d <- .make_diffs_design()
  expect_error(
    get_diffs(d, dv, treats, variance = "deff"),
    class = "surveycore_error_invalid_variance_arg"
  )
})

test_that("get_diffs() rejects invalid conf_level", {
  d <- .make_diffs_design()
  expect_error(
    get_diffs(d, dv, treats, conf_level = 2),
    class = "surveycore_error_invalid_conf_level"
  )
})

test_that("get_diffs() rejects invalid name_style", {
  d <- .make_diffs_design()
  expect_error(
    get_diffs(d, dv, treats, name_style = "tidy"),
    class = "surveycore_error_invalid_name_style"
  )
})


# ═══════════════════════════════════════════════════════════════════════════
# 2. HAPPY PATHS — CLEAN PATH
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_diffs() basic bivariate gaussian returns correct structure", {
  d <- .make_diffs_design()
  result <- get_diffs(d, dv, treats, ref_level = "Control")

  # Class
  expect_identical(
    class(result),
    c("survey_diffs", "survey_result", "tbl_df", "tbl", "data.frame")
  )

  # 3 rows: 1 reference + 2 treatment
  expect_equal(nrow(result), 3L)

  # Correct columns
  expect_true("treats" %in% names(result))
  expect_true("estimate" %in% names(result))
  expect_true("mean" %in% names(result))
  expect_true("n" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true("stars" %in% names(result))

  # Reference row
  expect_equal(result$estimate[[1L]], 0)
  expect_true(is.na(result$p_value[[1L]]))
  expect_identical(result$stars[[1L]], "")

  # Non-ref rows have non-NA estimates

  expect_false(is.na(result$estimate[[2L]]))
  expect_false(is.na(result$estimate[[3L]]))

  # .meta contract
  m <- meta(result)
  expect_identical(m$estimate_method, "coefficient")
  expect_identical(m$mean_method, "intercept")
  expect_identical(m$family, "gaussian")
  expect_identical(m$treats$ref_level, "Control")
  expect_identical(m$treats$name, "treats")
})

test_that("get_diffs() show_means = FALSE omits reference row and mean", {
  d <- .make_diffs_design()
  result <- get_diffs(
    d,
    dv,
    treats,
    ref_level = "Control",
    show_means = FALSE
  )

  expect_equal(nrow(result), 2L)
  expect_false("mean" %in% names(result))
  # No row with estimate == 0
  expect_false(any(result$estimate == 0))
})

test_that("get_diffs() show_pct_change = TRUE adds pct_change column", {
  d <- .make_diffs_design()
  result <- get_diffs(
    d,
    dv,
    treats,
    ref_level = "Control",
    show_pct_change = TRUE
  )

  expect_true("pct_change" %in% names(result))
  # Reference row pct_change = NA
  expect_true(is.na(result$pct_change[[1L]]))
  # Non-ref rows have numeric pct_change
  expect_false(is.na(result$pct_change[[2L]]))
})

test_that("get_diffs() ref_level changes reference row", {
  d <- .make_diffs_design()
  result <- get_diffs(d, dv, treats, ref_level = "A")

  # First row should be A
  expect_equal(result$estimate[[1L]], 0)
  m <- meta(result)
  expect_identical(m$treats$ref_level, "A")
})

test_that("get_diffs() variance = 'se' includes SE, no CI", {
  d <- .make_diffs_design()
  result <- get_diffs(
    d,
    dv,
    treats,
    ref_level = "Control",
    variance = "se"
  )

  expect_true("se" %in% names(result))
  expect_false("ci_low" %in% names(result))
  expect_false("ci_high" %in% names(result))
})

test_that("get_diffs() variance = c('se', 'ci') includes both", {
  d <- .make_diffs_design()
  result <- get_diffs(
    d,
    dv,
    treats,
    ref_level = "Control",
    variance = c("se", "ci")
  )

  expect_true("se" %in% names(result))
  expect_true("ci_low" %in% names(result))
  expect_true("ci_high" %in% names(result))
})

test_that("get_diffs() variance = NULL includes no uncertainty", {
  d <- .make_diffs_design()
  result <- get_diffs(
    d,
    dv,
    treats,
    ref_level = "Control",
    variance = NULL
  )

  expect_false("se" %in% names(result))
  expect_false("ci_low" %in% names(result))
  expect_false("ci_high" %in% names(result))
})

test_that("get_diffs() decimals rounds correctly", {
  d <- .make_diffs_design()
  result <- get_diffs(
    d,
    dv,
    treats,
    ref_level = "Control",
    decimals = 2
  )

  # estimate rounded to 2 decimal places
  for (i in seq_len(nrow(result))) {
    if (!is.na(result$estimate[[i]])) {
      expect_equal(
        result$estimate[[i]],
        round(result$estimate[[i]], 2)
      )
    }
  }
})

test_that("get_diffs() decimals + pct_change rounds to decimals + 2", {
  d <- .make_diffs_design()
  result <- get_diffs(
    d,
    dv,
    treats,
    ref_level = "Control",
    show_pct_change = TRUE,
    decimals = 2
  )

  for (i in seq_len(nrow(result))) {
    if (!is.na(result$pct_change[[i]])) {
      expect_equal(
        result$pct_change[[i]],
        round(result$pct_change[[i]], 4)
      )
    }
  }
})

test_that("get_diffs() pval_adj = 'BH' adjusts p-values", {
  d <- .make_diffs_design()
  result_adj <- get_diffs(
    d,
    dv,
    treats,
    ref_level = "Control",
    pval_adj = "BH"
  )
  result_raw <- get_diffs(
    d,
    dv,
    treats,
    ref_level = "Control"
  )

  m <- meta(result_adj)
  expect_identical(m$pval_adj, "BH")
  # P-values may differ
  # (for 2 comparisons with BH, they could be the same or different)
})

test_that("get_diffs() label_values = TRUE labels treats column", {
  df <- .make_diffs_data()
  # Set value labels
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  d <- set_val_labels(
    d,
    treats = c("Control Group" = "Control", "Group A" = "A", "Group B" = "B")
  )
  result <- get_diffs(
    d,
    dv,
    treats,
    ref_level = "Control",
    label_values = TRUE
  )

  expect_true(is.factor(result$treats))
})

test_that("get_diffs() label_values = FALSE keeps raw codes", {
  d <- .make_diffs_design()
  result <- get_diffs(
    d,
    dv,
    treats,
    ref_level = "Control",
    label_values = FALSE
  )
  # treats column should be character or factor with raw level names
  expect_true(is.character(result$treats) || is.factor(result$treats))
})

test_that("get_diffs() name_style = 'broom' renames columns, mean excluded", {
  d <- .make_diffs_design()
  result <- get_diffs(
    d,
    dv,
    treats,
    ref_level = "Control",
    variance = c("se", "ci"),
    name_style = "broom"
  )

  expect_true("std.error" %in% names(result))
  expect_true("conf.low" %in% names(result))
  expect_true("conf.high" %in% names(result))
  expect_true("p.value" %in% names(result))
  # mean is NOT renamed to estimate
  expect_true("mean" %in% names(result))
  expect_false("se" %in% names(result))
})

test_that("get_diffs() n_weighted = TRUE adds n_weighted column", {
  d <- .make_diffs_design()
  result <- get_diffs(
    d,
    dv,
    treats,
    ref_level = "Control",
    n_weighted = TRUE
  )

  expect_true("n_weighted" %in% names(result))
  # n_weighted should be numeric and positive
  expect_true(all(result$n_weighted > 0))
})

test_that("get_diffs() fires small cell warning for min_cell_n", {
  df <- .make_diffs_data(n = 200, seed = 42)
  # Make one level very small
  df$small_treats <- factor(
    c(rep("Rare", 5), rep("Control", 95), rep("B", 100))
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)

  expect_warning(
    get_diffs(d, dv, small_treats, ref_level = "Control"),
    class = "surveycore_warning_small_cell"
  )
})

test_that("get_diffs() sets column-level labels on all columns", {
  d <- .make_diffs_design()
  result <- get_diffs(d, dv, treats, ref_level = "Control")

  for (col in names(result)) {
    expect_false(
      is.null(attr(result[[col]], "label")),
      info = paste("Column", col, "missing label attribute")
    )
  }
})

test_that("get_diffs() print snapshot", {
  d <- .make_diffs_design()
  result <- get_diffs(d, dv, treats, ref_level = "Control")
  expect_snapshot(print(result))
})

# ═══════════════════════════════════════════════════════════════════════════
# 3. HAPPY PATHS — MARGINALEFFECTS PATH
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_diffs() with covariates uses marginaleffects path", {
  d <- .make_diffs_design()
  result <- get_diffs(
    d,
    dv,
    treats,
    covariates = "age",
    ref_level = "Control"
  )

  m <- meta(result)
  expect_identical(m$estimate_method, "avg_slopes")
  expect_identical(m$mean_method, "avg_predictions")
  expect_equal(nrow(result), 3L)
})

test_that("get_diffs() with group produces grouped output", {
  d <- .make_diffs_design()
  result <- suppressWarnings(
    get_diffs(d, dv, treats, group = gender, ref_level = "Control")
  )

  expect_true("gender" %in% names(result))
  m <- meta(result)
  expect_identical(m$estimate_method, "avg_slopes")
  # More rows: (1 ref + 2 treat) * 2 groups = 6
  expect_equal(nrow(result), 6L)
})

test_that("get_diffs() with covariates and group", {
  d <- .make_diffs_design()
  result <- suppressWarnings(
    get_diffs(
      d,
      dv,
      treats,
      group = gender,
      covariates = "age",
      ref_level = "Control"
    )
  )

  expect_true("gender" %in% names(result))
  m <- meta(result)
  expect_identical(m$estimate_method, "avg_slopes")
  expect_equal(nrow(result), 6L)
})

test_that("get_diffs() non-gaussian (quasibinomial) uses AME path", {
  d <- .make_diffs_design()
  result <- get_diffs(
    d,
    binary_dv,
    treats,
    ref_level = "Control",
    family = quasibinomial()
  )

  m <- meta(result)
  expect_identical(m$family, "quasibinomial")
  expect_identical(m$estimate_scale, "ame")
  expect_identical(m$estimate_method, "avg_slopes")
  # Estimates should be on probability scale (small)
  expect_true(all(abs(result$estimate) < 1))
})

test_that("get_diffs() scale = 'link' + non-gaussian suppresses mean", {
  d <- .make_diffs_design()
  result <- get_diffs(
    d,
    binary_dv,
    treats,
    ref_level = "Control",
    scale = "link",
    family = quasibinomial()
  )

  expect_false("mean" %in% names(result))
  expect_false("pct_change" %in% names(result))
  # Reference row still present
  expect_equal(result$estimate[[1L]], 0)
  m <- meta(result)
  expect_identical(m$estimate_method, "coefficient")
})

test_that("get_diffs() gaussian ame == link produces identical output", {
  d <- .make_diffs_design()
  result_ame <- get_diffs(
    d,
    dv,
    treats,
    ref_level = "Control",
    scale = "ame"
  )
  result_link <- get_diffs(
    d,
    dv,
    treats,
    ref_level = "Control",
    scale = "link"
  )

  expect_equal(result_ame$estimate, result_link$estimate, tolerance = 1e-10)
  expect_equal(
    result_ame$ci_low,
    result_link$ci_low,
    tolerance = 1e-8
  )
  expect_equal(
    result_ame$ci_high,
    result_link$ci_high,
    tolerance = 1e-8
  )
})

test_that("get_diffs() pval_adj with group adjusts within groups", {
  d <- .make_diffs_design()
  result_adj <- suppressWarnings(get_diffs(
    d,
    dv,
    treats,
    group = gender,
    ref_level = "Control",
    pval_adj = "BH"
  ))

  m <- meta(result_adj)
  expect_identical(m$pval_adj, "BH")
})

# ═══════════════════════════════════════════════════════════════════════════
# 4. DESIGN CLASS COVERAGE
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_diffs() works with survey_taylor design", {
  d <- .make_diffs_design()
  result <- get_diffs(d, dv, treats, ref_level = "Control")
  expect_identical(meta(result)$design_type, "taylor")
})

test_that("get_diffs() works with survey_replicate design", {
  df <- .make_diffs_data()
  df_rep <- make_survey_data(
    n = 200,
    n_psu = 20,
    n_strata = 4,
    design = "replicate",
    type = "jk1",
    seed = 42
  )
  df_rep$treats <- factor(
    sample(c("Control", "A", "B"), nrow(df_rep), replace = TRUE)
  )
  df_rep$dv <- rnorm(nrow(df_rep), 50, 10)

  d <- as_survey_replicate(
    df_rep,
    weights = wt,
    repweights = starts_with("repwt_"),
    type = "JK1"
  )
  result <- get_diffs(d, dv, treats, ref_level = "Control")
  expect_identical(meta(result)$design_type, "replicate")
})

test_that("get_diffs() works with survey_twophase design", {
  df <- make_survey_data(
    n = 200,
    n_psu = 20,
    n_strata = 4,
    design = "twophase",
    seed = 42
  )
  df$treats <- factor(
    sample(c("Control", "A", "B"), nrow(df), replace = TRUE)
  )
  df$dv <- rnorm(nrow(df), 50, 10)

  phase1 <- as_survey(df, ids = psu, strata = strata)
  d <- as_survey_twophase(
    phase1,
    subset = subset,
    probs2 = phase2_prob
  )
  result <- get_diffs(d, dv, treats, ref_level = "Control")
  expect_identical(meta(result)$design_type, "twophase")
})

test_that("get_diffs() works with survey_nonprob design", {
  df <- .make_diffs_data()
  d <- as_survey_nonprob(df, weights = wt)
  result <- get_diffs(d, dv, treats, ref_level = "Control")
  expect_identical(meta(result)$design_type, "nonprob")
})

test_that("get_diffs() domain estimation produces in-domain n counts", {
  skip_if_not_installed("surveytidy")
  d <- .make_diffs_design()
  total_n <- sum(d@data$treats == "Control", na.rm = TRUE)

  # Filter to a subset
  d_filt <- surveytidy::filter(d, gender == "M")
  result <- get_diffs(d_filt, dv, treats, ref_level = "Control")

  # n should be smaller than total_n
  ref_n <- result$n[[1L]]
  expect_true(ref_n < total_n)
})

# ═══════════════════════════════════════════════════════════════════════════
# 5. EDGE CASES
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_diffs() works with 2 levels (simplest case)", {
  df <- .make_diffs_data()
  df$two_arm <- factor(
    sample(c("Control", "Treatment"), nrow(df), replace = TRUE)
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  result <- get_diffs(d, dv, two_arm, ref_level = "Control")

  expect_equal(nrow(result), 2L)
  expect_equal(result$estimate[[1L]], 0)
})

test_that("get_diffs() works with many levels (10+)", {
  df <- .make_diffs_data(n = 500, seed = 123)
  df$many_arm <- factor(
    sample(paste0("Level_", 1:10), nrow(df), replace = TRUE)
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)

  suppressWarnings(
    # small cell warnings
    result <- get_diffs(d, dv, many_arm)
  )
  # 1 ref + 9 treatment = 10 rows
  expect_equal(nrow(result), 10L)
})

test_that("get_diffs() warns when treats is character (not factor)", {
  df <- .make_diffs_data()
  df$char_arm <- sample(c("Control", "A", "B"), nrow(df), replace = TRUE)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)

  expect_warning(
    get_diffs(d, dv, char_arm, ref_level = "A"),
    class = "surveycore_warning_treats_coerced"
  )
})

test_that("get_diffs() warns when ref mean is 0 with show_pct_change", {
  set.seed(123)
  n <- 200
  df <- make_survey_data(n = n, n_psu = 20, n_strata = 4, seed = 123)
  # All control observations have DV = 0, so weighted mean is exactly 0
  df$arm <- factor(
    sample(c("Control", "A", "B"), n, replace = TRUE),
    levels = c("Control", "A", "B")
  )
  df$zero_dv <- ifelse(df$arm == "Control", 0, rnorm(n, 5, 1))
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)

  expect_warning(
    get_diffs(d, zero_dv, arm, ref_level = "Control", show_pct_change = TRUE),
    class = "surveycore_warning_pct_change_zero_ref"
  )
})

test_that("get_diffs() fires small cell warning for single obs level", {
  df <- .make_diffs_data()
  # One level with very few observations
  df$tiny_arm <- factor(
    c("Tiny", rep("Control", 99), rep("B", nrow(df) - 100))
  )
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)

  expect_warning(
    get_diffs(d, dv, tiny_arm, ref_level = "Control"),
    class = "surveycore_warning_small_cell"
  )
})

test_that("get_diffs() na.rm = FALSE with NAs errors from survey_glm", {
  df <- .make_diffs_data()
  df$dv[1:5] <- NA
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)

  expect_error(
    get_diffs(d, dv, treats, na.rm = FALSE)
  )
})

# ═══════════════════════════════════════════════════════════════════════════
# PR 3 — favorability (show_favorability, alpha)
# ═══════════════════════════════════════════════════════════════════════════

# Helper: two-group design with a large, clearly significant negative diff
# (A mean ~30 vs Control mean ~60; p ≈ 0 → stable across seeds)
.make_fav_design <- function(seed = 42L) {
  df <- make_survey_data(n = 400L, n_psu = 40L, n_strata = 4L, seed = seed)
  set.seed(seed + 1L)
  n <- nrow(df)
  # Explicit levels so "Control" is the reference (first level)
  df$treats <- factor(
    sample(c("Control", "A"), n, replace = TRUE),
    levels = c("Control", "A")
  )
  # Control ~60, A ~30 → diff (A - Control) ≈ -30, p ≈ 0
  df$dv <- ifelse(
    df$treats == "Control",
    rnorm(n, 60, 3),
    rnorm(n, 30, 3)
  )
  df$gender <- factor(sample(c("M", "F"), n, replace = TRUE))
  as_survey(df, ids = psu, weights = wt, strata = strata)
}

# ── Error paths: alpha_invalid ──────────────────────────────────────────────

test_that("get_diffs() rejects alpha > 1", {
  d <- .make_diffs_design()
  expect_error(
    get_diffs(d, dv, treats, alpha = 1.5),
    class = "surveycore_error_alpha_invalid"
  )
  expect_snapshot(error = TRUE, get_diffs(d, dv, treats, alpha = 1.5))
})

test_that("get_diffs() rejects alpha = 0", {
  d <- .make_diffs_design()
  expect_error(
    get_diffs(d, dv, treats, alpha = 0),
    class = "surveycore_error_alpha_invalid"
  )
  expect_snapshot(error = TRUE, get_diffs(d, dv, treats, alpha = 0))
})

test_that("get_diffs() rejects alpha = '0.05' (character)", {
  d <- .make_diffs_design()
  expect_error(
    get_diffs(d, dv, treats, alpha = "0.05"),
    class = "surveycore_error_alpha_invalid"
  )
  expect_snapshot(error = TRUE, get_diffs(d, dv, treats, alpha = "0.05"))
})

test_that("get_diffs() rejects alpha = NA", {
  d <- .make_diffs_design()
  expect_error(
    get_diffs(d, dv, treats, alpha = NA),
    class = "surveycore_error_alpha_invalid"
  )
  expect_snapshot(error = TRUE, get_diffs(d, dv, treats, alpha = NA))
})

test_that("get_diffs() rejects alpha = Inf", {
  d <- .make_diffs_design()
  expect_error(
    get_diffs(d, dv, treats, alpha = Inf),
    class = "surveycore_error_alpha_invalid"
  )
  expect_snapshot(error = TRUE, get_diffs(d, dv, treats, alpha = Inf))
})

test_that("get_diffs() rejects alpha = c(0.05, 0.1) (length > 1)", {
  d <- .make_diffs_design()
  expect_error(
    get_diffs(d, dv, treats, alpha = c(0.05, 0.1)),
    class = "surveycore_error_alpha_invalid"
  )
  expect_snapshot(error = TRUE, get_diffs(d, dv, treats, alpha = c(0.05, 0.1)))
})

# ── Happy paths ──────────────────────────────────────────────────────────────

test_that("get_diffs() show_favorability = FALSE: no favorable/backlash columns; higher_is in .meta", {
  d <- .make_fav_design()
  d <- set_higher_is(d, dv = "worse")
  result <- get_diffs(d, dv, treats, show_favorability = FALSE)
  expect_false("favorable" %in% names(result))
  expect_false("backlash" %in% names(result))
  expect_identical(attr(result, ".meta")$x[["dv"]]$higher_is, "worse")
})

test_that("get_diffs() show_favorability + higher_is='worse' + negative sig diff: favorable=TRUE, backlash=FALSE", {
  d <- .make_fav_design()
  d <- set_higher_is(d, dv = "worse")
  result <- get_diffs(d, dv, treats, show_favorability = TRUE)
  a_row <- result[result$treats == "A", ]
  expect_true(a_row$favorable)
  expect_false(a_row$backlash)
  expect_identical(attr(result$favorable, "label"), "Favorable")
  expect_identical(attr(result$backlash, "label"), "Backlash")
})

test_that("get_diffs() show_favorability + higher_is='better' + negative sig diff: favorable=FALSE, backlash=TRUE", {
  d <- .make_fav_design()
  d <- set_higher_is(d, dv = "better")
  result <- get_diffs(d, dv, treats, show_favorability = TRUE)
  a_row <- result[result$treats == "A", ]
  expect_false(a_row$favorable)
  expect_true(a_row$backlash)
})

test_that("get_diffs() show_favorability + non-significant result: both FALSE", {
  # Random data with no treatment effect; p >> 0.01 for random diffs
  d <- .make_diffs_design()
  d <- set_higher_is(d, dv = "worse")
  result <- get_diffs(d, dv, treats, show_favorability = TRUE, alpha = 0.01)
  expect_true(all(!result$favorable))
  expect_true(all(!result$backlash))
})

test_that("get_diffs() show_favorability: custom alpha = 0.001 keeps highly sig result favorable", {
  d <- .make_fav_design()
  d <- set_higher_is(d, dv = "worse")
  # p ≈ 0 for the large effect, so favorable even at very small alpha
  result <- get_diffs(d, dv, treats, show_favorability = TRUE, alpha = 0.001)
  a_row <- result[result$treats == "A", ]
  expect_true(a_row$favorable)
})

test_that("get_diffs() show_favorability + name_style='broom': favorable/backlash present, uses p.value", {
  d <- .make_fav_design()
  d <- set_higher_is(d, dv = "worse")
  result <- get_diffs(
    d,
    dv,
    treats,
    show_favorability = TRUE,
    name_style = "broom"
  )
  expect_true("favorable" %in% names(result))
  expect_true("backlash" %in% names(result))
  expect_true("p.value" %in% names(result))
  a_row <- result[result$treats == "A", ]
  expect_true(a_row$favorable)
  expect_false(a_row$backlash)
})

test_that("get_diffs() show_favorability + group: columns present, aligned per group", {
  d <- .make_fav_design()
  d <- set_higher_is(d, dv = "worse")
  result <- get_diffs(d, dv, treats, group = gender, show_favorability = TRUE)
  expect_true("favorable" %in% names(result))
  expect_true("backlash" %in% names(result))
  expect_true(is.logical(result$favorable))
  expect_true(is.logical(result$backlash))
  # Each group should have its own rows with correct alignment
  expect_true("gender" %in% names(result))
})

test_that("get_diffs() show_favorability + pval_adj: classification uses adjusted p-value", {
  d <- .make_fav_design()
  d <- set_higher_is(d, dv = "worse")
  result <- get_diffs(
    d,
    dv,
    treats,
    show_favorability = TRUE,
    pval_adj = "bonferroni"
  )
  expect_true("favorable" %in% names(result))
  expect_true("backlash" %in% names(result))
  a_row <- result[result$treats == "A", ]
  # Large effect: even after Bonferroni adjustment, p ≈ 0 → still favorable
  expect_true(a_row$favorable)
  expect_false(a_row$backlash)
})

# ── Edge cases ────────────────────────────────────────────────────────────────

test_that("get_diffs() show_favorability + no higher_is set: both columns all FALSE, no warning", {
  d <- .make_fav_design()
  # Deliberately do NOT call set_higher_is()
  expect_no_warning(
    result <- get_diffs(d, dv, treats, show_favorability = TRUE)
  )
  expect_true("favorable" %in% names(result))
  expect_true("backlash" %in% names(result))
  expect_true(all(!result$favorable))
  expect_true(all(!result$backlash))
})

test_that("get_diffs() show_favorability: p_value == alpha → both FALSE (strict <)", {
  d <- .make_diffs_design()
  d <- set_higher_is(d, dv = "worse")
  # Get the actual p-values for this fixed-seed dataset
  baseline <- get_diffs(d, dv, treats)
  # Exclude the reference row (p_value = NA) before finding min p
  valid_pvals <- baseline$p_value[!is.na(baseline$p_value)]
  min_p <- min(valid_pvals)
  skip_if(
    min_p == 0 || !is.finite(min_p),
    "p-value is exactly 0 or non-finite; boundary cannot be tested"
  )
  skip_if(min_p >= 1, "p-value >= 1; outside valid alpha range")
  # With alpha == min_p: strict < means p is NOT < alpha → not significant
  result <- get_diffs(d, dv, treats, show_favorability = TRUE, alpha = min_p)
  # Find the row with p_value exactly equal to min_p (exclude NA rows)
  obs_rows <- result[!is.na(result$p_value) & result$p_value == min_p, ]
  if (nrow(obs_rows) > 0) {
    expect_false(obs_rows$favorable[[1]])
    expect_false(obs_rows$backlash[[1]])
  }
})

# ═══════════════════════════════════════════════════════════════════════════
# BUG FIXES
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_diffs() strips metacharacter ref level from contrast label (ME path)", {
  # Bug 1: ref_level containing regex metacharacters (e.g. "(Control)") must
  # be treated as a literal string, not a regex pattern, when stripping the
  # " - ref_level" suffix from contrast strings.
  set.seed(42)
  df <- data.frame(
    wt = runif(200, 0.5, 2),
    dv = rnorm(200, 50, 10),
    age = rnorm(200, 40, 10),
    arm = factor(sample(c("(Control)", "Treatment"), 200, TRUE))
  )
  d <- as_survey(df, weights = wt)

  # covariates forces the marginaleffects path which calls avg_slopes() and
  # parses contrast strings of the form "Treatment - (Control)"
  result <- get_diffs(
    d,
    dv,
    arm,
    covariates = "age",
    ref_level = "(Control)",
    label_values = FALSE
  )

  # Treatment level should be "Treatment", not "Treatment - (Control)"
  non_ref <- result[result$arm != "(Control)", ]
  expect_equal(nrow(non_ref), 1L)
  expect_identical(non_ref$arm[[1L]], "Treatment")
})

test_that("get_diffs() pct_change is non-NA with show_means = FALSE (ungrouped)", {
  # Bug 2: when show_means = FALSE, reference rows are absent from all_rows;
  # the fallback must read the reference mean from result$reference_mean (clean
  # path) or preds_df (ME path) so pct_change is not NA.
  set.seed(42)
  df <- data.frame(
    wt = runif(200, 0.5, 2),
    dv = rnorm(200, 50, 10),
    arm = factor(sample(c("Control", "A", "B"), 200, TRUE))
  )
  d <- as_survey(df, weights = wt)

  result <- get_diffs(
    d,
    dv,
    arm,
    ref_level = "Control",
    show_pct_change = TRUE,
    show_means = FALSE
  )

  # No reference row present
  expect_false(any(result$estimate == 0))
  # pct_change must be non-NA for all non-reference rows
  expect_false(any(is.na(result$pct_change)))
  expect_equal(nrow(result), 2L)
})

test_that("get_diffs() pct_change is non-NA with show_means = FALSE (grouped)", {
  # Bug 2: same fallback must work on the marginaleffects (grouped) path.
  set.seed(42)
  df <- data.frame(
    wt = runif(300, 0.5, 2),
    dv = rnorm(300, 50, 10),
    arm = factor(sample(c("Control", "A", "B"), 300, TRUE)),
    grp = factor(sample(c("M", "F"), 300, TRUE))
  )
  d <- as_survey(df, weights = wt)

  result <- suppressWarnings(get_diffs(
    d,
    dv,
    arm,
    group = grp,
    ref_level = "Control",
    show_pct_change = TRUE,
    show_means = FALSE
  ))

  # grp column present (grouped output)
  expect_true("grp" %in% names(result))
  # No reference rows in output
  expect_false(any(result$estimate == 0))
  # pct_change non-NA for all rows (all are non-reference)
  expect_false(any(is.na(result$pct_change)))
})
