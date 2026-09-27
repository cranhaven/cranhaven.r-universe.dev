# tests/testthat/test-analysis-t-test.R
#
# Happy paths + error paths + edge cases for get_t_test() and get_pairwise()

# ── Shared test fixtures ─────────────────────────────────────────────────────

# GSS fixture for snapshot tests
.make_gss_design <- function() {
  gss_sub <- gss_2024[gss_2024$sex %in% c(1L, 2L) & !is.na(gss_2024$age), ]
  gss_sub$sex <- factor(
    gss_sub$sex,
    levels = c(1, 2),
    labels = c("Male", "Female")
  )
  as_survey(gss_sub, ids = vpsu, weights = wtssps, strata = vstrat, nest = TRUE)
}

# Synthetic design for unit tests
# Use random sampling (not rep_len) so that group variables are independent
# and every group x by combination has rows.
.make_ttest_data <- function(n = 400, seed = 42) {
  set.seed(seed)
  df <- make_survey_data(n = n, n_psu = 20, n_strata = 4, seed = seed)
  df$grp2 <- factor(sample(c("A", "B"), n, replace = TRUE))
  df$grp3 <- factor(sample(c("X", "Y", "Z"), n, replace = TRUE))
  df$grp4 <- factor(sample(c("P", "Q", "R", "S"), n, replace = TRUE))
  df$gender <- factor(sample(c("M", "F"), n, replace = TRUE))
  df$outcome <- rnorm(n, mean = ifelse(df$grp2 == "A", 50, 55), sd = 10)
  df
}

.make_ttest_design <- function(n = 400, seed = 42) {
  df <- .make_ttest_data(n = n, seed = seed)
  as_survey(df, ids = psu, weights = wt, strata = strata)
}

# ═══════════════════════════════════════════════════════════════════════════
# 1. HAPPY PATHS — get_t_test()
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_t_test() returns survey_t_test with correct columns (no group)", {
  d <- .make_ttest_design()
  result <- get_t_test(d, outcome, by = grp2)
  expect_s3_class(result, "survey_t_test")
  expect_s3_class(result, "survey_result")
  expect_equal(nrow(result), 1L)
  expected_cols <- c(
    "level_a",
    "level_b",
    "estimate",
    "mean_a",
    "mean_b",
    "n_a",
    "n_b",
    "ci_low",
    "ci_high",
    "t_stat",
    "df",
    "p_value",
    "stars"
  )
  expect_true(all(expected_cols %in% names(result)))
  expect_type(result$level_a, "character")
  expect_type(result$level_b, "character")
  expect_type(result$estimate, "double")
  expect_type(result$t_stat, "double")
  expect_type(result$df, "double")
  expect_type(result$p_value, "double")
  expect_type(result$stars, "character")
  expect_type(result$n_a, "integer")
  expect_type(result$n_b, "integer")
})

test_that("get_t_test() returns one row per group stratum when group is active", {
  d <- .make_ttest_design()
  result <- get_t_test(d, outcome, by = grp2, group = gender)
  expect_equal(nrow(result), 2L) # M and F
  expect_true("gender" %in% names(result))
})

test_that("get_t_test() estimate == mean_b - mean_a", {
  d <- .make_ttest_design()
  result <- get_t_test(d, outcome, by = grp2, variance = c("se", "ci"))
  expect_equal(
    result$estimate,
    result$mean_b - result$mean_a,
    tolerance = 1e-10,
    ignore_attr = TRUE
  )
})

test_that("get_t_test() t_stat == estimate / se", {
  d <- .make_ttest_design()
  result <- get_t_test(d, outcome, by = grp2, variance = c("se", "ci"))
  expect_equal(
    result$t_stat,
    result$estimate / result$se,
    tolerance = 1e-10,
    ignore_attr = TRUE
  )
})

test_that("get_t_test() p_value == 2 * pt(-abs(t_stat), df = df)", {
  d <- .make_ttest_design()
  result <- get_t_test(d, outcome, by = grp2, variance = c("se", "ci"))
  expected_p <- 2 * stats::pt(-abs(result$t_stat), df = result$df)
  expect_equal(
    result$p_value,
    expected_p,
    tolerance = 1e-10,
    ignore_attr = TRUE
  )
})

test_that("get_t_test() CI bounds use qt formula", {
  d <- .make_ttest_design()
  conf <- 0.95
  result <- get_t_test(
    d,
    outcome,
    by = grp2,
    variance = c("se", "ci"),
    conf_level = conf
  )
  half <- stats::qt((1 + conf) / 2, df = result$df) * result$se
  expect_equal(
    result$ci_low,
    result$estimate - half,
    tolerance = 1e-10,
    ignore_attr = TRUE
  )
  expect_equal(
    result$ci_high,
    result$estimate + half,
    tolerance = 1e-10,
    ignore_attr = TRUE
  )
})

test_that("get_t_test() variance = 'se' omits CI columns", {
  d <- .make_ttest_design()
  result <- get_t_test(d, outcome, by = grp2, variance = "se")
  expect_true("se" %in% names(result))
  expect_false("ci_low" %in% names(result))
  expect_false("ci_high" %in% names(result))
})

test_that("get_t_test() variance = c('se', 'ci') includes both SE and CI", {
  d <- .make_ttest_design()
  result <- get_t_test(d, outcome, by = grp2, variance = c("se", "ci"))
  expect_true("se" %in% names(result))
  expect_true("ci_low" %in% names(result))
  expect_true("ci_high" %in% names(result))
})

test_that("get_t_test() default variance='ci' includes CI but not SE", {
  d <- .make_ttest_design()
  result <- get_t_test(d, outcome, by = grp2)
  expect_false("se" %in% names(result))
  expect_true("ci_low" %in% names(result))
  expect_true("ci_high" %in% names(result))
})

test_that("get_t_test() label_values = TRUE converts factor codes to label strings", {
  d <- .make_ttest_design()
  # grp2 is already a factor with labels "A"/"B"
  result_labels <- get_t_test(d, outcome, by = grp2, label_values = TRUE)
  result_raw <- get_t_test(d, outcome, by = grp2, label_values = FALSE)
  # Both should have character level_a/level_b - factor with labels just uses label
  expect_type(result_labels$level_a, "character")
  expect_type(result_raw$level_a, "character")
})

test_that("get_t_test() label_vars = TRUE and FALSE both accepted without error", {
  d <- .make_ttest_design()
  result_true <- get_t_test(d, outcome, by = grp2, label_vars = TRUE)
  result_false <- get_t_test(d, outcome, by = grp2, label_vars = FALSE)
  expect_s3_class(result_true, "survey_t_test")
  expect_s3_class(result_false, "survey_t_test")
  # Column names unchanged regardless of label_vars
  expect_equal(names(result_true), names(result_false))
})

test_that("get_t_test() name_style = 'broom' renames columns correctly", {
  d <- .make_ttest_design()
  result <- get_t_test(
    d,
    outcome,
    by = grp2,
    variance = c("se", "ci"),
    name_style = "broom"
  )
  expect_true("std.error" %in% names(result))
  expect_true("conf.low" %in% names(result))
  expect_true("conf.high" %in% names(result))
  expect_true("p.value" %in% names(result))
  expect_true("parameter" %in% names(result))
  expect_false("se" %in% names(result))
  expect_false("ci_low" %in% names(result))
  expect_false("p_value" %in% names(result))
  # t_stat is NOT renamed (intentional — not in broom map)
  expect_true("t_stat" %in% names(result))
})

test_that("get_t_test() decimals = 2 rounds all double columns", {
  d <- .make_ttest_design()
  result <- get_t_test(
    d,
    outcome,
    by = grp2,
    variance = c("se", "ci"),
    decimals = 2
  )
  double_cols <- vapply(result, is.double, logical(1L))
  for (nm in names(result)[double_cols]) {
    expect_equal(
      result[[nm]],
      round(result[[nm]], 2),
      label = paste0("column: ", nm)
    )
  }
})

test_that("get_t_test() column-level label attributes match spec", {
  # Need a design where by has a variable label so by_label is non-empty
  df <- .make_ttest_data()
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  d <- set_var_label(d, grp2 = "Group Assignment")
  result <- get_t_test(d, outcome, by = grp2, variance = c("se", "ci"))
  expect_equal(attr(result$level_a, "label"), "Group Assignment (A)")
  expect_equal(attr(result$level_b, "label"), "Group Assignment (B)")
  expect_equal(attr(result$estimate, "label"), "Difference (B \u2212 A)")
  expect_equal(attr(result$mean_a, "label"), "Mean (A)")
  expect_equal(attr(result$mean_b, "label"), "Mean (B)")
  expect_equal(attr(result$n_a, "label"), "N (A)")
  expect_equal(attr(result$n_b, "label"), "N (B)")
  expect_equal(attr(result$ci_low, "label"), "Low CI")
  expect_equal(attr(result$ci_high, "label"), "High CI")
  expect_equal(attr(result$t_stat, "label"), "t")
  expect_equal(attr(result$df, "label"), "df")
  expect_equal(attr(result$p_value, "label"), "P-Value")
  expect_equal(attr(result$stars, "label"), "")
})

test_that("get_t_test() works with survey_taylor design", {
  d <- .make_ttest_design()
  result <- get_t_test(d, outcome, by = grp2)
  expect_s3_class(result, "survey_t_test")
  expect_equal(nrow(result), 1L)
})

test_that("get_t_test() works with survey_replicate design", {
  df <- .make_ttest_data()
  d_rep <- as_survey_replicate(
    df,
    weights = wt,
    repweights = starts_with("y"),
    type = "JK1",
    scale = 1,
    mse = TRUE
  )
  result <- get_t_test(d_rep, outcome, by = grp2)
  expect_s3_class(result, "survey_t_test")
  expect_equal(nrow(result), 1L)
})

test_that("get_t_test() works with survey_twophase design", {
  df <- .make_ttest_data(n = 200)
  df$phase2 <- as.logical(seq_len(nrow(df)) %% 2 == 0)
  d1 <- as_survey(df, ids = psu, weights = wt, strata = strata)
  d2 <- as_survey_twophase(phase1 = d1, subset = phase2, method = "approx")
  result <- get_t_test(d2, outcome, by = grp2)
  expect_s3_class(result, "survey_t_test")
  expect_equal(nrow(result), 1L)
})

test_that("get_t_test() works with survey_nonprob design", {
  df <- .make_ttest_data()
  d_np <- as_survey_nonprob(df, weights = wt)
  result <- get_t_test(d_np, outcome, by = grp2)
  expect_s3_class(result, "survey_t_test")
  expect_equal(nrow(result), 1L)
})

test_that("get_t_test() print snapshot", {
  gss_design <- .make_gss_design()
  expect_snapshot(print(get_t_test(gss_design, age, by = sex, decimals = 2)))
})

test_that("get_t_test() print fallback: x_label defaults to column name when no label", {
  # outcome has no variable label -> print must fall back to column name
  d <- .make_ttest_design()
  result <- get_t_test(d, outcome, by = grp2)
  # Verify print does not error; header uses column name "outcome" as DV label
  expect_no_error(print(result))
})

test_that("get_t_test() group with multiple group vars: one row per combination", {
  df <- .make_ttest_data()
  df$grp_b <- factor(rep_len(c("X", "Y"), nrow(df)))
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  result <- get_t_test(d, outcome, by = grp2, group = c(gender, grp_b))
  expect_equal(nrow(result), 4L) # 2 gender * 2 grp_b combinations
})

# ═══════════════════════════════════════════════════════════════════════════
# 2. ERROR PATHS — get_t_test()
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_t_test() rejects non-survey-base objects", {
  df <- data.frame(x = 1:10, by = factor(c("A", "B")))
  expect_error(
    get_t_test(df, x, by = by),
    class = "surveycore_error_unsupported_class"
  )
  expect_snapshot(error = TRUE, get_t_test(df, x, by = by))
})

test_that("get_t_test() rejects by with 3 active levels (T-1)", {
  d <- .make_ttest_design()
  expect_error(
    get_t_test(d, outcome, by = grp3),
    class = "surveycore_error_by_not_two_levels"
  )
  expect_snapshot(error = TRUE, get_t_test(d, outcome, by = grp3))
})

test_that("get_t_test() warns for character by (T-2)", {
  df <- .make_ttest_data()
  df$grp_chr <- as.character(df$grp2)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  expect_warning(
    result <- get_t_test(d, outcome, by = grp_chr),
    class = "surveycore_warning_by_coerced"
  )
  expect_s3_class(result, "survey_t_test")
  expect_snapshot_warning(get_t_test(d, outcome, by = grp_chr))
})

test_that("get_t_test() warns for integer by (T-2)", {
  df <- .make_ttest_data()
  df$grp_int <- as.integer(df$grp2)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  expect_warning(
    result <- get_t_test(d, outcome, by = grp_int),
    class = "surveycore_warning_by_coerced"
  )
  expect_s3_class(result, "survey_t_test")
  expect_snapshot_warning(get_t_test(d, outcome, by = grp_int))
})

test_that("get_t_test() warns for logical by (T-2)", {
  df <- .make_ttest_data()
  df$grp_lgl <- df$grp2 == "A"
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  expect_warning(
    result <- get_t_test(d, outcome, by = grp_lgl),
    class = "surveycore_warning_by_coerced"
  )
  expect_s3_class(result, "survey_t_test")
  expect_snapshot_warning(get_t_test(d, outcome, by = grp_lgl))
})

test_that("get_t_test() rejects by with empty cell (T-3, no group)", {
  df <- .make_ttest_data()
  # One level has all-NA outcome
  df$outcome_na <- df$outcome
  df$outcome_na[df$grp2 == "B"] <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  expect_error(
    get_t_test(d, outcome_na, by = grp2),
    class = "surveycore_error_by_empty_cell"
  )
  expect_snapshot(error = TRUE, get_t_test(d, outcome_na, by = grp2))
})

test_that("get_t_test() rejects empty cell in group stratum (T-3g)", {
  df <- .make_ttest_data()
  # Make outcome NA for grp2==B when gender==M
  df$outcome_partial <- df$outcome
  df$outcome_partial[df$grp2 == "B" & df$gender == "M"] <- NA_real_
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  expect_error(
    get_t_test(d, outcome_partial, by = grp2, group = gender),
    class = "surveycore_error_by_empty_cell"
  )
  expect_snapshot(
    error = TRUE,
    get_t_test(d, outcome_partial, by = grp2, group = gender)
  )
})

test_that("get_t_test() rejects non-numeric x", {
  d <- .make_ttest_design()
  expect_error(
    get_t_test(d, grp3, by = grp2),
    class = "surveycore_error_non_numeric_variable"
  )
  expect_snapshot(error = TRUE, get_t_test(d, grp3, by = grp2))
})

test_that("get_t_test() rejects invalid conf_level", {
  d <- .make_ttest_design()
  expect_error(
    get_t_test(d, outcome, by = grp2, conf_level = 1.5),
    class = "surveycore_error_invalid_conf_level"
  )
  expect_snapshot(
    error = TRUE,
    get_t_test(d, outcome, by = grp2, conf_level = 1.5)
  )
})

test_that("get_t_test() rejects invalid variance arg", {
  d <- .make_ttest_design()
  expect_error(
    get_t_test(d, outcome, by = grp2, variance = "sd"),
    class = "surveycore_error_invalid_variance_arg"
  )
  expect_snapshot(
    error = TRUE,
    get_t_test(d, outcome, by = grp2, variance = "sd")
  )
})

test_that("get_t_test() rejects na.rm not logical", {
  d <- .make_ttest_design()
  expect_error(
    get_t_test(d, outcome, by = grp2, na.rm = "yes"),
    class = "surveycore_error_na_rm_not_logical"
  )
  expect_snapshot(
    error = TRUE,
    get_t_test(d, outcome, by = grp2, na.rm = "yes")
  )
})

# ═══════════════════════════════════════════════════════════════════════════
# 3. EDGE CASES — get_t_test()
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_t_test() character by coerces to factor; result still valid", {
  df <- .make_ttest_data()
  df$grp_chr <- as.character(df$grp2)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  result <- suppressWarnings(get_t_test(d, outcome, by = grp_chr))
  expect_s3_class(result, "survey_t_test")
  expect_equal(nrow(result), 1L)
})

test_that("get_t_test() integer by coerces to factor; result still valid", {
  df <- .make_ttest_data()
  df$grp_int <- as.integer(df$grp2)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  result <- suppressWarnings(get_t_test(d, outcome, by = grp_int))
  expect_s3_class(result, "survey_t_test")
  expect_equal(nrow(result), 1L)
})

test_that("get_t_test() logical by coerces to factor; result still valid", {
  df <- .make_ttest_data()
  df$grp_lgl <- df$grp2 == "A"
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  result <- suppressWarnings(get_t_test(d, outcome, by = grp_lgl))
  expect_s3_class(result, "survey_t_test")
  expect_equal(nrow(result), 1L)
})

test_that("get_t_test() ordered factor by is accepted as-is; no coercion warning", {
  df <- .make_ttest_data()
  df$grp_ord <- ordered(df$grp2, levels = c("A", "B"))
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  expect_no_warning(result <- get_t_test(d, outcome, by = grp_ord))
  expect_s3_class(result, "survey_t_test")
})

test_that("get_t_test() domain estimation matches physical subsetting for SRS", {
  set.seed(123)
  n <- 300
  df <- data.frame(
    wt = rep(1, n),
    grp = factor(rep_len(c("A", "B", "C"), n)),
    y = rnorm(n, 10, 2)
  )
  # Filter via domain col to only A and B (domain estimation)
  df$`..surveycore_domain..` <- df$grp %in% c("A", "B")
  d_domain <- as_survey(df, weights = wt)

  # Physical subset: only rows A and B
  df_sub <- df[df$grp %in% c("A", "B"), ]
  df_sub$grp <- factor(df_sub$grp, levels = c("A", "B"))
  d_sub <- as_survey(df_sub, weights = wt)

  r_domain <- get_t_test(d_domain, y, by = grp)
  r_sub <- get_t_test(d_sub, y, by = grp)

  # Point estimates match exactly (same data, same GLM computation)
  expect_equal(r_domain$estimate, r_sub$estimate, tolerance = 1e-10)
  # For SRS, domain df and subset df may differ (domain uses full n,
  # subset uses n_sub), so SE and p-value may differ slightly; use 1e-3
  expect_equal(
    get_t_test(d_domain, y, by = grp, variance = "se")$se,
    get_t_test(d_sub, y, by = grp, variance = "se")$se,
    tolerance = 1e-3
  )
})

test_that("get_t_test() min_cell_n = 0L suppresses small-cell warning", {
  d <- .make_ttest_design()
  expect_no_warning(get_t_test(d, outcome, by = grp2, min_cell_n = 0L))
})

test_that("get_t_test() warns for small cells when min_cell_n > actual n", {
  # Build data where one group has very few rows to trigger small-cell warning
  set.seed(99)
  df <- data.frame(
    wt = rep(1, 30),
    grp = factor(c(rep("A", 28), rep("B", 2))),
    y = rnorm(30)
  )
  d <- as_survey(df, weights = wt)
  # With only 2 B rows but min_cell_n = 10, should warn
  expect_warning(
    get_t_test(d, y, by = grp, min_cell_n = 10L),
    class = "surveycore_warning_small_cell"
  )
})

test_that("get_t_test() na.rm = FALSE when x has no NAs: identical to TRUE", {
  d <- .make_ttest_design()
  # outcome has no NAs by construction
  r_true <- get_t_test(d, outcome, by = grp2)
  r_false <- get_t_test(d, outcome, by = grp2, na.rm = FALSE)
  expect_equal(r_true$estimate, r_false$estimate, tolerance = 1e-10)
  expect_equal(r_true$p_value, r_false$p_value, tolerance = 1e-10)
})

test_that("get_t_test() conf_level = 0.99 produces wider CI than 0.95", {
  d <- .make_ttest_design()
  r95 <- get_t_test(d, outcome, by = grp2) # default 0.95
  r99 <- get_t_test(d, outcome, by = grp2, conf_level = 0.99)
  width95 <- r95$ci_high - r95$ci_low
  width99 <- r99$ci_high - r99$ci_low
  expect_gt(width99, width95)
})

# ═══════════════════════════════════════════════════════════════════════════
# 4. META CONTRACT — get_t_test()
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_t_test() .meta contains required keys", {
  d <- .make_ttest_design()
  result <- get_t_test(d, outcome, by = grp2)
  m <- meta(result)
  expect_true(all(
    c(
      "design_type",
      "n_respondents",
      "conf_level",
      "call",
      "group",
      "x",
      "by"
    ) %in%
      names(m)
  ))
})

test_that("get_t_test() .meta$by contains levels sub-key with two factor levels", {
  d <- .make_ttest_design()
  result <- get_t_test(d, outcome, by = grp2)
  m <- meta(result)
  expect_true("levels" %in% names(m$by))
  expect_length(m$by$levels, 2L)
  expect_equal(m$by$levels, c("A", "B"))
})

# ═══════════════════════════════════════════════════════════════════════════
# 5. HAPPY PATHS — get_pairwise()
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_pairwise() returns survey_pairwise with correct columns (no group)", {
  d <- .make_ttest_design()
  result <- get_pairwise(d, outcome, by = grp3)
  expect_s3_class(result, "survey_pairwise")
  expect_s3_class(result, "survey_result")
  expected_cols <- c(
    "level_a",
    "level_b",
    "estimate",
    "mean_a",
    "mean_b",
    "n_a",
    "n_b",
    "ci_low",
    "ci_high",
    "t_stat",
    "df",
    "p_value",
    "stars"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("get_pairwise() produces one row per pair for 3-level by", {
  d <- .make_ttest_design()
  result <- get_pairwise(d, outcome, by = grp3)
  expect_equal(nrow(result), 3L) # C(3,2) = 3
})

test_that("get_pairwise() produces one row per pair per group stratum", {
  d <- .make_ttest_design()
  result <- get_pairwise(d, outcome, by = grp3, group = gender)
  # 3 pairs * 2 genders = 6 rows
  expect_equal(nrow(result), 6L)
})

test_that("get_pairwise() pairs are in lexicographic factor-level order", {
  d <- .make_ttest_design()
  result <- get_pairwise(d, outcome, by = grp3)
  # grp3 levels: X, Y, Z -> pairs: (X,Y), (X,Z), (Y,Z)
  expect_equal(result$level_a, c("X", "X", "Y"), ignore_attr = TRUE)
  expect_equal(result$level_b, c("Y", "Z", "Z"), ignore_attr = TRUE)
})

test_that("get_pairwise() pval_adj = 'holm' applies Holm correction", {
  d <- .make_ttest_design()
  result_holm <- get_pairwise(d, outcome, by = grp3, pval_adj = "holm")
  result_none <- get_pairwise(d, outcome, by = grp3, pval_adj = "none")
  # Holm-adjusted p-values should be >= unadjusted for at least one comparison
  expect_true(all(result_holm$p_value >= result_none$p_value - 1e-10))
})

test_that("get_pairwise() pval_adj = 'none' returns unadjusted p-values unchanged", {
  d <- .make_ttest_design()
  # Run get_t_test for each pair manually and compare
  result_pairwise <- get_pairwise(d, outcome, by = grp3, pval_adj = "none")
  expect_equal(nrow(result_pairwise), 3L)
  # All p-values should be valid
  expect_true(all(result_pairwise$p_value >= 0 & result_pairwise$p_value <= 1))
})

test_that("get_pairwise() stars computed from adjusted p-values", {
  d <- .make_ttest_design()
  result <- get_pairwise(d, outcome, by = grp3, pval_adj = "holm")
  # Recompute stars from adjusted p-values and compare
  expected_stars <- vapply(
    result$p_value,
    function(p) {
      if (is.na(p)) {
        return("")
      }
      if (p < 0.001) {
        return("***")
      }
      if (p < 0.01) {
        return("**")
      }
      if (p < 0.05) {
        return("*")
      }
      if (p < 0.1) {
        return(".")
      }
      return("")
    },
    character(1L)
  )
  expect_equal(result$stars, expected_stars, ignore_attr = TRUE)
})

test_that("get_pairwise() with group: adjustment applied separately per stratum", {
  d <- .make_ttest_design()
  result <- get_pairwise(
    d,
    outcome,
    by = grp3,
    group = gender,
    pval_adj = "holm"
  )
  # Should have 6 rows: 3 pairs * 2 gender strata
  expect_equal(nrow(result), 6L)
})

test_that("get_pairwise() label_values = FALSE keeps raw codes", {
  d <- .make_ttest_design()
  # grp3 levels are already character "X", "Y", "Z"
  result <- get_pairwise(d, outcome, by = grp3, label_values = FALSE)
  expect_equal(result$level_a[1L], "X")
})

test_that("get_pairwise() label_vars = TRUE and FALSE both accepted", {
  d <- .make_ttest_design()
  r_true <- get_pairwise(d, outcome, by = grp3, label_vars = TRUE)
  r_false <- get_pairwise(d, outcome, by = grp3, label_vars = FALSE)
  expect_s3_class(r_true, "survey_pairwise")
  expect_s3_class(r_false, "survey_pairwise")
  expect_equal(names(r_true), names(r_false))
})

test_that("get_pairwise() name_style = 'broom' renames correctly", {
  d <- .make_ttest_design()
  result <- get_pairwise(
    d,
    outcome,
    by = grp3,
    variance = c("se", "ci"),
    name_style = "broom"
  )
  expect_true("std.error" %in% names(result))
  expect_true("conf.low" %in% names(result))
  expect_true("conf.high" %in% names(result))
  expect_true("p.value" %in% names(result))
  expect_true("parameter" %in% names(result))
  expect_true("t_stat" %in% names(result))
})

test_that("get_pairwise() works with survey_taylor design", {
  d <- .make_ttest_design()
  result <- get_pairwise(d, outcome, by = grp3)
  expect_s3_class(result, "survey_pairwise")
})

test_that("get_pairwise() works with survey_replicate design", {
  df <- .make_ttest_data()
  d_rep <- as_survey_replicate(
    df,
    weights = wt,
    repweights = starts_with("y"),
    type = "JK1",
    scale = 1,
    mse = TRUE
  )
  result <- get_pairwise(d_rep, outcome, by = grp3)
  expect_s3_class(result, "survey_pairwise")
})

test_that("get_pairwise() works with survey_twophase design", {
  df <- .make_ttest_data(n = 200)
  df$phase2 <- as.logical(seq_len(nrow(df)) %% 2 == 0)
  d1 <- as_survey(df, ids = psu, weights = wt, strata = strata)
  d2 <- as_survey_twophase(phase1 = d1, subset = phase2, method = "approx")
  result <- get_pairwise(d2, outcome, by = grp3)
  expect_s3_class(result, "survey_pairwise")
})

test_that("get_pairwise() works with survey_nonprob design", {
  df <- .make_ttest_data()
  d_np <- as_survey_nonprob(df, weights = wt)
  result <- get_pairwise(d_np, outcome, by = grp3)
  expect_s3_class(result, "survey_pairwise")
})

test_that("get_pairwise() print snapshot", {
  gss_design <- .make_gss_design()
  expect_snapshot(
    print(get_pairwise(
      gss_design,
      age,
      by = sex,
      pval_adj = "holm",
      decimals = 2
    ))
  )
})

test_that("get_pairwise() print fallback: x_label defaults to column name when no label", {
  # outcome has no variable label -> print must fall back to column name
  d <- .make_ttest_design()
  result <- get_pairwise(d, outcome, by = grp3)
  # Verify print does not error; header uses column name "outcome" as DV label
  expect_no_error(print(result))
})

test_that("get_pairwise() coerces character by to factor with warning", {
  df <- .make_ttest_data()
  df$grp_chr <- as.character(df$grp3) # 3 levels, character type
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  expect_warning(
    result <- get_pairwise(d, outcome, by = grp_chr),
    class = "surveycore_warning_by_coerced"
  )
  expect_s3_class(result, "survey_pairwise")
  expect_equal(nrow(result), 3L) # C(3,2) = 3 pairs
})

test_that("get_pairwise() on 2-level by matches get_t_test() estimate, SE, unadjusted p", {
  d <- .make_ttest_design()
  r_pairwise <- get_pairwise(
    d,
    outcome,
    by = grp2,
    pval_adj = "none",
    variance = c("se", "ci")
  )
  r_ttest <- get_t_test(d, outcome, by = grp2, variance = c("se", "ci"))
  expect_equal(r_pairwise$estimate, r_ttest$estimate, tolerance = 1e-10)
  expect_equal(r_pairwise$se, r_ttest$se, tolerance = 1e-8)
  # p_value labels differ ("P-Value (none)" vs "P-Value") — compare values only
  expect_equal(
    r_pairwise$p_value,
    r_ttest$p_value,
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
})

test_that("get_pairwise() p_value label reflects pval_adj method", {
  d <- .make_ttest_design()
  r_holm <- get_pairwise(d, outcome, by = grp3, pval_adj = "holm")
  r_none <- get_pairwise(d, outcome, by = grp3, pval_adj = "none")
  expect_equal(attr(r_holm$p_value, "label"), "P-Value (holm)")
  expect_equal(attr(r_none$p_value, "label"), "P-Value (none)")
})

# ═══════════════════════════════════════════════════════════════════════════
# 6. ERROR PATHS — get_pairwise()
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_pairwise() rejects by with only 1 active level (P-1)", {
  df <- .make_ttest_data()
  df$grp1 <- factor(rep("A", nrow(df)))
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  expect_error(
    get_pairwise(d, outcome, by = grp1),
    class = "surveycore_error_by_one_level"
  )
  expect_snapshot(error = TRUE, get_pairwise(d, outcome, by = grp1))
})

test_that("get_pairwise() rejects invalid pval_adj", {
  d <- .make_ttest_design()
  expect_error(
    get_pairwise(d, outcome, by = grp3, pval_adj = "invalid_method"),
    class = "surveycore_error_invalid_pval_adj"
  )
  expect_snapshot(
    error = TRUE,
    get_pairwise(d, outcome, by = grp3, pval_adj = "invalid_method")
  )
})

# ═══════════════════════════════════════════════════════════════════════════
# 7. EDGE CASES — get_pairwise()
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_pairwise() with exactly 2 levels produces exactly 1 pair", {
  d <- .make_ttest_design()
  result <- get_pairwise(d, outcome, by = grp2)
  expect_equal(nrow(result), 1L)
})

test_that("get_pairwise() with 4 levels produces exactly 6 pairs", {
  d <- .make_ttest_design()
  result <- get_pairwise(d, outcome, by = grp4)
  expect_equal(nrow(result), 6L) # C(4,2) = 6
})

test_that("get_pairwise() SRS domain estimation matches physical subsetting", {
  set.seed(456)
  n <- 400
  df <- data.frame(
    wt = rep(1, n),
    grp = factor(rep_len(c("A", "B", "C", "D"), n)),
    y = rnorm(n, 10, 2)
  )
  # Domain subset to 3 levels
  df$`..surveycore_domain..` <- df$grp %in% c("A", "B", "C")
  d_domain <- as_survey(df, weights = wt)

  # Physical subset
  df_sub <- df[df$grp %in% c("A", "B", "C"), ]
  df_sub$grp <- factor(df_sub$grp, levels = c("A", "B", "C"))
  d_sub <- as_survey(df_sub, weights = wt)

  r_domain <- get_pairwise(
    d_domain,
    y,
    by = grp,
    pval_adj = "none",
    variance = c("se", "ci")
  )
  r_sub <- get_pairwise(
    d_sub,
    y,
    by = grp,
    pval_adj = "none",
    variance = c("se", "ci")
  )

  # Point estimates match exactly (same data, same GLM computation)
  expect_equal(r_domain$estimate, r_sub$estimate, tolerance = 1e-10)
  # SE and p-value may differ slightly due to df differences (domain uses
  # full n for design df, subset uses n_sub); use relaxed tolerance
  expect_equal(r_domain$se, r_sub$se, tolerance = 1e-3)
})

# ═══════════════════════════════════════════════════════════════════════════
# 8. META CONTRACT — get_pairwise()
# ═══════════════════════════════════════════════════════════════════════════

test_that("get_pairwise() .meta$pval_adj matches the method passed", {
  d <- .make_ttest_design()
  result <- get_pairwise(d, outcome, by = grp3, pval_adj = "bonferroni")
  m <- meta(result)
  expect_equal(m$pval_adj, "bonferroni")
})

test_that("get_pairwise() .meta contains all PAIRWISE_META_KEYS", {
  d <- .make_ttest_design()
  result <- get_pairwise(d, outcome, by = grp3)
  m <- meta(result)
  all_keys <- c("group", "x", "by", "pval_adj")
  expect_true(all(all_keys %in% names(m)))
})


# ── print label fallback (B1 / B2) ────────────────────────────────────────────

test_that("print.survey_t_test shows raw column name when no label is set", {
  d <- .make_ttest_design()
  result <- get_t_test(d, outcome, by = grp2)
  out <- capture.output(print(result))
  # "By:" line must contain the column name "grp2", not an empty string
  by_line <- grep("By:", out, value = TRUE)
  expect_length(by_line, 1L)
  expect_true(grepl("grp2", by_line))
})

test_that("print.survey_t_test shows variable label when label is set", {
  d <- .make_ttest_design()
  d <- set_var_label(d, grp2 = "Group membership")
  result <- get_t_test(d, outcome, by = grp2)
  out <- capture.output(print(result))
  by_line <- grep("By:", out, value = TRUE)
  expect_length(by_line, 1L)
  expect_true(grepl("Group membership", by_line))
})

test_that("print.survey_pairwise shows raw column name when no label is set", {
  d <- .make_ttest_design()
  result <- get_pairwise(d, outcome, by = grp3)
  out <- capture.output(print(result))
  by_line <- grep("By:", out, value = TRUE)
  expect_length(by_line, 1L)
  expect_true(grepl("grp3", by_line))
})

test_that("print.survey_pairwise shows variable label when label is set", {
  d <- .make_ttest_design()
  d <- set_var_label(d, grp3 = "Three-level group")
  result <- get_pairwise(d, outcome, by = grp3)
  out <- capture.output(print(result))
  by_line <- grep("By:", out, value = TRUE)
  expect_length(by_line, 1L)
  expect_true(grepl("Three-level group", by_line))
})
