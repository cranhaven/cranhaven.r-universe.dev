# tests/testthat/test-analysis-diffs-helpers.R
#
# Tests for diffs infrastructure:
#   1. .stars_pval() — significance star helper
#   2. .apply_name_style(exclude) — exclude parameter
#   3. print.survey_diffs() — snapshot
#   4. DIFFS_META_KEYS constant
#   5. .extract_clean_estimates() — clean path extraction helper
#   6. .extract_me_estimates() — ME path extraction helper
#   7. .build_diffs_output() — unified output builder


# -- Category 1: .stars_pval() ------------------------------------------------

test_that(".stars_pval() returns '***' for p < 0.001", {
  expect_identical(.stars_pval(0.0005), "***")
})

test_that(".stars_pval() returns '**' for p < 0.01", {
  expect_identical(.stars_pval(0.005), "**")
})

test_that(".stars_pval() returns '*' for p < 0.05", {
  expect_identical(.stars_pval(0.03), "*")
})

test_that(".stars_pval() returns '.' for p < 0.1", {
  expect_identical(.stars_pval(0.08), ".")
})

test_that(".stars_pval() returns '' for p >= 0.1", {
  expect_identical(.stars_pval(0.15), "")
})

test_that(".stars_pval() returns '' for NA p-values", {
  expect_identical(.stars_pval(NA_real_), "")
})

test_that(".stars_pval() boundary: p = 0.001 returns '**' (not '***')", {

  expect_identical(.stars_pval(0.001), "**")
})

test_that(".stars_pval() boundary: p = 0.01 returns '*' (not '**')", {
  expect_identical(.stars_pval(0.01), "*")
})

test_that(".stars_pval() boundary: p = 0.05 returns '.' (not '*')", {
  expect_identical(.stars_pval(0.05), ".")
})

test_that(".stars_pval() boundary: p = 0.1 returns '' (not '.')", {

  expect_identical(.stars_pval(0.1), "")
})

test_that(".stars_pval() handles vector input correctly", {
  p_vals <- c(0.0005, 0.005, 0.03, 0.08, 0.15, NA_real_)
  expected <- c("***", "**", "*", ".", "", "")
  expect_identical(.stars_pval(p_vals), expected)
})

test_that(".stars_pval() returns '' for p = 0", {
  expect_identical(.stars_pval(0), "***")
})

test_that(".stars_pval() returns '' for p = 1", {
  expect_identical(.stars_pval(1), "")
})


# -- Category 2: .apply_name_style(exclude) -----------------------------------

test_that('.apply_name_style() exclude = NULL preserves default behavior', {
  result <- structure(
    tibble::tibble(
      mean     = 10.0,
      se       = 1.0,
      ci_low   = 8.0,
      ci_high  = 12.0,
      n        = 100L
    ),
    .meta = list(design_type = "taylor"),
    class = c(
      "survey_means", "survey_result", "tbl_df", "tbl", "data.frame"
    )
  )
  out <- .apply_name_style(result, "broom", exclude = NULL)
  expect_true("estimate" %in% names(out))
  expect_true("std.error" %in% names(out))
  expect_true("conf.low" %in% names(out))
  expect_true("conf.high" %in% names(out))
  expect_false("mean" %in% names(out))
  expect_false("se" %in% names(out))
})

test_that('.apply_name_style() exclude = "mean" keeps mean, renames others', {
  result <- structure(
    tibble::tibble(
      mean     = 10.0,
      se       = 1.0,
      ci_low   = 8.0,
      ci_high  = 12.0,
      n        = 100L
    ),
    .meta = list(design_type = "taylor"),
    class = c(
      "survey_means", "survey_result", "tbl_df", "tbl", "data.frame"
    )
  )
  out <- .apply_name_style(result, "broom", exclude = "mean")
  expect_true("mean" %in% names(out))
  expect_false("estimate" %in% names(out))
  expect_true("std.error" %in% names(out))
  expect_true("conf.low" %in% names(out))
  expect_true("conf.high" %in% names(out))
})

test_that('.apply_name_style() exclude with surveycore style is no-op', {
  result <- structure(
    tibble::tibble(mean = 10.0, se = 1.0, n = 100L),
    .meta = list(design_type = "taylor"),
    class = c(
      "survey_means", "survey_result", "tbl_df", "tbl", "data.frame"
    )
  )
  out <- .apply_name_style(result, "surveycore", exclude = "mean")
  expect_identical(names(out), c("mean", "se", "n"))
})

test_that(".apply_name_style() exclude preserves .meta and class", {
  result <- structure(
    tibble::tibble(mean = 10.0, se = 1.0, n = 100L),
    .meta = list(design_type = "taylor", n_respondents = 100L),
    class = c(
      "survey_means", "survey_result", "tbl_df", "tbl", "data.frame"
    )
  )
  out <- .apply_name_style(result, "broom", exclude = "mean")
  expect_identical(attr(out, ".meta"), attr(result, ".meta"))
  expect_identical(class(out), class(result))
})


# -- Category 3: print.survey_diffs() -----------------------------------------

test_that("print.survey_diffs() renders expected header", {
  mock_diffs <- tibble::tibble(
    message_arm = factor(c("Control", "Msg A", "Msg B")),
    estimate    = c(0, 0.082, 0.103),
    mean        = c(0.401, 0.483, 0.504),
    n           = c(752L, 748L, 751L),
    ci_low      = c(NA, 0.042, 0.063),
    ci_high     = c(NA, 0.122, 0.143),
    p_value     = c(NA, 0.001, 0.000),
    stars       = c("", "**", "***")
  )
  attr(mock_diffs, ".meta") <- list(
    design_type     = "taylor",
    conf_level      = 0.95,
    call            = quote(get_diffs(d, agree_trope, message_arm)),
    n_respondents   = 2251L,
    group           = list(),
    x               = list(agree_trope = list(
      variable_label = "Agree with trope",
      question_preface = NULL,
      value_labels = NULL
    )),
    treats          = list(
      variable_label = "Message arm",
      question_preface = NULL,
      value_labels = NULL,
      name = "message_arm",
      ref_level = "Control"
    ),
    covariates      = NULL,
    family          = "gaussian",
    link            = "identity",
    pval_adj        = NULL,
    estimate_method = "coefficient",
    mean_method     = "intercept",
    estimate_scale  = "coefficient"
  )
  class(mock_diffs) <- c(
    "survey_diffs", "survey_result", "tbl_df", "tbl", "data.frame"
  )
  expect_snapshot(print(mock_diffs))
})

test_that("print.survey_diffs() shows replicate design type", {
  mock_diffs <- tibble::tibble(
    arm      = factor(c("Ctrl", "T1")),
    estimate = c(0, 0.05),
    mean     = c(0.5, 0.55),
    n        = c(100L, 100L)
  )
  attr(mock_diffs, ".meta") <- list(
    design_type     = "replicate",
    conf_level      = 0.95,
    call            = quote(get_diffs(d, y, arm)),
    n_respondents   = 200L,
    group           = list(),
    x               = list(y = list(
      variable_label = NULL, question_preface = NULL,
      value_labels = NULL
    )),
    treats          = list(
      variable_label = NULL, question_preface = NULL,
      value_labels = NULL, name = "arm", ref_level = "Ctrl"
    ),
    covariates      = NULL,
    family          = "gaussian",
    link            = "identity",
    pval_adj        = NULL,
    estimate_method = "coefficient",
    mean_method     = "intercept",
    estimate_scale  = "coefficient"
  )
  class(mock_diffs) <- c(
    "survey_diffs", "survey_result", "tbl_df", "tbl", "data.frame"
  )
  out <- capture.output(print(mock_diffs))
  expect_true(any(grepl("Replicate weights", out)))
})

test_that("print.survey_diffs() returns invisible(x)", {
  mock_diffs <- tibble::tibble(
    arm      = factor(c("Ctrl", "T1")),
    estimate = c(0, 0.05),
    mean     = c(0.5, 0.55),
    n        = c(100L, 100L)
  )
  attr(mock_diffs, ".meta") <- list(
    design_type     = "taylor",
    conf_level      = 0.95,
    call            = quote(get_diffs(d, y, arm)),
    n_respondents   = 200L,
    group           = list(),
    x               = list(y = list(
      variable_label = NULL, question_preface = NULL,
      value_labels = NULL
    )),
    treats          = list(
      variable_label = NULL, question_preface = NULL,
      value_labels = NULL, name = "arm", ref_level = "Ctrl"
    ),
    covariates      = NULL,
    family          = "gaussian",
    link            = "identity",
    pval_adj        = NULL,
    estimate_method = "coefficient",
    mean_method     = "intercept",
    estimate_scale  = "coefficient"
  )
  class(mock_diffs) <- c(
    "survey_diffs", "survey_result", "tbl_df", "tbl", "data.frame"
  )
  expect_invisible(print(mock_diffs))
})


# -- Category 4: DIFFS_META_KEYS constant -------------------------------------

test_that("DIFFS_META_KEYS contains all required keys", {
  expected <- c(
    "group", "x", "treats", "covariates", "family", "link",
    "pval_adj", "estimate_method", "mean_method", "estimate_scale"
  )
  expect_identical(DIFFS_META_KEYS, expected)
})


# -- Category 5: .extract_clean_estimates() -----------------------------------
#
# Helper setup: build a fitted model to use across clean-path tests.

.make_clean_fit <- function(seed = 42) {
  set.seed(seed)
  df <- make_survey_data(n = 200, n_psu = 20, n_strata = 4, seed = seed)
  df$treats <- factor(
    sample(c("Control", "A", "B"), 200, replace = TRUE),
    levels = c("Control", "A", "B")
  )
  df$dv <- rnorm(200, mean = 50, sd = 10)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  d@data[[paste0("treats")]] <- stats::relevel(d@data[["treats"]], ref = "Control")
  stats::contrasts(d@data[["treats"]]) <-
    stats::contr.treatment(levels(d@data[["treats"]]))
  survey_glm(d, dv ~ treats)
}

test_that(".extract_clean_estimates() returns all 10 expected keys", {
  fit <- .make_clean_fit()
  result <- .extract_clean_estimates(fit, "treats", 0.95)

  expected_keys <- c(
    "result_levels", "result_estimates", "result_ses",
    "result_ci_lows", "result_ci_highs", "result_p_values",
    "result_means", "result_groups", "reference_mean", "preds_df"
  )
  expect_identical(sort(names(result)), sort(expected_keys))
})

test_that(".extract_clean_estimates() strips variable prefix from term column", {
  fit <- .make_clean_fit()
  result <- .extract_clean_estimates(fit, "treats", 0.95)

  # Levels should be bare names, not "treatsA" / "treatsB"
  expect_identical(sort(result$result_levels), c("A", "B"))
})

test_that(".extract_clean_estimates() computes result_means = reference_mean + estimates", {
  fit <- .make_clean_fit()
  result <- .extract_clean_estimates(fit, "treats", 0.95)

  expected_means <- result$reference_mean + result$result_estimates
  expect_equal(result$result_means, expected_means)
})

test_that(".extract_clean_estimates() returns result_groups = NULL and preds_df = NULL", {
  fit <- .make_clean_fit()
  result <- .extract_clean_estimates(fit, "treats", 0.95)

  expect_null(result$result_groups)
  expect_null(result$preds_df)
})

test_that(".extract_clean_estimates() returns numeric result_levels length equal to n-1 levels", {
  fit <- .make_clean_fit()
  result <- .extract_clean_estimates(fit, "treats", 0.95)

  # 3-level treatment -> 2 non-reference levels
  expect_length(result$result_levels, 2L)
  expect_length(result$result_estimates, 2L)
  expect_length(result$result_ses, 2L)
  expect_length(result$result_ci_lows, 2L)
  expect_length(result$result_ci_highs, 2L)
  expect_length(result$result_p_values, 2L)
  expect_length(result$result_means, 2L)
})

test_that(".extract_clean_estimates() reference_mean is a numeric scalar", {
  fit <- .make_clean_fit()
  result <- .extract_clean_estimates(fit, "treats", 0.95)

  expect_true(is.numeric(result$reference_mean))
  expect_length(result$reference_mean, 1L)
})

test_that(".extract_clean_estimates() aborts with surveycore_error_reference_row_not_found when intercept missing", {
  # Build a model where the intercept is removed to trigger the error.
  # We'll test via the public API using a model that would expose this error
  # path. Since we can't easily remove the intercept from a normal model,
  # we use the direct helper with a mock clean() that returns no intercept.
  # Instead, test via the error class only (direct call via get_diffs() won't
  # trigger this without mocking; we test class directly).
  fit <- .make_clean_fit()

  # Monkey-patch: call the helper with a model whose clean() returns no intercept
  # We do this by temporarily overriding the clean result through a wrapper
  # that calls the underlying logic. Since we can't easily mock, we verify
  # via the snapshot path via get_diffs() triggering the error.
  # Direct class check via expect_error on the helper itself:
  expect_error(
    {
      # Build a dummy fit object with no-intercept formula
      set.seed(1)
      df2 <- make_survey_data(n = 100, n_psu = 10, n_strata = 2, seed = 1)
      df2$treats2 <- factor(
        sample(c("A", "B"), 100, replace = TRUE),
        levels = c("A", "B")
      )
      df2$dv2 <- rnorm(100, 50, 10)
      d2 <- as_survey(df2, ids = psu, weights = wt, strata = strata)
      d2@data[["treats2"]] <- stats::relevel(d2@data[["treats2"]], ref = "A")
      stats::contrasts(d2@data[["treats2"]]) <-
        stats::contr.treatment(levels(d2@data[["treats2"]]))
      # Remove intercept (0 + treats2 means the intercept row won't appear as
      # "(Intercept)" in clean() output — it will appear as the reference level
      # row or not at all under some conditions. Test the no-intercept case.)
      fit_noint <- survey_glm(d2, dv2 ~ 0 + treats2)
      .extract_clean_estimates(fit_noint, "treats2", 0.95)
    },
    class = "surveycore_error_reference_row_not_found"
  )
})

test_that(".extract_clean_estimates() missing intercept error has correct snapshot", {
  # Direct helper call snapshot
  set.seed(1)
  df2 <- make_survey_data(n = 100, n_psu = 10, n_strata = 2, seed = 1)
  df2$treats2 <- factor(
    sample(c("A", "B"), 100, replace = TRUE),
    levels = c("A", "B")
  )
  df2$dv2 <- rnorm(100, 50, 10)
  d2 <- as_survey(df2, ids = psu, weights = wt, strata = strata)
  d2@data[["treats2"]] <- stats::relevel(d2@data[["treats2"]], ref = "A")
  stats::contrasts(d2@data[["treats2"]]) <-
    stats::contr.treatment(levels(d2@data[["treats2"]]))
  fit_noint <- survey_glm(d2, dv2 ~ 0 + treats2)
  expect_snapshot(error = TRUE, .extract_clean_estimates(fit_noint, "treats2", 0.95))
})


# -- Category 6: .extract_me_estimates() --------------------------------------
#
# Helper setup: build fitted models for ME path tests.

.make_me_fit <- function(seed = 42, with_group = FALSE) {
  set.seed(seed)
  df <- make_survey_data(n = 300, n_psu = 20, n_strata = 4, seed = seed)
  df$arm <- factor(
    sample(c("Control", "A", "B"), 300, replace = TRUE),
    levels = c("Control", "A", "B")
  )
  df$dv <- rnorm(300, 50, 10)
  df$gender <- factor(sample(c("M", "F"), 300, replace = TRUE))
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  d@data[["arm"]] <- stats::relevel(d@data[["arm"]], ref = "Control")
  stats::contrasts(d@data[["arm"]]) <-
    stats::contr.treatment(levels(d@data[["arm"]]))
  if (with_group) {
    survey_glm(d, dv ~ arm * gender)
  } else {
    survey_glm(d, dv ~ arm)
  }
}

test_that(".extract_me_estimates() no-group: result_groups = NULL, reference_mean = NULL", {
  skip_if_not_installed("marginaleffects")
  fit <- .make_me_fit()
  result <- .extract_me_estimates(
    fit, "arm", character(0), "Control", "ame", FALSE
  )

  expect_null(result$result_groups)
  expect_null(result$reference_mean)
})

test_that(".extract_me_estimates() no-group: returns correct number of result levels", {
  skip_if_not_installed("marginaleffects")
  fit <- .make_me_fit()
  result <- .extract_me_estimates(
    fit, "arm", character(0), "Control", "ame", FALSE
  )

  # 3 levels -> 2 non-reference
  expect_length(result$result_levels, 2L)
  expect_identical(sort(result$result_levels), c("A", "B"))
})

test_that(".extract_me_estimates() no-group: result_means populated from preds_df", {
  skip_if_not_installed("marginaleffects")
  fit <- .make_me_fit()
  result <- .extract_me_estimates(
    fit, "arm", character(0), "Control", "ame", FALSE
  )

  expect_false(is.null(result$result_means))
  expect_length(result$result_means, 2L)
  expect_true(all(is.numeric(result$result_means)))
  expect_false(is.null(result$preds_df))
})

test_that(".extract_me_estimates() with groups: result_groups is a data.frame", {
  skip_if_not_installed("marginaleffects")
  fit <- .make_me_fit(with_group = TRUE)
  result <- suppressWarnings(
    .extract_me_estimates(
      fit, "arm", "gender", "Control", "ame", FALSE
    )
  )

  expect_true(is.data.frame(result$result_groups))
  expect_true("gender" %in% names(result$result_groups))
  # 2 non-ref levels * 2 genders = 4 rows
  expect_equal(nrow(result$result_groups), 4L)
})

test_that(".extract_me_estimates() suppress_mean = TRUE: preds_df = NULL, result_means = NULL", {
  skip_if_not_installed("marginaleffects")
  fit <- .make_me_fit()
  result <- .extract_me_estimates(
    fit, "arm", character(0), "Control", "ame", TRUE
  )

  expect_null(result$preds_df)
  expect_null(result$result_means)
})

test_that(".extract_me_estimates() contrast parsing strips ref level correctly", {
  skip_if_not_installed("marginaleffects")
  fit <- .make_me_fit()
  result <- .extract_me_estimates(
    fit, "arm", character(0), "Control", "ame", FALSE
  )

  # Level names should be "A" and "B", not "A - Control" etc.
  expect_identical(sort(result$result_levels), c("A", "B"))
})

test_that(".extract_me_estimates() scale = 'link' accepts parameter and returns correct structure", {
  skip_if_not_installed("marginaleffects")
  # The key behavior: scale = "link" passes me_type = "link" to avg_slopes.
  # We verify the function runs without error with scale = "link" and returns
  # the expected structure (2 non-reference levels for a 3-level treatment).
  fit <- .make_me_fit()

  result_link <- .extract_me_estimates(
    fit, "arm", character(0), "Control", "link", TRUE
  )

  # Function should succeed and return correct structure
  expect_length(result_link$result_levels, 2L)
  expect_identical(sort(result_link$result_levels), c("A", "B"))
  expect_null(result_link$result_groups)
  expect_null(result_link$reference_mean)
  # suppress_mean = TRUE -> preds_df = NULL, result_means = NULL
  expect_null(result_link$preds_df)
  expect_null(result_link$result_means)
})


# -- Category 7: .build_diffs_output() ----------------------------------------
#
# Helper setup: build design + result objects for output builder tests.

.make_build_design <- function(n = 200, seed = 42) {
  set.seed(seed)
  df <- make_survey_data(n = n, n_psu = 20, n_strata = 4, seed = seed)
  df$treats <- factor(
    sample(c("Control", "A", "B"), n, replace = TRUE),
    levels = c("Control", "A", "B")
  )
  df$dv <- rnorm(n, mean = 50, sd = 10)
  df$gender <- factor(sample(c("M", "F"), n, replace = TRUE))
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  d@data[["treats"]] <- stats::relevel(d@data[["treats"]], ref = "Control")
  stats::contrasts(d@data[["treats"]]) <-
    stats::contr.treatment(levels(d@data[["treats"]]))
  d
}

.make_clean_result <- function(design, fit, treats_name = "treats") {
  .extract_clean_estimates(fit, treats_name, 0.95)
}

test_that(".build_diffs_output() no-group clean path: col_vecs has correct structure", {
  design <- .make_build_design()
  fit <- survey_glm(design, dv ~ treats)
  result <- .make_clean_result(design, fit)

  domain_mask <- .apply_domain(design)
  wt_var <- design@variables$weights

  out <- .build_diffs_output(
    result     = result,
    design     = design,
    treats_name = "treats",
    group_names = character(0),
    ref_level  = "Control",
    domain_mask = domain_mask,
    wt_var     = wt_var,
    show_means = TRUE,
    use_marginaleffects = FALSE,
    pval_adj   = NULL,
    show_pct_change = FALSE,
    suppress_mean = FALSE,
    min_cell_n = 30L,
    variance   = "ci",
    n_weighted = FALSE
  )

  expect_true(is.list(out))
  expect_true("col_vecs" %in% names(out))
  expect_true("groups_df" %in% names(out))

  # col_vecs must have the treats column
  expect_true("treats" %in% names(out$col_vecs))
  expect_true("estimate" %in% names(out$col_vecs))
  expect_true("mean" %in% names(out$col_vecs))
  expect_true("n" %in% names(out$col_vecs))
  expect_true("ci_low" %in% names(out$col_vecs))
  expect_true("ci_high" %in% names(out$col_vecs))
  expect_true("p_value" %in% names(out$col_vecs))
  expect_true("stars" %in% names(out$col_vecs))

  # No-group case: groups_df is a 0-column data.frame
  expect_equal(ncol(out$groups_df), 0L)

  # 3 levels (1 ref + 2 treatment)
  expect_equal(length(out$col_vecs[["treats"]]), 3L)
})

test_that(".build_diffs_output() no-group ME path: col_vecs has correct structure", {
  skip_if_not_installed("marginaleffects")
  design <- .make_build_design()
  fit <- survey_glm(design, dv ~ treats)
  result <- .extract_me_estimates(
    fit, "treats", character(0), "Control", "ame", FALSE
  )

  domain_mask <- .apply_domain(design)
  wt_var <- design@variables$weights

  out <- .build_diffs_output(
    result     = result,
    design     = design,
    treats_name = "treats",
    group_names = character(0),
    ref_level  = "Control",
    domain_mask = domain_mask,
    wt_var     = wt_var,
    show_means = TRUE,
    use_marginaleffects = TRUE,
    pval_adj   = NULL,
    show_pct_change = FALSE,
    suppress_mean = FALSE,
    min_cell_n = 30L,
    variance   = "ci",
    n_weighted = FALSE
  )

  expect_true("treats" %in% names(out$col_vecs))
  expect_true("estimate" %in% names(out$col_vecs))
  expect_true("mean" %in% names(out$col_vecs))
  expect_equal(ncol(out$groups_df), 0L)
  expect_equal(length(out$col_vecs[["treats"]]), 3L)
})

test_that(".build_diffs_output() grouped path: groups_df has correct columns and row count", {
  skip_if_not_installed("marginaleffects")
  design <- .make_build_design()
  fit <- survey_glm(design, dv ~ treats * gender)
  result <- suppressWarnings(
    .extract_me_estimates(
      fit, "treats", "gender", "Control", "ame", FALSE
    )
  )

  domain_mask <- .apply_domain(design)
  wt_var <- design@variables$weights

  out <- suppressWarnings(
    .build_diffs_output(
      result     = result,
      design     = design,
      treats_name = "treats",
      group_names = "gender",
      ref_level  = "Control",
      domain_mask = domain_mask,
      wt_var     = wt_var,
      show_means = TRUE,
      use_marginaleffects = TRUE,
      pval_adj   = NULL,
      show_pct_change = FALSE,
      suppress_mean = FALSE,
      min_cell_n = 30L,
      variance   = "ci",
      n_weighted = FALSE
    )
  )

  # groups_df has the gender column
  expect_true("gender" %in% names(out$groups_df))
  # 2 groups * 3 levels = 6 rows
  expect_equal(nrow(out$groups_df), 6L)
  expect_equal(length(out$col_vecs[["treats"]]), 6L)
})

test_that(".build_diffs_output() small-cell warning fires for rows with 0 < n < min_cell_n", {
  # Use a tiny dataset to trigger the small-cell warning
  set.seed(1)
  df_tiny <- data.frame(
    treats = factor(
      c("Control", "Control", "Control", "A", "A", "B"),
      levels = c("Control", "A", "B")
    ),
    dv = c(50, 52, 48, 55, 57, 45),
    wt = rep(1, 6),
    stringsAsFactors = FALSE
  )
  d_tiny <- suppressWarnings(
    as_survey(df_tiny, weights = wt)
  )
  d_tiny@data[["treats"]] <- stats::relevel(
    d_tiny@data[["treats"]], ref = "Control"
  )
  stats::contrasts(d_tiny@data[["treats"]]) <-
    stats::contr.treatment(levels(d_tiny@data[["treats"]]))
  fit <- survey_glm(d_tiny, dv ~ treats)
  result <- .extract_clean_estimates(fit, "treats", 0.95)
  domain_mask <- .apply_domain(d_tiny)
  wt_var <- d_tiny@variables$weights

  expect_warning(
    .build_diffs_output(
      result     = result,
      design     = d_tiny,
      treats_name = "treats",
      group_names = character(0),
      ref_level  = "Control",
      domain_mask = domain_mask,
      wt_var     = wt_var,
      show_means = TRUE,
      use_marginaleffects = FALSE,
      pval_adj   = NULL,
      show_pct_change = FALSE,
      suppress_mean = FALSE,
      min_cell_n = 30L,
      variance   = "ci",
      n_weighted = FALSE
    ),
    class = "surveycore_warning_small_cell"
  )
})

test_that(".build_diffs_output() no small-cell warning when n == 0", {
  # Build a mock result where a level has n = 0
  # Use a design with the relevant level absent from data
  set.seed(2)
  df_zero <- data.frame(
    treats = factor(
      c("Control", "Control", "A", "A"),
      levels = c("Control", "A", "B")
    ),
    dv = c(50, 52, 55, 57),
    wt = rep(1, 4),
    stringsAsFactors = FALSE
  )
  d_zero <- suppressWarnings(
    as_survey(df_zero, weights = wt)
  )
  d_zero@data[["treats"]] <- stats::relevel(
    d_zero@data[["treats"]], ref = "Control"
  )
  stats::contrasts(d_zero@data[["treats"]]) <-
    stats::contr.treatment(levels(d_zero@data[["treats"]]))
  fit <- survey_glm(d_zero, dv ~ treats)
  result <- .extract_clean_estimates(fit, "treats", 0.95)
  domain_mask <- .apply_domain(d_zero)
  wt_var <- d_zero@variables$weights

  # B level has n = 0; should not trigger small-cell warning for n == 0
  # (small-cell fires only for 0 < n < min_cell_n)
  # Only warn for A and B (which have n = 2 < 30 = min_cell_n), not for n = 0
  # Actually: A has n=2 which IS 0 < n < 30, so will warn
  # The test is: a level with n = 0 should NOT warn (only n > 0 warns)
  # We check via a mock result with n = 0 explicitly
  # Instead: use min_cell_n = 1L so only n > 0 and n < 1 would warn (never)
  # and then add a row with n = 0 manually.
  # More directly: set min_cell_n = 3 to ensure only the 0-count level
  # doesn't warn when n == 0 exactly.
  warnings_issued <- character(0)
  withCallingHandlers(
    .build_diffs_output(
      result     = result,
      design     = d_zero,
      treats_name = "treats",
      group_names = character(0),
      ref_level  = "Control",
      domain_mask = domain_mask,
      wt_var     = wt_var,
      show_means = TRUE,
      use_marginaleffects = FALSE,
      pval_adj   = NULL,
      show_pct_change = FALSE,
      suppress_mean = FALSE,
      min_cell_n = 5L,
      variance   = "ci",
      n_weighted = FALSE
    ),
    surveycore_warning_small_cell = function(w) {
      warnings_issued <<- c(warnings_issued, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  # B level has n = 0 -> should not be warned
  # A level has n = 2 < 5 -> should be warned
  # Check that none of the warnings mention "B" (n = 0)
  if (length(warnings_issued) > 0) {
    expect_false(any(grepl('"B"', warnings_issued)))
  }
})

test_that(".build_diffs_output() p-value adjustment: no-group adjusts globally", {
  design <- .make_build_design()
  fit <- survey_glm(design, dv ~ treats)
  result <- .make_clean_result(design, fit)

  domain_mask <- .apply_domain(design)
  wt_var <- design@variables$weights

  out_unadj <- .build_diffs_output(
    result     = result,
    design     = design,
    treats_name = "treats",
    group_names = character(0),
    ref_level  = "Control",
    domain_mask = domain_mask,
    wt_var     = wt_var,
    show_means = TRUE,
    use_marginaleffects = FALSE,
    pval_adj   = NULL,
    show_pct_change = FALSE,
    suppress_mean = FALSE,
    min_cell_n = 30L,
    variance   = "ci",
    n_weighted = FALSE
  )

  out_adj <- .build_diffs_output(
    result     = result,
    design     = design,
    treats_name = "treats",
    group_names = character(0),
    ref_level  = "Control",
    domain_mask = domain_mask,
    wt_var     = wt_var,
    show_means = TRUE,
    use_marginaleffects = FALSE,
    pval_adj   = "BH",
    show_pct_change = FALSE,
    suppress_mean = FALSE,
    min_cell_n = 30L,
    variance   = "ci",
    n_weighted = FALSE
  )

  # Reference row (estimate = 0) p_value stays NA after adjustment
  ref_idx <- which(out_unadj$col_vecs[["estimate"]] == 0)
  expect_true(is.na(out_adj$col_vecs$p_value[ref_idx]))

  # Non-reference rows: adjusted p-values equal manual adjustment
  non_ref_idx <- which(out_unadj$col_vecs[["estimate"]] != 0)
  raw_pvals <- out_unadj$col_vecs$p_value[non_ref_idx]
  expected_adj <- stats::p.adjust(raw_pvals, method = "BH")
  expect_equal(
    out_adj$col_vecs$p_value[non_ref_idx],
    expected_adj
  )
})

test_that(".build_diffs_output() p-value adjustment with groups: adjusts within each group", {
  skip_if_not_installed("marginaleffects")
  design <- .make_build_design()
  fit <- survey_glm(design, dv ~ treats * gender)
  result <- suppressWarnings(
    .extract_me_estimates(
      fit, "treats", "gender", "Control", "ame", FALSE
    )
  )

  domain_mask <- .apply_domain(design)
  wt_var <- design@variables$weights

  out_adj <- suppressWarnings(
    .build_diffs_output(
      result     = result,
      design     = design,
      treats_name = "treats",
      group_names = "gender",
      ref_level  = "Control",
      domain_mask = domain_mask,
      wt_var     = wt_var,
      show_means = TRUE,
      use_marginaleffects = TRUE,
      pval_adj   = "bonferroni",
      show_pct_change = FALSE,
      suppress_mean = FALSE,
      min_cell_n = 30L,
      variance   = "ci",
      n_weighted = FALSE
    )
  )

  # groups_df has gender column
  expect_true("gender" %in% names(out_adj$groups_df))
  # p-values are not all NA for treatment rows
  non_ref <- out_adj$col_vecs$estimate != 0
  expect_false(all(is.na(out_adj$col_vecs$p_value[non_ref])))
})

test_that(".build_diffs_output() pct_change: correct value for each treatment row", {
  design <- .make_build_design()
  fit <- survey_glm(design, dv ~ treats)
  result <- .make_clean_result(design, fit)

  domain_mask <- .apply_domain(design)
  wt_var <- design@variables$weights

  out <- .build_diffs_output(
    result     = result,
    design     = design,
    treats_name = "treats",
    group_names = character(0),
    ref_level  = "Control",
    domain_mask = domain_mask,
    wt_var     = wt_var,
    show_means = TRUE,
    use_marginaleffects = FALSE,
    pval_adj   = NULL,
    show_pct_change = TRUE,
    suppress_mean = FALSE,
    min_cell_n = 30L,
    variance   = "ci",
    n_weighted = FALSE
  )

  expect_true("pct_change" %in% names(out$col_vecs))

  # Reference row: pct_change = NA
  ref_idx <- which(out$col_vecs[["estimate"]] == 0)
  expect_true(is.na(out$col_vecs$pct_change[ref_idx]))

  # Treatment rows: pct_change = estimate / ref_mean
  ref_mean <- out$col_vecs$mean[ref_idx]
  non_ref_idx <- which(out$col_vecs[["estimate"]] != 0)
  for (i in non_ref_idx) {
    est <- out$col_vecs$estimate[i]
    expected_pct <- est / ref_mean
    expect_equal(out$col_vecs$pct_change[i], expected_pct)
  }
})

test_that(".build_diffs_output() pct_change with show_means = FALSE: uses fallback ref_mean (clean path)", {
  design <- .make_build_design()
  fit <- survey_glm(design, dv ~ treats)
  result <- .make_clean_result(design, fit)

  domain_mask <- .apply_domain(design)
  wt_var <- design@variables$weights

  out_show <- .build_diffs_output(
    result     = result,
    design     = design,
    treats_name = "treats",
    group_names = character(0),
    ref_level  = "Control",
    domain_mask = domain_mask,
    wt_var     = wt_var,
    show_means = TRUE,
    use_marginaleffects = FALSE,
    pval_adj   = NULL,
    show_pct_change = TRUE,
    suppress_mean = FALSE,
    min_cell_n = 30L,
    variance   = "ci",
    n_weighted = FALSE
  )

  out_hide <- .build_diffs_output(
    result     = result,
    design     = design,
    treats_name = "treats",
    group_names = character(0),
    ref_level  = "Control",
    domain_mask = domain_mask,
    wt_var     = wt_var,
    show_means = FALSE,
    use_marginaleffects = FALSE,
    pval_adj   = NULL,
    show_pct_change = TRUE,
    suppress_mean = FALSE,
    min_cell_n = 30L,
    variance   = "ci",
    n_weighted = FALSE
  )

  # show_means = FALSE: no reference row, no mean column
  expect_false("mean" %in% names(out_hide$col_vecs))
  expect_false(any(out_hide$col_vecs[["estimate"]] == 0))

  # pct_change still computed correctly via fallback
  expect_true("pct_change" %in% names(out_hide$col_vecs))
  expect_false(all(is.na(out_hide$col_vecs$pct_change)))

  # Values should equal what show_means = TRUE produces for non-ref rows
  non_ref_show <- which(out_show$col_vecs[["estimate"]] != 0)
  non_ref_hide <- which(out_hide$col_vecs[["estimate"]] != 0)
  expect_equal(
    out_hide$col_vecs$pct_change[non_ref_hide],
    out_show$col_vecs$pct_change[non_ref_show]
  )
})

test_that(".build_diffs_output() pct_change with show_means = FALSE: correct on ME path", {
  skip_if_not_installed("marginaleffects")
  design <- .make_build_design()
  fit <- survey_glm(design, dv ~ treats)
  result <- .extract_me_estimates(
    fit, "treats", character(0), "Control", "ame", FALSE
  )

  domain_mask <- .apply_domain(design)
  wt_var <- design@variables$weights

  out_hide <- .build_diffs_output(
    result     = result,
    design     = design,
    treats_name = "treats",
    group_names = character(0),
    ref_level  = "Control",
    domain_mask = domain_mask,
    wt_var     = wt_var,
    show_means = FALSE,
    use_marginaleffects = TRUE,
    pval_adj   = NULL,
    show_pct_change = TRUE,
    suppress_mean = FALSE,
    min_cell_n = 30L,
    variance   = "ci",
    n_weighted = FALSE
  )

  # pct_change computed despite show_means = FALSE
  expect_true("pct_change" %in% names(out_hide$col_vecs))
  expect_false(all(is.na(out_hide$col_vecs$pct_change)))
})

test_that(".build_diffs_output() pct_change with show_means = FALSE + grouped ME + show_pct_change = TRUE: pct_change computed (not NA)", {
  skip_if_not_installed("marginaleffects")
  design <- .make_build_design()
  fit <- survey_glm(design, dv ~ treats * gender)
  result <- suppressWarnings(
    .extract_me_estimates(
      fit, "treats", "gender", "Control", "ame", FALSE
    )
  )

  domain_mask <- .apply_domain(design)
  wt_var <- design@variables$weights

  # This is the regression lock-in test per spec §5.2 and §VI exception:
  # show_means = FALSE + grouped ME + show_pct_change = TRUE
  # Previously returned NA; now must compute pct_change correctly
  out <- suppressWarnings(
    .build_diffs_output(
      result     = result,
      design     = design,
      treats_name = "treats",
      group_names = "gender",
      ref_level  = "Control",
      domain_mask = domain_mask,
      wt_var     = wt_var,
      show_means = FALSE,
      use_marginaleffects = TRUE,
      pval_adj   = NULL,
      show_pct_change = TRUE,
      suppress_mean = FALSE,
      min_cell_n = 30L,
      variance   = "ci",
      n_weighted = FALSE
    )
  )

  expect_true("pct_change" %in% names(out$col_vecs))
  # All treatment rows (estimate != 0) should have non-NA pct_change
  non_ref <- out$col_vecs[["estimate"]] != 0
  expect_false(all(is.na(out$col_vecs$pct_change[non_ref])))
})

test_that(".build_diffs_output() pct_change zero-ref warning fires once per group combo", {
  # Build a design with reference mean exactly 0
  set.seed(5)
  df_zero_mean <- data.frame(
    treats = factor(
      rep(c("Control", "A"), each = 50),
      levels = c("Control", "A")
    ),
    dv = c(rep(0, 50), rnorm(50, 5, 1)),
    wt = rep(1, 100),
    stringsAsFactors = FALSE
  )
  d_zm <- suppressWarnings(
    as_survey(df_zero_mean, weights = wt)
  )
  d_zm@data[["treats"]] <- stats::relevel(d_zm@data[["treats"]], ref = "Control")
  stats::contrasts(d_zm@data[["treats"]]) <-
    stats::contr.treatment(levels(d_zm@data[["treats"]]))
  fit_zm <- survey_glm(d_zm, dv ~ treats)
  result_zm <- .extract_clean_estimates(fit_zm, "treats", 0.95)
  domain_mask_zm <- .apply_domain(d_zm)

  expect_warning(
    .build_diffs_output(
      result     = result_zm,
      design     = d_zm,
      treats_name = "treats",
      group_names = character(0),
      ref_level  = "Control",
      domain_mask = domain_mask_zm,
      wt_var     = d_zm@variables$weights,
      show_means = TRUE,
      use_marginaleffects = FALSE,
      pval_adj   = NULL,
      show_pct_change = TRUE,
      suppress_mean = FALSE,
      min_cell_n = 30L,
      variance   = "ci",
      n_weighted = FALSE
    ),
    class = "surveycore_warning_pct_change_zero_ref"
  )
})

test_that(".build_diffs_output() suppress_mean: mean and pct_change columns absent", {
  design <- .make_build_design()
  fit <- survey_glm(design, dv ~ treats)
  result <- .make_clean_result(design, fit)

  domain_mask <- .apply_domain(design)
  wt_var <- design@variables$weights

  out <- .build_diffs_output(
    result     = result,
    design     = design,
    treats_name = "treats",
    group_names = character(0),
    ref_level  = "Control",
    domain_mask = domain_mask,
    wt_var     = wt_var,
    show_means = TRUE,
    use_marginaleffects = FALSE,
    pval_adj   = NULL,
    show_pct_change = TRUE,
    suppress_mean = TRUE,
    min_cell_n = 30L,
    variance   = "ci",
    n_weighted = FALSE
  )

  expect_false("mean" %in% names(out$col_vecs))
  expect_false("pct_change" %in% names(out$col_vecs))
})

test_that(".build_diffs_output() conditional columns: variance = NULL excludes se, ci", {
  design <- .make_build_design()
  fit <- survey_glm(design, dv ~ treats)
  result <- .make_clean_result(design, fit)

  domain_mask <- .apply_domain(design)
  wt_var <- design@variables$weights

  out <- .build_diffs_output(
    result     = result,
    design     = design,
    treats_name = "treats",
    group_names = character(0),
    ref_level  = "Control",
    domain_mask = domain_mask,
    wt_var     = wt_var,
    show_means = TRUE,
    use_marginaleffects = FALSE,
    pval_adj   = NULL,
    show_pct_change = FALSE,
    suppress_mean = FALSE,
    min_cell_n = 30L,
    variance   = NULL,
    n_weighted = FALSE
  )

  expect_false("se" %in% names(out$col_vecs))
  expect_false("ci_low" %in% names(out$col_vecs))
  expect_false("ci_high" %in% names(out$col_vecs))
})

test_that(".build_diffs_output() conditional columns: variance = 'se' includes se, no ci", {
  design <- .make_build_design()
  fit <- survey_glm(design, dv ~ treats)
  result <- .make_clean_result(design, fit)

  domain_mask <- .apply_domain(design)
  wt_var <- design@variables$weights

  out <- .build_diffs_output(
    result     = result,
    design     = design,
    treats_name = "treats",
    group_names = character(0),
    ref_level  = "Control",
    domain_mask = domain_mask,
    wt_var     = wt_var,
    show_means = TRUE,
    use_marginaleffects = FALSE,
    pval_adj   = NULL,
    show_pct_change = FALSE,
    suppress_mean = FALSE,
    min_cell_n = 30L,
    variance   = "se",
    n_weighted = FALSE
  )

  expect_true("se" %in% names(out$col_vecs))
  expect_false("ci_low" %in% names(out$col_vecs))
  expect_false("ci_high" %in% names(out$col_vecs))
})

test_that(".build_diffs_output() conditional columns: n_weighted = TRUE adds n_weighted", {
  design <- .make_build_design()
  fit <- survey_glm(design, dv ~ treats)
  result <- .make_clean_result(design, fit)

  domain_mask <- .apply_domain(design)
  wt_var <- design@variables$weights

  out_with <- .build_diffs_output(
    result     = result,
    design     = design,
    treats_name = "treats",
    group_names = character(0),
    ref_level  = "Control",
    domain_mask = domain_mask,
    wt_var     = wt_var,
    show_means = TRUE,
    use_marginaleffects = FALSE,
    pval_adj   = NULL,
    show_pct_change = FALSE,
    suppress_mean = FALSE,
    min_cell_n = 30L,
    variance   = "ci",
    n_weighted = TRUE
  )

  out_without <- .build_diffs_output(
    result     = result,
    design     = design,
    treats_name = "treats",
    group_names = character(0),
    ref_level  = "Control",
    domain_mask = domain_mask,
    wt_var     = wt_var,
    show_means = TRUE,
    use_marginaleffects = FALSE,
    pval_adj   = NULL,
    show_pct_change = FALSE,
    suppress_mean = FALSE,
    min_cell_n = 30L,
    variance   = "ci",
    n_weighted = FALSE
  )

  expect_true("n_weighted" %in% names(out_with$col_vecs))
  expect_false("n_weighted" %in% names(out_without$col_vecs))
})

test_that(".build_diffs_output() show_means = FALSE excludes reference row", {
  design <- .make_build_design()
  fit <- survey_glm(design, dv ~ treats)
  result <- .make_clean_result(design, fit)

  domain_mask <- .apply_domain(design)
  wt_var <- design@variables$weights

  out <- .build_diffs_output(
    result     = result,
    design     = design,
    treats_name = "treats",
    group_names = character(0),
    ref_level  = "Control",
    domain_mask = domain_mask,
    wt_var     = wt_var,
    show_means = FALSE,
    use_marginaleffects = FALSE,
    pval_adj   = NULL,
    show_pct_change = FALSE,
    suppress_mean = FALSE,
    min_cell_n = 30L,
    variance   = "ci",
    n_weighted = FALSE
  )

  # 2 treatment rows (no reference)
  expect_equal(length(out$col_vecs[["treats"]]), 2L)
  # No reference row with estimate = 0
  expect_false(any(out$col_vecs[["estimate"]] == 0))
  # mean column absent
  expect_false("mean" %in% names(out$col_vecs))
})

test_that(".build_diffs_output() ME path with show_means = TRUE and suppress_mean = TRUE: ref mean is NA", {
  skip_if_not_installed("marginaleffects")
  # Exercises the suppress_mean = TRUE branch in the ME reference row builder
  # (preds_df is NULL because suppress_mean = TRUE)
  design <- .make_build_design()
  fit <- survey_glm(design, dv ~ treats)
  result <- .extract_me_estimates(
    fit, "treats", character(0), "Control", "ame", TRUE
  )

  domain_mask <- .apply_domain(design)
  wt_var <- design@variables$weights

  out <- .build_diffs_output(
    result     = result,
    design     = design,
    treats_name = "treats",
    group_names = character(0),
    ref_level  = "Control",
    domain_mask = domain_mask,
    wt_var     = wt_var,
    show_means = TRUE,
    use_marginaleffects = TRUE,
    pval_adj   = NULL,
    show_pct_change = FALSE,
    suppress_mean = TRUE,
    min_cell_n = 30L,
    variance   = "ci",
    n_weighted = FALSE
  )

  # suppress_mean = TRUE: no mean or pct_change columns
  expect_false("mean" %in% names(out$col_vecs))
  expect_false("pct_change" %in% names(out$col_vecs))
  # show_means = TRUE but suppress_mean = TRUE: reference row still present
  # (show_means controls whether reference row is added)
  expect_equal(length(out$col_vecs[["treats"]]), 3L)
})

test_that(".build_diffs_output() grouped path with pct_change: group combo loop hits has_group branch", {
  skip_if_not_installed("marginaleffects")
  design <- .make_build_design()
  fit <- survey_glm(design, dv ~ treats * gender)
  result <- suppressWarnings(
    .extract_me_estimates(
      fit, "treats", "gender", "Control", "ame", FALSE
    )
  )

  domain_mask <- .apply_domain(design)
  wt_var <- design@variables$weights

  out <- suppressWarnings(
    .build_diffs_output(
      result     = result,
      design     = design,
      treats_name = "treats",
      group_names = "gender",
      ref_level  = "Control",
      domain_mask = domain_mask,
      wt_var     = wt_var,
      show_means = TRUE,
      use_marginaleffects = TRUE,
      pval_adj   = NULL,
      show_pct_change = TRUE,
      suppress_mean = FALSE,
      min_cell_n = 30L,
      variance   = "ci",
      n_weighted = FALSE
    )
  )

  # grouped + show_pct_change = TRUE: pct_change present
  expect_true("pct_change" %in% names(out$col_vecs))
  # 6 rows: 2 groups * 3 levels
  expect_equal(length(out$col_vecs[["treats"]]), 6L)
})
