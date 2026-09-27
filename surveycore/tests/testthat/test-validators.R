# tests/testthat/test-validators.R
#
# Tests for Layer 2 validator helpers in R/02-validators.R.
#
# Coverage (per plans/error-messages.md):
#   Rows 10, 14, 17, 31, 32, 33, 34, 35, 37
#
# Testing approach (per .claude/rules/testing-standards.md):
#   - Direct tests of internal helpers (constructors not yet implemented)
#   - Happy path + error paths + edge cases per function
#   - expect_error(class = ...) for all error cases
#   - test_invariants() when creating/modifying survey objects


# ── .validate_data_frame ──────────────────────────────────────────────────────

test_that(".validate_data_frame() returns TRUE for a valid data frame", {
  df <- data.frame(x = 1:5, y = rnorm(5))
  expect_true(.validate_data_frame(df))
})

test_that(".validate_data_frame() errors for non-data-frame input", {
  expect_error(
    .validate_data_frame(list(x = 1:5)),
    class = "surveycore_error_not_data_frame"
  )
  expect_error(
    .validate_data_frame(matrix(1:6, nrow = 3)),
    class = "surveycore_error_not_data_frame"
  )
})

test_that(".validate_data_frame() errors for 0-row data frame", {
  empty_df <- data.frame(x = numeric(0))
  expect_error(
    .validate_data_frame(empty_df),
    class = "surveycore_error_empty_data"
  )
})

test_that(".validate_data_frame() errors for duplicate column names", {
  df        <- data.frame(x = 1:3, y = 4:6)
  names(df) <- c("x", "x")
  expect_error(
    .validate_data_frame(df),
    class = "surveycore_error_duplicate_names"
  )
})

test_that(".validate_data_frame() errors for 1-row data frame", {
  single_row <- data.frame(x = 1, y = 2)
  expect_error(
    .validate_data_frame(single_row),
    class = "surveycore_error_single_row"
  )
})


# ── .validate_weights ──────────────────────────────────────────────────────────

test_that(".validate_weights() returns TRUE for valid positive weights", {
  df <- data.frame(x = 1:5, wt = c(1.0, 2.0, 1.5, 3.0, 0.8))
  result <- .validate_weights("wt", df)
  expect_true(result)
})

test_that(".validate_weights() accepts weights with NA values mixed in", {
  # NAs are allowed — only non-NA values need to be positive
  df <- data.frame(x = 1:4, wt = c(1.0, NA_real_, 2.0, NA_real_))
  result <- .validate_weights("wt", df)
  expect_true(result)
})

test_that(".validate_weights() errors when weight column not found in data", {
  df <- data.frame(x = 1:3, other = 1:3)
  expect_error(
    .validate_weights("wt", df),
    class = "surveycore_error_design_var_missing"
  )
})

test_that(".validate_weights() errors when weight column is not numeric", {
  df <- data.frame(x = 1:3, wt = c("a", "b", "c"))
  expect_error(
    .validate_weights("wt", df),
    class = "surveycore_error_weights_not_numeric"
  )
})

test_that(".validate_weights() errors when all weights are zero", {
  df <- data.frame(x = 1:3, wt = c(0, 0, 0))
  expect_error(
    .validate_weights("wt", df),
    class = "surveycore_error_weights_all_zero"
  )
})

test_that(".validate_weights() errors when all weights are NA", {
  df <- data.frame(x = 1:3, wt = c(NA_real_, NA_real_, NA_real_))
  expect_error(
    .validate_weights("wt", df),
    class = "surveycore_error_weights_all_zero"
  )
})

test_that(".validate_weights() errors when weights contain non-positive values", {
  df <- data.frame(x = 1:4, wt = c(1.0, -0.5, 2.0, 0.0))
  expect_error(
    .validate_weights("wt", df),
    class = "surveycore_error_weights_nonpositive"
  )
})

test_that(".validate_weights() catches negative weights via nonpositive error", {
  df <- data.frame(x = 1:3, wt = c(1.0, 1.0, -1.0))
  expect_error(
    .validate_weights("wt", df),
    class = "surveycore_error_weights_nonpositive"
  )
})


# ── .validate_design_vars ─────────────────────────────────────────────────────

test_that(".validate_design_vars() returns TRUE for valid columns", {
  df     <- data.frame(psu = 1:3, strata = c("a", "b", "c"), wt = 1:3)
  result <- .validate_design_vars(c("psu", "strata"), df)
  expect_true(result)
})

test_that(".validate_design_vars() returns TRUE for NULL vars", {
  df     <- data.frame(x = 1:3)
  result <- .validate_design_vars(NULL, df)
  expect_true(result)
})

test_that(".validate_design_vars() returns TRUE for empty character vector", {
  df     <- data.frame(x = 1:3)
  result <- .validate_design_vars(character(0), df)
  expect_true(result)
})

test_that(".validate_design_vars() errors when column not found", {
  df <- data.frame(x = 1:3, strata = c("a", "b", "c"))
  expect_error(
    .validate_design_vars(c("psu", "strata"), df),
    class = "surveycore_error_design_var_missing"
  )
})

test_that(".validate_design_vars() errors when multiple columns not found", {
  df <- data.frame(x = 1:3)
  expect_error(
    .validate_design_vars(c("psu", "strata", "fpc"), df),
    class = "surveycore_error_design_var_missing"
  )
})

test_that(".validate_design_vars() errors when column is a list-column", {
  df      <- data.frame(x = 1:3)
  df$psu  <- list(1, 2, 3)
  expect_error(
    .validate_design_vars("psu", df),
    class = "surveycore_error_design_var_list"
  )
})

test_that(".validate_design_vars() checks existence before list-column check", {
  # A missing column should give design_var_missing, not design_var_list
  df <- data.frame(x = 1:3)
  expect_error(
    .validate_design_vars("psu", df),
    class = "surveycore_error_design_var_missing"
  )
})


# ── .validate_fpc ─────────────────────────────────────────────────────────────

test_that(".validate_fpc() returns TRUE for valid FPC column", {
  df     <- data.frame(x = 1:3, fpc = c(1000L, 1000L, 1000L))
  result <- .validate_fpc("fpc", df)
  expect_true(result)
})

test_that(".validate_fpc() returns TRUE for NULL fpc_var", {
  df     <- data.frame(x = 1:3)
  result <- .validate_fpc(NULL, df)
  expect_true(result)
})

test_that(".validate_fpc() errors when FPC column not found", {
  df <- data.frame(x = 1:3)
  expect_error(
    .validate_fpc("fpc", df),
    class = "surveycore_error_design_var_missing"
  )
})

test_that(".validate_fpc() errors when FPC column contains NA", {
  df <- data.frame(x = 1:3, fpc = c(1000L, NA_integer_, 1000L))
  expect_error(
    .validate_fpc("fpc", df),
    class = "surveycore_error_fpc_na"
  )
})

test_that(".validate_fpc() errors when all FPC values are NA", {
  df <- data.frame(
    x   = 1:3,
    fpc = c(NA_integer_, NA_integer_, NA_integer_)
  )
  expect_error(
    .validate_fpc("fpc", df),
    class = "surveycore_error_fpc_na"
  )
})


# ── .validate_repweights ───────────────────────────────────────────────────────

test_that(".validate_repweights() returns TRUE for valid numeric repweights", {
  df     <- make_survey_data(n = 100, n_psu = 10, design = "replicate", seed = 1L)
  rw_cols <- paste0("repwt_", seq_len(5))
  result  <- .validate_repweights(rw_cols, df)
  expect_true(result)
})

test_that(".validate_repweights() returns TRUE for NULL repweights_vars", {
  df     <- data.frame(x = 1:3)
  result <- .validate_repweights(NULL, df)
  expect_true(result)
})

test_that(".validate_repweights() returns TRUE for empty repweights_vars", {
  df     <- data.frame(x = 1:3)
  result <- .validate_repweights(character(0), df)
  expect_true(result)
})

test_that(".validate_repweights() errors when repweight column not found", {
  df <- data.frame(x = 1:3, repwt_1 = c(1.0, 1.1, 0.9))
  expect_error(
    .validate_repweights(c("repwt_1", "repwt_2"), df),
    class = "surveycore_error_design_var_missing"
  )
})

test_that(".validate_repweights() errors when repweight column is not numeric", {
  df <- data.frame(
    x      = 1:3,
    repwt_1 = c("a", "b", "c"),
    repwt_2 = c(1.0, 1.1, 0.9)
  )
  expect_error(
    .validate_repweights(c("repwt_1", "repwt_2"), df),
    class = "surveycore_error_repweights_not_numeric"
  )
})

test_that(".validate_repweights() reports ALL non-numeric columns in one error", {
  # Multiple bad columns: should all appear in the single error, not one by one
  df <- data.frame(
    x      = 1:3,
    repwt_1 = c("a", "b", "c"),  # character
    repwt_2 = c(1.0, 1.1, 0.9), # numeric — OK
    repwt_3 = c(TRUE, FALSE, TRUE) # logical — not numeric
  )
  err <- tryCatch(
    .validate_repweights(c("repwt_1", "repwt_2", "repwt_3"), df),
    error = function(e) e
  )
  expect_s3_class(err, "surveycore_error_repweights_not_numeric")
  # Both bad columns should be mentioned (not just the first one)
  expect_match(conditionMessage(err), "repwt_1")
  expect_match(conditionMessage(err), "repwt_3")
})


# ── .validate_psu_strata ───────────────────────────────────────────────────────

test_that(".validate_psu_strata() returns TRUE when no PSU-stratum overlap", {
  # Each PSU appears in exactly one stratum
  df <- data.frame(
    psu    = c("p1", "p1", "p2", "p2", "p3", "p3"),
    strata = c("s1", "s1", "s1", "s1", "s2", "s2")
  )
  result <- .validate_psu_strata("psu", "strata", df)
  expect_true(result)
})

test_that(".validate_psu_strata() returns TRUE for NULL ids", {
  df     <- data.frame(psu = 1:3, strata = c("a", "b", "c"))
  result <- .validate_psu_strata(NULL, "strata", df)
  expect_true(result)
})

test_that(".validate_psu_strata() returns TRUE for NULL strata", {
  df     <- data.frame(psu = 1:3, strata = c("a", "b", "c"))
  result <- .validate_psu_strata("psu", NULL, df)
  expect_true(result)
})

test_that(".validate_psu_strata() warns when PSU appears in multiple strata", {
  # PSU "p1" appears in both "s1" and "s2"
  df <- data.frame(
    psu    = c("p1", "p1", "p2", "p2"),
    strata = c("s1", "s2", "s1", "s1")
  )
  expect_warning(
    .validate_psu_strata("psu", "strata", df),
    class = "surveycore_warning_psu_multi_strata"
  )
})

test_that(".validate_psu_strata() still returns TRUE after warning", {
  df <- data.frame(
    psu    = c("p1", "p1", "p2"),
    strata = c("s1", "s2", "s1")
  )
  result <- suppressWarnings(
    .validate_psu_strata("psu", "strata", df)
  )
  expect_true(result)
})


# ── .validate_rscales ─────────────────────────────────────────────────────────

test_that(".validate_rscales() returns TRUE for correct-length rscales", {
  result <- .validate_rscales(rep(1, 10), n_rep = 10L)
  expect_true(result)
})

test_that(".validate_rscales() returns TRUE for NULL rscales", {
  result <- .validate_rscales(NULL, n_rep = 10L)
  expect_true(result)
})

test_that(".validate_rscales() errors when rscales length mismatches n_rep", {
  expect_error(
    .validate_rscales(rep(1, 5), n_rep = 10L),
    class = "surveycore_error_rscales_length"
  )
})

test_that(".validate_rscales() errors for length too long", {
  expect_error(
    .validate_rscales(rep(1, 15), n_rep = 10L),
    class = "surveycore_error_rscales_length"
  )
})

test_that(".validate_rscales() errors for NA values via as_survey_nonprob()", {
  df <- data.frame(y = 1:20, w = rep(1, 20), rw1 = runif(20), rw2 = runif(20))
  expect_error(
    as_survey_nonprob(
      df,
      weights = w,
      repweights = c(rw1, rw2),
      rscales = c(1, NA_real_)
    ),
    class = "surveycore_error_rscales_na"
  )
  expect_snapshot(
    error = TRUE,
    as_survey_nonprob(
      df,
      weights = w,
      repweights = c(rw1, rw2),
      rscales = c(1, NA_real_)
    )
  )
})

test_that(".validate_rscales() errors for negative values via as_survey_nonprob()", {
  df <- data.frame(y = 1:20, w = rep(1, 20), rw1 = runif(20), rw2 = runif(20))
  expect_error(
    as_survey_nonprob(
      df,
      weights = w,
      repweights = c(rw1, rw2),
      rscales = c(1, -0.5)
    ),
    class = "surveycore_error_rscales_na"
  )
  expect_snapshot(
    error = TRUE,
    as_survey_nonprob(
      df,
      weights = w,
      repweights = c(rw1, rw2),
      rscales = c(1, -0.5)
    )
  )
})

test_that(".validate_rscales() errors for NA values via as_survey_replicate()", {
  df_r <- make_survey_data(design = "replicate", type = "BRR", seed = 101)
  repwt_cols <- grep("^repwt_", names(df_r), value = TRUE)
  bad_rscales <- rep(1, length(repwt_cols))
  bad_rscales[1] <- NA_real_

  expect_error(
    as_survey_replicate(
      df_r,
      weights = wt,
      repweights = all_of(repwt_cols),
      type = "BRR",
      rscales = bad_rscales
    ),
    class = "surveycore_error_rscales_na"
  )
  expect_snapshot(
    error = TRUE,
    as_survey_replicate(
      df_r,
      weights = wt,
      repweights = all_of(repwt_cols),
      type = "BRR",
      rscales = bad_rscales
    )
  )
})


# ── .update_design_var_names ───────────────────────────────────────────────────
#
# These tests work on the @variables list directly (not on a full survey object)
# because the rename operation requires atomic reconstruction: @data and
# @variables must be consistent at the moment the S7 validator fires. The
# helper returns an updated list; the rename method is responsible for
# combining the new list with new @data in a fresh constructor call.

test_that(".update_design_var_names() renames weights in a variables list", {
  vars <- list(
    ids           = NULL,
    weights       = "old_wt",
    strata        = NULL,
    fpc           = NULL,
    nest          = FALSE,
    probs_provided = FALSE
  )
  result <- .update_design_var_names(vars, c(old_wt = "weight"))
  expect_identical(result$weights, "weight")
})

test_that(".update_design_var_names() renames strata and fpc in a variables list", {
  vars <- list(
    ids           = NULL,
    weights       = "wt",
    strata        = "old_st",
    fpc           = "old_fp",
    nest          = FALSE,
    probs_provided = FALSE
  )
  result <- .update_design_var_names(vars, c(old_st = "strata", old_fp = "fpc"))
  expect_identical(result$strata, "strata")
  expect_identical(result$fpc, "fpc")
})

test_that(".update_design_var_names() renames ids in a variables list", {
  vars <- list(
    ids           = c("old_psu", "old_ssu"),
    weights       = "wt",
    strata        = NULL,
    fpc           = NULL,
    nest          = FALSE,
    probs_provided = FALSE
  )
  result <- .update_design_var_names(vars, c(old_psu = "psu"))
  expect_identical(result$ids, c("psu", "old_ssu"))
})

test_that(".update_design_var_names() renames repweights in a variables list", {
  vars <- list(
    weights    = "wt",
    repweights = c("repwt_1", "repwt_2", "repwt_3"),
    type       = "BRR",
    scale      = 1,
    rscales    = NULL,
    fpc        = NULL,
    fpctype    = "fraction",
    mse        = TRUE
  )
  result <- .update_design_var_names(vars, c(repwt_1 = "rep_1"))
  expect_true("rep_1" %in% result$repweights)
  expect_false("repwt_1" %in% result$repweights)
  # Other repweights unchanged
  expect_true("repwt_2" %in% result$repweights)
})

test_that(".update_design_var_names() leaves unmatched columns unchanged", {
  vars <- list(
    ids           = NULL,
    weights       = "wt",
    strata        = "stratum",
    fpc           = NULL,
    nest          = FALSE,
    probs_provided = FALSE
  )
  # Rename a non-design column — nothing in @variables should change
  result <- .update_design_var_names(vars, c(outcome_y = "y"))
  expect_identical(result$weights, "wt")
  expect_identical(result$strata, "stratum")
})


# ── .rename_metadata_keys ─────────────────────────────────────────────────────

test_that(".rename_metadata_keys() renames variable_labels keys", {
  m <- survey_metadata(
    variable_labels = list(old_age = "Age in years", income = "Annual income")
  )
  m2 <- .rename_metadata_keys(m, c(old_age = "age"))
  expect_true("age" %in% names(m2@variable_labels))
  expect_false("old_age" %in% names(m2@variable_labels))
  expect_identical(m2@variable_labels[["age"]], "Age in years")
})

test_that(".rename_metadata_keys() renames value_labels keys", {
  m <- survey_metadata(
    value_labels = list(old_sex = c(Male = 1L, Female = 2L))
  )
  m2 <- .rename_metadata_keys(m, c(old_sex = "sex"))
  expect_true("sex" %in% names(m2@value_labels))
  expect_false("old_sex" %in% names(m2@value_labels))
})

test_that(".rename_metadata_keys() renames keys in all metadata slots", {
  m <- survey_metadata(
    variable_labels   = list(old_y = "Outcome"),
    value_labels      = list(old_y = c(No = 0L, Yes = 1L)),
    question_prefaces = list(old_y = "For each item..."),
    notes             = list(old_y = "Analyst note")
  )
  m2 <- .rename_metadata_keys(m, c(old_y = "y"))
  expect_true("y" %in% names(m2@variable_labels))
  expect_true("y" %in% names(m2@value_labels))
  expect_true("y" %in% names(m2@question_prefaces))
  expect_true("y" %in% names(m2@notes))
})

test_that(".rename_metadata_keys() leaves unmatched keys unchanged", {
  m <- survey_metadata(
    variable_labels = list(age = "Age in years", income = "Annual income")
  )
  m2 <- .rename_metadata_keys(m, c(age = "respondent_age"))
  expect_true("respondent_age" %in% names(m2@variable_labels))
  expect_true("income" %in% names(m2@variable_labels))
  expect_false("age" %in% names(m2@variable_labels))
})

test_that(".rename_metadata_keys() handles empty metadata without error", {
  m      <- survey_metadata()
  result <- .rename_metadata_keys(m, c(x = "y"))
  expect_true(S7::S7_inherits(result, survey_metadata))
  expect_identical(length(result@variable_labels), 0L)
})
