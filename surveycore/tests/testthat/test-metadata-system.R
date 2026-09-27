# tests/testthat/test-metadata-system.R
#
# Tests for R/01-metadata-system.R
# Error/warning coverage map (plans/error-messages.md):
#   Row 27: surveycore_error_var_not_found   — singular setters
#   Row 28: surveycore_error_vars_not_found  — plural setters
#   Row 29: surveycore_error_labels_unnamed  — set_val_labels / set_value_labels
#   Row 30: surveycore_warning_missing_labels — set_val_labels / set_value_labels

# ── Test fixtures ─────────────────────────────────────────────────────────────

# Minimal survey_taylor for metadata tests (constructed directly because
# as_survey() is not implemented until Component 3).
make_design <- function() {
  df <- data.frame(
    age = c(25L, 30L, 45L, 50L, 35L),
    sex = c(1L, 2L, 1L, 2L, 1L),
    income = c(40000, 80000, 60000, 90000, 55000),
    wt = c(1.2, 0.9, 1.1, 1.0, 1.3),
    stringsAsFactors = FALSE
  )
  survey_taylor(
    data = df,
    variables = list(
      ids = NULL,
      weights = "wt",
      strata = NULL,
      fpc = NULL,
      nest = FALSE,
      probs_provided = FALSE
    )
  )
}

# Design with labels, universe, and missing codes for integration tests.
# Uses new unified setters — works after Steps 3.4–3.11 are complete.
make_labeled_design <- function(seed = 42) {
  df <- make_survey_data(n = 100, seed = seed)
  svy <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  svy <- set_var_label(svy, y1 = "Outcome 1", y2 = "Outcome 2")
  svy <- set_val_labels(svy, y3 = c(No = 0L, Yes = 1L))
  svy <- set_universe(svy, y1 = "All respondents")
  svy <- set_missing_codes(svy, y1 = c("Missing" = -1L))
  svy
}


# ── extract_var_label() ───────────────────────────────────────────────────────

test_that("extract_var_label() single variable returns named character vector (not scalar)", {
  d <- make_labeled_design()
  result <- extract_var_label(d, y1)
  expect_identical(result, c(y1 = "Outcome 1"))
})

test_that("extract_var_label() multiple variables returns named char vector with all names", {
  d <- make_labeled_design()
  result <- extract_var_label(d, y1, y2)
  expect_identical(result, c(y1 = "Outcome 1", y2 = "Outcome 2"))
})

test_that("extract_var_label() no var arg returns metadata for all variables", {
  d <- make_labeled_design()
  result <- extract_var_label(d)
  expect_true("y1" %in% names(result))
  expect_true("y2" %in% names(result))
  expect_identical(result[["y1"]], "Outcome 1")
  expect_identical(result[["y2"]], "Outcome 2")
})

test_that("extract_var_label() no var arg returns empty char(0) when no labels set", {
  d <- make_design()
  result <- extract_var_label(d)
  expect_identical(result, character(0))
})

test_that("extract_var_label() format = 'named_vector' (default) returns named char vector", {
  d <- make_labeled_design()
  result <- extract_var_label(d, y1, format = "named_vector")
  expect_identical(result, c(y1 = "Outcome 1"))
  expect_true(is.character(result))
})

test_that("extract_var_label() format = 'list' returns named list", {
  d <- make_labeled_design()
  result <- extract_var_label(d, y1, format = "list")
  expect_identical(result, list(y1 = "Outcome 1"))
})

test_that("extract_var_label() format = 'data_frame' returns tibble with variable/label columns", {
  d <- make_labeled_design()
  result <- extract_var_label(d, y1, y2, format = "data_frame")
  expect_true(inherits(result, "tbl_df"))
  expect_named(result, c("variable", "label"))
  expect_identical(result$variable, c("y1", "y2"))
  expect_identical(result$label, c("Outcome 1", "Outcome 2"))
})

test_that("extract_var_label() format = 'data_frame' empty result: zero-row tibble with correct types", {
  d <- make_design()
  result <- extract_var_label(d, format = "data_frame")
  expect_true(inherits(result, "tbl_df"))
  expect_named(result, c("variable", "label"))
  expect_equal(nrow(result), 0L)
  expect_true(is.character(result$variable))
  expect_true(is.character(result$label))
})

test_that("extract_var_label() fill = NULL (default): unlabeled variables omitted", {
  d <- make_labeled_design()
  result <- extract_var_label(d, y1, y3)
  expect_named(result, "y1")
  expect_false("y3" %in% names(result))
})

test_that("extract_var_label() fill = NA_character_: unlabeled variables included with NA", {
  d <- make_labeled_design()
  result <- extract_var_label(d, y1, y3, fill = NA_character_)
  expect_named(result, c("y1", "y3"))
  expect_identical(result[["y1"]], "Outcome 1")
  expect_identical(result[["y3"]], NA_character_)
})

test_that("extract_var_label() fill = NA_character_ in 'list' format: NA_character_ list entry", {
  d <- make_labeled_design()
  result <- extract_var_label(d, y1, y3, format = "list", fill = NA_character_)
  expect_identical(result, list(y1 = "Outcome 1", y3 = NA_character_))
})

test_that("extract_var_label() mix: some labeled, some not — fill = NULL omits unset", {
  d <- make_labeled_design()
  result <- extract_var_label(d, y1, y2, y3)
  expect_named(result, c("y1", "y2"))
})

test_that("extract_var_label() data frame: reads attr(df[[var]], 'label')", {
  df <- data.frame(age = 1:5, sex = c(1L, 2L, 1L, 2L, 1L))
  attr(df$age, "label") <- "Age in years"
  result <- extract_var_label(df, age)
  expect_identical(result, c(age = "Age in years"))
})

test_that("extract_var_label() data frame: returns same structure as for survey objects", {
  df <- data.frame(age = 1:5, sex = c(1L, 2L, 1L, 2L, 1L))
  attr(df$age, "label") <- "Age in years"
  result <- extract_var_label(df, age, format = "data_frame")
  expect_true(inherits(result, "tbl_df"))
  expect_named(result, c("variable", "label"))
  expect_identical(result$variable, "age")
  expect_identical(result$label, "Age in years")
})

test_that("extract_var_label() errors with surveycore_error_not_survey_or_df for list x", {
  expect_error(
    extract_var_label(list(a = 1), a),
    class = "surveycore_error_not_survey_or_df"
  )
})

test_that("extract_var_label() errors with surveycore_error_format_invalid for invalid format", {
  d <- make_labeled_design()
  expect_error(
    extract_var_label(d, format = "tibble"),
    class = "surveycore_error_format_invalid"
  )
})

test_that("extract_var_label() errors for bare nonexistent column (tidyselect behavior)", {
  d <- make_labeled_design()
  expect_error(extract_var_label(d, nonexistent))
})

test_that("extract_var_label() any_of() silently omits missing columns", {
  d <- make_labeled_design()
  result <- extract_var_label(d, any_of(c("y1", "nonexistent")))
  expect_named(result, "y1")
})

test_that("extract_var_label() errors with surveycore_error_fill_invalid for fill = 'include'", {
  d <- make_labeled_design()
  expect_error(
    extract_var_label(d, fill = "include"),
    class = "surveycore_error_fill_invalid"
  )
})

test_that("snapshot: extract_var_label() surveycore_error_format_invalid", {
  d <- make_labeled_design()
  expect_snapshot(error = TRUE, extract_var_label(d, format = "tibble"))
})


# ── extract_val_labels() ──────────────────────────────────────────────────────

test_that("extract_val_labels() single variable returns named list (not bare named vector)", {
  d <- make_labeled_design()
  result <- extract_val_labels(d, y3)
  expect_true(is.list(result))
  expect_named(result, "y3")
  expect_identical(result$y3, c(No = 0L, Yes = 1L))
})

test_that("extract_val_labels() format = 'list' (default) returns named list", {
  d <- make_labeled_design()
  result <- extract_val_labels(d, y3, format = "list")
  expect_true(is.list(result))
  expect_named(result, "y3")
})

test_that("extract_val_labels() format = 'data_frame' returns long tibble with variable/label/value cols", {
  d <- make_labeled_design()
  result <- extract_val_labels(d, y3, format = "data_frame")
  expect_true(inherits(result, "tbl_df"))
  expect_named(result, c("variable", "label", "value"))
  expect_equal(nrow(result), 2L)
})

test_that("extract_val_labels() format = 'data_frame' coerces codes to character", {
  d <- make_labeled_design()
  result <- extract_val_labels(d, y3, format = "data_frame")
  expect_true(is.character(result$value))
})

test_that("extract_val_labels() format = 'named_vector' errors with surveycore_error_format_invalid", {
  d <- make_labeled_design()
  expect_error(
    extract_val_labels(d, format = "named_vector"),
    class = "surveycore_error_format_invalid"
  )
})

test_that("extract_val_labels() fill = NA_character_ in 'list' format: NULL entries (not NA)", {
  d <- make_labeled_design()
  result <- extract_val_labels(d, y1, fill = NA_character_)
  expect_true(is.list(result))
  expect_named(result, "y1")
  expect_null(result$y1)
})

test_that("extract_val_labels() data frame: reads attr(df[[var]], 'labels')", {
  df <- data.frame(sex = c(1L, 2L, 1L))
  attr(df$sex, "labels") <- c(Male = 1L, Female = 2L)
  result <- extract_val_labels(df, sex)
  expect_true(is.list(result))
  expect_named(result, "sex")
  expect_identical(result$sex, c(Male = 1L, Female = 2L))
})


# ── extract_question_preface() ────────────────────────────────────────────────

test_that("extract_question_preface() single variable returns named character vector", {
  d <- make_design()
  d <- set_question_preface(d, age = "How old are you?")
  result <- extract_question_preface(d, age)
  expect_identical(result, c(age = "How old are you?"))
})

test_that("extract_question_preface() no var arg returns only set variables (fill = NULL)", {
  d <- make_design()
  d <- set_question_preface(d, age = "How old are you?")
  result <- extract_question_preface(d)
  expect_named(result, "age")
})

test_that("extract_question_preface() format = 'data_frame' returns tibble with variable/preface cols", {
  d <- make_design()
  d <- set_question_preface(d, age = "How old are you?")
  result <- extract_question_preface(d, age, format = "data_frame")
  expect_true(inherits(result, "tbl_df"))
  expect_named(result, c("variable", "preface"))
  expect_identical(result$variable, "age")
  expect_identical(result$preface, "How old are you?")
})

test_that("extract_question_preface() format = 'list' returns named list", {
  d <- make_design()
  d <- set_question_preface(d, age = "How old are you?")
  result <- extract_question_preface(d, age, format = "list")
  expect_identical(result, list(age = "How old are you?"))
})

test_that("extract_question_preface() fill = NA_character_ includes unlabeled variables", {
  d <- make_design()
  d <- set_question_preface(d, age = "How old are you?")
  result <- extract_question_preface(d, age, income, fill = NA_character_)
  expect_named(result, c("age", "income"))
  expect_identical(result[["income"]], NA_character_)
})

test_that("extract_question_preface() data frame: reads attr(df[[var]], 'question_preface')", {
  df <- data.frame(q1 = c(1, 2, 3))
  attr(df$q1, "question_preface") <- "How do you feel?"
  result <- extract_question_preface(df, q1)
  expect_identical(result, c(q1 = "How do you feel?"))
})

test_that("extract_question_preface() errors with surveycore_error_not_survey_or_df for list x", {
  expect_error(
    extract_question_preface(list(a = 1), a),
    class = "surveycore_error_not_survey_or_df"
  )
})

test_that("extract_question_preface() errors with surveycore_error_format_invalid for invalid format", {
  d <- make_design()
  expect_error(
    extract_question_preface(d, format = "tibble"),
    class = "surveycore_error_format_invalid"
  )
})

test_that("extract_question_preface() errors for bare nonexistent column (tidyselect behavior)", {
  d <- make_design()
  expect_error(extract_question_preface(d, nonexistent))
})

test_that("extract_question_preface() errors with surveycore_error_fill_invalid for fill = 'include'", {
  d <- make_design()
  expect_error(
    extract_question_preface(d, fill = "include"),
    class = "surveycore_error_fill_invalid"
  )
})


# ── extract_var_note() ────────────────────────────────────────────────────────

test_that("extract_var_note() single variable returns named character vector", {
  d <- make_design()
  d <- set_var_note(d, age = "Imputed.")
  result <- extract_var_note(d, age)
  expect_identical(result, c(age = "Imputed."))
})

test_that("extract_var_note() no var arg returns only set variables (fill = NULL)", {
  d <- make_design()
  d <- set_var_note(d, age = "Imputed.")
  result <- extract_var_note(d)
  expect_named(result, "age")
})

test_that("extract_var_note() format = 'data_frame' returns tibble with variable/note cols", {
  d <- make_design()
  d <- set_var_note(d, age = "Imputed.")
  result <- extract_var_note(d, age, format = "data_frame")
  expect_true(inherits(result, "tbl_df"))
  expect_named(result, c("variable", "note"))
  expect_identical(result$variable, "age")
  expect_identical(result$note, "Imputed.")
})

test_that("extract_var_note() format = 'list' returns named list", {
  d <- make_design()
  d <- set_var_note(d, age = "Imputed.")
  result <- extract_var_note(d, age, format = "list")
  expect_identical(result, list(age = "Imputed."))
})

test_that("extract_var_note() fill = NA_character_ includes unlabeled variables", {
  d <- make_design()
  d <- set_var_note(d, age = "Imputed.")
  result <- extract_var_note(d, age, income, fill = NA_character_)
  expect_named(result, c("age", "income"))
  expect_identical(result[["income"]], NA_character_)
})

test_that("extract_var_note() data frame: reads attr(df[[var]], 'note')", {
  df <- data.frame(y = c(1, 2, 3))
  attr(df$y, "note") <- "Top-coded at 200."
  result <- extract_var_note(df, y)
  expect_identical(result, c(y = "Top-coded at 200."))
})

test_that("extract_var_note() errors with surveycore_error_not_survey_or_df for list x", {
  expect_error(
    extract_var_note(list(a = 1), a),
    class = "surveycore_error_not_survey_or_df"
  )
})

test_that("extract_var_note() errors with surveycore_error_format_invalid for invalid format", {
  d <- make_design()
  expect_error(
    extract_var_note(d, format = "tibble"),
    class = "surveycore_error_format_invalid"
  )
})

test_that("extract_var_note() errors for bare nonexistent column (tidyselect behavior)", {
  d <- make_design()
  expect_error(extract_var_note(d, nonexistent))
})

test_that("extract_var_note() errors with surveycore_error_fill_invalid for fill = 'include'", {
  d <- make_design()
  expect_error(
    extract_var_note(d, fill = "include"),
    class = "surveycore_error_fill_invalid"
  )
})


# ── extract_universe() ────────────────────────────────────────────────────────

test_that("extract_universe() single variable returns named character vector", {
  d <- make_labeled_design()
  result <- extract_universe(d, y1)
  expect_identical(result, c(y1 = "All respondents"))
})

test_that("extract_universe() no var arg returns only variables with universe set", {
  d <- make_labeled_design()
  result <- extract_universe(d)
  expect_named(result, "y1")
})

test_that("extract_universe() format = 'data_frame' returns tibble with variable/universe cols", {
  d <- make_labeled_design()
  result <- extract_universe(d, y1, format = "data_frame")
  expect_true(inherits(result, "tbl_df"))
  expect_named(result, c("variable", "universe"))
  expect_identical(result$variable, "y1")
  expect_identical(result$universe, "All respondents")
})

test_that("extract_universe() format = 'list' returns named list", {
  d <- make_labeled_design()
  result <- extract_universe(d, y1, format = "list")
  expect_identical(result, list(y1 = "All respondents"))
})

test_that("extract_universe() fill = NA_character_ includes variables without universe", {
  d <- make_labeled_design()
  result <- extract_universe(d, y1, y2, fill = NA_character_)
  expect_named(result, c("y1", "y2"))
  expect_identical(result[["y2"]], NA_character_)
})

test_that("extract_universe() data frame: reads attr(df[[var]], 'universe')", {
  df <- data.frame(vote = c(1L, 2L, 3L))
  attr(df$vote, "universe") <- "Registered voters only"
  result <- extract_universe(df, vote)
  expect_identical(result, c(vote = "Registered voters only"))
})

test_that("extract_universe() errors with surveycore_error_not_survey_or_df for list x", {
  expect_error(
    extract_universe(list(a = 1), a),
    class = "surveycore_error_not_survey_or_df"
  )
})

test_that("extract_universe() errors with surveycore_error_format_invalid for invalid format", {
  d <- make_labeled_design()
  expect_error(
    extract_universe(d, format = "tibble"),
    class = "surveycore_error_format_invalid"
  )
})

test_that("extract_universe() errors for bare nonexistent column (tidyselect behavior)", {
  d <- make_labeled_design()
  expect_error(extract_universe(d, nonexistent))
})

test_that("extract_universe() errors with surveycore_error_fill_invalid for fill = 'include'", {
  d <- make_labeled_design()
  expect_error(
    extract_universe(d, fill = "include"),
    class = "surveycore_error_fill_invalid"
  )
})


# ── extract_missing_codes() ───────────────────────────────────────────────────

test_that("extract_missing_codes() single variable returns named list", {
  d <- make_labeled_design()
  result <- extract_missing_codes(d, y1)
  expect_true(is.list(result))
  expect_named(result, "y1")
  expect_identical(result$y1, c("Missing" = -1L))
})

test_that("extract_missing_codes() format = 'list' (default): named list with named codes preserved", {
  d <- make_labeled_design()
  result <- extract_missing_codes(d, y1, format = "list")
  expect_identical(result, list(y1 = c("Missing" = -1L)))
})

test_that("extract_missing_codes() format = 'data_frame': long tibble with variable/description/code cols", {
  d <- make_labeled_design()
  result <- extract_missing_codes(d, y1, format = "data_frame")
  expect_true(inherits(result, "tbl_df"))
  expect_named(result, c("variable", "description", "code"))
  expect_identical(result$variable, "y1")
  expect_identical(result$description, "Missing")
  expect_identical(result$code, "-1")
})

test_that("extract_missing_codes() format = 'data_frame': description = NA when codes vector is unnamed", {
  d <- make_design()
  d <- set_missing_codes(d, age = c(-1L, -2L))
  result <- extract_missing_codes(d, age, format = "data_frame")
  expect_true(all(is.na(result$description)))
  expect_identical(result$code, c("-1", "-2"))
})

test_that("extract_missing_codes() format = 'named_vector' errors with surveycore_error_format_invalid", {
  d <- make_labeled_design()
  expect_error(
    extract_missing_codes(d, format = "named_vector"),
    class = "surveycore_error_format_invalid"
  )
})

test_that("extract_missing_codes() fill = NA_character_ in 'list' format: NULL entries", {
  d <- make_labeled_design()
  result <- extract_missing_codes(d, y1, y2, fill = NA_character_)
  expect_named(result, c("y1", "y2"))
  expect_null(result$y2)
  expect_identical(result$y1, c("Missing" = -1L))
})

test_that("extract_missing_codes() data frame: reads attr(df[[var]], 'missing_codes')", {
  df <- data.frame(q5 = c(1L, 99L, 98L))
  attr(df$q5, "missing_codes") <- c("Refused" = 99L, "DK" = 98L)
  result <- extract_missing_codes(df, q5)
  expect_identical(result, list(q5 = c("Refused" = 99L, "DK" = 98L)))
})


# ── set_var_label() ───────────────────────────────────────────────────────────

test_that("set_var_label() convention 1 sets label for one variable", {
  d <- make_design()
  d <- set_var_label(d, age = "Age in years")
  expect_identical(d@metadata@variable_labels[["age"]], "Age in years")
})

test_that("set_var_label() convention 1 sets labels for multiple variables", {
  d <- make_design()
  d <- set_var_label(d, age = "Age in years", income = "Annual income")
  expect_identical(d@metadata@variable_labels[["age"]], "Age in years")
  expect_identical(d@metadata@variable_labels[["income"]], "Annual income")
})

test_that("set_var_label() convention 1 with !!! splicing sets labels", {
  d <- make_design()
  lbls <- list(age = "Age in years", income = "Annual income")
  d <- set_var_label(d, !!!lbls)
  expect_identical(d@metadata@variable_labels[["age"]], "Age in years")
  expect_identical(d@metadata@variable_labels[["income"]], "Annual income")
})

test_that("set_var_label() convention 2 (named char vector in ...) sets labels", {
  d <- make_design()
  d <- set_var_label(d, c(age = "Age in years", income = "Annual income"))
  expect_identical(d@metadata@variable_labels[["age"]], "Age in years")
  expect_identical(d@metadata@variable_labels[["income"]], "Annual income")
})

test_that("set_var_label() convention 3 (variable + label) sets one label", {
  d <- make_design()
  d <- set_var_label(d, variable = "age", label = "Age in years")
  expect_identical(d@metadata@variable_labels[["age"]], "Age in years")
})

test_that("set_var_label() convention 3 (variable + label) sets multiple labels", {
  d <- make_design()
  d <- set_var_label(
    d,
    variable = c("age", "income"),
    label = c("Age in years", "Annual income")
  )
  expect_identical(d@metadata@variable_labels[["age"]], "Age in years")
  expect_identical(d@metadata@variable_labels[["income"]], "Annual income")
})

test_that("set_var_label() returns x invisibly", {
  d <- make_design()
  result <- withVisible(set_var_label(d, age = "Age in years"))
  expect_false(result$visible)
  expect_identical(
    result$value@metadata@variable_labels[["age"]],
    "Age in years"
  )
})

test_that("set_var_label() survives pipe chain of three calls", {
  d <- make_design() |>
    set_var_label(age = "Age") |>
    set_var_label(sex = "Sex") |>
    set_var_label(income = "Income")
  expect_identical(d@metadata@variable_labels[["age"]], "Age")
  expect_identical(d@metadata@variable_labels[["sex"]], "Sex")
  expect_identical(d@metadata@variable_labels[["income"]], "Income")
})

test_that("set_var_label() NULL label deletes the existing entry", {
  d <- make_design()
  d <- set_var_label(d, age = "Age in years")
  d <- set_var_label(d, age = NULL)
  expect_false("age" %in% names(d@metadata@variable_labels))
})

test_that("set_var_label() data frame: sets attr(df$age, 'label')", {
  df <- data.frame(age = 1:3, wt = 1:3)
  df2 <- set_var_label(df, age = "Age in years")
  expect_identical(attr(df2$age, "label"), "Age in years")
})

test_that("set_var_label() does not modify other metadata", {
  d <- make_design()
  d <- set_val_labels(d, sex = c(Male = 1L, Female = 2L))
  d <- set_var_label(d, age = "Age in years")
  expect_identical(d@metadata@value_labels[["sex"]], c(Male = 1L, Female = 2L))
})

test_that("set_var_label() errors with surveycore_error_not_survey_or_df for list x", {
  expect_error(
    set_var_label(list(x = 1), age = "A"),
    class = "surveycore_error_not_survey_or_df"
  )
})

test_that("set_var_label() errors with surveycore_error_setter_ambiguous", {
  d <- make_design()
  expect_error(
    set_var_label(d, age = "A", variable = "income"),
    class = "surveycore_error_setter_ambiguous"
  )
})

test_that("set_var_label() errors with surveycore_error_setter_empty", {
  d <- make_design()
  expect_error(
    set_var_label(d),
    class = "surveycore_error_setter_empty"
  )
})

test_that("set_var_label() errors with surveycore_error_setter_mismatched_lengths", {
  d <- make_design()
  expect_error(
    set_var_label(d, variable = c("age", "income"), label = "Age"),
    class = "surveycore_error_setter_mismatched_lengths"
  )
})

test_that("set_var_label() errors with surveycore_error_old_positional_setter for old form", {
  d <- make_design()
  expect_error(
    set_var_label(d, age, "Age in years"),
    class = "surveycore_error_old_positional_setter"
  )
})

test_that("set_var_label() errors with surveycore_error_label_not_scalar for non-character", {
  d <- make_design()
  expect_error(
    set_var_label(d, age = 123L),
    class = "surveycore_error_label_not_scalar"
  )
})

test_that("set_var_label() errors with surveycore_error_label_not_scalar for length > 1", {
  d <- make_design()
  expect_error(
    set_var_label(d, age = c("A", "B")),
    class = "surveycore_error_label_not_scalar"
  )
})

test_that("set_var_label() errors with surveycore_error_setter_mixed_dots for unnamed ...", {
  d <- make_design()
  expect_error(
    set_var_label(d, "Age in years"),
    class = "surveycore_error_setter_mixed_dots"
  )
})

test_that("set_var_label() warns with surveycore_warning_var_not_found for missing variable", {
  d <- make_design()
  expect_warning(
    set_var_label(d, zzz_missing = "Label"),
    class = "surveycore_warning_var_not_found"
  )
})

test_that("set_var_label() skips missing var but still sets valid vars", {
  d <- make_design()
  result <- suppressWarnings(
    set_var_label(d, age = "Age", zzz_missing = "Gone")
  )
  expect_identical(result@metadata@variable_labels[["age"]], "Age")
  expect_false("zzz_missing" %in% names(result@metadata@variable_labels))
})

test_that("set_var_label() warns with surveycore_warning_setter_empty_variables for variable = character(0)", {
  d <- make_design()
  expect_warning(
    set_var_label(d, variable = character(0)),
    class = "surveycore_warning_setter_empty_variables"
  )
})

test_that("snapshot: set_var_label() surveycore_error_not_survey_or_df", {
  expect_snapshot(error = TRUE, set_var_label(list(x = 1), age = "A"))
})

test_that("snapshot: set_var_label() surveycore_error_setter_ambiguous", {
  d <- make_design()
  expect_snapshot(
    error = TRUE,
    set_var_label(d, age = "A", variable = "income")
  )
})

test_that("snapshot: set_var_label() surveycore_error_setter_empty", {
  d <- make_design()
  expect_snapshot(error = TRUE, set_var_label(d))
})

test_that("snapshot: set_var_label() surveycore_error_setter_mismatched_lengths", {
  d <- make_design()
  expect_snapshot(
    error = TRUE,
    set_var_label(d, variable = c("age", "income"), label = "Age")
  )
})

test_that("snapshot: set_var_label() surveycore_error_old_positional_setter", {
  d <- make_design()
  expect_snapshot(error = TRUE, set_var_label(d, age, "Age in years"))
})

test_that("snapshot: set_var_label() surveycore_error_label_not_scalar", {
  d <- make_design()
  expect_snapshot(error = TRUE, set_var_label(d, age = 123L))
})

test_that("snapshot: set_var_label() surveycore_warning_var_not_found", {
  d <- make_design()
  expect_snapshot(set_var_label(d, zzz_missing = "Label"))
})

test_that("snapshot: set_var_label() surveycore_warning_setter_empty_variables", {
  d <- make_design()
  expect_snapshot(set_var_label(d, variable = character(0)))
})


# ── set_val_labels() ──────────────────────────────────────────────────────────

test_that("set_val_labels() convention 1 sets labels for one variable", {
  d <- make_design()
  d <- set_val_labels(d, sex = c(Male = 1L, Female = 2L))
  expect_identical(d@metadata@value_labels[["sex"]], c(Male = 1L, Female = 2L))
})

test_that("set_val_labels() convention 1 sets labels for multiple variables", {
  d <- make_design()
  d <- set_val_labels(
    d,
    sex = c(Male = 1L, Female = 2L),
    age = c("25" = 25L, "30" = 30L, "35" = 35L, "45" = 45L, "50" = 50L)
  )
  expect_identical(d@metadata@value_labels[["sex"]], c(Male = 1L, Female = 2L))
  expect_false(is.null(d@metadata@value_labels[["age"]]))
})

test_that("set_val_labels() convention 2 (single named list in ...) sets labels", {
  d <- make_design()
  d <- set_val_labels(d, list(sex = c(Male = 1L, Female = 2L)))
  expect_identical(d@metadata@value_labels[["sex"]], c(Male = 1L, Female = 2L))
})

test_that("set_val_labels() convention 3 (variable + labels list) sets labels", {
  d <- make_design()
  d <- set_val_labels(
    d,
    variable = "sex",
    labels = list(c(Male = 1L, Female = 2L))
  )
  expect_identical(d@metadata@value_labels[["sex"]], c(Male = 1L, Female = 2L))
})

test_that("set_val_labels() convention 3 bare named vector accepted when length(variable) == 1", {
  d <- make_design()
  d <- set_val_labels(d, variable = "sex", labels = c(Male = 1L, Female = 2L))
  expect_identical(d@metadata@value_labels[["sex"]], c(Male = 1L, Female = 2L))
})

test_that("set_val_labels() returns x invisibly", {
  d <- make_design()
  result <- withVisible(set_val_labels(d, sex = c(Male = 1L, Female = 2L)))
  expect_false(result$visible)
  expect_identical(
    result$value@metadata@value_labels[["sex"]],
    c(Male = 1L, Female = 2L)
  )
})

test_that("set_val_labels() NULL value deletes the entry", {
  d <- make_design()
  d <- set_val_labels(d, sex = c(Male = 1L, Female = 2L))
  d <- set_val_labels(d, sex = NULL)
  expect_false("sex" %in% names(d@metadata@value_labels))
})

test_that("set_val_labels() data frame: sets attr(df$sex, 'labels')", {
  df <- data.frame(sex = c(1L, 2L, 1L))
  df2 <- set_val_labels(df, sex = c(Male = 1L, Female = 2L))
  expect_identical(attr(df2$sex, "labels"), c(Male = 1L, Female = 2L))
})

test_that("set_val_labels() allows extra labels not present in data", {
  d <- make_design()
  expect_no_warning(
    set_val_labels(d, sex = c(Male = 1L, Female = 2L, Other = 3L))
  )
})

test_that("set_val_labels() errors with surveycore_error_labels_unnamed for unnamed vector", {
  d <- make_design()
  expect_error(
    set_val_labels(d, sex = c(1L, 2L)),
    class = "surveycore_error_labels_unnamed"
  )
})

test_that("set_val_labels() errors with surveycore_error_labels_unnamed for partially named", {
  d <- make_design()
  expect_error(
    set_val_labels(d, sex = c(Male = 1L, 2L)),
    class = "surveycore_error_labels_unnamed"
  )
})

test_that("set_val_labels() warns with surveycore_warning_missing_labels for partially labeled", {
  d <- make_design()
  expect_warning(
    set_val_labels(d, sex = c(Male = 1L)),
    class = "surveycore_warning_missing_labels"
  )
})

test_that("set_val_labels() warns with surveycore_warning_missing_labels on data frame", {
  df <- data.frame(sex = c(1L, 2L, 1L))
  expect_warning(
    set_val_labels(df, sex = c(Male = 1L)),
    class = "surveycore_warning_missing_labels"
  )
})

test_that("set_val_labels() errors with surveycore_error_not_survey_or_df for list x", {
  expect_error(
    set_val_labels(list(x = 1), sex = c(Male = 1L)),
    class = "surveycore_error_not_survey_or_df"
  )
})

test_that("set_val_labels() errors with surveycore_error_setter_empty", {
  d <- make_design()
  expect_error(
    set_val_labels(d),
    class = "surveycore_error_setter_empty"
  )
})

test_that("set_val_labels() errors with surveycore_error_setter_ambiguous", {
  d <- make_design()
  expect_error(
    set_val_labels(d, sex = c(Male = 1L), variable = "age"),
    class = "surveycore_error_setter_ambiguous"
  )
})

test_that("set_val_labels() errors with surveycore_error_setter_mismatched_lengths", {
  d <- make_design()
  expect_error(
    set_val_labels(d, variable = c("sex", "age"), labels = list(c(Male = 1L))),
    class = "surveycore_error_setter_mismatched_lengths"
  )
})

test_that("set_val_labels() errors with surveycore_error_setter_mixed_dots for unnamed ...", {
  d <- make_design()
  expect_error(
    set_val_labels(d, c(-1L, -2L)),
    class = "surveycore_error_setter_mixed_dots"
  )
})

test_that("set_val_labels() warns with surveycore_warning_var_not_found for missing variable", {
  d <- make_design()
  expect_warning(
    set_val_labels(d, zzz_missing = c(A = 1L)),
    class = "surveycore_warning_var_not_found"
  )
})

test_that("set_val_labels() skips missing var but still sets valid vars", {
  d <- make_design()
  result <- suppressWarnings(
    set_val_labels(d, sex = c(Male = 1L, Female = 2L), zzz_missing = c(A = 1L))
  )
  expect_identical(
    result@metadata@value_labels[["sex"]],
    c(Male = 1L, Female = 2L)
  )
  expect_false("zzz_missing" %in% names(result@metadata@value_labels))
})

test_that("set_val_labels() warns with surveycore_warning_setter_empty_variables for variable = character(0)", {
  d <- make_design()
  expect_warning(
    set_val_labels(d, variable = character(0)),
    class = "surveycore_warning_setter_empty_variables"
  )
})

test_that("snapshot: set_val_labels() surveycore_error_labels_unnamed", {
  d <- make_design()
  expect_snapshot(error = TRUE, set_val_labels(d, sex = c(1L, 2L)))
})

test_that("snapshot: set_val_labels() surveycore_warning_missing_labels", {
  d <- make_design()
  expect_snapshot(set_val_labels(d, sex = c(Male = 1L)))
})

test_that("snapshot: set_val_labels() surveycore_error_not_survey_or_df", {
  expect_snapshot(error = TRUE, set_val_labels(list(x = 1), sex = c(Male = 1L)))
})

test_that("snapshot: set_val_labels() surveycore_error_setter_empty", {
  d <- make_design()
  expect_snapshot(error = TRUE, set_val_labels(d))
})

test_that("snapshot: set_val_labels() surveycore_error_setter_ambiguous", {
  d <- make_design()
  expect_snapshot(
    error = TRUE,
    set_val_labels(d, sex = c(Male = 1L), variable = "age")
  )
})

test_that("snapshot: set_val_labels() surveycore_error_setter_mismatched_lengths", {
  d <- make_design()
  expect_snapshot(
    error = TRUE,
    set_val_labels(d, variable = c("sex", "age"), labels = list(c(Male = 1L)))
  )
})

test_that("snapshot: set_val_labels() surveycore_warning_var_not_found", {
  d <- make_design()
  expect_snapshot(set_val_labels(d, zzz_missing = c(A = 1L)))
})

test_that("snapshot: set_val_labels() surveycore_warning_setter_empty_variables", {
  d <- make_design()
  expect_snapshot(set_val_labels(d, variable = character(0)))
})


# ── set_question_preface() ────────────────────────────────────────────────────

test_that("set_question_preface() convention 1 sets preface for one variable", {
  d <- make_design()
  d <- set_question_preface(d, age = "How old are you?")
  expect_identical(d@metadata@question_prefaces[["age"]], "How old are you?")
})

test_that("set_question_preface() convention 1 sets prefaces for multiple variables", {
  d <- make_design()
  d <- set_question_preface(
    d,
    age = "In the past year...",
    income = "For your household..."
  )
  expect_identical(d@metadata@question_prefaces[["age"]], "In the past year...")
  expect_identical(
    d@metadata@question_prefaces[["income"]],
    "For your household..."
  )
})

test_that("set_question_preface() convention 2 (named char vector in ...) sets prefaces", {
  d <- make_design()
  d <- set_question_preface(
    d,
    c(age = "In the past year...", income = "For your household...")
  )
  expect_identical(d@metadata@question_prefaces[["age"]], "In the past year...")
  expect_identical(
    d@metadata@question_prefaces[["income"]],
    "For your household..."
  )
})

test_that("set_question_preface() convention 3 (variable + preface) sets preface", {
  d <- make_design()
  d <- set_question_preface(d, variable = "age", preface = "How old are you?")
  expect_identical(d@metadata@question_prefaces[["age"]], "How old are you?")
})

test_that("set_question_preface() returns x invisibly", {
  d <- make_design()
  result <- withVisible(set_question_preface(d, age = "How old are you?"))
  expect_false(result$visible)
  expect_identical(
    result$value@metadata@question_prefaces[["age"]],
    "How old are you?"
  )
})

test_that("set_question_preface() NULL preface deletes the entry", {
  d <- make_design()
  d <- set_question_preface(d, age = "How old are you?")
  d <- set_question_preface(d, age = NULL)
  expect_false("age" %in% names(d@metadata@question_prefaces))
})

test_that("set_question_preface() data frame: sets attr(df$age, 'question_preface')", {
  df <- data.frame(age = 1:3)
  df2 <- set_question_preface(df, age = "How old are you?")
  expect_identical(attr(df2$age, "question_preface"), "How old are you?")
})

test_that("set_question_preface() errors with surveycore_error_not_survey_or_df for list x", {
  expect_error(
    set_question_preface(list(x = 1), age = "Q text"),
    class = "surveycore_error_not_survey_or_df"
  )
})

test_that("set_question_preface() errors with surveycore_error_label_not_scalar for non-character", {
  d <- make_design()
  expect_error(
    set_question_preface(d, age = 123L),
    class = "surveycore_error_label_not_scalar"
  )
})

test_that("set_question_preface() warns with surveycore_warning_var_not_found for missing variable", {
  d <- make_design()
  expect_warning(
    set_question_preface(d, zzz_missing = "Q text"),
    class = "surveycore_warning_var_not_found"
  )
})

test_that("set_question_preface() errors with surveycore_error_setter_ambiguous", {
  d <- make_design()
  expect_error(
    set_question_preface(d, age = "Q text", variable = "income"),
    class = "surveycore_error_setter_ambiguous"
  )
})

test_that("set_question_preface() errors with surveycore_error_setter_empty", {
  d <- make_design()
  expect_error(
    set_question_preface(d),
    class = "surveycore_error_setter_empty"
  )
})

test_that("set_question_preface() errors with surveycore_error_setter_mismatched_lengths", {
  d <- make_design()
  expect_error(
    set_question_preface(d, variable = c("age", "income"), preface = "Q"),
    class = "surveycore_error_setter_mismatched_lengths"
  )
})

test_that("set_question_preface() errors with surveycore_error_setter_mixed_dots for unnamed ...", {
  d <- make_design()
  expect_error(
    set_question_preface(d, "Q text"),
    class = "surveycore_error_setter_mixed_dots"
  )
})

test_that("set_question_preface() skips missing var but still sets valid vars", {
  d <- make_design()
  result <- suppressWarnings(
    set_question_preface(d, age = "Q text", zzz_missing = "Q2")
  )
  expect_identical(result@metadata@question_prefaces[["age"]], "Q text")
  expect_false("zzz_missing" %in% names(result@metadata@question_prefaces))
})

test_that("set_question_preface() warns with surveycore_warning_setter_empty_variables for variable = character(0)", {
  d <- make_design()
  expect_warning(
    set_question_preface(d, variable = character(0)),
    class = "surveycore_warning_setter_empty_variables"
  )
})

test_that("snapshot: set_question_preface() surveycore_error_not_survey_or_df", {
  expect_snapshot(
    error = TRUE,
    set_question_preface(list(x = 1), age = "Q text")
  )
})

test_that("snapshot: set_question_preface() surveycore_error_label_not_scalar", {
  d <- make_design()
  expect_snapshot(error = TRUE, set_question_preface(d, age = 123L))
})

test_that("snapshot: set_question_preface() surveycore_error_setter_ambiguous", {
  d <- make_design()
  expect_snapshot(
    error = TRUE,
    set_question_preface(d, age = "Q text", variable = "income")
  )
})

test_that("snapshot: set_question_preface() surveycore_error_setter_empty", {
  d <- make_design()
  expect_snapshot(error = TRUE, set_question_preface(d))
})

test_that("snapshot: set_question_preface() surveycore_error_setter_mismatched_lengths", {
  d <- make_design()
  expect_snapshot(
    error = TRUE,
    set_question_preface(d, variable = c("age", "income"), preface = "Q")
  )
})

test_that("snapshot: set_question_preface() surveycore_warning_var_not_found", {
  d <- make_design()
  expect_snapshot(set_question_preface(d, zzz_missing = "Q text"))
})

test_that("snapshot: set_question_preface() surveycore_warning_setter_empty_variables", {
  d <- make_design()
  expect_snapshot(set_question_preface(d, variable = character(0)))
})


# ── set_var_note() ────────────────────────────────────────────────────────────

test_that("set_var_note() convention 1 sets note for one variable", {
  d <- make_design()
  d <- set_var_note(d, income = "Top-coded at 999999")
  expect_identical(d@metadata@notes[["income"]], "Top-coded at 999999")
})

test_that("set_var_note() convention 1 sets notes for multiple variables", {
  d <- make_design()
  d <- set_var_note(
    d,
    age = "Recoded from continuous.",
    income = "Top-coded at 999999."
  )
  expect_identical(d@metadata@notes[["age"]], "Recoded from continuous.")
  expect_identical(d@metadata@notes[["income"]], "Top-coded at 999999.")
})

test_that("set_var_note() convention 2 (named char vector in ...) sets notes", {
  d <- make_design()
  d <- set_var_note(d, c(age = "Recoded.", income = "Top-coded."))
  expect_identical(d@metadata@notes[["age"]], "Recoded.")
  expect_identical(d@metadata@notes[["income"]], "Top-coded.")
})

test_that("set_var_note() convention 3 (variable + note) sets note", {
  d <- make_design()
  d <- set_var_note(d, variable = "income", note = "Top-coded at 999999")
  expect_identical(d@metadata@notes[["income"]], "Top-coded at 999999")
})

test_that("set_var_note() returns x invisibly", {
  d <- make_design()
  result <- withVisible(set_var_note(d, income = "Top-coded at 999999"))
  expect_false(result$visible)
  expect_identical(
    result$value@metadata@notes[["income"]],
    "Top-coded at 999999"
  )
})

test_that("set_var_note() NULL note deletes the entry", {
  d <- make_design()
  d <- set_var_note(d, income = "Top-coded at 999999")
  d <- set_var_note(d, income = NULL)
  expect_false("income" %in% names(d@metadata@notes))
})

test_that("set_var_note() data frame: sets attr(df$age, 'note')", {
  df <- data.frame(age = 1:3)
  df2 <- set_var_note(df, age = "A note")
  expect_identical(attr(df2$age, "note"), "A note")
})

test_that("set_var_note() errors with surveycore_error_not_survey_or_df for list x", {
  expect_error(
    set_var_note(list(x = 1), age = "A note"),
    class = "surveycore_error_not_survey_or_df"
  )
})

test_that("set_var_note() errors with surveycore_error_label_not_scalar for non-character", {
  d <- make_design()
  expect_error(
    set_var_note(d, age = 123L),
    class = "surveycore_error_label_not_scalar"
  )
})

test_that("set_var_note() warns with surveycore_warning_var_not_found for missing variable", {
  d <- make_design()
  expect_warning(
    set_var_note(d, zzz_missing = "Some note"),
    class = "surveycore_warning_var_not_found"
  )
})

test_that("set_var_note() errors with surveycore_error_setter_ambiguous", {
  d <- make_design()
  expect_error(
    set_var_note(d, age = "A note", variable = "income"),
    class = "surveycore_error_setter_ambiguous"
  )
})

test_that("set_var_note() errors with surveycore_error_setter_empty", {
  d <- make_design()
  expect_error(
    set_var_note(d),
    class = "surveycore_error_setter_empty"
  )
})

test_that("set_var_note() errors with surveycore_error_setter_mismatched_lengths", {
  d <- make_design()
  expect_error(
    set_var_note(d, variable = c("age", "income"), note = "A note"),
    class = "surveycore_error_setter_mismatched_lengths"
  )
})

test_that("set_var_note() errors with surveycore_error_setter_mixed_dots for unnamed ...", {
  d <- make_design()
  expect_error(
    set_var_note(d, "A note"),
    class = "surveycore_error_setter_mixed_dots"
  )
})

test_that("set_var_note() skips missing var but still sets valid vars", {
  d <- make_design()
  result <- suppressWarnings(
    set_var_note(d, income = "Top-coded", zzz_missing = "Note")
  )
  expect_identical(result@metadata@notes[["income"]], "Top-coded")
  expect_false("zzz_missing" %in% names(result@metadata@notes))
})

test_that("set_var_note() warns with surveycore_warning_setter_empty_variables for variable = character(0)", {
  d <- make_design()
  expect_warning(
    set_var_note(d, variable = character(0)),
    class = "surveycore_warning_setter_empty_variables"
  )
})

test_that("snapshot: set_var_note() surveycore_error_not_survey_or_df", {
  expect_snapshot(error = TRUE, set_var_note(list(x = 1), age = "A note"))
})

test_that("snapshot: set_var_note() surveycore_error_setter_ambiguous", {
  d <- make_design()
  expect_snapshot(
    error = TRUE,
    set_var_note(d, age = "A note", variable = "income")
  )
})

test_that("snapshot: set_var_note() surveycore_error_setter_empty", {
  d <- make_design()
  expect_snapshot(error = TRUE, set_var_note(d))
})

test_that("snapshot: set_var_note() surveycore_error_setter_mismatched_lengths", {
  d <- make_design()
  expect_snapshot(
    error = TRUE,
    set_var_note(d, variable = c("age", "income"), note = "A note")
  )
})

test_that("snapshot: set_var_note() surveycore_error_label_not_scalar", {
  d <- make_design()
  expect_snapshot(error = TRUE, set_var_note(d, age = 123L))
})

test_that("snapshot: set_var_note() surveycore_warning_var_not_found", {
  d <- make_design()
  expect_snapshot(set_var_note(d, zzz_missing = "Some note"))
})

test_that("snapshot: set_var_note() surveycore_warning_setter_empty_variables", {
  d <- make_design()
  expect_snapshot(set_var_note(d, variable = character(0)))
})


# ── set_universe() ────────────────────────────────────────────────────────────

test_that("set_universe() convention 1 sets universe for one variable", {
  d <- make_design()
  d <- set_universe(d, age = "Adults 18+")
  expect_identical(d@metadata@universe[["age"]], "Adults 18+")
})

test_that("set_universe() convention 1 sets universe for multiple variables", {
  d <- make_design()
  d <- set_universe(d, age = "Adults 18+", income = "Employed adults")
  expect_identical(d@metadata@universe[["age"]], "Adults 18+")
  expect_identical(d@metadata@universe[["income"]], "Employed adults")
})

test_that("set_universe() convention 2 (named char vector in ...) sets universe", {
  d <- make_design()
  d <- set_universe(d, c(age = "Adults 18+", income = "Employed adults"))
  expect_identical(d@metadata@universe[["age"]], "Adults 18+")
  expect_identical(d@metadata@universe[["income"]], "Employed adults")
})

test_that("set_universe() convention 3 (variable + universe) sets universe", {
  d <- make_design()
  d <- set_universe(d, variable = "age", universe = "Adults 18+")
  expect_identical(d@metadata@universe[["age"]], "Adults 18+")
})

test_that("set_universe() returns x invisibly", {
  d <- make_design()
  result <- withVisible(set_universe(d, age = "Adults 18+"))
  expect_false(result$visible)
  expect_identical(result$value@metadata@universe[["age"]], "Adults 18+")
})

test_that("set_universe() NULL universe deletes the entry", {
  d <- make_design()
  d <- set_universe(d, age = "Adults 18+")
  d <- set_universe(d, age = NULL)
  expect_false("age" %in% names(d@metadata@universe))
})

test_that("set_universe() data frame: sets attr(df$age, 'universe')", {
  df <- data.frame(age = 1:3)
  df2 <- set_universe(df, age = "Adults 18+")
  expect_identical(attr(df2$age, "universe"), "Adults 18+")
})

test_that("set_universe() errors with surveycore_error_not_survey_or_df for list x", {
  expect_error(
    set_universe(list(x = 1), age = "Adults 18+"),
    class = "surveycore_error_not_survey_or_df"
  )
})

test_that("set_universe() errors with surveycore_error_label_not_scalar for non-character", {
  d <- make_design()
  expect_error(
    set_universe(d, age = 123L),
    class = "surveycore_error_label_not_scalar"
  )
})

test_that("set_universe() warns with surveycore_warning_var_not_found for missing variable", {
  d <- make_design()
  expect_warning(
    set_universe(d, zzz_missing = "Some universe"),
    class = "surveycore_warning_var_not_found"
  )
})

test_that("set_universe() errors with surveycore_error_setter_ambiguous", {
  d <- make_design()
  expect_error(
    set_universe(d, age = "Adults 18+", variable = "income"),
    class = "surveycore_error_setter_ambiguous"
  )
})

test_that("set_universe() errors with surveycore_error_setter_empty", {
  d <- make_design()
  expect_error(
    set_universe(d),
    class = "surveycore_error_setter_empty"
  )
})

test_that("set_universe() errors with surveycore_error_setter_mismatched_lengths", {
  d <- make_design()
  expect_error(
    set_universe(d, variable = c("age", "income"), universe = "Adults 18+"),
    class = "surveycore_error_setter_mismatched_lengths"
  )
})

test_that("set_universe() errors with surveycore_error_setter_mixed_dots for unnamed ...", {
  d <- make_design()
  expect_error(
    set_universe(d, "Adults 18+"),
    class = "surveycore_error_setter_mixed_dots"
  )
})

test_that("set_universe() skips missing var but still sets valid vars", {
  d <- make_design()
  result <- suppressWarnings(
    set_universe(d, age = "Adults 18+", zzz_missing = "Some universe")
  )
  expect_identical(result@metadata@universe[["age"]], "Adults 18+")
  expect_false("zzz_missing" %in% names(result@metadata@universe))
})

test_that("set_universe() warns with surveycore_warning_setter_empty_variables for variable = character(0)", {
  d <- make_design()
  expect_warning(
    set_universe(d, variable = character(0)),
    class = "surveycore_warning_setter_empty_variables"
  )
})

test_that("snapshot: set_universe() surveycore_error_not_survey_or_df", {
  expect_snapshot(error = TRUE, set_universe(list(x = 1), age = "Adults 18+"))
})

test_that("snapshot: set_universe() surveycore_error_label_not_scalar", {
  d <- make_design()
  expect_snapshot(error = TRUE, set_universe(d, age = 123L))
})

test_that("snapshot: set_universe() surveycore_error_setter_ambiguous", {
  d <- make_design()
  expect_snapshot(
    error = TRUE,
    set_universe(d, age = "Adults 18+", variable = "income")
  )
})

test_that("snapshot: set_universe() surveycore_error_setter_empty", {
  d <- make_design()
  expect_snapshot(error = TRUE, set_universe(d))
})

test_that("snapshot: set_universe() surveycore_error_setter_mismatched_lengths", {
  d <- make_design()
  expect_snapshot(
    error = TRUE,
    set_universe(d, variable = c("age", "income"), universe = "Adults 18+")
  )
})

test_that("snapshot: set_universe() surveycore_warning_var_not_found", {
  d <- make_design()
  expect_snapshot(set_universe(d, zzz_missing = "Some universe"))
})

test_that("snapshot: set_universe() surveycore_warning_setter_empty_variables", {
  d <- make_design()
  expect_snapshot(set_universe(d, variable = character(0)))
})


# ── set_missing_codes() ───────────────────────────────────────────────────────

test_that("set_missing_codes() convention 1 sets codes for one variable", {
  d <- make_design()
  d <- set_missing_codes(d, age = c("Missing" = -1L, "Refused" = -2L))
  expect_identical(
    d@metadata@missing_codes[["age"]],
    c("Missing" = -1L, "Refused" = -2L)
  )
})

test_that("set_missing_codes() convention 1 sets codes for multiple variables", {
  d <- make_design()
  d <- set_missing_codes(
    d,
    age = c("Missing" = -1L),
    income = c("Refused" = -2L)
  )
  expect_identical(d@metadata@missing_codes[["age"]], c("Missing" = -1L))
  expect_identical(d@metadata@missing_codes[["income"]], c("Refused" = -2L))
})

test_that("set_missing_codes() convention 2 (single named list in ...) sets codes", {
  d <- make_design()
  d <- set_missing_codes(d, list(age = c("Missing" = -1L)))
  expect_identical(d@metadata@missing_codes[["age"]], c("Missing" = -1L))
})

test_that("set_missing_codes() convention 3 (variable + codes list) sets codes", {
  d <- make_design()
  d <- set_missing_codes(d, variable = "age", codes = list(c("Missing" = -1L)))
  expect_identical(d@metadata@missing_codes[["age"]], c("Missing" = -1L))
})

test_that("set_missing_codes() convention 3 bare named vector accepted when length(variable) == 1", {
  d <- make_design()
  d <- set_missing_codes(d, variable = "age", codes = c("Missing" = -1L))
  expect_identical(d@metadata@missing_codes[["age"]], c("Missing" = -1L))
})

test_that("set_missing_codes() returns x invisibly", {
  d <- make_design()
  result <- withVisible(set_missing_codes(d, age = c("Missing" = -1L)))
  expect_false(result$visible)
  expect_identical(
    result$value@metadata@missing_codes[["age"]],
    c("Missing" = -1L)
  )
})

test_that("set_missing_codes() NULL codes deletes the entry", {
  d <- make_design()
  d <- set_missing_codes(d, age = c("Missing" = -1L))
  d <- set_missing_codes(d, age = NULL)
  expect_false("age" %in% names(d@metadata@missing_codes))
})

test_that("set_missing_codes() data frame: sets attr(df$age, 'missing_codes')", {
  df <- data.frame(age = c(1L, -1L, 2L))
  df2 <- set_missing_codes(df, age = c("Missing" = -1L))
  expect_identical(attr(df2$age, "missing_codes"), c("Missing" = -1L))
})

test_that("set_missing_codes() errors with surveycore_error_missing_codes_not_vector for list entry", {
  d <- make_design()
  expect_error(
    set_missing_codes(d, age = list("bad")),
    class = "surveycore_error_missing_codes_not_vector"
  )
})

test_that("set_missing_codes() warns with surveycore_warning_var_not_found for missing variable", {
  d <- make_design()
  expect_warning(
    set_missing_codes(d, zzz_missing = c("Missing" = -1L)),
    class = "surveycore_warning_var_not_found"
  )
})

test_that("set_missing_codes() errors with surveycore_error_not_survey_or_df for list x", {
  expect_error(
    set_missing_codes(list(x = 1), age = c("Missing" = -1L)),
    class = "surveycore_error_not_survey_or_df"
  )
})

test_that("set_missing_codes() errors with surveycore_error_setter_ambiguous", {
  d <- make_design()
  expect_error(
    set_missing_codes(d, age = c("Missing" = -1L), variable = "income"),
    class = "surveycore_error_setter_ambiguous"
  )
})

test_that("set_missing_codes() errors with surveycore_error_setter_empty", {
  d <- make_design()
  expect_error(
    set_missing_codes(d),
    class = "surveycore_error_setter_empty"
  )
})

test_that("set_missing_codes() errors with surveycore_error_setter_mismatched_lengths", {
  d <- make_design()
  expect_error(
    set_missing_codes(
      d,
      variable = c("age", "income"),
      codes = list(c("Missing" = -1L))
    ),
    class = "surveycore_error_setter_mismatched_lengths"
  )
})

test_that("set_missing_codes() errors with surveycore_error_setter_mixed_dots for unnamed ...", {
  d <- make_design()
  expect_error(
    set_missing_codes(d, c(-1L, -2L)),
    class = "surveycore_error_setter_mixed_dots"
  )
})

test_that("set_missing_codes() skips missing var but still sets valid vars", {
  d <- make_design()
  result <- suppressWarnings(
    set_missing_codes(
      d,
      age = c("Missing" = -1L),
      zzz_missing = c("Missing" = -1L)
    )
  )
  expect_identical(result@metadata@missing_codes[["age"]], c("Missing" = -1L))
  expect_false("zzz_missing" %in% names(result@metadata@missing_codes))
})

test_that("set_missing_codes() warns with surveycore_warning_setter_empty_variables for variable = character(0)", {
  d <- make_design()
  expect_warning(
    set_missing_codes(d, variable = character(0)),
    class = "surveycore_warning_setter_empty_variables"
  )
})

test_that("snapshot: set_missing_codes() surveycore_error_missing_codes_not_vector", {
  d <- make_design()
  expect_snapshot(error = TRUE, set_missing_codes(d, age = list("bad")))
})

test_that("snapshot: set_missing_codes() surveycore_error_not_survey_or_df", {
  expect_snapshot(
    error = TRUE,
    set_missing_codes(list(x = 1), age = c("Missing" = -1L))
  )
})

test_that("snapshot: set_missing_codes() surveycore_error_setter_ambiguous", {
  d <- make_design()
  expect_snapshot(
    error = TRUE,
    set_missing_codes(d, age = c("Missing" = -1L), variable = "income")
  )
})

test_that("snapshot: set_missing_codes() surveycore_error_setter_empty", {
  d <- make_design()
  expect_snapshot(error = TRUE, set_missing_codes(d))
})

test_that("snapshot: set_missing_codes() surveycore_error_setter_mismatched_lengths", {
  d <- make_design()
  expect_snapshot(
    error = TRUE,
    set_missing_codes(
      d,
      variable = c("age", "income"),
      codes = list(c("Missing" = -1L))
    )
  )
})

test_that("snapshot: set_missing_codes() surveycore_warning_var_not_found", {
  d <- make_design()
  expect_snapshot(set_missing_codes(d, zzz_missing = c("Missing" = -1L)))
})

test_that("snapshot: set_missing_codes() surveycore_warning_setter_empty_variables", {
  d <- make_design()
  expect_snapshot(set_missing_codes(d, variable = character(0)))
})


# ── Removed plural setters ────────────────────────────────────────────────────

test_that("set_variable_labels() is removed — calling it errors with 'could not find function'", {
  expect_error(
    set_variable_labels(make_design(), age = "A"),
    regexp = "could not find function"
  )
})

test_that("set_value_labels() is removed — calling it errors with 'could not find function'", {
  expect_error(
    set_value_labels(make_design(), sex = c(Male = 1L)),
    regexp = "could not find function"
  )
})

test_that("set_question_prefaces() is removed — calling it errors with 'could not find function'", {
  expect_error(
    set_question_prefaces(make_design(), age = "Q"),
    regexp = "could not find function"
  )
})

test_that("set_variable_notes() is removed — calling it errors with 'could not find function'", {
  expect_error(
    set_variable_notes(make_design(), age = "note"),
    regexp = "could not find function"
  )
})


# ── .extract_haven_metadata() ────────────────────────────────────────────────

test_that(".extract_haven_metadata() returns empty survey_metadata when no attrs", {
  df <- data.frame(x = 1:3, y = letters[1:3])
  out <- surveycore:::.extract_haven_metadata(df)
  expect_true(S7::S7_inherits(out, survey_metadata))
  expect_identical(out@variable_labels, list())
  expect_identical(out@value_labels, list())
})

test_that(".extract_haven_metadata() extracts haven variable labels", {
  df <- data.frame(age = 1:3, sex = c(1L, 2L, 1L))
  attr(df$age, "label") <- "Age in years"
  attr(df$sex, "label") <- "Biological sex"
  out <- surveycore:::.extract_haven_metadata(df)
  expect_identical(out@variable_labels[["age"]], "Age in years")
  expect_identical(out@variable_labels[["sex"]], "Biological sex")
})

test_that(".extract_haven_metadata() extracts haven value labels", {
  df <- data.frame(sex = c(1L, 2L))
  attr(df$sex, "labels") <- c(Male = 1L, Female = 2L)
  out <- surveycore:::.extract_haven_metadata(df)
  expect_identical(out@value_labels[["sex"]], c(Male = 1L, Female = 2L))
})

test_that(".extract_haven_metadata() silently ignores zero-length label strings", {
  df <- data.frame(x = 1:3)
  attr(df$x, "label") <- "" # empty string — should be ignored
  out <- surveycore:::.extract_haven_metadata(df)
  expect_identical(out@variable_labels, list())
})

test_that(".extract_haven_metadata() preserves NA keys in value labels", {
  df <- data.frame(x = c(1L, 2L, NA_integer_))
  lbl <- c("Yes" = 1L, "No" = 2L, "Unknown" = NA_integer_)
  attr(df$x, "labels") <- lbl
  out <- surveycore:::.extract_haven_metadata(df)
  # NA key should be preserved
  expect_true("Unknown" %in% names(out@value_labels[["x"]]))
  expect_true(any(is.na(out@value_labels[["x"]])))
})

test_that(".extract_haven_metadata() does not store empty labels vectors", {
  df <- data.frame(x = 1:3)
  attr(df$x, "labels") <- c() # empty — should not be stored
  out <- surveycore:::.extract_haven_metadata(df)
  expect_null(out@value_labels[["x"]])
})

test_that(".extract_haven_metadata() works with make_survey_data(with_labels=TRUE)", {
  df <- make_survey_data(n = 50L, seed = 1L, with_labels = TRUE)
  out <- surveycore:::.extract_haven_metadata(df)
  expect_true(S7::S7_inherits(out, survey_metadata))
  expect_identical(
    out@variable_labels[["y1"]],
    "Outcome variable 1 (continuous)"
  )
  expect_identical(
    out@variable_labels[["y2"]],
    "Outcome variable 2 (continuous)"
  )
  expect_identical(out@value_labels[["y3"]], c("No" = 0L, "Yes" = 1L))
})


# ── Edge cases ────────────────────────────────────────────────────────────────

test_that("metadata operations do not modify @data", {
  d <- make_design()
  orig <- d@data
  d <- set_var_label(d, age = "Age in years")
  d <- set_val_labels(d, sex = c(Male = 1L, Female = 2L))
  expect_identical(d@data, orig)
})

test_that("metadata operations do not corrupt other metadata properties", {
  d <- make_design()
  d <- set_var_label(d, age = "Age label")
  d <- set_val_labels(d, sex = c(Male = 1L, Female = 2L))
  d <- set_question_preface(d, income = "Preface text")
  d <- set_var_note(d, wt = "A note")

  # Each property holds only what was set
  expect_identical(d@metadata@variable_labels[["age"]], "Age label")
  expect_null(d@metadata@variable_labels[["sex"]])
  expect_identical(d@metadata@value_labels[["sex"]], c(Male = 1L, Female = 2L))
  expect_null(d@metadata@value_labels[["age"]])
  expect_identical(d@metadata@question_prefaces[["income"]], "Preface text")
  expect_null(d@metadata@question_prefaces[["age"]])
  expect_identical(d@metadata@notes[["wt"]], "A note")
  expect_null(d@metadata@notes[["age"]])
})

test_that("set_val_labels() does not warn for character value labels", {
  df <- data.frame(group = c("A", "B", "C"), wt = c(1, 1, 1))
  d <- survey_taylor(
    data = df,
    variables = list(
      ids = NULL,
      weights = "wt",
      strata = NULL,
      fpc = NULL,
      nest = FALSE,
      probs_provided = FALSE
    )
  )
  expect_no_warning(
    set_val_labels(
      d,
      group = c("Group A" = "A", "Group B" = "B", "Group C" = "C")
    )
  )
})

test_that("set_var_label() and extract_var_label() roundtrip for design col (wt)", {
  d <- make_design()
  d <- set_var_label(d, wt = "Survey weight")
  expect_identical(extract_var_label(d, wt), c(wt = "Survey weight"))
})


# ── Coverage: .validate_val_labels() strict = TRUE path ──────────────────────

test_that(".validate_val_labels() errors in strict mode with unlabeled values [direct]", {
  var <- c(1L, 2L, 3L, 1L, 2L) # value 3 is unlabeled
  labels <- c(a = 1L, b = 2L)
  expect_error(
    surveycore:::.validate_val_labels(
      var,
      labels,
      var_name = "x",
      strict = TRUE
    ),
    class = "surveycore_error_missing_labels"
  )
})


# ── survey_weighting_history() ────────────────────────────────────────────────

test_that("survey_weighting_history() returns list() when no history set", {
  d <- make_design()
  expect_identical(survey_weighting_history(d), list())
})

test_that("survey_weighting_history() returns the history list when set", {
  df <- make_survey_data(n = 100L, seed = 201L)
  history <- list(list(step = 1L, operation = "raking"))
  attr(df, "weighting_history") <- history
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  expect_identical(survey_weighting_history(d), history)
})


# ── .check_is_survey_or_df() ─────────────────────────────────────────────────

test_that(".check_is_survey_or_df() returns invisibly NULL for a survey_taylor object", {
  d <- make_design()
  result <- withVisible(surveycore:::.check_is_survey_or_df(d))
  expect_null(result$value)
  expect_false(result$visible)
})

test_that(".check_is_survey_or_df() returns invisibly NULL for a plain data.frame", {
  df <- data.frame(x = 1:3)
  result <- withVisible(surveycore:::.check_is_survey_or_df(df))
  expect_null(result$value)
  expect_false(result$visible)
})

test_that(".check_is_survey_or_df() errors with surveycore_error_not_survey_or_df for list input", {
  expect_error(
    surveycore:::.check_is_survey_or_df(list(x = 1)),
    class = "surveycore_error_not_survey_or_df"
  )
})

test_that(".check_is_survey_or_df() errors with surveycore_error_not_survey_or_df for character input", {
  expect_error(
    surveycore:::.check_is_survey_or_df("not a survey"),
    class = "surveycore_error_not_survey_or_df"
  )
})

test_that("snapshot: .check_is_survey_or_df() surveycore_error_not_survey_or_df for list input", {
  expect_snapshot(
    error = TRUE,
    surveycore:::.check_is_survey_or_df(list(x = 1))
  )
})


# ── .parse_setter_input() ────────────────────────────────────────────────────

test_that(".parse_setter_input() convention 1 (named ...) returns correct named list", {
  result <- surveycore:::.parse_setter_input(
    dots = list(age = "Age in years", income = "Annual income"),
    variable = NULL,
    content = NULL,
    content_arg_name = "label",
    content_type = "scalar",
    fn_name = "set_var_label"
  )
  expect_identical(result, list(age = "Age in years", income = "Annual income"))
})

test_that(".parse_setter_input() convention 1 with !!! splicing returns correct named list", {
  lbls <- list(age = "Age in years", income = "Annual income")
  dots <- rlang::list2(!!!lbls)
  result <- surveycore:::.parse_setter_input(
    dots = dots,
    variable = NULL,
    content = NULL,
    content_arg_name = "label",
    content_type = "scalar",
    fn_name = "set_var_label"
  )
  expect_identical(result, list(age = "Age in years", income = "Annual income"))
})

test_that(".parse_setter_input() convention 2 scalar (single named char vector) returns correct named list", {
  # Simulates: set_var_label(svy, c(age = "Age", income = "Annual income"))
  dots <- list(c(age = "Age in years", income = "Annual income"))
  result <- surveycore:::.parse_setter_input(
    dots = dots,
    variable = NULL,
    content = NULL,
    content_arg_name = "label",
    content_type = "scalar",
    fn_name = "set_var_label"
  )
  expect_identical(result, list(age = "Age in years", income = "Annual income"))
})

test_that(".parse_setter_input() convention 2 vector (single named list) returns correct named list", {
  # Simulates: set_val_labels(svy, list(sex = c(Male=1L), region = c(N=1L)))
  dots <- list(list(
    sex = c(Male = 1L, Female = 2L),
    region = c(N = 1L, S = 2L)
  ))
  result <- surveycore:::.parse_setter_input(
    dots = dots,
    variable = NULL,
    content = NULL,
    content_arg_name = "labels",
    content_type = "vector",
    fn_name = "set_val_labels"
  )
  expect_identical(
    result,
    list(sex = c(Male = 1L, Female = 2L), region = c(N = 1L, S = 2L))
  )
})

test_that(".parse_setter_input() convention 3 (variable + content) returns correct named list", {
  result <- surveycore:::.parse_setter_input(
    dots = list(),
    variable = c("age", "income"),
    content = c("Age in years", "Annual income"),
    content_arg_name = "label",
    content_type = "scalar",
    fn_name = "set_var_label"
  )
  expect_identical(result, list(age = "Age in years", income = "Annual income"))
})

test_that(".parse_setter_input() convention 3 length mismatch errors with surveycore_error_setter_mismatched_lengths", {
  expect_error(
    surveycore:::.parse_setter_input(
      dots = list(),
      variable = c("age", "income"),
      content = c("Age in years"),
      content_arg_name = "label",
      content_type = "scalar",
      fn_name = "set_var_label"
    ),
    class = "surveycore_error_setter_mismatched_lengths"
  )
})

test_that(".parse_setter_input() both ... and variable errors with surveycore_error_setter_ambiguous", {
  expect_error(
    surveycore:::.parse_setter_input(
      dots = list(age = "Age"),
      variable = "income",
      content = "Annual income",
      content_arg_name = "label",
      content_type = "scalar",
      fn_name = "set_var_label"
    ),
    class = "surveycore_error_setter_ambiguous"
  )
})

test_that(".parse_setter_input() neither ... nor variable errors with surveycore_error_setter_empty", {
  expect_error(
    surveycore:::.parse_setter_input(
      dots = list(),
      variable = NULL,
      content = NULL,
      content_arg_name = "label",
      content_type = "scalar",
      fn_name = "set_var_label"
    ),
    class = "surveycore_error_setter_empty"
  )
})

test_that(".parse_setter_input() unnamed ... elements errors with surveycore_error_setter_mixed_dots", {
  # Unnamed character vector (not a named vector) in ...
  dots <- list(c("Age in years", "Annual income"))
  expect_error(
    surveycore:::.parse_setter_input(
      dots = dots,
      variable = NULL,
      content = NULL,
      content_arg_name = "label",
      content_type = "scalar",
      fn_name = "set_var_label"
    ),
    class = "surveycore_error_setter_mixed_dots"
  )
})

test_that(".parse_setter_input() NULL values pass through in returned list", {
  result <- surveycore:::.parse_setter_input(
    dots = list(age = NULL, income = "Annual income"),
    variable = NULL,
    content = NULL,
    content_arg_name = "label",
    content_type = "scalar",
    fn_name = "set_var_label"
  )
  expect_null(result[["age"]])
  expect_identical(result[["income"]], "Annual income")
})

test_that("snapshot: surveycore_error_setter_mismatched_lengths message", {
  expect_snapshot(
    error = TRUE,
    surveycore:::.parse_setter_input(
      dots = list(),
      variable = c("age", "income"),
      content = c("Age in years"),
      content_arg_name = "label",
      content_type = "scalar",
      fn_name = "set_var_label"
    )
  )
})

test_that("snapshot: surveycore_error_setter_ambiguous message", {
  expect_snapshot(
    error = TRUE,
    surveycore:::.parse_setter_input(
      dots = list(age = "Age"),
      variable = "income",
      content = "Annual income",
      content_arg_name = "label",
      content_type = "scalar",
      fn_name = "set_var_label"
    )
  )
})

test_that("snapshot: surveycore_error_setter_empty message", {
  expect_snapshot(
    error = TRUE,
    surveycore:::.parse_setter_input(
      dots = list(),
      variable = NULL,
      content = NULL,
      content_arg_name = "label",
      content_type = "scalar",
      fn_name = "set_var_label"
    )
  )
})

test_that("snapshot: surveycore_error_setter_mixed_dots message", {
  dots <- list(c("Age in years", "Annual income"))
  expect_snapshot(
    error = TRUE,
    surveycore:::.parse_setter_input(
      dots = dots,
      variable = NULL,
      content = NULL,
      content_arg_name = "label",
      content_type = "scalar",
      fn_name = "set_var_label"
    )
  )
})


# ── .format_scalar_result() ───────────────────────────────────────────────────

test_that(".format_scalar_result() format = 'named_vector' returns named character vector", {
  result_list <- list(age = "Age in years", income = "Annual income")
  result <- surveycore:::.format_scalar_result(
    result_list,
    format = "named_vector",
    col_name = "label",
    empty_value = NULL
  )
  expect_identical(result, c(age = "Age in years", income = "Annual income"))
})

test_that(".format_scalar_result() format = 'list' returns named list", {
  result_list <- list(age = "Age in years", income = "Annual income")
  result <- surveycore:::.format_scalar_result(
    result_list,
    format = "list",
    col_name = "label",
    empty_value = NULL
  )
  expect_identical(result, list(age = "Age in years", income = "Annual income"))
})

test_that(".format_scalar_result() format = 'data_frame' returns tibble with variable/label columns", {
  result_list <- list(age = "Age in years", income = "Annual income")
  result <- surveycore:::.format_scalar_result(
    result_list,
    format = "data_frame",
    col_name = "label",
    empty_value = NULL
  )
  expect_s3_class(result, "tbl_df")
  expect_identical(names(result), c("variable", "label"))
  expect_identical(result$variable, c("age", "income"))
  expect_identical(result$label, c("Age in years", "Annual income"))
})

test_that(".format_scalar_result() fill = NULL omits entries with NULL values", {
  result_list <- list(age = "Age in years", income = NULL)
  result <- surveycore:::.format_scalar_result(
    result_list,
    format = "named_vector",
    col_name = "label",
    empty_value = NULL
  )
  expect_identical(result, c(age = "Age in years"))
  expect_false("income" %in% names(result))
})

test_that(".format_scalar_result() fill = NA_character_ includes entries with NA", {
  result_list <- list(age = "Age in years", income = NULL)
  result <- surveycore:::.format_scalar_result(
    result_list,
    format = "named_vector",
    col_name = "label",
    empty_value = NA_character_
  )
  expect_identical(result, c(age = "Age in years", income = NA_character_))
})


# ── .format_list_result() ─────────────────────────────────────────────────────

test_that(".format_list_result() format = 'list' returns named list", {
  result_list <- list(
    sex = c(Male = 1L, Female = 2L),
    region = c(N = 1L, S = 2L)
  )
  result <- surveycore:::.format_list_result(
    result_list,
    format = "list",
    fn_name = "extract_val_labels"
  )
  expect_identical(result, result_list)
})

test_that(".format_list_result() format = 'data_frame' returns long tibble", {
  result_list <- list(sex = c(Male = 1L, Female = 2L))
  result <- surveycore:::.format_list_result(
    result_list,
    format = "data_frame",
    fn_name = "extract_val_labels"
  )
  expect_s3_class(result, "tbl_df")
  expect_identical(names(result), c("variable", "label", "value"))
  expect_equal(nrow(result), 2L)
  expect_identical(result$variable, c("sex", "sex"))
  expect_identical(result$label, c("Male", "Female"))
  expect_identical(result$value, c("1", "2"))
})

test_that(".format_list_result() format = 'named_vector' errors with surveycore_error_format_invalid", {
  result_list <- list(sex = c(Male = 1L, Female = 2L))
  expect_error(
    surveycore:::.format_list_result(
      result_list,
      format = "named_vector",
      fn_name = "extract_val_labels"
    ),
    class = "surveycore_error_format_invalid"
  )
})

test_that("snapshot: .format_list_result() surveycore_error_format_invalid message", {
  result_list <- list(sex = c(Male = 1L, Female = 2L))
  expect_snapshot(
    error = TRUE,
    surveycore:::.format_list_result(
      result_list,
      format = "named_vector",
      fn_name = "extract_val_labels"
    )
  )
})


# ── Round-trip: data frame → as_survey() → extractor ─────────────────────────

test_that("round-trip: set_var_label() on df -> as_survey() -> extract_var_label() matches", {
  df <- make_survey_data(n = 50L, seed = 1L)
  df <- set_var_label(df, y1 = "Outcome A")
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  result <- extract_var_label(d, y1)
  expect_identical(result, c(y1 = "Outcome A"))
})

test_that("round-trip: set_val_labels() on df -> as_survey() -> extract_val_labels() matches", {
  df <- make_survey_data(n = 50L, seed = 2L)
  df <- set_val_labels(df, y3 = c(No = 0L, Yes = 1L))
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  result <- extract_val_labels(d, y3)
  expect_identical(result, list(y3 = c(No = 0L, Yes = 1L)))
})

test_that("round-trip: set_question_preface() on df -> as_survey() -> extract_question_preface() matches", {
  df <- make_survey_data(n = 50L, seed = 3L)
  df <- set_question_preface(df, y1 = "Please indicate...")
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  result <- extract_question_preface(d, y1)
  expect_identical(result, c(y1 = "Please indicate..."))
})

test_that("round-trip: set_var_note() on df -> as_survey() -> extract_var_note() matches", {
  df <- make_survey_data(n = 50L, seed = 4L)
  df <- set_var_note(df, y1 = "Top-coded at 100.")
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  result <- extract_var_note(d, y1)
  expect_identical(result, c(y1 = "Top-coded at 100."))
})

test_that("round-trip: set_universe() on df -> as_survey() -> extract_universe() matches", {
  df <- make_survey_data(n = 50L, seed = 5L)
  df <- set_universe(df, y1 = "Adults 18+")
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  result <- extract_universe(d, y1)
  expect_identical(result, c(y1 = "Adults 18+"))
})

test_that("round-trip: set_missing_codes() on df -> as_survey() -> extract_missing_codes() matches", {
  df <- make_survey_data(n = 50L, seed = 6L)
  df <- set_missing_codes(df, y1 = c("Refused" = -1L))
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  result <- extract_missing_codes(d, y1)
  expect_identical(result, list(y1 = c("Refused" = -1L)))
})


# ── extract_metadata() ────────────────────────────────────────────────────────

test_that("extract_metadata() single variable with all fields returns 7-key list", {
  d <- make_labeled_design()
  d <- set_question_preface(d, y1 = "Please rate...")
  d <- set_var_note(d, y1 = "Imputed for 2% of cases.")
  result <- extract_metadata(d, y1)
  expect_named(result, "y1")
  expect_named(
    result$y1,
    c(
      "variable_label",
      "value_labels",
      "question_preface",
      "note",
      "universe",
      "missing_codes",
      "transformations"
    )
  )
})

test_that("extract_metadata() result keys are in spec order", {
  d <- make_labeled_design()
  result <- extract_metadata(d, y1)
  expect_identical(
    names(result$y1),
    c(
      "variable_label",
      "value_labels",
      "question_preface",
      "note",
      "universe",
      "missing_codes",
      "transformations"
    )
  )
})

test_that("extract_metadata() does NOT include weighting_history in any entry", {
  d <- make_labeled_design()
  result <- extract_metadata(d, fill = "include")
  for (entry in result) {
    expect_false("weighting_history" %in% names(entry))
  }
})

test_that("extract_metadata() transformations is always list(), never NULL", {
  d <- make_labeled_design()
  result <- extract_metadata(d, fill = "include")
  for (entry in result) {
    expect_true(is.list(entry$transformations))
  }
})

test_that("extract_metadata() fill = NULL (default): variable with at least one field included", {
  d <- make_labeled_design()
  result <- extract_metadata(d)
  expect_true("y1" %in% names(result))
})

test_that("extract_metadata() fill = NULL: variable with no fields omitted", {
  d <- make_labeled_design()
  result <- extract_metadata(d)
  expect_false("wt" %in% names(result))
})

test_that("extract_metadata() fill = 'include': variable with no fields included with all-NULL values", {
  d <- make_labeled_design()
  result <- extract_metadata(d, fill = "include")
  expect_true("wt" %in% names(result))
  expect_null(result$wt$variable_label)
  expect_null(result$wt$value_labels)
  expect_null(result$wt$question_preface)
  expect_null(result$wt$note)
  expect_null(result$wt$universe)
  expect_null(result$wt$missing_codes)
  expect_identical(result$wt$transformations, list())
})

test_that("extract_metadata() multiple variables, mixed metadata: fill = NULL returns only annotated", {
  d <- make_labeled_design()
  result <- extract_metadata(d, y1, y2, wt)
  expect_true("y1" %in% names(result))
  expect_true("y2" %in% names(result))
  expect_false("wt" %in% names(result))
})

test_that("extract_metadata() multiple variables: fill = 'include' returns all", {
  d <- make_labeled_design()
  result <- extract_metadata(d, y1, y2, wt, fill = "include")
  expect_named(result, c("y1", "y2", "wt"))
})

test_that("extract_metadata() no var arg: fill = NULL returns only annotated variables", {
  d <- make_labeled_design()
  result <- extract_metadata(d)
  for (col in c("y1", "y2", "y3")) {
    expect_true(col %in% names(result))
  }
  expect_false("psu" %in% names(result))
})

test_that("extract_metadata() no var arg: fill = 'include' returns all columns", {
  d <- make_labeled_design()
  result <- extract_metadata(d, fill = "include")
  expect_equal(length(result), ncol(d@data))
})

test_that("extract_metadata() all variables with no metadata + fill = NULL: returns list()", {
  d <- make_design()
  result <- extract_metadata(d)
  expect_identical(result, list())
})

test_that("extract_metadata() all variables with no metadata + fill = 'include': returns full-length list", {
  d <- make_design()
  result <- extract_metadata(d, fill = "include")
  expect_equal(length(result), ncol(d@data))
  for (entry in result) {
    expect_null(entry$variable_label)
    expect_identical(entry$transformations, list())
  }
})

test_that("extract_metadata() single-column data frame with fill = 'include'", {
  df <- data.frame(x = 1:3)
  result <- extract_metadata(df, fill = "include")
  expect_named(result, "x")
  expect_null(result$x$variable_label)
  expect_identical(result$x$transformations, list())
})

test_that("extract_metadata() data frame: reads column attributes correctly", {
  df <- data.frame(age = 1:5, sex = c(1L, 2L, 1L, 2L, 1L))
  attr(df$age, "label") <- "Age in years"
  attr(df$sex, "labels") <- c(Male = 1L, Female = 2L)
  result <- extract_metadata(df, fill = "include")
  expect_identical(result$age$variable_label, "Age in years")
  expect_identical(result$sex$value_labels, c(Male = 1L, Female = 2L))
  expect_null(result$age$value_labels)
  expect_null(result$sex$variable_label)
})

test_that("extract_metadata() data frame: transformations = list() for all variables", {
  df <- data.frame(x = 1:3, y = 4:6)
  attr(df$x, "label") <- "Variable X"
  result <- extract_metadata(df, fill = "include")
  expect_identical(result$x$transformations, list())
  expect_identical(result$y$transformations, list())
})

test_that("extract_metadata() errors with surveycore_error_not_survey_or_df for list x", {
  expect_error(
    extract_metadata(list(a = 1), a),
    class = "surveycore_error_not_survey_or_df"
  )
})

test_that("extract_metadata() errors for bare nonexistent column (tidyselect behavior)", {
  d <- make_labeled_design()
  expect_error(extract_metadata(d, y1, nonexistent))
})

test_that("extract_metadata() errors with surveycore_error_fill_invalid for fill = NA_character_", {
  d <- make_labeled_design()
  expect_error(
    extract_metadata(d, fill = NA_character_),
    class = "surveycore_error_fill_invalid"
  )
})

test_that("snapshot: extract_metadata() surveycore_error_not_survey_or_df", {
  expect_snapshot(error = TRUE, extract_metadata(list(a = 1), a))
})

test_that("snapshot: extract_metadata() surveycore_error_fill_invalid", {
  d <- make_labeled_design()
  expect_snapshot(error = TRUE, extract_metadata(d, fill = NA_character_))
})


# ── .rename_metadata_keys() sata integration ───────────────────────────────────

test_that(".rename_metadata_keys() propagates sata flag through rename", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d@metadata@sata[["riagendr"]] <- TRUE
  d@metadata <- surveycore:::.rename_metadata_keys(
    d@metadata,
    c(riagendr = "gender")
  )
  expect_null(d@metadata@sata[["riagendr"]])
  expect_true(isTRUE(d@metadata@sata[["gender"]]))
})


# ── tidyselect support in extract_*() ─────────────────────────────────────────

test_that("extract_var_label() supports starts_with() selector", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  result <- extract_var_label(d, starts_with("sdmv"))
  expect_true(all(startsWith(names(result), "sdmv")))
})

test_that("extract_question_preface() supports all_of() selector", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_question_preface(d, riagendr = "Demographics")
  result <- extract_question_preface(d, all_of(c("riagendr", "ridageyr")))
  expect_true("riagendr" %in% names(result))
})

test_that("extract_var_label() any_of() silently omits missing columns", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  result <- extract_var_label(
    d,
    any_of(c("riagendr", "col_that_does_not_exist"))
  )
  expect_false("col_that_does_not_exist" %in% names(result))
  expect_true("riagendr" %in% names(result))
})


# ── set_sata() — happy path ────────────────────────────────────────────────

test_that("set_sata() marks variables as SATA on a survey object", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_sata(d, riagendr, ridageyr)
  expect_true(isTRUE(d@metadata@sata[["riagendr"]]))
  expect_true(isTRUE(d@metadata@sata[["ridageyr"]]))
})

test_that("set_sata() marks variables as SATA on a plain data frame", {
  df <- data.frame(
    a = 1:5,
    b = 1:5,
    c = 1:5,
    stringsAsFactors = FALSE
  )
  df <- set_sata(df, a, b)
  expect_true(isTRUE(attr(df$a, "sata", exact = TRUE)))
  expect_true(isTRUE(attr(df$b, "sata", exact = TRUE)))
  expect_null(attr(df$c, "sata", exact = TRUE))
})

test_that("set_sata() works with tidy-select starts_with()", {
  df <- data.frame(
    news_tv = 1:5,
    news_online = 1:5,
    news_radio = 1:5,
    other = 1:5,
    stringsAsFactors = FALSE
  )
  df <- set_sata(df, starts_with("news_"))
  expect_true(isTRUE(attr(df$news_tv, "sata", exact = TRUE)))
  expect_true(isTRUE(attr(df$news_online, "sata", exact = TRUE)))
  expect_true(isTRUE(attr(df$news_radio, "sata", exact = TRUE)))
  expect_null(attr(df$other, "sata", exact = TRUE))
})

test_that("set_sata() Convention B: variable = character vector", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_sata(d, variable = c("riagendr", "ridageyr"))
  expect_true(isTRUE(d@metadata@sata[["riagendr"]]))
  expect_true(isTRUE(d@metadata@sata[["ridageyr"]]))
})

test_that("set_sata(sata = FALSE) removes SATA flag from survey object", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_sata(d, riagendr, ridageyr)
  d <- set_sata(d, riagendr, sata = FALSE)
  expect_null(d@metadata@sata[["riagendr"]])
  expect_true(isTRUE(d@metadata@sata[["ridageyr"]]))
})

test_that("set_sata() on already-SATA variable is idempotent (no error)", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_sata(d, riagendr)
  expect_silent(d <- set_sata(d, riagendr))
  expect_true(isTRUE(d@metadata@sata[["riagendr"]]))
})

test_that("set_sata(sata = FALSE) on non-SATA variable is a no-op", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_silent(d <- set_sata(d, riagendr, sata = FALSE))
  expect_null(d@metadata@sata[["riagendr"]])
  expect_identical(length(d@metadata@sata), 0L)
})

test_that("set_sata() returns invisible(x)", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_invisible(set_sata(d, riagendr))
})


# ── set_sata() — error paths ───────────────────────────────────────────────

test_that("set_sata() errors when x is not a survey or data frame", {
  expect_error(
    set_sata(list(a = 1), a),
    class = "surveycore_error_not_survey_or_df"
  )
  expect_snapshot(error = TRUE, set_sata(list(a = 1), a))
})

test_that("set_sata() errors when both ... and variable provided", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    set_sata(d, riagendr, variable = "ridageyr"),
    class = "surveycore_error_sata_ambiguous_input"
  )
  expect_snapshot(
    error = TRUE,
    set_sata(d, riagendr, variable = "ridageyr")
  )
})

test_that("set_sata() errors when neither ... nor variable provided", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    set_sata(d),
    class = "surveycore_error_sata_no_vars"
  )
  expect_snapshot(error = TRUE, set_sata(d))
})

test_that("set_sata() errors when sata = NA", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    set_sata(d, riagendr, sata = NA),
    class = "surveycore_error_sata_not_logical"
  )
  expect_snapshot(error = TRUE, set_sata(d, riagendr, sata = NA))
})

test_that("set_sata() errors when sata = 'yes' (non-logical)", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    set_sata(d, riagendr, sata = "yes"),
    class = "surveycore_error_sata_not_logical"
  )
  expect_snapshot(error = TRUE, set_sata(d, riagendr, sata = "yes"))
})

test_that("set_sata(variable = character(0)) errors like no-vars", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    set_sata(d, variable = character(0)),
    class = "surveycore_error_sata_no_vars"
  )
})

test_that("set_sata() warns for non-existent variable (variable = path)", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_warning(
    set_sata(d, variable = c("riagendr", "not_a_column")),
    class = "surveycore_warning_var_not_found"
  )
})


# ── extract_sata() — happy path ────────────────────────────────────────────

test_that("extract_sata() returns TRUE for marked variables", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_sata(d, riagendr)
  result <- extract_sata(d, riagendr)
  expect_identical(result, c(riagendr = TRUE))
})

test_that("extract_sata() returns FALSE for unmarked variables (default fill)", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  result <- extract_sata(d, riagendr, ridageyr)
  expect_identical(result, c(riagendr = FALSE, ridageyr = FALSE))
})

test_that("extract_sata() fill = NULL returns only marked variables", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_sata(d, riagendr)
  result <- extract_sata(d, riagendr, ridageyr, fill = NULL)
  expect_identical(result, c(riagendr = TRUE))
  expect_false("ridageyr" %in% names(result))
})

test_that("extract_sata() format = 'data_frame' returns correct tibble", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_sata(d, riagendr)
  result <- extract_sata(d, riagendr, ridageyr, format = "data_frame")
  expect_true(inherits(result, "tbl_df"))
  expect_named(result, c("variable", "sata"))
  expect_identical(result$variable, c("riagendr", "ridageyr"))
  expect_identical(result$sata, c(TRUE, FALSE))
})

test_that("extract_sata() format = 'list' returns correct list", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_sata(d, riagendr)
  result <- extract_sata(d, riagendr, ridageyr, format = "list")
  expect_identical(result, list(riagendr = TRUE, ridageyr = FALSE))
})

test_that("extract_sata() on data frame reads column attributes", {
  df <- data.frame(
    a = 1:5,
    b = 1:5,
    c = 1:5,
    stringsAsFactors = FALSE
  )
  df <- set_sata(df, a, b)
  result <- extract_sata(df, a, b, c)
  expect_identical(result, c(a = TRUE, b = TRUE, c = FALSE))
})

test_that("extract_sata() empty ... + fill = FALSE returns dense result", {
  df <- data.frame(a = 1:3, b = 1:3, stringsAsFactors = FALSE)
  df <- set_sata(df, a)
  result <- extract_sata(df)
  expect_identical(result, c(a = TRUE, b = FALSE))
})

test_that("extract_sata() empty ... + fill = NULL returns sparse result", {
  df <- data.frame(a = 1:3, b = 1:3, stringsAsFactors = FALSE)
  df <- set_sata(df, a)
  result <- extract_sata(df, fill = NULL)
  expect_identical(result, c(a = TRUE))
})

test_that("extract_sata() with named vars + fill = NULL omits non-SATA vars", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_sata(d, riagendr)
  result <- extract_sata(d, riagendr, ridageyr, fill = NULL)
  expect_identical(result, c(riagendr = TRUE))
})

test_that("roundtrip: set_sata() then extract_sata() recovers flags", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_sata(d, riagendr, ridageyr)
  result <- extract_sata(d, riagendr, ridageyr)
  expect_identical(result, c(riagendr = TRUE, ridageyr = TRUE))
})

test_that("survey object with zero SATA variables: length(x@metadata@sata) == 0L", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_identical(length(d@metadata@sata), 0L)
})


# ── extract_sata() — error paths ──────────────────────────────────────────

test_that("extract_sata() errors when x is not a survey or data frame", {
  expect_error(
    extract_sata(list(a = 1)),
    class = "surveycore_error_not_survey_or_df"
  )
  expect_snapshot(error = TRUE, extract_sata(list(a = 1)))
})

test_that("extract_sata() errors when fill = TRUE", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    extract_sata(d, fill = TRUE),
    class = "surveycore_error_fill_not_logical"
  )
  expect_snapshot(error = TRUE, extract_sata(d, fill = TRUE))
})

test_that("extract_sata() errors when fill = 'x' (invalid)", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    extract_sata(d, fill = "x"),
    class = "surveycore_error_fill_not_logical"
  )
  expect_snapshot(error = TRUE, extract_sata(d, fill = "x"))
})

test_that("extract_sata() errors when format is invalid", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    extract_sata(d, format = "tibble"),
    class = "surveycore_error_format_invalid"
  )
  expect_snapshot(error = TRUE, extract_sata(d, format = "tibble"))
})


# ── .extract_var_meta() integration with sata metadata ───────────────────────

test_that(".extract_var_meta() includes sata key in output", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_sata(d, riagendr)
  meta_sata <- surveycore:::.extract_var_meta(d, "riagendr")
  meta_no_sata <- surveycore:::.extract_var_meta(d, "ridageyr")
  expect_true(meta_sata$sata)
  expect_false(meta_no_sata$sata)
})


# ══════════════════════════════════════════════════════════════════════════════
# PR 1 — higher_is
# ══════════════════════════════════════════════════════════════════════════════

# ── set_higher_is() — happy paths ─────────────────────────────────────────────

test_that("set_higher_is() Conv 1 single: stores direction under variable name", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_higher_is(d, bpxsy1 = "worse")
  expect_identical(d@metadata@higher_is[["bpxsy1"]], "worse")
})

test_that("set_higher_is() Conv 1 multi-var: stores both directions", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_higher_is(d, bpxsy1 = "worse", bpxdi1 = "better")
  expect_identical(d@metadata@higher_is[["bpxsy1"]], "worse")
  expect_identical(d@metadata@higher_is[["bpxdi1"]], "better")
})

test_that("set_higher_is() Conv 2 named vector: stores both directions", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_higher_is(d, c(bpxsy1 = "worse", bpxdi1 = "better"))
  expect_identical(d@metadata@higher_is[["bpxsy1"]], "worse")
  expect_identical(d@metadata@higher_is[["bpxdi1"]], "better")
})

test_that("set_higher_is() Conv 3 scalar: variable + direction args", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_higher_is(d, variable = "bpxsy1", direction = "worse")
  expect_identical(d@metadata@higher_is[["bpxsy1"]], "worse")
})

test_that("set_higher_is() Conv 3 vector: multiple variables + direction vector", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_higher_is(
    d,
    variable = c("bpxsy1", "bpxdi1"),
    direction = c("worse", "worse")
  )
  expect_identical(d@metadata@higher_is[["bpxsy1"]], "worse")
  expect_identical(d@metadata@higher_is[["bpxdi1"]], "worse")
})

test_that("set_higher_is() NULL direction removes the entry", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_higher_is(d, bpxsy1 = "worse")
  d <- set_higher_is(d, bpxsy1 = NULL)
  expect_null(d@metadata@higher_is[["bpxsy1"]])
})

test_that("set_higher_is() on data frame stores attr on column", {
  df <- data.frame(score = 1:5, rating = 1:5)
  df <- set_higher_is(df, score = "worse")
  expect_identical(attr(df[["score"]], "higher_is"), "worse")
})

test_that("extract_higher_is() single bare name returns named character vector", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_higher_is(d, bpxsy1 = "worse")
  result <- extract_higher_is(d, bpxsy1)
  expect_identical(result, c(bpxsy1 = "worse"))
})

test_that("extract_higher_is() c() of bare names returns both", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_higher_is(d, bpxsy1 = "worse", bpxdi1 = "better")
  result <- extract_higher_is(d, c(bpxsy1, bpxdi1))
  expect_identical(result, c(bpxsy1 = "worse", bpxdi1 = "better"))
})

test_that("extract_higher_is() no ... returns all vars; unset are NA_character_", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_higher_is(d, bpxsy1 = "worse")
  result <- extract_higher_is(d)
  expect_identical(result[["bpxsy1"]], "worse")
  expect_identical(result[["ridageyr"]], NA_character_)
  expect_true(is.character(result))
  expect_true(!is.null(names(result)))
})

test_that("extract_higher_is() variable = character returns named vector", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_higher_is(d, bpxsy1 = "worse")
  result <- extract_higher_is(d, variable = "bpxsy1")
  expect_identical(result, c(bpxsy1 = "worse"))
})

# ── set_higher_is() — error paths ─────────────────────────────────────────────

test_that("set_higher_is() errors when x is not a survey or data frame", {
  expect_error(
    set_higher_is(list(x = 1), bpxsy1 = "worse"),
    class = "surveycore_error_not_survey_or_df"
  )
  expect_snapshot(error = TRUE, set_higher_is(list(x = 1), bpxsy1 = "worse"))
})

test_that("extract_higher_is() errors when x is not a survey or data frame", {
  expect_error(
    extract_higher_is(list(x = 1), bpxsy1),
    class = "surveycore_error_not_survey_or_df"
  )
  expect_snapshot(error = TRUE, extract_higher_is(list(x = 1), bpxsy1))
})

test_that("set_higher_is() errors for invalid direction (Conv 1)", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    set_higher_is(d, bpxsy1 = "neutral"),
    class = "surveycore_error_direction_invalid"
  )
  expect_snapshot(error = TRUE, set_higher_is(d, bpxsy1 = "neutral"))
})

test_that("set_higher_is() errors for invalid direction (Conv 3)", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    set_higher_is(d, variable = "bpxsy1", direction = "neutral"),
    class = "surveycore_error_direction_invalid"
  )
  expect_snapshot(
    error = TRUE,
    set_higher_is(d, variable = "bpxsy1", direction = "neutral")
  )
})

test_that("set_higher_is() errors when both ... and variable provided", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    set_higher_is(d, bpxsy1 = "worse", variable = "bpxsy1"),
    class = "surveycore_error_higher_is_ambiguous_input"
  )
  expect_snapshot(
    error = TRUE,
    set_higher_is(d, bpxsy1 = "worse", variable = "bpxsy1")
  )
})

test_that("extract_higher_is() errors when both ... and variable provided", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    extract_higher_is(d, bpxsy1, variable = "bpxsy1"),
    class = "surveycore_error_higher_is_ambiguous_input"
  )
  expect_snapshot(
    error = TRUE,
    extract_higher_is(d, bpxsy1, variable = "bpxsy1")
  )
})

test_that("set_higher_is() errors when no variable names provided", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    set_higher_is(d),
    class = "surveycore_error_higher_is_no_vars"
  )
  expect_snapshot(error = TRUE, set_higher_is(d))
})

test_that("set_higher_is() warns when variable not found in x (setter)", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_warning(
    set_higher_is(d, nonexistent_var_xyz = "worse"),
    class = "surveycore_warning_var_not_found"
  )
  expect_snapshot(set_higher_is(d, nonexistent_var_xyz = "worse"))
})

test_that("extract_higher_is() warns and returns character(0) when variable not found", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_warning(
    result <- extract_higher_is(d, variable = "nonexistent_var_xyz"),
    class = "surveycore_warning_var_not_found"
  )
  expect_identical(result, character(0))
  expect_null(names(result))
})

# ── set_higher_is() — edge cases ──────────────────────────────────────────────

test_that("set_higher_is() overwrite: last write wins", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_higher_is(d, bpxsy1 = "worse")
  d <- set_higher_is(d, bpxsy1 = "better")
  expect_identical(d@metadata@higher_is[["bpxsy1"]], "better")
})

test_that("extract_higher_is() all-NA when no higher_is set on any variable", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  result <- extract_higher_is(d)
  expect_true(all(is.na(result)))
  expect_true(is.character(result))
})

test_that(".extract_var_meta() includes higher_is key in output", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_higher_is(d, bpxsy1 = "worse")
  meta <- surveycore:::.extract_var_meta(d, "bpxsy1")
  expect_identical(meta$higher_is, "worse")
})

# ── PR 2 — reverse_coded ──────────────────────────────────────────────────────

# ── set_reverse_coded() — happy paths ─────────────────────────────────────────

test_that("set_reverse_coded() Convention A (bare name) stores TRUE for variable", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_reverse_coded(d, bpxsy1)
  expect_identical(d@metadata@reverse_coded[["bpxsy1"]], TRUE)
})

test_that("set_reverse_coded() Convention A multiple bare names stores TRUE for both", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_reverse_coded(d, bpxsy1, ridageyr)
  expect_identical(d@metadata@reverse_coded[["bpxsy1"]], TRUE)
  expect_identical(d@metadata@reverse_coded[["ridageyr"]], TRUE)
})

test_that("set_reverse_coded() with reverse_coded = FALSE removes the entry", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_reverse_coded(d, bpxsy1)
  d <- set_reverse_coded(d, bpxsy1, reverse_coded = FALSE)
  expect_null(d@metadata@reverse_coded[["bpxsy1"]])
})

test_that("set_reverse_coded() Convention B (variable=) stores TRUE for all listed vars", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_reverse_coded(d, variable = c("bpxsy1", "ridageyr"))
  expect_identical(d@metadata@reverse_coded[["bpxsy1"]], TRUE)
  expect_identical(d@metadata@reverse_coded[["ridageyr"]], TRUE)
})

test_that("set_reverse_coded() data frame path stores attr on column", {
  df <- data.frame(x = 1:5, y = 6:10)
  df <- set_reverse_coded(df, x)
  expect_identical(attr(df$x, "reverse_coded"), TRUE)
  expect_null(attr(df$y, "reverse_coded"))
})

test_that("extract_reverse_coded() single bare name returns named logical TRUE", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_reverse_coded(d, bpxsy1)
  result <- extract_reverse_coded(d, bpxsy1)
  expect_identical(result, c(bpxsy1 = TRUE))
})

test_that("extract_reverse_coded() no var arg returns all variables; unset => FALSE", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_reverse_coded(d, bpxsy1)
  result <- extract_reverse_coded(d)
  expect_true(is.logical(result))
  expect_true(result[["bpxsy1"]])
  expect_false(result[["ridageyr"]])
})

test_that("extract_reverse_coded() variable = char vector returns named logical", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_reverse_coded(d, bpxsy1)
  result <- extract_reverse_coded(d, variable = "bpxsy1")
  expect_identical(result, c(bpxsy1 = TRUE))
})

# ── set_reverse_coded() / extract_reverse_coded() — error paths ───────────────

test_that("set_reverse_coded() errors for non-survey non-df input", {
  expect_error(
    set_reverse_coded(list(x = 1), bpxsy1),
    class = "surveycore_error_not_survey_or_df"
  )
  expect_snapshot(error = TRUE, set_reverse_coded(list(x = 1), bpxsy1))
})

test_that("extract_reverse_coded() errors for non-survey non-df input", {
  expect_error(
    extract_reverse_coded(list(x = 1), bpxsy1),
    class = "surveycore_error_not_survey_or_df"
  )
  expect_snapshot(error = TRUE, extract_reverse_coded(list(x = 1), bpxsy1))
})

test_that("set_reverse_coded() errors when reverse_coded = NA", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    set_reverse_coded(d, bpxsy1, reverse_coded = NA),
    class = "surveycore_error_reverse_coded_not_logical"
  )
  expect_snapshot(
    error = TRUE,
    set_reverse_coded(d, bpxsy1, reverse_coded = NA)
  )
})

test_that("set_reverse_coded() errors when both ... and variable provided (setter)", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    set_reverse_coded(d, bpxsy1, variable = "bpxsy1"),
    class = "surveycore_error_reverse_coded_ambiguous_input"
  )
  expect_snapshot(
    error = TRUE,
    set_reverse_coded(d, bpxsy1, variable = "bpxsy1")
  )
})

test_that("extract_reverse_coded() errors when both ... and variable provided (extractor)", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    extract_reverse_coded(d, bpxsy1, variable = "bpxsy1"),
    class = "surveycore_error_reverse_coded_ambiguous_input"
  )
  expect_snapshot(
    error = TRUE,
    extract_reverse_coded(d, bpxsy1, variable = "bpxsy1")
  )
})

test_that("set_reverse_coded() errors when no variable names provided", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    set_reverse_coded(d),
    class = "surveycore_error_reverse_coded_no_vars"
  )
  expect_snapshot(error = TRUE, set_reverse_coded(d))
})

test_that("set_reverse_coded() warns when variable not found in x", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_warning(
    set_reverse_coded(d, variable = "nonexistent_var_xyz"),
    class = "surveycore_warning_var_not_found"
  )
  expect_snapshot(set_reverse_coded(d, variable = "nonexistent_var_xyz"))
})

test_that("extract_reverse_coded() warns and returns logical(0) when variable not found", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_warning(
    result <- extract_reverse_coded(d, variable = "nonexistent_var_xyz"),
    class = "surveycore_warning_var_not_found"
  )
  expect_identical(result, logical(0))
})

# ── set_reverse_coded() / extract_reverse_coded() — edge cases ────────────────

test_that("set then unset: extract_reverse_coded() returns FALSE after reverse_coded = FALSE", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_reverse_coded(d, bpxsy1)
  expect_true(extract_reverse_coded(d, bpxsy1)[["bpxsy1"]])
  d <- set_reverse_coded(d, bpxsy1, reverse_coded = FALSE)
  expect_false(extract_reverse_coded(d, bpxsy1)[["bpxsy1"]])
})

test_that("extract_reverse_coded() returns logical(0) when zero-length variable= result", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_warning(
    result <- extract_reverse_coded(d, variable = "nonexistent_xyz"),
    class = "surveycore_warning_var_not_found"
  )
  expect_identical(result, logical(0))
  expect_true(is.logical(result))
})


# ── .check_is_survey() — D44: merged v-bullet (no bare ' ' key) ───────────

test_that(".check_is_survey() error message uses single v bullet (no split ' ' key)", {
  expect_error(
    surveycore:::.check_is_survey(list(x = 1)),
    class = "surveycore_error_not_survey"
  )
  expect_snapshot(error = TRUE, surveycore:::.check_is_survey(list(x = 1)))
})


# ── extract_sata() — D45: fill error class is surveycore_error_fill_not_logical

test_that("extract_sata() fill = TRUE errors with surveycore_error_fill_not_logical", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    extract_sata(d, fill = TRUE),
    class = "surveycore_error_fill_not_logical"
  )
  expect_snapshot(error = TRUE, extract_sata(d, fill = TRUE))
})

test_that("extract_sata() fill = 1L errors with surveycore_error_fill_not_logical", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    extract_sata(d, fill = 1L),
    class = "surveycore_error_fill_not_logical"
  )
})

test_that("set_sata() sata = 'yes' still errors with surveycore_error_sata_not_logical [regression]", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    set_sata(d, riagendr, sata = "yes"),
    class = "surveycore_error_sata_not_logical"
  )
})
