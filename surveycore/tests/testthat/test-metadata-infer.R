# tests/testthat/test-metadata-infer.R
#
# Tests for R/metadata-infer.R
# Error/warning coverage map (plans/error-messages.md):
#   Row 78: surveycore_error_not_survey_or_df        — unknown input type
#   Row 79: surveycore_warning_preface_not_overwritten — overwrite = FALSE
#   Row 80: surveycore_warning_empty_label_after_trim  — empty suffix after trim


# ── Test fixtures ─────────────────────────────────────────────────────────────

# Data frame with 3 battery variables sharing "- " preface + 1 unlabelled var.
make_battery_df <- function() {
  df <- data.frame(
    discrim_a = 1:5,
    discrim_b = 2:6,
    discrim_c = 3:7,
    age       = 25:29,
    stringsAsFactors = FALSE
  )
  attr(df$discrim_a, "label") <-
    "Please rate discrimination - Evangelical Christians"
  attr(df$discrim_b, "label") <- "Please rate discrimination - Muslims"
  attr(df$discrim_c, "label") <- "Please rate discrimination - Jews"
  df
}

# Survey object built via as_survey() so labels land in @metadata.
make_battery_survey <- function() {
  df    <- make_battery_df()
  df$wt <- rep(1.0, 5L)
  as_survey(df, weights = wt)
}

# Survey object built directly (bypassing as_survey) so labels stay on @data.
make_battery_survey_direct <- function() {
  df    <- make_battery_df()
  df$wt <- rep(1.0, 5L)
  survey_taylor(
    data = df,
    variables = list(
      ids            = NULL,
      weights        = "wt",
      strata         = NULL,
      fpc            = NULL,
      nest           = FALSE,
      probs_provided = FALSE
    )
  )
}


# ── Happy path: separator detection ───────────────────────────────────────────

test_that("infer_question_prefaces() extracts preface via ' - ' separator on data frame", {
  df     <- make_battery_df()
  result <- infer_question_prefaces(df, verbose = FALSE)

  # Preface applied to all three battery variables.
  expect_identical(
    attr(result$discrim_a, "question_preface"),
    "Please rate discrimination"
  )
  expect_identical(
    attr(result$discrim_b, "question_preface"),
    "Please rate discrimination"
  )
  expect_identical(
    attr(result$discrim_c, "question_preface"),
    "Please rate discrimination"
  )

  # Labels trimmed to unique suffixes.
  expect_identical(attr(result$discrim_a, "label"), "Evangelical Christians")
  expect_identical(attr(result$discrim_b, "label"), "Muslims")
  expect_identical(attr(result$discrim_c, "label"), "Jews")

  # Unlabelled column untouched.
  expect_null(attr(result$age, "question_preface"))
  expect_null(attr(result$age, "label"))
})

test_that("infer_question_prefaces() handles two distinct batteries in one data frame", {
  df <- data.frame(
    q1_a = 1:5,
    q1_b = 2:6,
    q2_a = 1:5,
    q2_b = 2:6,
    stringsAsFactors = FALSE
  )
  attr(df$q1_a, "label") <- "Battery One: item A"
  attr(df$q1_b, "label") <- "Battery One: item B"
  attr(df$q2_a, "label") <- "Battery Two: item A"
  attr(df$q2_b, "label") <- "Battery Two: item B"

  result <- infer_question_prefaces(df, verbose = FALSE)

  expect_identical(attr(result$q1_a, "question_preface"), "Battery One")
  expect_identical(attr(result$q2_a, "question_preface"), "Battery Two")
  expect_identical(attr(result$q1_a, "label"), "item A")
  expect_identical(attr(result$q1_b, "label"), "item B")
  expect_identical(attr(result$q2_a, "label"), "item A")
  expect_identical(attr(result$q2_b, "label"), "item B")
})

test_that("infer_question_prefaces() emits cli output when verbose = TRUE", {
  df <- make_battery_df()
  expect_message(infer_question_prefaces(df, verbose = TRUE))
})

test_that("infer_question_prefaces() suppresses cli output when verbose = FALSE", {
  df <- make_battery_df()
  expect_no_message(infer_question_prefaces(df, verbose = FALSE))
})


# ── Happy path: LCP detection ─────────────────────────────────────────────────

test_that("infer_question_prefaces() extracts preface via LCP when no separator matches", {
  df <- data.frame(
    trust_a = 1:5,
    trust_b = 2:6,
    trust_c = 3:7,
    stringsAsFactors = FALSE
  )
  attr(df$trust_a, "label") <- "How much do you trust the government institutions"
  attr(df$trust_b, "label") <- "How much do you trust the media organizations"
  attr(df$trust_c, "label") <- "How much do you trust the scientific community"

  result <- infer_question_prefaces(
    df,
    sep     = c(" - "), # separator not present in these labels
    lcp_min = 20L,
    verbose = FALSE
  )

  # LCP = "How much do you trust the " → trim to word boundary → "How much do you trust"
  expect_identical(
    attr(result$trust_a, "question_preface"),
    "How much do you trust"
  )
  expect_identical(attr(result$trust_a, "label"), "the government institutions")
  expect_identical(attr(result$trust_b, "label"), "the media organizations")
  expect_identical(attr(result$trust_c, "label"), "the scientific community")
})

test_that("infer_question_prefaces() does not extract when LCP is below lcp_min", {
  df <- data.frame(x = 1:5, y = 2:6, stringsAsFactors = FALSE)
  # LCP = "ABC item " → trimmed to word boundary "ABC item" = 8 chars < 20
  attr(df$x, "label") <- "ABC item A"
  attr(df$y, "label") <- "ABC item B"

  result <- infer_question_prefaces(
    df,
    sep     = character(0L), # skip separator pass
    lcp_min = 20L,
    verbose = FALSE
  )

  # No extraction: labels unchanged, no preface attribute added.
  expect_null(attr(result$x, "question_preface"))
  expect_identical(attr(result$x, "label"), "ABC item A")
  expect_identical(attr(result$y, "label"), "ABC item B")
})

test_that("infer_question_prefaces() falls through to LCP when sep = character(0L)", {
  df <- data.frame(x = 1:5, y = 2:6, stringsAsFactors = FALSE)
  attr(df$x, "label") <- "How much do you trust the government"
  attr(df$y, "label") <- "How much do you trust the media"

  result <- infer_question_prefaces(
    df,
    sep     = character(0L),
    lcp_min = 20L,
    verbose = FALSE
  )

  # LCP = "How much do you trust the " → "How much do you trust" (21 chars ≥ 20)
  expect_identical(
    attr(result$x, "question_preface"),
    "How much do you trust"
  )
  expect_identical(attr(result$x, "label"), "the government")
  expect_identical(attr(result$y, "label"), "the media")
})


# ── Happy path: dispatch ──────────────────────────────────────────────────────

test_that("infer_question_prefaces() writes to @metadata for survey objects (as_survey path)", {
  d      <- make_battery_survey()
  result <- infer_question_prefaces(d, verbose = FALSE)

  expect_true(S7::S7_inherits(result, survey_base))
  expect_identical(
    result@metadata@question_prefaces[["discrim_a"]],
    "Please rate discrimination"
  )
  expect_identical(
    result@metadata@variable_labels[["discrim_a"]],
    "Evangelical Christians"
  )
  # Other variables also trimmed.
  expect_identical(result@metadata@variable_labels[["discrim_b"]], "Muslims")
  expect_identical(result@metadata@variable_labels[["discrim_c"]], "Jews")
})

test_that("infer_question_prefaces() reads haven fallback labels for direct survey objects", {
  # survey_taylor built directly — labels live in @data attrs, not @metadata.
  d      <- make_battery_survey_direct()
  result <- infer_question_prefaces(d, verbose = FALSE)

  expect_identical(
    result@metadata@question_prefaces[["discrim_a"]],
    "Please rate discrimination"
  )
  expect_identical(
    result@metadata@variable_labels[["discrim_a"]],
    "Evangelical Christians"
  )
})

test_that("infer_question_prefaces() on data frame: as_survey() picks up preface", {
  df  <- make_battery_df()
  df$wt <- rep(1.0, 5L)
  df2 <- infer_question_prefaces(df, verbose = FALSE)
  d   <- as_survey(df2, weights = wt)

  expect_identical(
    d@metadata@question_prefaces[["discrim_a"]],
    "Please rate discrimination"
  )
  expect_identical(
    d@metadata@variable_labels[["discrim_a"]],
    "Evangelical Christians"
  )
})


# ── overwrite argument ────────────────────────────────────────────────────────

test_that("infer_question_prefaces() skips vars with existing prefaces when overwrite = FALSE", {
  d <- make_battery_survey()
  d <- set_question_preface(d, discrim_a = "Existing preface")

  expect_warning(
    result <- infer_question_prefaces(d, overwrite = FALSE, verbose = FALSE),
    class = "surveycore_warning_preface_not_overwritten"
  )

  # discrim_a: kept existing preface, label NOT trimmed.
  expect_identical(
    result@metadata@question_prefaces[["discrim_a"]],
    "Existing preface"
  )
  expect_identical(
    result@metadata@variable_labels[["discrim_a"]],
    "Please rate discrimination - Evangelical Christians"
  )

  # discrim_b and discrim_c: processed normally.
  expect_identical(
    result@metadata@question_prefaces[["discrim_b"]],
    "Please rate discrimination"
  )
  expect_identical(result@metadata@variable_labels[["discrim_b"]], "Muslims")
})

test_that("infer_question_prefaces() replaces existing prefaces when overwrite = TRUE", {
  d <- make_battery_survey()
  d <- set_question_preface(d, discrim_a = "Old preface")

  expect_no_warning(
    result <- infer_question_prefaces(d, overwrite = TRUE, verbose = FALSE)
  )

  expect_identical(
    result@metadata@question_prefaces[["discrim_a"]],
    "Please rate discrimination"
  )
  expect_identical(
    result@metadata@variable_labels[["discrim_a"]],
    "Evangelical Christians"
  )
})


# ── Edge cases ────────────────────────────────────────────────────────────────

test_that("infer_question_prefaces() returns x unchanged when no shared preface", {
  df <- data.frame(x = 1:5, y = 2:6, stringsAsFactors = FALSE)
  attr(df$x, "label") <- "Completely different label one"
  attr(df$y, "label") <- "Totally unrelated label two"

  result <- infer_question_prefaces(df, verbose = FALSE)

  expect_null(attr(result$x, "question_preface"))
  expect_null(attr(result$y, "question_preface"))
  expect_identical(attr(result$x, "label"), "Completely different label one")
  expect_identical(attr(result$y, "label"), "Totally unrelated label two")
})

test_that("infer_question_prefaces() ignores groups below min_vars", {
  df <- data.frame(
    bat_a = 1:5,
    other = 2:6,
    stringsAsFactors = FALSE
  )
  attr(df$bat_a, "label") <- "Battery - item A"
  attr(df$other, "label") <- "Something completely different"

  # bat_a contains " - " but is the only var with preface "Battery" → < min_vars=2
  result <- infer_question_prefaces(df, verbose = FALSE)

  expect_null(attr(result$bat_a, "question_preface"))
  expect_identical(attr(result$bat_a, "label"), "Battery - item A") # unchanged
})

test_that("infer_question_prefaces() skips variable when trimming produces empty suffix", {
  df <- data.frame(
    bat_a = 1:5,
    bat_b = 2:6,
    stringsAsFactors = FALSE
  )
  attr(df$bat_a, "label") <- "Battery - "   # suffix after " - " would be ""
  attr(df$bat_b, "label") <- "Battery - item B"

  expect_warning(
    result <- infer_question_prefaces(df, verbose = FALSE),
    class = "surveycore_warning_empty_label_after_trim"
  )

  # bat_a: skipped, label and preface unchanged.
  expect_null(attr(result$bat_a, "question_preface"))
  expect_identical(attr(result$bat_a, "label"), "Battery - ")

  # bat_b: applied normally.
  expect_identical(attr(result$bat_b, "question_preface"), "Battery")
  expect_identical(attr(result$bat_b, "label"), "item B")
})

test_that("infer_question_prefaces() returns data frame unchanged when no label attrs", {
  df     <- data.frame(x = 1:5, y = 2:6, stringsAsFactors = FALSE)
  result <- infer_question_prefaces(df, verbose = FALSE)

  expect_null(attr(result$x, "question_preface"))
  expect_null(attr(result$y, "question_preface"))
})

test_that("infer_question_prefaces() handles mix of unique and shared labels", {
  df <- data.frame(
    bat_a = 1:5,
    bat_b = 2:6,
    bat_c = 3:7,
    age   = 25:29,
    stringsAsFactors = FALSE
  )
  attr(df$bat_a, "label") <- "How much do you agree - Statement A"
  attr(df$bat_b, "label") <- "How much do you agree - Statement B"
  attr(df$bat_c, "label") <- "How much do you agree - Statement C"
  attr(df$age,   "label") <- "Age of respondent - measured in years"

  result <- infer_question_prefaces(df, verbose = FALSE)

  # Battery group extracted correctly.
  expect_identical(
    attr(result$bat_a, "question_preface"),
    "How much do you agree"
  )
  expect_identical(attr(result$bat_a, "label"), "Statement A")
  expect_identical(attr(result$bat_b, "label"), "Statement B")
  expect_identical(attr(result$bat_c, "label"), "Statement C")

  # age: "Age of respondent" appears in only 1 label → not extracted.
  expect_null(attr(result$age, "question_preface"))
  expect_identical(attr(result$age, "label"), "Age of respondent - measured in years")
})

test_that("infer_question_prefaces() splits on FIRST separator occurrence only", {
  df <- data.frame(
    q1 = 1:5,
    q2 = 2:6,
    stringsAsFactors = FALSE
  )
  # Separator appears twice in each label.
  attr(df$q1, "label") <- "Q1 - Part A - Detail"
  attr(df$q2, "label") <- "Q1 - Part B - Detail"

  result <- infer_question_prefaces(df, verbose = FALSE)

  # Preface = text before FIRST " - "; embedded separator preserved in suffix.
  expect_identical(attr(result$q1, "question_preface"), "Q1")
  expect_identical(attr(result$q1, "label"), "Part A - Detail")
  expect_identical(attr(result$q2, "label"), "Part B - Detail")
})

test_that("infer_question_prefaces() returns invisible(x)", {
  df <- make_battery_df()
  # invisible() means the value is not auto-printed but is returned.
  ret <- withVisible(infer_question_prefaces(df, verbose = FALSE))
  expect_false(ret$visible)
  expect_s3_class(ret$value, "data.frame")
})


# ── Errors ────────────────────────────────────────────────────────────────────

test_that("infer_question_prefaces() rejects non-survey non-data-frame input", {
  expect_error(
    infer_question_prefaces(list(x = 1)),
    class = "surveycore_error_not_survey_or_df"
  )
  expect_snapshot(error = TRUE, infer_question_prefaces(list(x = 1)))
})

# ---------------------------------------------------------------------------
# Additional coverage: .find_lcp() and .trim_to_word_boundary() edge cases
# ---------------------------------------------------------------------------

test_that(".find_lcp() returns empty string for empty input", {
  result <- surveycore:::.find_lcp(character(0))
  expect_identical(result, "")
})

test_that(".find_lcp() returns the string itself for single-element input", {
  result <- surveycore:::.find_lcp("hello world")
  expect_identical(result, "hello world")
})

test_that(".find_lcp() returns empty string when shortest string has zero characters", {
  result <- surveycore:::.find_lcp(c("abc", ""))
  expect_identical(result, "")
})

test_that(".find_lcp() stops at first mismatch and returns correct prefix", {
  result <- surveycore:::.find_lcp(c("abcde", "abcfg", "abcxy"))
  expect_identical(result, "abc")
})

test_that(".find_lcp() returns full string when all strings are identical", {
  result <- surveycore:::.find_lcp(c("hello", "hello", "hello"))
  expect_identical(result, "hello")
})

test_that(".trim_to_word_boundary() returns empty string for empty input", {
  result <- surveycore:::.trim_to_word_boundary("")
  expect_identical(result, "")
})

test_that(".trim_to_word_boundary() always trims to last word boundary", {
  # "hello world" → last space at pos 6 → returns "hello" (trims "world")
  result <- surveycore:::.trim_to_word_boundary("hello world")
  expect_identical(result, "hello")
})

test_that(".trim_to_word_boundary() trims trailing whitespace then trims to last word", {
  # "hello world  " → trimws → "hello world" → trims "world" → "hello"
  result <- surveycore:::.trim_to_word_boundary("hello world  ")
  expect_identical(result, "hello")
})

test_that(".trim_to_word_boundary() returns trimmed string when no internal whitespace", {
  # Single word with no spaces: last_space == -1 → return trimmed string
  result <- surveycore:::.trim_to_word_boundary("nospaces")
  expect_identical(result, "nospaces")
})

test_that("infer_question_prefaces() handles labels with no common prefix", {
  # Labels with no shared prefix → empty LCP → no preface extracted
  df <- data.frame(
    x = 1:4, y = 1:4, z = 1:4, w = rep(1, 4)
  )
  attr(df$x, "label") <- "Apple is red"
  attr(df$y, "label") <- "Banana is yellow"
  attr(df$z, "label") <- "Cherry is red"
  sc <- suppressWarnings(as_survey(df, weights = w))
  result <- infer_question_prefaces(sc)
  # No common prefix → prefaces should be NULL or absent
  expect_true(is.null(result@metadata@question_prefaces[["x"]]) ||
              identical(result@metadata@question_prefaces[["x"]], ""))
})
