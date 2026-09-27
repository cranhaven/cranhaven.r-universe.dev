# Tests for classify_question_type()
#
# classify_question_type() groups variables by their question_preface metadata
# and classifies each group into one of three types:
#   - "single"  — variable has no question_preface, OR is the only requested var
#                 with its preface
#   - "sata"    — variables share a question_preface AND at least one is flagged
#                 as SATA via set_sata()
#   - "battery" — variables share a question_preface but none are flagged SATA

# ── classify_question_type() — happy path ────────────────────────────────────

test_that("classify_question_type() returns type = 'single' for vars with no preface", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  result <- classify_question_type(d, riagendr, ridageyr)

  expect_s3_class(result, "tbl_df")
  expect_identical(nrow(result), 2L)
  expect_identical(result$variable, c("riagendr", "ridageyr"))
  expect_identical(result$type, c("single", "single"))
  expect_identical(result$question_preface, c(NA_character_, NA_character_))
})

test_that("classify_question_type() classifies SATA group as 'sata'", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_question_preface(
    d,
    riagendr = "Demographics preface",
    ridageyr = "Demographics preface"
  )
  d <- set_sata(d, riagendr, ridageyr)

  result <- classify_question_type(d, riagendr, ridageyr)

  expect_identical(nrow(result), 2L)
  expect_identical(result$type, c("sata", "sata"))
  expect_identical(result$group, c(1L, 1L))
})

test_that("classify_question_type() classifies shared-preface non-SATA as 'battery'", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_question_preface(
    d,
    riagendr = "Battery preface",
    ridageyr = "Battery preface"
  )

  result <- classify_question_type(d, riagendr, ridageyr)

  expect_identical(nrow(result), 2L)
  expect_identical(result$type, c("battery", "battery"))
  expect_identical(result$group, c(1L, 1L))
})

test_that("classify_question_type() handles mixed types with sequential group numbers", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_question_preface(
    d,
    riagendr = "SATA preface",
    ridageyr = "SATA preface",
    bpxsy1 = "Battery preface",
    bpxdi1 = "Battery preface"
  )
  d <- set_sata(d, riagendr, ridageyr)

  result <- classify_question_type(
    d,
    indfmpir,
    riagendr,
    ridageyr,
    bpxsy1,
    bpxdi1
  )

  expect_identical(nrow(result), 5L)
  expect_identical(
    result$variable,
    c("indfmpir", "riagendr", "ridageyr", "bpxsy1", "bpxdi1")
  )
  expect_identical(
    result$type,
    c("single", "sata", "sata", "battery", "battery")
  )
  expect_identical(result$group, c(1L, 2L, 2L, 3L, 3L))
})

test_that("classify_question_type() output tibble has expected columns and types", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  result <- classify_question_type(d, riagendr)

  expect_identical(
    names(result),
    c("variable", "question_preface", "type", "group")
  )
  expect_type(result$variable, "character")
  expect_type(result$question_preface, "character")
  expect_type(result$type, "character")
  expect_type(result$group, "integer")
})

test_that("classify_question_type() variable = interface works", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_question_preface(
    d,
    riagendr = "Demographics",
    ridageyr = "Demographics"
  )
  d <- set_sata(d, riagendr, ridageyr)

  result <- classify_question_type(
    d,
    variable = c("riagendr", "ridageyr")
  )

  expect_identical(nrow(result), 2L)
  expect_identical(result$type, c("sata", "sata"))
})

test_that("classify_question_type() group numbers are sequential by first appearance", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_question_preface(
    d,
    riagendr = "Preface A",
    ridageyr = "Preface A",
    bpxsy1 = "Preface B",
    bpxdi1 = "Preface B"
  )

  # Order: B, A, B, A — groups assigned in first-appearance order
  result <- classify_question_type(d, bpxsy1, riagendr, bpxdi1, ridageyr)
  expect_identical(result$group, c(1L, 2L, 1L, 2L))
})

# ── classify_question_type() — error paths ────────────────────────────────────

test_that("classify_question_type() errors when no variables provided", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    classify_question_type(d),
    class = "surveycore_error_detect_no_vars"
  )
  expect_snapshot(error = TRUE, classify_question_type(d))
})

test_that("classify_question_type() errors when both ... and variable provided", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    classify_question_type(d, riagendr, variable = "ridageyr"),
    class = "surveycore_error_detect_ambiguous_input"
  )
  expect_snapshot(
    error = TRUE,
    classify_question_type(d, riagendr, variable = "ridageyr")
  )
})

test_that("classify_question_type() errors on non-survey non-data-frame input", {
  expect_error(
    classify_question_type(list(a = 1), a),
    class = "surveycore_error_not_survey_or_df"
  )
  expect_snapshot(error = TRUE, classify_question_type(list(a = 1), a))
})

# ── classify_question_type() — edge cases ────────────────────────────────────

test_that("classify_question_type() single var with question_preface → 'single'", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_question_preface(d, riagendr = "Solo preface")

  result <- classify_question_type(d, riagendr)

  expect_identical(nrow(result), 1L)
  expect_identical(result$type, "single")
  expect_identical(result$question_preface, "Solo preface")
})

test_that("classify_question_type() warns and returns 'single' for SATA var with unique preface", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_question_preface(d, riagendr = "Solo SATA preface")
  d <- set_sata(d, riagendr)

  expect_warning(
    result <- classify_question_type(d, riagendr),
    class = "surveycore_warning_sata_no_preface"
  )
  expect_identical(result$type, "single")
})

test_that("classify_question_type() warns on mixed SATA status within a preface group", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_question_preface(
    d,
    riagendr = "Mixed preface",
    ridageyr = "Mixed preface"
  )
  d <- set_sata(d, riagendr) # only one of the two marked SATA

  expect_warning(
    result <- classify_question_type(d, riagendr, ridageyr),
    class = "surveycore_warning_sata_mixed_group"
  )
  expect_identical(result$type, c("sata", "sata"))
})

test_that("classify_question_type() returns zero-row tibble when all vars not found", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )

  suppressWarnings(
    result <- classify_question_type(
      d,
      variable = c("not_a_col", "also_not_a_col")
    )
  )
  expect_identical(nrow(result), 0L)
  expect_identical(
    names(result),
    c("variable", "question_preface", "type", "group")
  )
})

test_that("classify_question_type(variable = character(0)) errors with no_vars class", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_error(
    classify_question_type(d, variable = character(0)),
    class = "surveycore_error_detect_no_vars"
  )
})

test_that("classify_question_type() works on a data frame via column attributes", {
  df <- data.frame(
    q1 = 1:5,
    news_tv = 1:5,
    news_online = 1:5,
    age = 21:25
  )
  attr(df$news_tv, "question_preface") <- "News preface"
  attr(df$news_online, "question_preface") <- "News preface"
  attr(df$news_tv, "sata") <- TRUE
  attr(df$news_online, "sata") <- TRUE

  result <- classify_question_type(df, q1, news_tv, news_online, age)

  expect_identical(nrow(result), 4L)
  expect_identical(result$type, c("single", "sata", "sata", "single"))
  expect_identical(result$group, c(1L, 2L, 2L, 3L))
})

test_that("classify_question_type() shared preface but only one requested → 'single'", {
  # Spec Section IX edge case: when only one var from a preface group is
  # requested, the grouping is among requested vars only → type = single.
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  d <- set_question_preface(
    d,
    riagendr = "Shared preface",
    ridageyr = "Shared preface"
  )
  # Only request one of the two sharers
  result <- classify_question_type(d, riagendr)

  expect_identical(nrow(result), 1L)
  expect_identical(result$type, "single")
})

test_that("classify_question_type() skips missing vars with warning", {
  d <- as_survey(
    nhanes_2017,
    ids = sdmvpsu,
    weights = wtint2yr,
    strata = sdmvstra,
    nest = TRUE
  )
  expect_warning(
    result <- classify_question_type(
      d,
      variable = c("riagendr", "not_a_column")
    ),
    class = "surveycore_warning_var_not_found"
  )
  expect_identical(nrow(result), 1L)
  expect_identical(result$variable, "riagendr")
})
