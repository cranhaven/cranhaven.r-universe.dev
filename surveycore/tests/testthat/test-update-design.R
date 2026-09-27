# tests/testthat/test-update-design.R
#
# Tests for update_design() — R/08-update-design.R
#
# Coverage map (error-messages.md):
#   Row 36: cli_inform fires when at least one design variable changes

# ── Happy path: survey_taylor ──────────────────────────────────────────────────

test_that("update_design() updates weights on survey_taylor", {
  df <- make_survey_data(n = 200L, seed = 1L)
  df$wt2 <- df$wt * 1.1
  d <- as_survey(df, weights = wt, strata = strata)
  d2 <- suppressMessages(update_design(d, weights = wt2))
  test_invariants(d2)
  expect_identical(d2@variables$weights, "wt2")
})

test_that("update_design() updates strata on survey_taylor", {
  df <- make_survey_data(n = 200L, seed = 2L)
  df$st2 <- df$strata
  d <- as_survey(df, weights = wt, strata = strata)
  d2 <- suppressMessages(update_design(d, strata = st2))
  test_invariants(d2)
  expect_identical(d2@variables$strata, "st2")
})

test_that("update_design() updates multiple vars on survey_taylor", {
  df <- make_survey_data(n = 200L, seed = 3L)
  df$wt2 <- df$wt * 1.1
  df$st2 <- df$strata
  d <- as_survey(df, weights = wt, strata = strata)
  d2 <- suppressMessages(update_design(d, weights = wt2, strata = st2))
  test_invariants(d2)
  expect_identical(d2@variables$weights, "wt2")
  expect_identical(d2@variables$strata, "st2")
})

test_that("update_design() updates ids on survey_taylor", {
  df <- make_survey_data(n = 200L, seed = 4L)
  df$psu2 <- paste0("new_", df$psu)
  d <- as_survey(df, ids = psu, weights = wt)
  d2 <- suppressMessages(update_design(d, ids = psu2))
  test_invariants(d2)
  expect_identical(d2@variables$ids, "psu2")
})

test_that("update_design() updates fpc on survey_taylor", {
  df <- make_survey_data(n = 200L, seed = 5L)
  df$fpc2 <- df$fpc + 10L
  d <- as_survey(df, weights = wt, strata = strata, fpc = fpc)
  d2 <- suppressMessages(update_design(d, fpc = fpc2))
  test_invariants(d2)
  expect_identical(d2@variables$fpc, "fpc2")
})


# ── Happy path: survey_replicate ───────────────────────────────────────────────

test_that("update_design() updates weights on survey_replicate", {
  df <- make_survey_data(
    n = 100L,
    n_psu = 20L,
    design = "replicate",
    type = "brr",
    seed = 10L
  )
  df$wt2 <- df$wt * 1.05
  d <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::starts_with("repwt"),
    type = "BRR"
  )
  d2 <- suppressMessages(update_design(d, weights = wt2))
  test_invariants(d2)
  expect_identical(d2@variables$weights, "wt2")
})

test_that("update_design() updates repweights on survey_replicate", {
  df <- make_survey_data(
    n = 100L,
    n_psu = 20L,
    design = "replicate",
    type = "brr",
    seed = 11L
  )
  d <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::starts_with("repwt"),
    type = "BRR"
  )
  # Update to use only the first 3 replicate columns
  d2 <- suppressMessages(
    update_design(d, repweights = c(repwt_1, repwt_2, repwt_3))
  )
  test_invariants(d2)
  expect_identical(d2@variables$repweights, c("repwt_1", "repwt_2", "repwt_3"))
})


# ── Inform message (error-messages.md row 36) ──────────────────────────────────

test_that("update_design() emits inform message listing changed vars", {
  df <- make_survey_data(n = 200L, seed = 20L)
  df$wt2 <- df$wt * 1.1
  d <- as_survey(df, weights = wt, strata = strata)
  expect_message(
    update_design(d, weights = wt2),
    regexp = "Survey design updated"
  )
})

test_that("update_design() inform message names the changed variable", {
  df <- make_survey_data(n = 200L, seed = 21L)
  df$wt2 <- df$wt * 1.1
  d <- as_survey(df, weights = wt, strata = strata)
  expect_message(
    update_design(d, weights = wt2),
    regexp = "weights"
  )
})

test_that("update_design() emits no message when no args provided", {
  df <- make_survey_data(n = 200L, seed = 22L)
  d <- as_survey(df, weights = wt, strata = strata)
  expect_no_message(update_design(d))
})

test_that("update_design() emits no message when all args are NULL", {
  df <- make_survey_data(n = 200L, seed = 23L)
  d <- as_survey(df, weights = wt, strata = strata)
  expect_no_message(
    update_design(d, ids = NULL, weights = NULL, strata = NULL, fpc = NULL)
  )
})


# ── validate = TRUE / FALSE ───────────────────────────────────────────────────

test_that("update_design() validate=TRUE catches invalid weight column", {
  df <- make_survey_data(n = 200L, seed = 30L)
  d <- as_survey(df, weights = wt, strata = strata)
  df$bad_wt <- -1 # all negative
  d@data <- df
  expect_error(
    suppressMessages(update_design(d, weights = bad_wt, validate = TRUE)),
    class = "surveycore_error_weights_nonpositive"
  )
})

test_that("update_design() validate=FALSE skips structural validation", {
  df <- make_survey_data(n = 200L, seed = 31L)
  d <- as_survey(df, weights = wt, strata = strata)
  df$bad_wt <- -1 # all negative
  d@data <- df
  # No error even though weight column is invalid
  expect_no_error(
    suppressMessages(update_design(d, weights = bad_wt, validate = FALSE))
  )
})

test_that("update_design() validate=TRUE result passes test_invariants()", {
  df <- make_survey_data(n = 200L, seed = 32L)
  df$wt2 <- df$wt * 1.1
  d <- as_survey(df, weights = wt, strata = strata)
  d2 <- suppressMessages(update_design(d, weights = wt2, validate = TRUE))
  test_invariants(d2)
})


# ── Error: survey_twophase not supported ──────────────────────────────────────

test_that("update_design() errors for survey_twophase with unsupported class", {
  df <- make_survey_data(n = 100L, design = "twophase", seed = 40L)
  phase1 <- as_survey(df, weights = wt, strata = strata)
  d2 <- as_survey_twophase(phase1, subset = subset)
  expect_error(
    update_design(d2, weights = wt),
    class = "surveycore_error_unsupported_class"
  )
})

test_that("update_design() errors for non-survey objects", {
  expect_error(
    update_design(data.frame(x = 1)),
    class = "surveycore_error_unsupported_class"
  )
})


# ── Edge cases ────────────────────────────────────────────────────────────────

test_that("update_design() no-op (all NULL) returns object unchanged", {
  df <- make_survey_data(n = 200L, seed = 50L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  d2 <- update_design(d)
  test_invariants(d2)
  expect_identical(d2@variables$ids, d@variables$ids)
  expect_identical(d2@variables$weights, d@variables$weights)
  expect_identical(d2@variables$strata, d@variables$strata)
  expect_identical(d2@variables$fpc, d@variables$fpc)
})

test_that("update_design() returns invisibly", {
  df <- make_survey_data(n = 200L, seed = 51L)
  d <- as_survey(df, weights = wt, strata = strata)
  result <- withVisible(update_design(d))
  expect_false(result$visible)
})

test_that("update_design() supports tidy-select bare names", {
  df <- make_survey_data(n = 200L, seed = 52L)
  df$wt2 <- df$wt * 1.1
  d <- as_survey(df, weights = wt, strata = strata)
  d2 <- suppressMessages(update_design(d, weights = wt2))
  expect_identical(d2@variables$weights, "wt2")
})

test_that("update_design() preserves unchanged design vars", {
  df <- make_survey_data(n = 200L, seed = 53L)
  df$wt2 <- df$wt * 1.1
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  d2 <- suppressMessages(update_design(d, weights = wt2))
  test_invariants(d2)
  # Only weights changed; ids, strata, fpc preserved
  expect_identical(d2@variables$weights, "wt2")
  expect_identical(d2@variables$ids, d@variables$ids)
  expect_identical(d2@variables$strata, d@variables$strata)
  expect_identical(d2@variables$fpc, d@variables$fpc)
})


# ── Coverage: .resolve_update_arg() error when expression selects >1 column ─

test_that("update_design() errors when strata arg selects multiple columns", {
  df <- make_survey_data(n = 200L, seed = 60L)
  df$st2 <- df$strata
  d <- as_survey(df, weights = wt, strata = strata)
  expect_error(
    update_design(d, strata = starts_with("st")),
    class = "surveycore_error_strata_multiple"
  )
})

test_that("update_design() errors when weights arg selects multiple columns", {
  df <- make_survey_data(n = 200L, seed = 61L)
  df$wt2 <- df$wt * 1.1
  d <- as_survey(df, weights = wt, strata = strata)
  expect_error(
    update_design(d, weights = starts_with("wt")),
    class = "surveycore_error_weights_multiple"
  )
})


# ── Coverage: validate = FALSE path for survey_replicate ────────────────────

test_that("update_design() validate=FALSE skips validation for survey_replicate", {
  df <- make_survey_data(
    n = 100L,
    n_psu = 20L,
    design = "replicate",
    type = "brr",
    seed = 70L
  )
  df$bad_wt <- -1
  d <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::starts_with("repwt"),
    type = "BRR"
  )
  d@data <- df
  expect_no_error(
    suppressMessages(update_design(d, weights = bad_wt, validate = FALSE))
  )
})


# ── Calibration guard ──────────────────────────────────────────────────────────

test_that("update_design() warns when weight column changes on calibrated design [B-6]", {
  df <- make_survey_data(n = 200L, seed = 42L)
  df$wt2 <- df$wt * 1.1
  d <- as_survey(df, ids = psu, weights = wt, strata = strata, nest = TRUE)
  # Attach a caldata element to simulate a calibrated design
  cd <- as_caldata(df$wt, rep(1.05, nrow(df)), matrix(1, nrow(df), 1))
  d@calibration <- list(cd)

  expect_warning(
    suppressMessages(update_design(d, weights = wt2)),
    class = "surveycore_warning_weight_change_invalidates_calibration"
  )
  # Snapshot verifies the CLI warning message text
  expect_snapshot(
    suppressMessages(update_design(d, weights = wt2))
  )
})
