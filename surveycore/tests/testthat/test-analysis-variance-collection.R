# tests/testthat/test-analysis-variance-collection.R
#
# PR 2 of get-variance: collection dispatch tests for get_variance().
# Mirrors the patterns in test-survey-collection-dispatch.R but scoped to
# get_variance() so the variance-specific write surface remains cohesive.
#
# Covers acceptance criteria for survey_collection dispatch:
#   - Happy path (dispatch identity + NSE forwarding)
#   - .meta carry-over ($collection, $per_survey)
#   - Custom .id and .id collision (C7)
#   - Missing-variable error (C5) and all-skipped error (C6)
#   - Skipped-surveys message (C9)
#   - Meta divergence warning (C11)
#   - .id validation (C13)
#   - .if_missing_var = "skip" partial-drop
#   - Length-1 collection edge
#   - C10: variable-not-found surfaces underlying class

# ── Shared fixture ───────────────────────────────────────────────────────────

.make_variance_collection <- function(n_surveys = 3L, seed = 42L) {
  surveys <- lapply(seq_len(n_surveys), function(i) {
    df <- make_survey_data(
      n = 120L,
      n_psu = 12L,
      n_strata = 3L,
      design = "taylor",
      seed = seed + i
    )
    df$grp2 <- factor(sample(c("A", "B"), nrow(df), replace = TRUE))
    as_survey(df, ids = psu, weights = wt, strata = strata)
  })
  names(surveys) <- paste0("w", seq_len(n_surveys))
  do.call(as_survey_collection, surveys)
}

.oracle_variance_dispatch <- function(coll, ...) {
  per <- lapply(names(coll), function(nm) get_variance(coll[[nm]], ...))
  names(per) <- names(coll)
  dplyr::bind_rows(per, .id = ".survey")
}

# ══════════════════════════════════════════════════════════════════════════════
# Happy paths
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_variance() dispatches over a 3-survey collection", {
  coll <- .make_variance_collection()
  result <- get_variance(coll, y1)

  expect_s3_class(result, "tbl_df")
  expect_identical(names(result)[1], ".survey")
  expect_setequal(unique(result$.survey), names(coll))
})

test_that("get_variance() dispatch matches per-survey bind oracle", {
  coll <- .make_variance_collection()
  got <- get_variance(coll, y1)
  want <- .oracle_variance_dispatch(coll, y1)

  numeric_cols <- intersect(
    names(got)[vapply(got, is.numeric, logical(1L))],
    names(want)
  )
  for (col in numeric_cols) {
    expect_equal(got[[col]], want[[col]], tolerance = 1e-12)
  }
})

test_that("get_variance() forwards `group` via {{ }}", {
  coll <- .make_variance_collection()
  got <- get_variance(coll, y1, group = grp2)
  want <- .oracle_variance_dispatch(coll, y1, group = grp2)

  expect_identical(nrow(got), nrow(want))
  expect_true("grp2" %in% names(got))
})

# ══════════════════════════════════════════════════════════════════════════════
# Custom .id
# ══════════════════════════════════════════════════════════════════════════════

test_that("custom .id renames the identifier column", {
  coll <- .make_variance_collection()
  result <- get_variance(coll, y1, .id = "wave")

  expect_identical(names(result)[1], "wave")
  expect_false(".survey" %in% names(result))
  expect_setequal(unique(result$wave), names(coll))
})

# ══════════════════════════════════════════════════════════════════════════════
# .meta carry-over
# ══════════════════════════════════════════════════════════════════════════════

test_that(".meta$collection$surveys records contributing survey names", {
  coll <- .make_variance_collection()
  result <- get_variance(coll, y1)
  meta <- attr(result, ".meta")

  expect_identical(meta$collection$surveys, names(coll))
})

test_that(".meta$per_survey preserves each survey's original .meta", {
  coll <- .make_variance_collection()
  result <- get_variance(coll, y1)
  meta <- attr(result, ".meta")

  expect_identical(names(meta$per_survey), names(coll))
  per_survey_y1 <- get_variance(coll[["w1"]], y1)
  actual <- meta$per_survey$w1
  expected <- attr(per_survey_y1, ".meta")
  actual$call <- NULL
  expected$call <- NULL
  expect_identical(actual, expected)
})

# ══════════════════════════════════════════════════════════════════════════════
# Error paths
# ══════════════════════════════════════════════════════════════════════════════

test_that("C5: .if_missing_var = 'error' aborts when one survey lacks the variable", {
  surveys <- list(
    w1 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 1L)
      df$focal <- rnorm(nrow(df))
      as_survey(df, ids = psu, weights = wt, strata = strata)
    },
    w2 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 2L)
      as_survey(df, ids = psu, weights = wt, strata = strata)
    }
  )
  coll <- do.call(as_survey_collection, surveys)

  expect_error(
    get_variance(coll, focal, .if_missing_var = "error"),
    class = "surveycore_error_collection_missing_var"
  )
  expect_snapshot(
    error = TRUE,
    get_variance(coll, focal, .if_missing_var = "error")
  )
})

test_that("C6: .if_missing_var = 'skip' with all surveys missing aborts", {
  surveys <- list(
    w1 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 1L)
      as_survey(df, ids = psu, weights = wt, strata = strata)
    },
    w2 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 2L)
      as_survey(df, ids = psu, weights = wt, strata = strata)
    }
  )
  coll <- do.call(as_survey_collection, surveys)

  expect_error(
    get_variance(coll, focal, .if_missing_var = "skip"),
    class = "surveycore_error_collection_all_skipped"
  )
  expect_snapshot(
    error = TRUE,
    get_variance(coll, focal, .if_missing_var = "skip")
  )
})

test_that("C7: .id collision with an existing result column aborts", {
  coll <- .make_variance_collection()

  expect_error(
    get_variance(coll, y1, .id = "variance"),
    class = "surveycore_error_collection_id_collision"
  )
  expect_snapshot(
    error = TRUE,
    get_variance(coll, y1, .id = "variance")
  )
})

test_that("C13: .id rejects empty, NA, non-char, wrong length", {
  # NOTE: as of PR `feature/collection-id-if-missing-var-class`, `.id = NULL`
  # is a sentinel for "use the stored property" rather than an invalid value.
  # The other four invalid shapes still error.
  coll <- .make_variance_collection()

  expect_error(
    get_variance(coll, y1, .id = ""),
    class = "surveycore_error_collection_invalid_id"
  )
  expect_error(
    get_variance(coll, y1, .id = NA_character_),
    class = "surveycore_error_collection_invalid_id"
  )
  expect_error(
    get_variance(coll, y1, .id = c("a", "b")),
    class = "surveycore_error_collection_invalid_id"
  )
  expect_error(
    get_variance(coll, y1, .id = 1L),
    class = "surveycore_error_collection_invalid_id"
  )
  expect_snapshot(error = TRUE, get_variance(coll, y1, .id = NA_character_))
})

test_that("C10: tidy-selected variable absent on single design raises variable_not_found", {
  df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 1L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)

  expect_error(
    get_variance(d, nonexistent_variable),
    class = "surveycore_error_variable_not_found"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# Messages
# ══════════════════════════════════════════════════════════════════════════════

test_that("C9: .if_missing_var = 'skip' emits skipped-surveys message", {
  surveys <- list(
    w1 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 1L)
      df$focal <- rnorm(nrow(df))
      as_survey(df, ids = psu, weights = wt, strata = strata)
    },
    w2 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 2L)
      as_survey(df, ids = psu, weights = wt, strata = strata)
    }
  )
  coll <- do.call(as_survey_collection, surveys)

  expect_message(
    get_variance(coll, focal, .if_missing_var = "skip"),
    class = "surveycore_message_collection_skipped_surveys"
  )
})

test_that(".if_missing_var = 'skip' drops survey missing the focal var", {
  surveys <- list(
    w1 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 1L)
      df$focal <- rnorm(nrow(df))
      as_survey(df, ids = psu, weights = wt, strata = strata)
    },
    w2 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 2L)
      as_survey(df, ids = psu, weights = wt, strata = strata)
    }
  )
  coll <- do.call(as_survey_collection, surveys)

  result <- suppressMessages(
    get_variance(coll, focal, .if_missing_var = "skip")
  )
  expect_setequal(unique(result$.survey), "w1")
  meta <- attr(result, ".meta")
  expect_identical(meta$collection$surveys, "w1")
  expect_false("w2" %in% names(meta$per_survey))
})

# ══════════════════════════════════════════════════════════════════════════════
# Meta divergence (C11)
# ══════════════════════════════════════════════════════════════════════════════

test_that("C11: diverging value_labels across surveys emits divergence warning", {
  surveys <- list(
    w1 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 1L)
      attr(df$y1, "label") <- "Outcome 1"
      as_survey(df, ids = psu, weights = wt, strata = strata)
    },
    w2 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 2L)
      as_survey(df, ids = psu, weights = wt, strata = strata)
    }
  )
  coll <- do.call(as_survey_collection, surveys)

  expect_warning(
    get_variance(coll, y1),
    class = "surveycore_warning_collection_meta_divergence"
  )
})

test_that("C11: identical meta across surveys → no divergence warning", {
  coll <- .make_variance_collection()
  expect_warning(
    result <- get_variance(coll, y1),
    class = "surveycore_warning_collection_meta_divergence",
    regexp = NA
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# Length-1 collection
# ══════════════════════════════════════════════════════════════════════════════

test_that("length-1 collection dispatches", {
  coll <- .make_variance_collection(n_surveys = 1L)
  result <- get_variance(coll, y1)
  direct <- get_variance(coll[["w1"]], y1)

  expect_identical(nrow(result), nrow(direct))
  expect_identical(unique(result$.survey), "w1")
})
