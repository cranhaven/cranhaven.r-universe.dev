# tests/testthat/test-collection-distinct.R
#
# distinct.survey_collection — PR 2b.
#
# Spec sections: §III.1 (template), §IV.11 (distinct contract — per-survey
# only, no cross-survey collapse, class-catch detection), §V.5 (V9 — V9
# is documented in §IX.3.

# ── happy path ───────────────────────────────────────────────────────────────

test_that("distinct.survey_collection deduplicates each member independently (cross-design)", {
  coll <- make_test_collection(seed = 42)
  test_collection_invariants(coll)
  for (member in coll@surveys) {
    test_invariants(member)
  }

  # distinct() always issues surveycore_warning_physical_subset (per-member)
  out <- suppressWarnings(dplyr::distinct(coll))

  test_collection_invariants(out)
  for (member in out@surveys) {
    test_invariants(member)
    # All columns retained (.keep_all = TRUE always for survey_base)
    expect_true(all(c("y1", "y2", "y3", "wt") %in% names(member@data)))
  }
  expect_identical(names(out@surveys), names(coll@surveys))
  expect_identical(out@id, coll@id)
})

test_that("distinct.survey_collection accepts column expressions", {
  coll <- make_test_collection(seed = 42)

  out <- suppressWarnings(dplyr::distinct(coll, group))

  test_collection_invariants(out)
  for (member in out@surveys) {
    # Result deduplicated by group, but all columns retained
    expect_true(all(c("y1", "y2", "wt", "group") %in% names(member@data)))
    # Group should now have unique values within the member
    expect_false(anyDuplicated(member@data$group) > 0L)
  }
})

# ── V9: NO cross-survey collapse ────────────────────────────────────────────

test_that("distinct.survey_collection does NOT collapse identical rows across members", {
  # V9 contract: per-survey only — when two members have a literally
  # identical row, the row appears in BOTH members' results post-distinct.
  base <- make_survey_data(n = 200L, n_psu = 20L, n_strata = 4L, seed = 42L)

  # Build two members with the same first row deliberately included
  m1_data <- base
  m2_data <- base

  to_taylor <- function(df) {
    surveycore::as_survey(
      df,
      ids = psu,
      strata = strata,
      weights = wt,
      fpc = fpc
    )
  }

  coll <- surveycore::as_survey_collection(
    m1 = to_taylor(m1_data),
    m2 = to_taylor(m2_data),
    .id = ".survey",
    .if_missing_var = "error"
  )

  out <- suppressWarnings(dplyr::distinct(coll))
  test_collection_invariants(out)

  # The same y1 values should appear in both members (V9 — no cross-survey
  # collapse). Specifically, m1 and m2's first rows are identical and BOTH
  # should remain post-distinct.
  expect_true(out@surveys$m1@data$y1[[1L]] %in% out@surveys$m2@data$y1)
})

# ── per-member physical-subset warning ──────────────────────────────────────

test_that("distinct.survey_collection fires per-member physical-subset warning", {
  coll <- make_test_collection(seed = 42)

  warning_count <- 0L
  withCallingHandlers(
    dplyr::distinct(coll),
    surveycore_warning_physical_subset = function(cnd) {
      warning_count <<- warning_count + 1L
      rlang::cnd_muffle(cnd)
    }
  )
  expect_identical(warning_count, length(coll@surveys))
})

# ── .if_missing_var ──────────────────────────────────────────────────────────

test_that("distinct.survey_collection .if_missing_var = 'error' raises typed", {
  coll <- make_heterogeneous_collection(seed = 42)

  expect_error(
    suppressWarnings(dplyr::distinct(coll, tidyselect::all_of("region"))),
    class = "surveytidy_error_collection_verb_failed"
  )
})

test_that("distinct.survey_collection .if_missing_var = 'skip' drops bad members", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  expect_message(
    out <- suppressWarnings(
      dplyr::distinct(coll_skip, tidyselect::all_of("y3"))
    ),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  expect_identical(names(out@surveys), c("m1", "m3"))
  expect_identical(out@if_missing_var, "skip")
})

test_that("distinct.survey_collection raises emptied error when all members skipped", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  expect_error(
    suppressWarnings(
      dplyr::distinct(coll_skip, tidyselect::all_of("ghost_col_xyz"))
    ),
    class = "surveytidy_error_collection_verb_emptied"
  )
})
