# tests/testthat/test-collection-arrange.R
#
# arrange.survey_collection — PR 2a.
#
# Spec sections: §III.1 (template), §IV.6 (arrange contract — pre-check
# detection, no .by), §IX.3 (per-verb test categories).
#
# Note: arrange does NOT accept `.by`, so there is no .by-rejection test.

# ── happy path ───────────────────────────────────────────────────────────────

test_that("arrange.survey_collection sorts every member (cross-design)", {
  coll <- make_test_collection(seed = 42)
  test_collection_invariants(coll)
  for (member in coll@surveys) {
    test_invariants(member)
  }

  out <- dplyr::arrange(coll, y1)

  test_collection_invariants(out)
  for (member in out@surveys) {
    test_invariants(member)
    expect_identical(member@data$y1, sort(member@data$y1))
  }
  expect_identical(names(out@surveys), names(coll@surveys))
  expect_identical(out@id, coll@id)
  expect_identical(out@if_missing_var, coll@if_missing_var)
})

# ── .if_missing_var ──────────────────────────────────────────────────────────

test_that("arrange.survey_collection .if_missing_var = 'error' raises typed", {
  coll <- make_heterogeneous_collection(seed = 42)

  expect_error(
    dplyr::arrange(coll, region),
    class = "surveytidy_error_collection_verb_failed"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::arrange(coll, region)
  )
})

test_that("arrange.survey_collection .if_missing_var = 'skip' drops bad members", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  # y3 missing on m2 only; m1 and m3 retain y3
  expect_message(
    out <- dplyr::arrange(coll_skip, y3),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  expect_identical(names(out@surveys), c("m1", "m3"))
  expect_identical(out@if_missing_var, "skip")
})

test_that("arrange.survey_collection .if_missing_var precedence", {
  coll <- make_heterogeneous_collection(seed = 42)

  # Stored "error", per-call "skip" → skip wins
  expect_message(
    out <- dplyr::arrange(coll, y3, .if_missing_var = "skip"),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  expect_identical(names(out@surveys), c("m1", "m3"))

  # Stored "skip", per-call "error" → error wins
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")
  expect_error(
    dplyr::arrange(coll_skip, y3, .if_missing_var = "error"),
    class = "surveytidy_error_collection_verb_failed"
  )
})

test_that("arrange.survey_collection raises emptied error when all members skipped", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  expect_error(
    dplyr::arrange(coll_skip, ghost_col_xyz),
    class = "surveytidy_error_collection_verb_emptied"
  )
})

# ── domain preservation ─────────────────────────────────────────────────────

test_that("arrange.survey_collection preserves domain column on every member", {
  coll <- make_test_collection(seed = 42)
  coll_filtered <- dplyr::filter(coll, y1 > 50)

  out <- dplyr::arrange(coll_filtered, y2)
  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL

  for (i in seq_along(out@surveys)) {
    member_in <- coll_filtered@surveys[[i]]
    member_out <- out@surveys[[i]]
    expect_true(domain_col %in% names(member_out@data))
    # Domain values should still match the original row's domain after sort.
    # Reorder member_out's domain by sorting member_in by y2 to verify the
    # domain column travels with the rows.
    sorted_idx <- order(member_in@data$y2)
    expect_identical(
      member_out@data[[domain_col]],
      member_in@data[[domain_col]][sorted_idx]
    )
  }
})

# ── visible_vars preservation ───────────────────────────────────────────────

test_that("arrange.survey_collection preserves visible_vars on every member", {
  coll <- make_test_collection(seed = 42)
  for (i in seq_along(coll@surveys)) {
    m <- coll@surveys[[i]]
    new_vars <- m@variables
    new_vars$visible_vars <- c("y1", "y2")
    attr(m, "variables") <- new_vars
    S7::validate(m)
    coll@surveys[[i]] <- m
  }

  out <- dplyr::arrange(coll, y1)
  for (member in out@surveys) {
    expect_identical(member@variables$visible_vars, c("y1", "y2"))
  }
})
