# tests/testthat/test-collection-rowwise.R
#
# rowwise.survey_collection — PR 2b.
#
# Spec sections: §III.1 (template), §IV.10 (rowwise contract — per-member
# rowwise, class-catch detection, .may_change_groups = FALSE), §IX.3
# (per-verb test categories).

# ── happy path ───────────────────────────────────────────────────────────────

test_that("rowwise.survey_collection sets per-member rowwise state (cross-design)", {
  coll <- make_test_collection(seed = 42)
  test_collection_invariants(coll)
  for (member in coll@surveys) {
    test_invariants(member)
  }

  out <- dplyr::rowwise(coll)

  test_collection_invariants(out)
  for (member in out@surveys) {
    test_invariants(member)
    expect_true(isTRUE(member@variables$rowwise))
    expect_identical(member@variables$rowwise_id_cols, character(0))
  }
})

test_that("rowwise.survey_collection accepts id columns", {
  coll <- make_test_collection(seed = 42)

  out <- dplyr::rowwise(coll, group)

  test_collection_invariants(out)
  for (member in out@surveys) {
    expect_true(isTRUE(member@variables$rowwise))
    expect_identical(member@variables$rowwise_id_cols, "group")
  }
})

# ── @groups invariant ────────────────────────────────────────────────────────

test_that("rowwise.survey_collection does not change @groups", {
  coll <- make_test_collection(seed = 42)
  # Set @groups on each member and on the collection (group_by.survey_collection
  # is in PR 2c — bypass S7 per-assignment validation here).
  surveys <- coll@surveys
  for (nm in names(surveys)) {
    member <- surveys[[nm]]
    attr(member, "groups") <- "group"
    surveys[[nm]] <- member
  }
  attr(coll, "surveys") <- surveys
  attr(coll, "groups") <- "group"
  S7::validate(coll)

  out <- dplyr::rowwise(coll)

  test_collection_invariants(out)
  expect_identical(out@groups, "group")
})

test_that("rowwise.survey_collection preserves @id and @if_missing_var", {
  coll <- make_test_collection(seed = 42)

  out <- dplyr::rowwise(coll)

  expect_identical(out@id, coll@id)
  expect_identical(out@if_missing_var, coll@if_missing_var)
})

# ── .if_missing_var ──────────────────────────────────────────────────────────

test_that("rowwise.survey_collection .if_missing_var = 'error' raises typed", {
  coll <- make_heterogeneous_collection(seed = 42)

  expect_error(
    dplyr::rowwise(coll, tidyselect::all_of("region")),
    class = "surveytidy_error_collection_verb_failed"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::rowwise(coll, tidyselect::all_of("region"))
  )
})

test_that("rowwise.survey_collection .if_missing_var = 'skip' drops bad members", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  expect_message(
    out <- dplyr::rowwise(coll_skip, tidyselect::all_of("y3")),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  expect_identical(names(out@surveys), c("m1", "m3"))
  expect_identical(out@if_missing_var, "skip")
})

test_that("rowwise.survey_collection .if_missing_var precedence", {
  coll <- make_heterogeneous_collection(seed = 42)

  # Stored "error", per-call "skip" → skip wins
  expect_message(
    out <- dplyr::rowwise(
      coll,
      tidyselect::all_of("y3"),
      .if_missing_var = "skip"
    ),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  expect_identical(names(out@surveys), c("m1", "m3"))

  # Stored "skip", per-call "error" → error wins
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")
  expect_error(
    dplyr::rowwise(
      coll_skip,
      tidyselect::all_of("y3"),
      .if_missing_var = "error"
    ),
    class = "surveytidy_error_collection_verb_failed"
  )
})

test_that("rowwise.survey_collection raises emptied error when all members skipped", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  expect_error(
    dplyr::rowwise(coll_skip, tidyselect::all_of("ghost_col_xyz")),
    class = "surveytidy_error_collection_verb_emptied"
  )
})
