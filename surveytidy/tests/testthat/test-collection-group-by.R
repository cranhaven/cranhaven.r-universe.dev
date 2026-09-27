# tests/testthat/test-collection-group-by.R
#
# group_by.survey_collection / ungroup.survey_collection /
# group_vars.survey_collection / is_rowwise.survey_collection — PR 2c.
#
# Spec sections: §III.1 (template), §III.4 (group-affecting verbs whitelist),
# §IV.7 (group_by — pre-check + .may_change_groups = TRUE),
# §IV.8 (ungroup — none + .may_change_groups = TRUE),
# §IV.9 (group_vars — one-liner, no dispatcher),
# §IV.10 (rowwise / is_rowwise — one-liner, no dispatcher),
# §IX.3 (per-verb test categories).

# ── group_by — happy path ────────────────────────────────────────────────────

test_that("group_by.survey_collection sets @groups on coll and every member", {
  coll <- make_test_collection(seed = 42)
  test_collection_invariants(coll)
  for (member in coll@surveys) {
    test_invariants(member)
  }

  out <- dplyr::group_by(coll, group)

  test_collection_invariants(out)
  for (member in out@surveys) {
    test_invariants(member)
    expect_identical(member@groups, "group")
  }
  expect_identical(out@groups, "group")
})

test_that("group_by.survey_collection .add = TRUE appends to existing groups", {
  coll <- dplyr::group_by(make_test_collection(seed = 42), group)
  out <- dplyr::group_by(coll, strata, .add = TRUE)

  test_collection_invariants(out)
  expect_identical(out@groups, c("group", "strata"))
  for (member in out@surveys) {
    expect_identical(member@groups, c("group", "strata"))
  }
})

test_that("group_by.survey_collection .add = FALSE replaces existing groups", {
  coll <- dplyr::group_by(make_test_collection(seed = 42), group)
  out <- dplyr::group_by(coll, strata)

  test_collection_invariants(out)
  expect_identical(out@groups, "strata")
  for (member in out@surveys) {
    expect_identical(member@groups, "strata")
  }
})

test_that("group_by.survey_collection .drop passes through without error", {
  coll <- make_test_collection(seed = 42)

  out <- dplyr::group_by(coll, group, .drop = FALSE)

  test_collection_invariants(out)
  expect_identical(out@groups, "group")
})

test_that("group_by.survey_collection preserves @id and @if_missing_var", {
  coll <- make_test_collection(seed = 42)

  out <- dplyr::group_by(coll, group)

  expect_identical(out@id, coll@id)
  expect_identical(out@if_missing_var, coll@if_missing_var)
})

# ── group_by — .if_missing_var ───────────────────────────────────────────────

test_that("group_by.survey_collection .if_missing_var = 'error' raises typed", {
  coll <- make_heterogeneous_collection(seed = 42)

  expect_error(
    dplyr::group_by(coll, region),
    class = "surveytidy_error_collection_verb_failed"
  )
  expect_snapshot(error = TRUE, dplyr::group_by(coll, region))
})

test_that("group_by.survey_collection .if_missing_var = 'skip' drops missing", {
  coll <- surveycore::set_collection_if_missing_var(
    make_heterogeneous_collection(seed = 42),
    "skip"
  )

  # Part 1 — happy-path skip (no G1b violation, normal dispatch).
  # m1 and m2 are skipped because they lack `region`; m3 survives and is
  # group_by()'d on `region`. The rebuilt collection has @groups = "region"
  # and the surviving member's @groups = "region". G1b is structurally
  # satisfied because every surviving member has the column — the safety
  # net does not fire on this path.
  expect_message(
    out <- dplyr::group_by(coll, region),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  test_collection_invariants(out)
  expect_identical(names(out@surveys), "m3")
  expect_identical(out@groups, "region")
  expect_identical(out@surveys[["m3"]]@groups, "region")
})

test_that("group_by.survey_collection raises emptied when all members skipped", {
  coll <- surveycore::set_collection_if_missing_var(
    make_heterogeneous_collection(seed = 42),
    "skip"
  )

  expect_error(
    dplyr::group_by(coll, ghost_col_xyz),
    class = "surveytidy_error_collection_verb_emptied"
  )
})

# ── group_by — G1b safety net (synthetic regression catch) ───────────────────
#
# G1b is unreachable through normal dispatch; this test exercises
# the validator's defense-in-depth against a regression in
# surveycore's per-member enforcement.

test_that("survey_collection validator G1b catches synthetic group-col removal", {
  coll <- dplyr::group_by(make_test_collection(seed = 42), group)

  surveys <- coll@surveys
  first <- surveys[[1L]]
  df_no_group <- first@data[, setdiff(names(first@data), "group"), drop = FALSE]
  attr(first, "data") <- df_no_group
  surveys[[1L]] <- first
  attr(coll, "surveys") <- surveys

  expect_error(
    S7::validate(coll),
    class = "surveycore_error_collection_group_not_in_member_data"
  )
})

# ── group_by — visible_vars preservation ─────────────────────────────────────

test_that("group_by.survey_collection preserves visible_vars on every member", {
  coll <- make_test_collection(seed = 42)
  surveys <- coll@surveys
  for (nm in names(surveys)) {
    member <- surveys[[nm]]
    vars <- member@variables
    vars$visible_vars <- c("y1", "y2")
    attr(member, "variables") <- vars
    surveys[[nm]] <- member
  }
  attr(coll, "surveys") <- surveys
  S7::validate(coll)

  out <- dplyr::group_by(coll, group)

  for (member in out@surveys) {
    expect_identical(member@variables$visible_vars, c("y1", "y2"))
  }
})

# ── ungroup — happy path ─────────────────────────────────────────────────────

test_that("ungroup.survey_collection clears @groups on coll and every member", {
  coll <- dplyr::group_by(make_test_collection(seed = 42), group)
  test_collection_invariants(coll)

  out <- dplyr::ungroup(coll)

  test_collection_invariants(out)
  expect_identical(out@groups, character(0))
  for (member in out@surveys) {
    expect_identical(member@groups, character(0))
  }
})

test_that("ungroup.survey_collection on already-ungrouped is a no-op", {
  coll <- make_test_collection(seed = 42)

  out <- dplyr::ungroup(coll)

  test_collection_invariants(out)
  expect_identical(out@groups, character(0))
  for (member in out@surveys) {
    expect_identical(member@groups, character(0))
  }
})

test_that("ungroup.survey_collection preserves @id and @if_missing_var", {
  coll <- dplyr::group_by(make_test_collection(seed = 42), group)

  out <- dplyr::ungroup(coll)

  expect_identical(out@id, coll@id)
  expect_identical(out@if_missing_var, coll@if_missing_var)
})

# ── ungroup — visible_vars preservation ──────────────────────────────────────

test_that("ungroup.survey_collection preserves visible_vars on every member", {
  coll <- dplyr::group_by(make_test_collection(seed = 42), group)
  surveys <- coll@surveys
  for (nm in names(surveys)) {
    member <- surveys[[nm]]
    vars <- member@variables
    vars$visible_vars <- c("y1", "y2")
    attr(member, "variables") <- vars
    surveys[[nm]] <- member
  }
  attr(coll, "surveys") <- surveys
  S7::validate(coll)

  out <- dplyr::ungroup(coll)

  for (member in out@surveys) {
    expect_identical(member@variables$visible_vars, c("y1", "y2"))
  }
})

# ── group_vars — one-liner, does NOT invoke dispatcher ───────────────────────

test_that("group_vars.survey_collection returns coll@groups directly", {
  coll <- dplyr::group_by(make_test_collection(seed = 42), group)

  result <- dplyr::group_vars(coll)

  expect_identical(result, coll@groups)
  expect_identical(result, "group")
})

test_that("group_vars.survey_collection on ungrouped returns character(0)", {
  coll <- make_test_collection(seed = 42)

  expect_identical(dplyr::group_vars(coll), character(0))
})

test_that("group_vars.survey_collection does not invoke the dispatcher", {
  skip_if_not_installed("mockery")
  coll <- dplyr::group_by(make_test_collection(seed = 42), group)
  ns <- asNamespace("surveytidy")
  method <- get("group_vars.survey_collection", envir = ns)

  stub_fn <- mockery::mock()
  mockery::stub(method, ".dispatch_verb_over_collection", stub_fn)

  result <- method(coll)

  expect_identical(result, "group")
  mockery::expect_called(stub_fn, 0L)
})

# ── is_rowwise — one-liner, does NOT invoke dispatcher ───────────────────────

test_that("is_rowwise.survey_collection returns TRUE iff every member rowwise", {
  coll <- make_test_collection(seed = 42)
  expect_false(is_rowwise(coll))

  coll_rw <- dplyr::rowwise(coll)
  expect_true(is_rowwise(coll_rw))

  # Mixed: only the first member is rowwise.
  coll_mixed <- make_test_collection(seed = 42)
  surveys <- coll_mixed@surveys
  surveys[[1L]] <- dplyr::rowwise(surveys[[1L]])
  attr(coll_mixed, "surveys") <- surveys
  S7::validate(coll_mixed)

  expect_false(is_rowwise(coll_mixed))
})
