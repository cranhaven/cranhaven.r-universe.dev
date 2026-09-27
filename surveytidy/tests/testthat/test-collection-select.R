# tests/testthat/test-collection-select.R
#
# select.survey_collection and relocate.survey_collection — PR 2b.
#
# Spec sections: §III.1 (template), §IV.3 (select/relocate contract —
# class-catch detection, group-removal pre-flight on select only), §IX.3
# (per-verb test categories).

# ── select() happy path ─────────────────────────────────────────────────────

test_that("select.survey_collection keeps user cols + design vars on every member", {
  coll <- make_test_collection(seed = 42)
  test_collection_invariants(coll)
  for (member in coll@surveys) {
    test_invariants(member)
  }

  out <- dplyr::select(coll, y1, y2)

  test_collection_invariants(out)
  for (member in out@surveys) {
    test_invariants(member)
    # Design vars are preserved in @data
    design_vars <- intersect(
      c("psu", "strata", "wt", "fpc"),
      names(member@data)
    )
    for (dv in design_vars) {
      expect_true(dv %in% names(member@data))
    }
    # User selection becomes visible_vars
    expect_identical(member@variables$visible_vars, c("y1", "y2"))
  }
  expect_identical(names(out@surveys), names(coll@surveys))
  expect_identical(out@id, coll@id)
  expect_identical(out@if_missing_var, coll@if_missing_var)
})

test_that("select.survey_collection preserves @groups when group col is in selection", {
  coll <- make_test_collection(seed = 42)
  grouped_members <- lapply(coll@surveys, dplyr::group_by, group)
  coll_g <- surveycore::as_survey_collection(
    !!!grouped_members,
    .id = coll@id,
    .if_missing_var = coll@if_missing_var
  )

  out <- dplyr::select(coll_g, group, y1)

  test_collection_invariants(out)
  expect_identical(out@groups, "group")
  for (member in out@surveys) {
    expect_identical(member@groups, "group")
    expect_true("group" %in% names(member@data))
  }
})

test_that("select.survey_collection accepts everything()", {
  coll <- make_test_collection(seed = 42)
  out <- dplyr::select(coll, dplyr::everything())
  test_collection_invariants(out)
})

# ── select() group-removal pre-flight ───────────────────────────────────────

test_that("select.survey_collection raises group_removed when group col is excluded", {
  coll <- make_test_collection(seed = 42)
  grouped_members <- lapply(coll@surveys, dplyr::group_by, group)
  coll_g <- surveycore::as_survey_collection(
    !!!grouped_members,
    .id = coll@id,
    .if_missing_var = coll@if_missing_var
  )

  # Selecting only y1 would drop "group" — pre-flight must catch this.
  expect_error(
    dplyr::select(coll_g, y1),
    class = "surveytidy_error_collection_select_group_removed"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::select(coll_g, y1)
  )
})

test_that("select.survey_collection raises group_removed for negative tidyselect", {
  coll <- make_test_collection(seed = 42)
  grouped_members <- lapply(coll@surveys, dplyr::group_by, group)
  coll_g <- surveycore::as_survey_collection(
    !!!grouped_members,
    .id = coll@id,
    .if_missing_var = coll@if_missing_var
  )

  expect_error(
    dplyr::select(coll_g, -group),
    class = "surveytidy_error_collection_select_group_removed"
  )
})

test_that("select.survey_collection group_removed pre-flight runs before dispatch", {
  # Use a heterogeneous collection where dispatch would also fail (region
  # missing). Asserting the group-removed class fires first proves the
  # pre-flight runs before per-member dispatch.
  coll <- make_heterogeneous_collection(seed = 42)
  grouped_members <- lapply(coll@surveys, dplyr::group_by, group)
  coll_g <- surveycore::as_survey_collection(
    !!!grouped_members,
    .id = coll@id,
    .if_missing_var = coll@if_missing_var
  )

  expect_error(
    dplyr::select(coll_g, y1),
    class = "surveytidy_error_collection_select_group_removed"
  )
})

# ── select() .if_missing_var ─────────────────────────────────────────────────

test_that("select.survey_collection .if_missing_var = 'error' raises typed", {
  coll <- make_heterogeneous_collection(seed = 42)

  # region exists only on m3 — m1 and m2 are missing it; all_of() raises
  # vctrs_error_subscript_oob, caught by the dispatcher's class_catch handler.
  expect_error(
    dplyr::select(coll, tidyselect::all_of("region")),
    class = "surveytidy_error_collection_verb_failed"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::select(coll, tidyselect::all_of("region"))
  )
})

test_that("select.survey_collection .if_missing_var = 'skip' drops bad members", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  # y3 is missing on m2 only; m1 and m3 retain y3
  expect_message(
    out <- dplyr::select(coll_skip, tidyselect::all_of("y3")),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  expect_identical(names(out@surveys), c("m1", "m3"))
  expect_identical(out@if_missing_var, "skip")
})

test_that("select.survey_collection .if_missing_var precedence", {
  coll <- make_heterogeneous_collection(seed = 42)

  # Stored "error", per-call "skip" → skip wins
  expect_message(
    out <- dplyr::select(
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
    dplyr::select(
      coll_skip,
      tidyselect::all_of("y3"),
      .if_missing_var = "error"
    ),
    class = "surveytidy_error_collection_verb_failed"
  )
})

test_that("select.survey_collection raises emptied error when all members skipped", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  expect_error(
    dplyr::select(coll_skip, tidyselect::all_of("ghost_col_xyz")),
    class = "surveytidy_error_collection_verb_emptied"
  )
})

test_that("select.survey_collection any_of() silently drops missing per-survey", {
  coll <- make_heterogeneous_collection(seed = 42)

  # any_of() never raises for missing names — V2 path
  out <- dplyr::select(coll, tidyselect::any_of(c("y1", "y2", "ghost")))
  test_collection_invariants(out)
  expect_identical(names(out@surveys), c("m1", "m2", "m3"))
})

# ── select() visible_vars propagation ───────────────────────────────────────

test_that("select.survey_collection sets visible_vars to user selection on every member", {
  coll <- make_test_collection(seed = 42)
  out <- dplyr::select(coll, y1, y2)

  for (member in out@surveys) {
    expect_identical(member@variables$visible_vars, c("y1", "y2"))
  }
})


# ── relocate() happy path ───────────────────────────────────────────────────

test_that("relocate.survey_collection reorders columns on every member", {
  coll <- make_test_collection(seed = 42)
  test_collection_invariants(coll)

  out <- dplyr::relocate(coll, y2, .before = y1)

  test_collection_invariants(out)
  for (member in out@surveys) {
    test_invariants(member)
    # When visible_vars is NULL, column order in @data reflects the relocate
    cols <- names(member@data)
    expect_lt(match("y2", cols), match("y1", cols))
  }
})

test_that("relocate.survey_collection accepts .after argument", {
  coll <- make_test_collection(seed = 42)

  out <- dplyr::relocate(coll, y1, .after = y2)
  test_collection_invariants(out)
  for (member in out@surveys) {
    cols <- names(member@data)
    expect_lt(match("y2", cols), match("y1", cols))
  }
})

test_that("relocate.survey_collection forwards both .before and .after to dplyr", {
  # dplyr::relocate errors when both .before and .after are supplied. The
  # collection method's both-provided closure path forwards both to dplyr,
  # which raises the dplyr error per-member.
  coll <- make_test_collection(seed = 42)

  expect_error(
    dplyr::relocate(coll, y1, .before = y2, .after = wt)
  )
})

test_that("relocate.survey_collection works without .before or .after", {
  coll <- make_test_collection(seed = 42)

  # Default relocate moves the named column to the front.
  out <- dplyr::relocate(coll, y2)
  test_collection_invariants(out)
  for (member in out@surveys) {
    expect_identical(names(member@data)[[1L]], "y2")
  }
})

test_that("relocate.survey_collection does NOT trigger group-removal pre-flight", {
  # Negative tidyselect on a grouped collection: relocate(-group, .before = wt)
  # is legal because relocate only reorders — it never drops columns. select()
  # would error with surveytidy_error_collection_select_group_removed; relocate
  # must NOT.
  coll <- make_test_collection(seed = 42)
  grouped_members <- lapply(coll@surveys, dplyr::group_by, group)
  coll_g <- surveycore::as_survey_collection(
    !!!grouped_members,
    .id = coll@id,
    .if_missing_var = coll@if_missing_var
  )

  out <- dplyr::relocate(coll_g, -group, .before = wt)
  test_collection_invariants(out)
  expect_identical(out@groups, "group")
  for (member in out@surveys) {
    expect_true("group" %in% names(member@data))
  }
})

test_that("relocate.survey_collection .if_missing_var = 'skip' drops bad members", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  # y3 missing on m2 only
  expect_message(
    out <- dplyr::relocate(coll_skip, tidyselect::all_of("y3"), .before = wt),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  expect_identical(names(out@surveys), c("m1", "m3"))
})

test_that("relocate.survey_collection .if_missing_var = 'error' raises typed", {
  coll <- make_heterogeneous_collection(seed = 42)

  expect_error(
    dplyr::relocate(coll, tidyselect::all_of("region"), .before = wt),
    class = "surveytidy_error_collection_verb_failed"
  )
})

# ── Coverage gap: select group-removal pre-flight defers when m1 cannot resolve

test_that("select.survey_collection group-removal pre-flight defers when first member can't resolve tidyselect", {
  # Covers R/select.R:386 — when the collection has @groups but
  # tidyselect::eval_select against the first member's @data errors (e.g.,
  # all_of() of a column missing on m1), the pre-flight returns
  # invisible(NULL) and defers to per-member dispatch where class-catch +
  # .if_missing_var govern the outcome.
  #
  # The proof that L386 executed is that the resulting error is NOT the
  # group-removed pre-flight error — it is the downstream dispatcher /
  # validator error from per-member dispatch.
  coll <- make_heterogeneous_collection(seed = 42)
  grouped_members <- lapply(coll@surveys, dplyr::group_by, group)
  coll_g <- surveycore::as_survey_collection(
    !!!grouped_members,
    .id = coll@id,
    .if_missing_var = coll@if_missing_var
  )

  # region exists only on m3; m1 cannot resolve all_of("region"), so the
  # group-removal pre-flight defers (returns NULL via L386). Per-member
  # dispatch then errors via class-catch (m1/m2 lack region under
  # .if_missing_var = "error").
  err <- tryCatch(
    dplyr::select(coll_g, tidyselect::all_of("region")),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  # Crucially, the error class is NOT the group-removed pre-flight class —
  # confirming L386 executed (returning NULL) rather than L390-399 firing.
  expect_false(
    inherits(err, "surveytidy_error_collection_select_group_removed")
  )
})
