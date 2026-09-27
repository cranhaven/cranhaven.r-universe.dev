# tests/testthat/test-collection-filter.R
#
# filter.survey_collection and filter_out.survey_collection — PR 2a.
#
# Spec sections: §III.1 (template + .by rejection), §IV.1 (filter contract),
# §IX.3 (per-verb test categories).

# ── filter() ─────────────────────────────────────────────────────────────────

test_that("filter.survey_collection marks domain on every member (cross-design)", {
  coll <- make_test_collection(seed = 42)
  test_collection_invariants(coll)
  for (member in coll@surveys) {
    test_invariants(member)
  }

  out <- dplyr::filter(coll, y1 > 50)

  test_collection_invariants(out)
  for (member in out@surveys) {
    test_invariants(member)
  }
  expect_identical(names(out@surveys), names(coll@surveys))
  expect_identical(out@id, coll@id)
  expect_identical(out@if_missing_var, coll@if_missing_var)

  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL
  for (i in seq_along(out@surveys)) {
    member_in <- coll@surveys[[i]]
    member_out <- out@surveys[[i]]
    expect_true(domain_col %in% names(member_out@data))
    expect_identical(
      member_out@data[[domain_col]],
      member_in@data$y1 > 50
    )
  }
})

test_that("filter.survey_collection preserves @groups on a grouped collection", {
  coll <- make_test_collection(seed = 42)
  grouped_members <- lapply(coll@surveys, dplyr::group_by, group)
  coll <- surveycore::as_survey_collection(
    !!!grouped_members,
    .id = coll@id,
    .if_missing_var = coll@if_missing_var
  )

  out <- dplyr::filter(coll, y1 > 50)
  expect_identical(out@groups, "group")
  for (member in out@surveys) {
    expect_identical(member@groups, "group")
  }
})

test_that("filter.survey_collection .if_missing_var = 'error' raises typed", {
  coll <- make_heterogeneous_collection(seed = 42)

  # region exists only on m3 — m1 and m2 are missing it
  expect_error(
    dplyr::filter(coll, region == "north"),
    class = "surveytidy_error_collection_verb_failed"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::filter(coll, region == "north")
  )
})

test_that("filter.survey_collection .if_missing_var = 'skip' drops bad members", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  # y3 is missing on m2 only; m1 and m3 retain y3
  expect_message(
    out <- dplyr::filter(coll_skip, y3 > 0),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  expect_identical(names(out@surveys), c("m1", "m3"))
  expect_identical(out@if_missing_var, "skip")
})

test_that("filter.survey_collection .if_missing_var precedence", {
  coll <- make_heterogeneous_collection(seed = 42)

  # Stored "error", per-call "skip" → skip wins
  expect_message(
    out <- dplyr::filter(coll, y3 > 0, .if_missing_var = "skip"),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  expect_identical(names(out@surveys), c("m1", "m3"))

  # Stored "skip", per-call "error" → error wins
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")
  expect_error(
    dplyr::filter(coll_skip, y3 > 0, .if_missing_var = "error"),
    class = "surveytidy_error_collection_verb_failed"
  )
})

test_that("filter.survey_collection raises emptied error when all members skipped", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  expect_error(
    dplyr::filter(coll_skip, ghost_col_xyz > 0),
    class = "surveytidy_error_collection_verb_emptied"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::filter(coll_skip, ghost_col_xyz > 0)
  )
})

test_that("filter.survey_collection rejects .by", {
  coll <- make_test_collection(seed = 42)

  expect_error(
    dplyr::filter(coll, y1 > 0, .by = "group"),
    class = "surveytidy_error_collection_by_unsupported"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::filter(coll, y1 > 0, .by = "group")
  )
})

test_that("filter.survey_collection rejects .by before any member is touched", {
  # Use a fixture that would error per-member if the .by guard were skipped:
  # ghost_col would trigger the dispatcher's pre-check. Asserting that .by
  # raises *first* (with its own class) confirms the guard runs before
  # dispatch.
  coll <- make_heterogeneous_collection(seed = 42)
  expect_error(
    dplyr::filter(coll, ghost_col > 0, .by = "group"),
    class = "surveytidy_error_collection_by_unsupported"
  )
})

test_that("filter.survey_collection preserves visible_vars on every member", {
  coll <- make_test_collection(seed = 42)

  # Set visible_vars on each member via the attr<-/validate bypass pattern.
  for (i in seq_along(coll@surveys)) {
    m <- coll@surveys[[i]]
    new_vars <- m@variables
    new_vars$visible_vars <- c("y1", "y2")
    attr(m, "variables") <- new_vars
    S7::validate(m)
    coll@surveys[[i]] <- m
  }

  out <- dplyr::filter(coll, y1 > 0)
  for (member in out@surveys) {
    expect_identical(member@variables$visible_vars, c("y1", "y2"))
  }
})


# ── filter_out() ─────────────────────────────────────────────────────────────

test_that("filter_out.survey_collection marks complement domain on members", {
  coll <- make_test_collection(seed = 42)
  out <- filter_out(coll, y1 > 50)

  test_collection_invariants(out)
  for (member in out@surveys) {
    test_invariants(member)
  }
  expect_identical(names(out@surveys), names(coll@surveys))

  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL
  for (i in seq_along(out@surveys)) {
    member_in <- coll@surveys[[i]]
    member_out <- out@surveys[[i]]
    expect_true(domain_col %in% names(member_out@data))
    expect_identical(
      member_out@data[[domain_col]],
      !(member_in@data$y1 > 50)
    )
  }
})

test_that("filter_out.survey_collection rejects .by", {
  coll <- make_test_collection(seed = 42)
  expect_error(
    filter_out(coll, y1 > 0, .by = "group"),
    class = "surveytidy_error_collection_by_unsupported"
  )
})

test_that("filter_out.survey_collection .if_missing_var = 'skip' drops bad members", {
  coll <- make_heterogeneous_collection(seed = 42)

  # y3 is missing on m2 only; m1 and m3 retain y3
  expect_message(
    out <- filter_out(coll, y3 > 0, .if_missing_var = "skip"),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  expect_identical(names(out@surveys), c("m1", "m3"))
})

test_that("filter_out.survey_collection preserves visible_vars on every member", {
  coll <- make_test_collection(seed = 42)
  for (i in seq_along(coll@surveys)) {
    m <- coll@surveys[[i]]
    new_vars <- m@variables
    new_vars$visible_vars <- c("y1", "y2")
    attr(m, "variables") <- new_vars
    S7::validate(m)
    coll@surveys[[i]] <- m
  }

  # y1 has min ≈ 25 in test data; y1 > 1e6 excludes nothing → no empty-domain warning
  out <- filter_out(coll, y1 > 1e6)
  for (member in out@surveys) {
    expect_identical(member@variables$visible_vars, c("y1", "y2"))
  }
})
