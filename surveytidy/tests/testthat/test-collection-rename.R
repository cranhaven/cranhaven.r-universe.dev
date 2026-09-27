# tests/testthat/test-collection-rename.R
#
# rename.survey_collection and rename_with.survey_collection — PR 2b.
#
# Spec sections: §III.1 (template), §IV.4 (rename contract — class-catch
# detection, group-coverage pre-flight), §IX.3 (per-verb test categories).

# ── rename() happy path ─────────────────────────────────────────────────────

test_that("rename.survey_collection renames non-group col on every member (cross-design)", {
  coll <- make_test_collection(seed = 42)
  test_collection_invariants(coll)
  for (member in coll@surveys) {
    test_invariants(member)
  }

  out <- dplyr::rename(coll, outcome1 = y1)

  test_collection_invariants(out)
  for (member in out@surveys) {
    test_invariants(member)
    expect_true("outcome1" %in% names(member@data))
    expect_false("y1" %in% names(member@data))
  }
  expect_identical(names(out@surveys), names(coll@surveys))
  expect_identical(out@id, coll@id)
  expect_identical(out@if_missing_var, coll@if_missing_var)
})

test_that("rename.survey_collection accepts multiple pairs", {
  coll <- make_test_collection(seed = 42)

  out <- dplyr::rename(coll, outcome1 = y1, outcome2 = y2)

  test_collection_invariants(out)
  for (member in out@surveys) {
    expect_true(all(c("outcome1", "outcome2") %in% names(member@data)))
    expect_false(any(c("y1", "y2") %in% names(member@data)))
  }
})

# ── rename() group column ───────────────────────────────────────────────────

test_that("rename.survey_collection renaming group col updates @groups on coll and members", {
  coll <- make_test_collection(seed = 42)
  grouped_members <- lapply(coll@surveys, dplyr::group_by, group)
  coll_g <- surveycore::as_survey_collection(
    !!!grouped_members,
    .id = coll@id,
    .if_missing_var = coll@if_missing_var
  )

  out <- suppressWarnings(dplyr::rename(coll_g, demo = group))

  test_collection_invariants(out)
  expect_identical(out@groups, "demo")
  for (member in out@surveys) {
    expect_identical(member@groups, "demo")
    expect_true("demo" %in% names(member@data))
    expect_false("group" %in% names(member@data))
  }
})

test_that("rename.survey_collection raises group_partial when group col missing from a member", {
  # Spec §IV.4 reachability note: for plain rename, the partial branch can
  # only fire under a regression in surveycore's G1b enforcement. To exercise
  # the pre-flight here we must bypass the validator and construct the broken
  # state directly (this is the defense-in-depth scenario the class catches).
  coll <- make_test_collection(seed = 42)
  grouped_members <- lapply(coll@surveys, dplyr::group_by, group)
  coll_g <- surveycore::as_survey_collection(
    !!!grouped_members,
    .id = coll@id,
    .if_missing_var = coll@if_missing_var
  )

  # Bypass S7 validation on both the member ($data assignment) AND the
  # collection ($surveys assignment): use attr<- on each, then skip the
  # final S7::validate() — the broken state is the test fixture.
  surveys <- coll_g@surveys
  m2 <- surveys[[2L]]
  new_data <- m2@data[, setdiff(names(m2@data), "group"), drop = FALSE]
  attr(m2, "data") <- new_data
  surveys[[2L]] <- m2
  attr(coll_g, "surveys") <- surveys

  expect_error(
    dplyr::rename(coll_g, demo = group),
    class = "surveytidy_error_collection_rename_group_partial"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::rename(coll_g, demo = group)
  )
})

# ── rename() design-var warning multiplicity ─────────────────────────────────

test_that("rename.survey_collection fires per-member design-var warning N times", {
  coll <- make_test_collection(seed = 42)

  warning_count <- 0L
  expect_warning(
    out <- withCallingHandlers(
      dplyr::rename(coll, sampling_weight = wt),
      surveytidy_warning_rename_design_var = function(cnd) {
        warning_count <<- warning_count + 1L
        rlang::cnd_muffle(cnd)
      }
    ),
    regexp = NA
  )
  expect_identical(warning_count, length(coll@surveys))
  for (member in out@surveys) {
    expect_true("sampling_weight" %in% names(member@data))
    expect_false("wt" %in% names(member@data))
  }
})

# ── rename() .if_missing_var ────────────────────────────────────────────────

test_that("rename.survey_collection .if_missing_var = 'error' raises typed", {
  coll <- make_heterogeneous_collection(seed = 42)

  # region exists only on m3 — m1 and m2 are missing it; eval_rename raises
  # vctrs_error_subscript_oob, caught by the dispatcher's class_catch handler.
  expect_error(
    dplyr::rename(coll, area = region),
    class = "surveytidy_error_collection_verb_failed"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::rename(coll, area = region)
  )
})

test_that("rename.survey_collection .if_missing_var = 'skip' drops bad members", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  # y3 is missing on m2 only; m1 and m3 retain y3
  expect_message(
    out <- dplyr::rename(coll_skip, outcome3 = y3),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  expect_identical(names(out@surveys), c("m1", "m3"))
  for (member in out@surveys) {
    expect_true("outcome3" %in% names(member@data))
  }
})

test_that("rename.survey_collection .if_missing_var precedence", {
  coll <- make_heterogeneous_collection(seed = 42)

  # Stored "error", per-call "skip" → skip wins
  expect_message(
    out <- dplyr::rename(coll, outcome3 = y3, .if_missing_var = "skip"),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  expect_identical(names(out@surveys), c("m1", "m3"))

  # Stored "skip", per-call "error" → error wins
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")
  expect_error(
    dplyr::rename(coll_skip, outcome3 = y3, .if_missing_var = "error"),
    class = "surveytidy_error_collection_verb_failed"
  )
})

test_that("rename.survey_collection raises emptied error when all members skipped", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  expect_error(
    dplyr::rename(coll_skip, ghost = ghost_col_xyz),
    class = "surveytidy_error_collection_verb_emptied"
  )
})

# ── rename() domain / visible_vars preservation ─────────────────────────────

test_that("rename.survey_collection preserves domain column on every member", {
  coll <- make_test_collection(seed = 42)
  coll_filtered <- dplyr::filter(coll, y1 > 50)

  out <- dplyr::rename(coll_filtered, outcome1 = y1)
  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL

  for (i in seq_along(out@surveys)) {
    member_in <- coll_filtered@surveys[[i]]
    member_out <- out@surveys[[i]]
    expect_true(domain_col %in% names(member_out@data))
    expect_identical(
      member_out@data[[domain_col]],
      member_in@data[[domain_col]]
    )
  }
})

test_that("rename.survey_collection preserves visible_vars on every member", {
  coll <- make_test_collection(seed = 42)
  for (i in seq_along(coll@surveys)) {
    m <- coll@surveys[[i]]
    new_vars <- m@variables
    new_vars$visible_vars <- c("y1", "y2")
    attr(m, "variables") <- new_vars
    S7::validate(m)
    coll@surveys[[i]] <- m
  }

  out <- dplyr::rename(coll, outcome1 = y1)
  for (member in out@surveys) {
    expect_true("outcome1" %in% member@variables$visible_vars)
    expect_false("y1" %in% member@variables$visible_vars)
  }
})

# ── rename_with() happy path ────────────────────────────────────────────────

test_that("rename_with.survey_collection applies fn to selected col names on every member", {
  coll <- make_test_collection(seed = 42)
  test_collection_invariants(coll)

  out <- dplyr::rename_with(coll, toupper, .cols = c(y1, y2))

  test_collection_invariants(out)
  for (member in out@surveys) {
    test_invariants(member)
    expect_true(all(c("Y1", "Y2") %in% names(member@data)))
    expect_false(any(c("y1", "y2") %in% names(member@data)))
  }
})

test_that("rename_with.survey_collection accepts formula form", {
  coll <- make_test_collection(seed = 42)

  out <- dplyr::rename_with(
    coll,
    ~ paste0(., "_v2"),
    .cols = c(y1, y2)
  )

  test_collection_invariants(out)
  for (member in out@surveys) {
    expect_true(all(c("y1_v2", "y2_v2") %in% names(member@data)))
  }
})

# ── rename_with() group_partial pre-flight ──────────────────────────────────

test_that("rename_with.survey_collection raises group_partial when .cols resolves differently across members", {
  # Build a grouped collection where one member has the group col as factor
  # and another as character. .cols = where(is.factor) selects "group" on
  # the factor member but not on the character member — the partial scenario.
  coll <- make_test_collection(seed = 42)

  # Convert "group" to factor on m1 only (other members keep character).
  m1 <- coll@surveys[[1L]]
  new_data <- m1@data
  new_data$group <- factor(new_data$group)
  attr(m1, "data") <- new_data
  S7::validate(m1)

  m_rest <- coll@surveys[-1L]
  coll_mixed <- surveycore::as_survey_collection(
    m1,
    !!!m_rest,
    .id = coll@id,
    .if_missing_var = coll@if_missing_var
  )

  # Group on "group" on every member.
  grouped_members <- lapply(coll_mixed@surveys, dplyr::group_by, group)
  coll_g <- surveycore::as_survey_collection(
    !!!grouped_members,
    .id = coll@id,
    .if_missing_var = coll@if_missing_var
  )

  expect_error(
    dplyr::rename_with(
      coll_g,
      toupper,
      .cols = tidyselect::where(is.factor)
    ),
    class = "surveytidy_error_collection_rename_group_partial"
  )
})

# ── rename_with() .if_missing_var ───────────────────────────────────────────

test_that("rename_with.survey_collection .if_missing_var = 'error' raises typed", {
  coll <- make_heterogeneous_collection(seed = 42)

  # region exists only on m3
  expect_error(
    dplyr::rename_with(
      coll,
      toupper,
      .cols = tidyselect::all_of("region")
    ),
    class = "surveytidy_error_collection_verb_failed"
  )
})

test_that("rename_with.survey_collection .if_missing_var = 'skip' drops bad members", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  # y3 missing on m2 only
  expect_message(
    out <- dplyr::rename_with(
      coll_skip,
      toupper,
      .cols = tidyselect::all_of("y3")
    ),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  expect_identical(names(out@surveys), c("m1", "m3"))
  for (member in out@surveys) {
    expect_true("Y3" %in% names(member@data))
  }
})

# Coverage: when @groups is non-empty but no member is renaming a particular
# group column, the per-group loop in .check_group_rename_coverage() hits the
# `next` branch (R/rename.R L455-456). Triggered by renaming a NON-group
# column on a grouped collection — n_renaming = 0 for the group col.
test_that("rename.survey_collection no-op for group cols when renaming a non-group column", {
  coll <- make_test_collection(seed = 42)
  grouped_members <- lapply(coll@surveys, dplyr::group_by, group)
  coll_g <- surveycore::as_survey_collection(
    !!!grouped_members,
    .id = coll@id,
    .if_missing_var = coll@if_missing_var
  )

  # Rename a NON-group column — every member's rename_olds = c("y1");
  # for g = "group", n_renaming = 0 → loop hits `next`.
  out <- dplyr::rename(coll_g, outcome1 = y1)

  test_collection_invariants(out)
  expect_identical(out@groups, "group")
  for (member in out@surveys) {
    expect_identical(member@groups, "group")
    expect_true("outcome1" %in% names(member@data))
    expect_false("y1" %in% names(member@data))
    # Group column untouched.
    expect_true("group" %in% names(member@data))
  }
})

# Coverage: direct unit test for the early-return branch in
# .check_group_rename_coverage() (R/rename.R L442-443). The public
# rename.survey_collection / rename_with.survey_collection paths only call
# this helper when length(@groups) > 0L, so the early return is unreachable
# from the public API. We exercise it directly with the same wrapper-call
# pattern used for `.sc_propagate_or_match()` in test-utils.R.
test_that(".check_group_rename_coverage() early-returns when @groups is empty", {
  coll <- make_test_collection(seed = 42L) # @groups is character(0)
  expect_identical(coll@groups, character(0L))

  result <- .check_group_rename_coverage(
    coll,
    "rename",
    rename_olds_per_member = stats::setNames(
      lapply(coll@surveys, function(.x) "y1"),
      names(coll@surveys)
    )
  )
  expect_null(result)
})

test_that("rename_with.survey_collection any_of() silently drops missing per-survey", {
  coll <- make_heterogeneous_collection(seed = 42)

  out <- dplyr::rename_with(
    coll,
    toupper,
    .cols = tidyselect::any_of(c("y1", "ghost"))
  )

  test_collection_invariants(out)
  expect_identical(names(out@surveys), c("m1", "m2", "m3"))
  # y1 exists on m1 and m2 only — Y1 should appear there
  expect_true("Y1" %in% names(out@surveys$m1@data))
  expect_true("Y1" %in% names(out@surveys$m2@data))
  # m3 has no y1 → no Y1; any_of() silently drops on m3
  expect_false("Y1" %in% names(out@surveys$m3@data))
})
