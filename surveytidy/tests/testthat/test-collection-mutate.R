# tests/testthat/test-collection-mutate.R
#
# mutate.survey_collection — PR 2a.
#
# Spec sections: §III.1 (template + .by rejection), §IV.5 (mutate contract,
# rowwise mixed-state pre-check, per-member warning multiplicity), §IX.3.

# ── happy path ───────────────────────────────────────────────────────────────

test_that("mutate.survey_collection adds column to every member (cross-design)", {
  coll <- make_test_collection(seed = 42)
  test_collection_invariants(coll)
  for (member in coll@surveys) {
    test_invariants(member)
  }

  out <- dplyr::mutate(coll, z = y1 + 1)

  test_collection_invariants(out)
  for (member in out@surveys) {
    test_invariants(member)
    expect_true("z" %in% names(member@data))
    expect_identical(member@data$z, member@data$y1 + 1)
  }
  expect_identical(names(out@surveys), names(coll@surveys))
  expect_identical(out@id, coll@id)
  expect_identical(out@if_missing_var, coll@if_missing_var)
})

# ── .if_missing_var ──────────────────────────────────────────────────────────

test_that("mutate.survey_collection .if_missing_var = 'error' raises typed", {
  coll <- make_heterogeneous_collection(seed = 42)

  expect_error(
    dplyr::mutate(coll, z = region),
    class = "surveytidy_error_collection_verb_failed"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::mutate(coll, z = region)
  )
})

test_that("mutate.survey_collection .if_missing_var = 'skip' drops bad members", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  # y3 missing on m2 only; m1 and m3 retain y3
  expect_message(
    out <- dplyr::mutate(coll_skip, z = y3 + 1),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  expect_identical(names(out@surveys), c("m1", "m3"))
  for (member in out@surveys) {
    expect_true("z" %in% names(member@data))
  }
})

test_that("mutate.survey_collection .if_missing_var precedence", {
  coll <- make_heterogeneous_collection(seed = 42)

  # Stored "error", per-call "skip" → skip wins
  expect_message(
    out <- dplyr::mutate(coll, z = y3 + 1, .if_missing_var = "skip"),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  expect_identical(names(out@surveys), c("m1", "m3"))

  # Stored "skip", per-call "error" → error wins
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")
  expect_error(
    dplyr::mutate(coll_skip, z = y3 + 1, .if_missing_var = "error"),
    class = "surveytidy_error_collection_verb_failed"
  )
})

test_that("mutate.survey_collection raises emptied error when all members skipped", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  expect_error(
    dplyr::mutate(coll_skip, z = ghost_col_xyz + 1),
    class = "surveytidy_error_collection_verb_emptied"
  )
})

# ── .by rejection ────────────────────────────────────────────────────────────

test_that("mutate.survey_collection rejects .by", {
  coll <- make_test_collection(seed = 42)

  expect_error(
    dplyr::mutate(coll, z = y1 + 1, .by = "group"),
    class = "surveytidy_error_collection_by_unsupported"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::mutate(coll, z = y1 + 1, .by = "group")
  )
})

# ── per-member warning multiplicity ─────────────────────────────────────────

test_that("mutate.survey_collection fires per-member weight warning N times", {
  coll <- make_test_collection(seed = 42)

  weight_warning_count <- 0L
  expect_warning(
    out <- withCallingHandlers(
      dplyr::mutate(coll, wt = wt * 1),
      surveytidy_warning_mutate_weight_col = function(cnd) {
        weight_warning_count <<- weight_warning_count + 1L
        rlang::cnd_muffle(cnd)
      }
    ),
    regexp = NA
  )
  expect_identical(weight_warning_count, length(coll@surveys))
})

# ── rowwise mixed-state pre-check (spec §IV.5 / steps 22a-22e) ──────────────

test_that("mutate.survey_collection warns once on rowwise mixed state", {
  coll <- make_test_collection(seed = 42)
  # Make m1 rowwise; leave m2 and m3 non-rowwise.
  coll@surveys[[1L]] <- dplyr::rowwise(coll@surveys[[1L]])

  rowwise_warning_count <- 0L
  expect_warning(
    out <- withCallingHandlers(
      dplyr::mutate(coll, z = y1 + 1),
      surveytidy_warning_collection_rowwise_mixed = function(cnd) {
        rowwise_warning_count <<- rowwise_warning_count + 1L
        rlang::cnd_muffle(cnd)
      }
    ),
    regexp = NA
  )
  expect_identical(rowwise_warning_count, 1L)

  # Dispatch still proceeds normally.
  test_collection_invariants(out)
  for (member in out@surveys) {
    test_invariants(member)
    expect_true("z" %in% names(member@data))
  }

  # Snapshot the warning text (call again, capturing the warning object).
  expect_snapshot(
    {
      withCallingHandlers(
        dplyr::mutate(coll, z = y1 + 1),
        surveytidy_warning_collection_rowwise_mixed = function(cnd) {
          message(conditionMessage(cnd))
          rlang::cnd_muffle(cnd)
        }
      )
      invisible()
    }
  )
})

test_that("mutate.survey_collection: uniform rowwise state never warns", {
  coll <- make_test_collection(seed = 42)

  # Uniformly non-rowwise (default)
  rowwise_warning_count <- 0L
  withCallingHandlers(
    dplyr::mutate(coll, z = y1 + 1),
    surveytidy_warning_collection_rowwise_mixed = function(cnd) {
      rowwise_warning_count <<- rowwise_warning_count + 1L
      rlang::cnd_muffle(cnd)
    }
  )
  expect_identical(rowwise_warning_count, 0L)

  # Uniformly rowwise
  coll_rw <- coll
  for (i in seq_along(coll_rw@surveys)) {
    coll_rw@surveys[[i]] <- dplyr::rowwise(coll_rw@surveys[[i]])
  }
  rowwise_warning_count <- 0L
  withCallingHandlers(
    dplyr::mutate(coll_rw, z = y1 + 1),
    surveytidy_warning_collection_rowwise_mixed = function(cnd) {
      rowwise_warning_count <<- rowwise_warning_count + 1L
      rlang::cnd_muffle(cnd)
    }
  )
  expect_identical(rowwise_warning_count, 0L)
})

# ── domain preservation ─────────────────────────────────────────────────────

test_that("mutate.survey_collection preserves domain column on every member", {
  coll <- make_test_collection(seed = 42)
  coll_filtered <- dplyr::filter(coll, y1 > 50)

  out <- dplyr::mutate(coll_filtered, z = y1 + 1)
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

# ── visible_vars preservation ───────────────────────────────────────────────

test_that("mutate.survey_collection preserves visible_vars on every member", {
  coll <- make_test_collection(seed = 42)
  for (i in seq_along(coll@surveys)) {
    m <- coll@surveys[[i]]
    new_vars <- m@variables
    new_vars$visible_vars <- c("y1", "y2")
    attr(m, "variables") <- new_vars
    S7::validate(m)
    coll@surveys[[i]] <- m
  }

  out <- dplyr::mutate(coll, z = y1 + 1)
  for (member in out@surveys) {
    # mutate adds "z" to visible_vars per surveytidy convention; assert the
    # original two are still present (preservation, not exact equality).
    expect_true(all(c("y1", "y2") %in% member@variables$visible_vars))
  }
})
