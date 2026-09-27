# tests/testthat/test-collection-drop-na.R
#
# drop_na.survey_collection — PR 2b.
#
# Spec sections: §III.1 (template), §IV.2 (drop_na contract — class-catch
# detection, mirrors filter shape), §IX.3 (per-verb test categories).

# ── happy path ───────────────────────────────────────────────────────────────

test_that("drop_na.survey_collection marks NA rows out-of-domain on every member (cross-design)", {
  coll <- make_test_collection(seed = 42)
  test_collection_invariants(coll)
  for (member in coll@surveys) {
    test_invariants(member)
  }

  # Inject some NAs into y1 on every member
  for (i in seq_along(coll@surveys)) {
    m <- coll@surveys[[i]]
    new_data <- m@data
    new_data$y1[1:5] <- NA_real_
    attr(m, "data") <- new_data
    S7::validate(m)
    coll@surveys[[i]] <- m
  }

  out <- tidyr::drop_na(coll, y1)
  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL

  test_collection_invariants(out)
  for (member in out@surveys) {
    test_invariants(member)
    expect_true(domain_col %in% names(member@data))
    # Rows 1:5 should be out-of-domain
    expect_true(all(!member@data[[domain_col]][1:5]))
    # Other rows should be in-domain
    expect_true(all(member@data[[domain_col]][6:nrow(member@data)]))
  }
})

test_that("drop_na.survey_collection with no cols checks all columns", {
  coll <- make_test_collection(seed = 42)

  # Inject NAs into different columns on different rows
  for (i in seq_along(coll@surveys)) {
    m <- coll@surveys[[i]]
    new_data <- m@data
    new_data$y1[1:3] <- NA_real_
    new_data$y2[4:6] <- NA_real_
    attr(m, "data") <- new_data
    S7::validate(m)
    coll@surveys[[i]] <- m
  }

  out <- tidyr::drop_na(coll)
  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL

  test_collection_invariants(out)
  for (member in out@surveys) {
    expect_true(all(!member@data[[domain_col]][1:6]))
  }
})

# ── .if_missing_var ──────────────────────────────────────────────────────────

test_that("drop_na.survey_collection .if_missing_var = 'error' raises typed", {
  coll <- make_heterogeneous_collection(seed = 42)

  expect_error(
    tidyr::drop_na(coll, tidyselect::all_of("region")),
    class = "surveytidy_error_collection_verb_failed"
  )
  expect_snapshot(
    error = TRUE,
    tidyr::drop_na(coll, tidyselect::all_of("region"))
  )
})

test_that("drop_na.survey_collection .if_missing_var = 'skip' drops bad members", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  expect_message(
    out <- tidyr::drop_na(coll_skip, tidyselect::all_of("y3")),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  expect_identical(names(out@surveys), c("m1", "m3"))
  expect_identical(out@if_missing_var, "skip")
})

test_that("drop_na.survey_collection cannot accept per-call .if_missing_var (tidyr generic check_dots_unnamed)", {
  # tidyr::drop_na's generic calls check_dots_unnamed() before dispatch,
  # which rejects any named ... arg. drop_na.survey_collection therefore
  # only supports the stored mode (via set_collection_if_missing_var()).
  coll <- make_heterogeneous_collection(seed = 42)

  expect_error(
    tidyr::drop_na(
      coll,
      tidyselect::all_of("y3"),
      .if_missing_var = "skip"
    ),
    "must be passed by position"
  )
})

test_that("drop_na.survey_collection raises emptied error when all members skipped", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  expect_error(
    tidyr::drop_na(coll_skip, tidyselect::all_of("ghost_col_xyz")),
    class = "surveytidy_error_collection_verb_emptied"
  )
})

# ── domain accumulation ─────────────────────────────────────────────────────

test_that("drop_na.survey_collection accumulates with prior filter() domain", {
  coll <- make_test_collection(seed = 42)

  # Inject NAs in y1
  for (i in seq_along(coll@surveys)) {
    m <- coll@surveys[[i]]
    new_data <- m@data
    new_data$y1[1:5] <- NA_real_
    attr(m, "data") <- new_data
    S7::validate(m)
    coll@surveys[[i]] <- m
  }

  filtered <- dplyr::filter(coll, y2 > 0)
  out <- tidyr::drop_na(filtered, y1)
  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL

  for (i in seq_along(out@surveys)) {
    member_in <- coll@surveys[[i]]
    member_out <- out@surveys[[i]]
    expected <- (member_in@data$y2 > 0) & !is.na(member_in@data$y1)
    expect_identical(member_out@data[[domain_col]], expected)
  }
})
