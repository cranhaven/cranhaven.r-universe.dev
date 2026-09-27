# tests/testthat/test-collection-pull.R
#
# pull.survey_collection — PR 3.
#
# Spec sections: §V.1 (pull contract — class-catch detection only, vctrs::vec_c()
# combination, name = NULL / coll@id sentinel / "<other_col>" semantics, domain
# inclusion, V8 collapsing return type), §IX.3 (per-verb test categories).

# ── pull() happy path ───────────────────────────────────────────────────────

test_that("pull.survey_collection returns combined vector across members", {
  coll <- make_test_collection(seed = 42)
  test_collection_invariants(coll)
  for (member in coll@surveys) {
    test_invariants(member)
  }

  out <- dplyr::pull(coll, y1)

  expected_len <- sum(vapply(
    coll@surveys,
    function(m) nrow(m@data),
    integer(1L)
  ))
  expect_type(out, "double")
  expect_length(out, expected_len)
  expect_null(names(out))
})

# ── pull() naming variants ──────────────────────────────────────────────────

test_that("pull.survey_collection name = NULL leaves vector unnamed", {
  coll <- make_test_collection(seed = 42)
  out <- dplyr::pull(coll, y1, name = NULL)
  expect_null(names(out))
})

test_that("pull.survey_collection name = coll@id labels by default .survey", {
  coll <- make_test_collection(seed = 42)
  out <- dplyr::pull(coll, y1, name = coll@id)

  per_member_lens <- vapply(
    coll@surveys,
    function(m) nrow(m@data),
    integer(1L)
  )
  expected_names <- rep(names(coll@surveys), per_member_lens)
  expect_identical(names(out), expected_names)
})

test_that("pull.survey_collection name = coll@id labels by user-set .id", {
  designs <- make_all_designs(seed = 42)
  coll <- surveycore::as_survey_collection(
    !!!designs,
    .id = "wave",
    .if_missing_var = "error"
  )
  expect_identical(coll@id, "wave")

  out <- dplyr::pull(coll, y1, name = coll@id)
  per_member_lens <- vapply(
    coll@surveys,
    function(m) nrow(m@data),
    integer(1L)
  )
  expected_names <- rep(names(coll@surveys), per_member_lens)
  expect_identical(names(out), expected_names)
})

test_that("pull.survey_collection name = '<other_col>' passes through", {
  coll <- make_test_collection(seed = 42)
  # `psu` is in every member's @data; per-row names come from psu values.
  out <- dplyr::pull(coll, y1, name = psu)
  per_member_psus <- unlist(
    lapply(coll@surveys, function(m) as.character(m@data$psu)),
    use.names = FALSE
  )
  expect_identical(names(out), per_member_psus)
})

# ── pull() class-catch (missing column) ────────────────────────────────────

test_that("pull.survey_collection .if_missing_var = 'error' re-raises with vctrs parent", {
  coll <- make_heterogeneous_collection(seed = 42)

  # `region` exists only on m3; using `all_of("region")` raises
  # vctrs_error_subscript_oob on m1/m2 — the standard class-catch path.
  # (Bare-name `region` would raise rlang_error → simpleError, which is not
  # part of the documented class-catch contract — see spec §V.1 step 2.)
  expect_error(
    dplyr::pull(coll, tidyselect::all_of("region")),
    class = "surveytidy_error_collection_verb_failed"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::pull(coll, tidyselect::all_of("region"))
  )

  cnd <- tryCatch(
    dplyr::pull(coll, tidyselect::all_of("region")),
    error = function(e) e
  )
  expect_true(inherits(cnd$parent, "vctrs_error_subscript_oob"))
})

test_that("pull.survey_collection .if_missing_var = 'skip' drops bad members", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  expect_message(
    out <- dplyr::pull(coll_skip, tidyselect::all_of("region")),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  # Only m3 has `region`; output length matches m3's row count.
  expect_length(out, nrow(coll@surveys$m3@data))
})

test_that("pull.survey_collection empty result raises surveytidy_error_collection_verb_emptied", {
  # All members lack the column under .if_missing_var = "skip" — every member
  # gets dropped, leaving an empty result. The verb-emptied error fires.
  coll <- make_test_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  expect_error(
    suppressMessages(
      dplyr::pull(coll_skip, tidyselect::all_of("definitely_not_a_column"))
    ),
    class = "surveytidy_error_collection_verb_emptied"
  )

  # Per-call .if_missing_var should produce the per-call source-line in the
  # error message (covers the alternate id_from_stored = FALSE branch).
  expect_error(
    suppressMessages(
      dplyr::pull(
        coll,
        tidyselect::all_of("definitely_not_a_column"),
        .if_missing_var = "skip"
      )
    ),
    class = "surveytidy_error_collection_verb_emptied"
  )
})

test_that("pull.survey_collection class-catch handler also catches missing name column", {
  coll <- make_heterogeneous_collection(seed = 42)

  # Using `name = "region"` (string) raises vctrs_error_subscript_oob from
  # tidyselect::vars_pull on m1 — the documented class-catch path for the
  # `name` arg per spec §V.1 step 2.
  expect_error(
    dplyr::pull(coll, y1, name = "region"),
    class = "surveytidy_error_collection_verb_failed"
  )

  cnd <- tryCatch(
    dplyr::pull(coll, y1, name = "region"),
    error = function(e) e
  )
  expect_true(inherits(cnd$parent, "vctrs_error_subscript_oob"))
})

# ── pull() vctrs::vec_c() type incompatibility ─────────────────────────────

# Build a 2-member taylor collection where `flag` is character on m1 and
# integer on m2 — vctrs::vec_c() will refuse to combine them.
.make_type_clash_collection <- function(seed = 42) {
  set.seed(seed)
  base <- make_survey_data(n = 80L, n_psu = 8L, n_strata = 2L, seed = seed)
  m1_data <- base
  m1_data$flag <- sample(c("a", "b"), nrow(base), replace = TRUE)
  m2_data <- base
  m2_data$flag <- sample(0:1, nrow(base), replace = TRUE)
  to_taylor <- function(df) {
    surveycore::as_survey(
      df,
      ids = psu,
      strata = strata,
      weights = wt,
      fpc = fpc
    )
  }
  surveycore::as_survey_collection(
    m1 = to_taylor(m1_data),
    m2 = to_taylor(m2_data),
    .id = ".survey",
    .if_missing_var = "error"
  )
}

test_that("pull.survey_collection raises typed error on vec_c type clash", {
  coll <- .make_type_clash_collection(seed = 42)

  expect_error(
    dplyr::pull(coll, flag),
    class = "surveytidy_error_collection_pull_incompatible_types"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::pull(coll, flag)
  )

  cnd <- tryCatch(
    dplyr::pull(coll, flag),
    error = function(e) e
  )
  expect_true(inherits(cnd$parent, "vctrs_error_incompatible_type"))
})

# ── pull() domain inclusion ────────────────────────────────────────────────

test_that("pull.survey_collection includes both in-domain and out-of-domain rows", {
  coll <- make_test_collection(seed = 42)
  # Pre-filter on a threshold that leaves a mix of in- and out-of-domain rows
  # (y1 is roughly N(50, 10), so y1 > 60 leaves roughly 1/6 of rows out).
  coll_filtered <- dplyr::filter(coll, y1 > 60)

  # Confirm setup: at least one member has at least one out-of-domain row.
  has_oo <- vapply(
    coll_filtered@surveys,
    function(m) {
      domain_col <- surveycore::SURVEYCORE_DOMAIN_COL
      any(!m@data[[domain_col]])
    },
    logical(1L)
  )
  expect_true(any(has_oo))

  # pull(y1) should include BOTH in-domain and out-of-domain values: the
  # output length matches the sum of per-member nrow, not the in-domain count.
  out <- dplyr::pull(coll_filtered, y1)
  expected_len <- sum(vapply(
    coll_filtered@surveys,
    function(m) nrow(m@data),
    integer(1L)
  ))
  expect_length(out, expected_len)

  # Out-of-domain values are present: minimum of `out` is below the filter
  # threshold (60) on at least one member.
  expect_true(any(out <= 60))
})

# ── pull() tidyselect helpers (verifies class-catch over pre-check) ────────

# Homogeneous all-taylor collection for tidyselect-helper tests: every member
# has identical schema, so `last_col()` resolves to the same column on each
# member and `vctrs::vec_c()` can combine cleanly. (`make_test_collection()`
# mixes design types, so the trailing columns differ across members.)
.make_homogeneous_collection <- function(seed = 42) {
  base <- make_survey_data(n = 80L, n_psu = 8L, n_strata = 2L, seed = seed)
  to_taylor <- function(df) {
    surveycore::as_survey(
      df,
      ids = psu,
      strata = strata,
      weights = wt,
      fpc = fpc
    )
  }
  surveycore::as_survey_collection(
    m1 = to_taylor(base),
    m2 = to_taylor(base),
    .id = ".survey",
    .if_missing_var = "error"
  )
}

test_that("pull.survey_collection works with last_col() helper", {
  coll <- .make_homogeneous_collection(seed = 42)
  # last_col() resolves through tidyselect (returns an integer index). An
  # env-aware pre-check would misclassify the helper name as missing. The
  # class-catch detection path avoids that false positive — verifies Issue
  # 22 / Pass 2 fix.
  out <- dplyr::pull(coll, tidyselect::last_col())
  expected_len <- sum(vapply(
    coll@surveys,
    function(m) nrow(m@data),
    integer(1L)
  ))
  expect_length(out, expected_len)
})
