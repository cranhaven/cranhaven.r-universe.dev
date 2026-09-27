# tests/testthat/test-survey-collection.R
#
# Tests for R/survey-collection.R and the survey_collection S7 class defined
# in R/core-classes.R.
#
# Coverage: error-messages.md rows C1, C2, C2a, C3, C4, C8.
#
# Layer 1 (S7 validator) errors: class= only, no snapshot.
# Layer 3 (constructor / mutator) errors: dual pattern (class= + snapshot).
# Duplicate-name repair warnings: dual pattern (class= + snapshot).

# ── Helpers ────────────────────────────────────────────────────────────────────

# Build two distinct, small survey_taylor objects for collection tests.
.coll_surveys <- function() {
  df1 <- make_survey_data(n = 40L, n_psu = 8L, n_strata = 2L, seed = 11L)
  df2 <- make_survey_data(n = 60L, n_psu = 10L, n_strata = 2L, seed = 12L)
  d1 <- suppressMessages(suppressWarnings(
    as_survey(df1, ids = psu, weights = wt, strata = strata, fpc = fpc)
  ))
  d2 <- suppressMessages(suppressWarnings(
    as_survey(df2, ids = psu, weights = wt, strata = strata, fpc = fpc)
  ))
  list(d1 = d1, d2 = d2)
}

# Build three distinct survey_taylor objects for print / mutator tests.
.coll_surveys3 <- function() {
  out <- .coll_surveys()
  df3 <- make_survey_data(n = 80L, n_psu = 8L, n_strata = 2L, seed = 13L)
  out$d3 <- suppressMessages(suppressWarnings(
    as_survey(df3, ids = psu, weights = wt, strata = strata, fpc = fpc)
  ))
  out
}


# ── §7.1 Section 2 — S7 validator errors (Layer 1, class= only) ────────────────

test_that("survey_collection() validator accepts valid direct construction", {
  s <- .coll_surveys()
  coll <- survey_collection(surveys = list(a = s$d1, b = s$d2))
  expect_true(S7::S7_inherits(coll, survey_collection))
  expect_identical(names(coll@surveys), c("a", "b"))
})

test_that("survey_collection() validator rejects empty list", {
  expect_error(
    survey_collection(surveys = list()),
    class = "surveycore_error_collection_empty"
  )
})

test_that("survey_collection() validator rejects unnamed elements with surveycore_error_collection_unnamed", {
  s <- .coll_surveys()
  expect_error(
    survey_collection(surveys = list(s$d1, s$d2)),
    class = "surveycore_error_collection_unnamed"
  )
})

test_that("survey_collection() validator rejects any empty name with surveycore_error_collection_unnamed", {
  s <- .coll_surveys()
  expect_error(
    survey_collection(surveys = list(a = s$d1, s$d2)),
    class = "surveycore_error_collection_unnamed"
  )
})

test_that("survey_collection() validator rejects NA names with surveycore_error_collection_unnamed", {
  s <- .coll_surveys()
  lst <- list(s$d1, s$d2)
  names(lst) <- c("a", NA_character_)
  expect_error(
    survey_collection(surveys = lst),
    class = "surveycore_error_collection_unnamed"
  )
})

# Regression: empty collection still errors with surveycore_error_collection_empty
test_that("survey_collection() validator rejects empty list with surveycore_error_collection_empty (regression)", {
  expect_error(
    survey_collection(surveys = list()),
    class = "surveycore_error_collection_empty"
  )
})

test_that("survey_collection() validator rejects duplicate names (backstop)", {
  s <- .coll_surveys()
  expect_error(
    survey_collection(surveys = list(a = s$d1, a = s$d2)),
    class = "surveycore_error_collection_duplicate_name"
  )
})

test_that("survey_collection() validator rejects non-survey_base element", {
  s <- .coll_surveys()
  expect_error(
    survey_collection(surveys = list(a = s$d1, b = data.frame(x = 1))),
    class = "surveycore_error_collection_bad_element"
  )
})

test_that("survey_collection() validator rejects nested collection (invariant 4)", {
  s <- .coll_surveys()
  inner <- survey_collection(surveys = list(a = s$d1))
  # Nested collection: since survey_collection does NOT inherit survey_base,
  # invariant 4 reduces to invariant 3 (bad element).
  expect_error(
    survey_collection(surveys = list(a = s$d1, b = inner)),
    class = "surveycore_error_collection_bad_element"
  )
})

test_that("survey_collection does NOT inherit survey_base", {
  s <- .coll_surveys()
  coll <- survey_collection(surveys = list(a = s$d1, b = s$d2))
  expect_false(S7::S7_inherits(coll, survey_base))
})


# ── §7.1 Section 1 — as_survey_collection() happy paths ────────────────────────

test_that("as_survey_collection() accepts explicit names", {
  s <- .coll_surveys()
  coll <- as_survey_collection("2017-18" = s$d1, "2019-20" = s$d2)
  test_collection_invariants(coll)
  expect_identical(names(coll), c("2017-18", "2019-20"))
  expect_equal(length(coll), 2L)
})

test_that("as_survey_collection() auto-names bare symbols", {
  s <- .coll_surveys()
  d_2017 <- s$d1
  d_2019 <- s$d2
  coll <- as_survey_collection(d_2017, d_2019)
  test_collection_invariants(coll)
  expect_identical(names(coll), c("d_2017", "d_2019"))
})

test_that("as_survey_collection() mixes named and bare symbols", {
  s <- .coll_surveys()
  d_2019 <- s$d2
  coll <- as_survey_collection("wave1" = s$d1, d_2019)
  test_collection_invariants(coll)
  expect_identical(names(coll), c("wave1", "d_2019"))
})

test_that("as_survey_collection() accepts length-1 collections", {
  s <- .coll_surveys()
  coll <- as_survey_collection("d" = s$d1)
  test_collection_invariants(coll)
  expect_identical(names(coll), "d")
  expect_equal(length(coll), 1L)
})

test_that("as_survey_collection() accepts mixed design types", {
  # Mix a taylor design with a replicate design — no warning expected.
  df_rep <- make_survey_data(
    n = 40L,
    n_psu = 8L,
    n_strata = 2L,
    design = "replicate",
    type = "JK1",
    seed = 21L
  )
  s <- .coll_surveys()
  d_rep <- suppressMessages(suppressWarnings(
    as_survey_replicate(
      df_rep,
      weights = wt,
      repweights = tidyselect::starts_with("repwt_"),
      type = "JK1"
    )
  ))
  expect_no_warning(
    coll <- as_survey_collection(taylor = s$d1, rep = d_rep)
  )
  test_collection_invariants(coll)
  expect_true(S7::S7_inherits(coll@surveys$taylor, survey_taylor))
  expect_true(S7::S7_inherits(coll@surveys$rep, survey_replicate))
})


# ── §7.1 Section 10 — as_survey_collection() errors ──────────────────────────

test_that("as_survey_collection() rejects unnamed non-symbol arguments", {
  df1 <- make_survey_data(n = 40L, n_psu = 8L, n_strata = 2L, seed = 11L)
  expect_error(
    as_survey_collection(suppressMessages(suppressWarnings(
      as_survey(df1, ids = psu, weights = wt, strata = strata, fpc = fpc)
    ))),
    class = "surveycore_error_collection_unnamed_expr"
  )
  expect_snapshot(
    error = TRUE,
    as_survey_collection(suppressMessages(suppressWarnings(
      as_survey(df1, ids = psu, weights = wt, strata = strata, fpc = fpc)
    )))
  )
})


# ── §7.1 Section 10 — duplicate-name repair warnings ───────────────────────

test_that("as_survey_collection() repairs duplicate bare-symbol names", {
  s <- .coll_surveys()
  d1 <- s$d1
  expect_warning(
    coll <- as_survey_collection(d1, d1),
    class = "surveycore_warning_collection_duplicate_name_repaired"
  )
  test_collection_invariants(coll)
  expect_identical(names(coll), c("d1", "d1_1"))
})

test_that("as_survey_collection() repairs cascading duplicate names", {
  s <- .coll_surveys()
  s3 <- .coll_surveys3()
  # Cascading per spec §3.3.1 (left-to-right, `seen` only tracks past output):
  # ("x", "x", "x_1") → ("x", "x_1", "x_1_1"). Arg 2's "x" is repaired to
  # "x_1" (free at that point); arg 3's original "x_1" then collides with
  # output[2] and is bumped to "x_1_1".
  expect_warning(
    coll <- as_survey_collection(
      "x" = s$d1,
      "x" = s$d2,
      "x_1" = s3$d3
    ),
    class = "surveycore_warning_collection_duplicate_name_repaired"
  )
  test_collection_invariants(coll)
  expect_identical(names(coll), c("x", "x_1", "x_1_1"))
})

test_that("as_survey_collection() duplicate-name repair snapshot", {
  s <- .coll_surveys()
  d1 <- s$d1
  expect_snapshot(coll <- as_survey_collection(d1, d1))
})

test_that("as_survey_collection() three identical names repair to suffix 1, 2", {
  s <- .coll_surveys()
  s3 <- .coll_surveys3()
  expect_warning(
    coll <- as_survey_collection("y" = s$d1, "y" = s$d2, "y" = s3$d3),
    class = "surveycore_warning_collection_duplicate_name_repaired"
  )
  test_collection_invariants(coll)
  expect_identical(names(coll), c("y", "y_1", "y_2"))
})


# ── §7.1 Section 1 — ergonomics (length, names, [[) ─────────────────────────

test_that("length(coll) returns integer element count", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1, b = s$d2)
  expect_identical(length(coll), 2L)
})

test_that("names(coll) returns the survey names", {
  s <- .coll_surveys()
  coll <- as_survey_collection(wave1 = s$d1, wave2 = s$d2)
  expect_identical(names(coll), c("wave1", "wave2"))
})

test_that("coll[[name]] returns the underlying survey_base", {
  s <- .coll_surveys()
  coll <- as_survey_collection(wave1 = s$d1, wave2 = s$d2)
  w1 <- coll[["wave1"]]
  expect_true(S7::S7_inherits(w1, survey_base))
  expect_true(S7::S7_inherits(w1, survey_taylor))
})

test_that("coll[[integer]] returns the i-th survey", {
  s <- .coll_surveys()
  coll <- as_survey_collection(wave1 = s$d1, wave2 = s$d2)
  expect_true(S7::S7_inherits(coll[[1L]], survey_base))
  expect_identical(coll[[1L]], coll[["wave1"]])
})

test_that("coll[[unknown character]] returns NULL", {
  s <- .coll_surveys()
  coll <- as_survey_collection(wave1 = s$d1, wave2 = s$d2)
  expect_null(coll[["nonexistent"]])
})

test_that("coll[[out-of-bounds integer]] errors", {
  s <- .coll_surveys()
  coll <- as_survey_collection(wave1 = s$d1, wave2 = s$d2)
  expect_error(coll[[99L]])
})


# ── §7.1 Section 1 — add_survey() happy paths ──────────────────────────────

test_that("add_survey() appends with explicit name", {
  s <- .coll_surveys3()
  coll <- as_survey_collection(a = s$d1, b = s$d2)
  coll2 <- add_survey(coll, "new" = s$d3)
  test_collection_invariants(coll2)
  expect_identical(names(coll2), c("a", "b", "new"))
  # original unchanged
  expect_identical(names(coll), c("a", "b"))
})

test_that("add_survey() auto-names bare symbols", {
  s <- .coll_surveys3()
  d3 <- s$d3
  coll <- as_survey_collection(a = s$d1, b = s$d2)
  coll2 <- add_survey(coll, d3)
  test_collection_invariants(coll2)
  expect_identical(names(coll2), c("a", "b", "d3"))
})

test_that("add_survey() accepts multiple additions", {
  s <- .coll_surveys3()
  coll <- as_survey_collection(a = s$d1)
  coll2 <- add_survey(coll, x = s$d2, y = s$d3)
  test_collection_invariants(coll2)
  expect_identical(names(coll2), c("a", "x", "y"))
})

test_that("add_survey() with no new surveys returns the original", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1, b = s$d2)
  coll2 <- add_survey(coll)
  expect_identical(names(coll2), names(coll))
  expect_identical(length(coll2), length(coll))
})


# ── §7.1 Section 10 — add_survey() duplicate-name repair ────────────────────

test_that("add_survey() repairs new-vs-existing name collisions", {
  s <- .coll_surveys3()
  coll <- as_survey_collection(x = s$d1, y = s$d2)
  expect_warning(
    coll2 <- add_survey(coll, "x" = s$d3),
    class = "surveycore_warning_collection_duplicate_name_repaired"
  )
  test_collection_invariants(coll2)
  # Existing "x" is preserved; new "x" is suffixed to "x_1"
  expect_identical(names(coll2), c("x", "y", "x_1"))
})

test_that("add_survey() preserves existing names during repair", {
  s <- .coll_surveys3()
  coll <- as_survey_collection("wave" = s$d1)
  expect_warning(
    coll2 <- add_survey(coll, "wave" = s$d2, "wave" = s$d3),
    class = "surveycore_warning_collection_duplicate_name_repaired"
  )
  test_collection_invariants(coll2)
  expect_identical(names(coll2), c("wave", "wave_1", "wave_2"))
})


# ── §7.1 Section 1 — remove_survey() happy paths ────────────────────────────

test_that("remove_survey() drops a single named survey", {
  s <- .coll_surveys3()
  coll <- as_survey_collection(a = s$d1, b = s$d2, c = s$d3)
  coll2 <- remove_survey(coll, "b")
  test_collection_invariants(coll2)
  expect_identical(names(coll2), c("a", "c"))
})

test_that("remove_survey() drops multiple named surveys", {
  s <- .coll_surveys3()
  coll <- as_survey_collection(a = s$d1, b = s$d2, c = s$d3)
  coll2 <- remove_survey(coll, c("a", "c"))
  test_collection_invariants(coll2)
  expect_identical(names(coll2), "b")
})


# ── §7.1 Section 10 — remove_survey() errors ────────────────────────────────

test_that("remove_survey() errors on unknown name", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1, b = s$d2)
  expect_error(
    remove_survey(coll, "nope"),
    class = "surveycore_error_collection_name_not_found"
  )
  expect_snapshot(error = TRUE, remove_survey(coll, "nope"))
})

test_that("remove_survey() removing all surveys errors empty", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1, b = s$d2)
  expect_error(
    remove_survey(coll, names(coll)),
    class = "surveycore_error_collection_empty"
  )
})


# ── Type-check errors on mutators ──────────────────────────────────────────

test_that("add_survey() rejects non-survey_collection first arg", {
  s <- .coll_surveys()
  expect_error(
    add_survey(s$d1, new = s$d2),
    class = "surveycore_error_not_survey_collection"
  )
})

test_that("remove_survey() rejects non-survey_collection first arg", {
  s <- .coll_surveys()
  expect_error(
    remove_survey(s$d1, "a"),
    class = "surveycore_error_not_survey_collection"
  )
})

test_that("remove_survey() rejects integer name with surveycore_error_invalid_name_type", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1, b = s$d2)
  expect_error(
    remove_survey(coll, 1L),
    class = "surveycore_error_invalid_name_type"
  )
})

test_that("remove_survey() rejects logical name with surveycore_error_invalid_name_type", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1, b = s$d2)
  expect_error(
    remove_survey(coll, TRUE),
    class = "surveycore_error_invalid_name_type"
  )
})

test_that("remove_survey() rejects NULL name with surveycore_error_invalid_name_type", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1, b = s$d2)
  expect_error(
    remove_survey(coll, NULL),
    class = "surveycore_error_invalid_name_type"
  )
})

test_that("remove_survey() NA_character_ passes type check but errors on name not found", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1, b = s$d2)
  expect_error(
    remove_survey(coll, NA_character_),
    class = "surveycore_error_collection_name_not_found"
  )
})

test_that("remove_survey() error snapshot for invalid name type", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1, b = s$d2)
  expect_snapshot(error = TRUE, remove_survey(coll, 1L))
})

# Regression: non-survey_collection x still errors with not_survey_collection
test_that("remove_survey() rejects non-survey_collection first arg (regression)", {
  s <- .coll_surveys()
  expect_error(
    remove_survey(s$d1, "a"),
    class = "surveycore_error_not_survey_collection"
  )
})


# ── .resolve_caller_names() NULL names branch ──────────────────────────────

test_that(".resolve_caller_names() handles NULL names on quosures list", {
  q <- list(rlang::new_quosure(quote(x)))
  out <- .resolve_caller_names(q)
  expect_identical(out, "x")
})


# ── §C13/C15 — validator helpers ───────────────────────────────────────────

test_that(".validate_collection_id() returns invisible(value) on success", {
  r <- withVisible(.validate_collection_id("ok", "id"))
  expect_false(r$visible)
  expect_identical(r$value, "ok")
})

test_that(".validate_collection_id() rejects NA character", {
  expect_error(
    .validate_collection_id(NA_character_, "id"),
    class = "surveycore_error_collection_invalid_id"
  )
})

test_that(".validate_collection_id() rejects empty string", {
  expect_error(
    .validate_collection_id("", "id"),
    class = "surveycore_error_collection_invalid_id"
  )
})

test_that(".validate_collection_id() rejects length > 1", {
  expect_error(
    .validate_collection_id(c("a", "b"), "id"),
    class = "surveycore_error_collection_invalid_id"
  )
})

test_that(".validate_collection_id() rejects non-character (integer)", {
  expect_error(
    .validate_collection_id(1L, "id"),
    class = "surveycore_error_collection_invalid_id"
  )
})

test_that(".validate_collection_id() rejects NULL (length 0)", {
  expect_error(
    .validate_collection_id(NULL, "id"),
    class = "surveycore_error_collection_invalid_id"
  )
})

test_that(".validate_collection_if_missing_var() returns invisible(value)", {
  r <- withVisible(
    .validate_collection_if_missing_var("skip", "if_missing_var")
  )
  expect_false(r$visible)
  expect_identical(r$value, "skip")
})

test_that(".validate_collection_if_missing_var() accepts \"error\"", {
  expect_identical(
    .validate_collection_if_missing_var("error", "if_missing_var"),
    "error"
  )
})

test_that(".validate_collection_if_missing_var() rejects NA", {
  expect_error(
    .validate_collection_if_missing_var(NA_character_, "if_missing_var"),
    class = "surveycore_error_collection_invalid_if_missing_var"
  )
})

test_that(".validate_collection_if_missing_var() rejects \"warn\"", {
  expect_error(
    .validate_collection_if_missing_var("warn", "if_missing_var"),
    class = "surveycore_error_collection_invalid_if_missing_var"
  )
})

test_that(".validate_collection_if_missing_var() rejects length > 1", {
  expect_error(
    .validate_collection_if_missing_var(
      c("error", "skip"),
      "if_missing_var"
    ),
    class = "surveycore_error_collection_invalid_if_missing_var"
  )
})

test_that(".validate_collection_if_missing_var() rejects non-character", {
  expect_error(
    .validate_collection_if_missing_var(TRUE, "if_missing_var"),
    class = "surveycore_error_collection_invalid_if_missing_var"
  )
})


# ── §C13/C15 — S7 class property defaults ─────────────────────────────────

test_that("survey_collection() default @id is \".survey\"", {
  s <- .coll_surveys()
  coll <- survey_collection(surveys = list(a = s$d1))
  test_collection_invariants(coll)
  expect_identical(coll@id, ".survey")
})

test_that("survey_collection() default @if_missing_var is \"error\"", {
  s <- .coll_surveys()
  coll <- survey_collection(surveys = list(a = s$d1))
  test_collection_invariants(coll)
  expect_identical(coll@if_missing_var, "error")
})

test_that("survey_collection() round-trips non-default @id and @if_missing_var", {
  s <- .coll_surveys()
  coll <- survey_collection(
    surveys = list(a = s$d1),
    id = "wave",
    if_missing_var = "skip"
  )
  test_collection_invariants(coll)
  expect_identical(coll@id, "wave")
  expect_identical(coll@if_missing_var, "skip")
})


# ── §C13/C15 — S7 class validator ─────────────────────────────────────────

test_that("survey_collection() S7 validator rejects bad @id (NA)", {
  s <- .coll_surveys()
  expect_error(
    survey_collection(surveys = list(a = s$d1), id = NA_character_),
    class = "surveycore_error_collection_invalid_id"
  )
})

test_that("survey_collection() S7 validator rejects bad @id (empty)", {
  s <- .coll_surveys()
  expect_error(
    survey_collection(surveys = list(a = s$d1), id = ""),
    class = "surveycore_error_collection_invalid_id"
  )
})

test_that("survey_collection() S7 validator rejects bad @id (length>1)", {
  s <- .coll_surveys()
  expect_error(
    survey_collection(surveys = list(a = s$d1), id = c("a", "b")),
    class = "surveycore_error_collection_invalid_id"
  )
})

test_that("survey_collection() S7 validator rejects bad @if_missing_var (NA)", {
  s <- .coll_surveys()
  expect_error(
    survey_collection(
      surveys = list(a = s$d1),
      if_missing_var = NA_character_
    ),
    class = "surveycore_error_collection_invalid_if_missing_var"
  )
})

test_that("survey_collection() S7 validator rejects bad @if_missing_var (\"warn\")", {
  s <- .coll_surveys()
  expect_error(
    survey_collection(
      surveys = list(a = s$d1),
      if_missing_var = "warn"
    ),
    class = "surveycore_error_collection_invalid_if_missing_var"
  )
})

test_that("survey_collection() S7 validator: @id error fires before @if_missing_var", {
  # Ordering test: when both are simultaneously invalid, the @id error
  # has precedence (validator branches run id-first).
  s <- .coll_surveys()
  expect_error(
    survey_collection(
      surveys = list(a = s$d1),
      id = NA_character_,
      if_missing_var = "warn"
    ),
    class = "surveycore_error_collection_invalid_id"
  )
})


# ── §C13/C15 — as_survey_collection() happy paths ─────────────────────────

test_that("as_survey_collection() default .id is \".survey\"", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1, b = s$d2)
  expect_identical(coll@id, ".survey")
})

test_that("as_survey_collection() default .if_missing_var is \"error\"", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1, b = s$d2)
  expect_identical(coll@if_missing_var, "error")
})

test_that("as_survey_collection() stores explicit .id on @id", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1, b = s$d2, .id = "wave")
  expect_identical(coll@id, "wave")
})

test_that("as_survey_collection() stores explicit .if_missing_var", {
  s <- .coll_surveys()
  coll <- as_survey_collection(
    a = s$d1,
    b = s$d2,
    .if_missing_var = "skip"
  )
  expect_identical(coll@if_missing_var, "skip")
})

test_that("as_survey_collection() stores both .id and .if_missing_var", {
  s <- .coll_surveys()
  coll <- as_survey_collection(
    a = s$d1,
    b = s$d2,
    .id = "wave",
    .if_missing_var = "skip"
  )
  expect_identical(coll@id, "wave")
  expect_identical(coll@if_missing_var, "skip")
})

test_that("as_survey_collection() accepts mixed default + explicit", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1, b = s$d2, .id = "wave")
  expect_identical(coll@id, "wave")
  expect_identical(coll@if_missing_var, "error")
})


# ── §C13/C15 — as_survey_collection() error paths (Layer 3 dual pattern) ──

test_that("as_survey_collection() rejects .id = NA_character_", {
  s <- .coll_surveys()
  expect_error(
    as_survey_collection(a = s$d1, .id = NA_character_),
    class = "surveycore_error_collection_invalid_id"
  )
  expect_snapshot(
    error = TRUE,
    as_survey_collection(a = s$d1, .id = NA_character_)
  )
})

test_that("as_survey_collection() rejects .id = c(\"a\", \"b\")", {
  s <- .coll_surveys()
  expect_error(
    as_survey_collection(a = s$d1, .id = c("a", "b")),
    class = "surveycore_error_collection_invalid_id"
  )
})

test_that("as_survey_collection() rejects .id = \"\"", {
  s <- .coll_surveys()
  expect_error(
    as_survey_collection(a = s$d1, .id = ""),
    class = "surveycore_error_collection_invalid_id"
  )
})

test_that("as_survey_collection() rejects .if_missing_var = \"warn\"", {
  s <- .coll_surveys()
  expect_error(
    as_survey_collection(a = s$d1, .if_missing_var = "warn"),
    class = "surveycore_error_collection_invalid_if_missing_var"
  )
  expect_snapshot(
    error = TRUE,
    as_survey_collection(a = s$d1, .if_missing_var = "warn")
  )
})

test_that("as_survey_collection() rejects .if_missing_var = NA_character_", {
  s <- .coll_surveys()
  expect_error(
    as_survey_collection(
      a = s$d1,
      .if_missing_var = NA_character_
    ),
    class = "surveycore_error_collection_invalid_if_missing_var"
  )
})

test_that("as_survey_collection() does not silently ignore .on_missing", {
  # Spec: the silent no-op .on_missing argument has been removed. The
  # constructor surfaces R's positional-argument-mismatch path when the
  # caller still supplies the old name.
  s <- .coll_surveys()
  expect_error(
    as_survey_collection(a = s$d1, .on_missing = "skip")
  )
})


# ── §C15 — set_collection_id() ────────────────────────────────────────────

test_that("set_collection_id() updates @id and returns invisible(x)", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1)
  r <- withVisible(set_collection_id(coll, "wave"))
  expect_false(r$visible)
  expect_identical(r$value@id, "wave")
})

test_that("set_collection_id() preserves @surveys, @groups, @if_missing_var", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1, b = s$d2, .if_missing_var = "skip")
  coll2 <- set_collection_id(coll, "wave")
  expect_identical(names(coll2), names(coll))
  expect_identical(coll2@groups, coll@groups)
  expect_identical(coll2@if_missing_var, "skip")
})

test_that("set_collection_id() is idempotent", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1)
  coll2 <- set_collection_id(coll, coll@id)
  expect_identical(coll2@id, coll@id)
  expect_identical(names(coll2), names(coll))
})

test_that("set_collection_id() rejects non-survey_collection input", {
  s <- .coll_surveys()
  expect_error(
    set_collection_id(s$d1, "wave"),
    class = "surveycore_error_not_survey_collection"
  )
  expect_error(
    set_collection_id(list(), "wave"),
    class = "surveycore_error_not_survey_collection"
  )
})

test_that("set_collection_id() rejects bad id (NA)", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1)
  expect_error(
    set_collection_id(coll, NA_character_),
    class = "surveycore_error_collection_invalid_id"
  )
})

test_that("set_collection_id() rejects bad id (empty)", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1)
  expect_error(
    set_collection_id(coll, ""),
    class = "surveycore_error_collection_invalid_id"
  )
})

test_that("set_collection_id() rejects bad id (length > 1)", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1)
  expect_error(
    set_collection_id(coll, c("a", "b")),
    class = "surveycore_error_collection_invalid_id"
  )
})

test_that("set_collection_id() rejects bad id (integer)", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1)
  expect_error(
    set_collection_id(coll, 1L),
    class = "surveycore_error_collection_invalid_id"
  )
})


# ── §C15 — set_collection_if_missing_var() ────────────────────────────────

test_that("set_collection_if_missing_var() updates and returns invisible", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1)
  r <- withVisible(set_collection_if_missing_var(coll, "skip"))
  expect_false(r$visible)
  expect_identical(r$value@if_missing_var, "skip")
})

test_that("set_collection_if_missing_var() preserves other properties", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1, b = s$d2, .id = "wave")
  coll2 <- set_collection_if_missing_var(coll, "skip")
  expect_identical(names(coll2), names(coll))
  expect_identical(coll2@groups, coll@groups)
  expect_identical(coll2@id, "wave")
})

test_that("set_collection_if_missing_var() is idempotent", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1)
  coll2 <- set_collection_if_missing_var(coll, coll@if_missing_var)
  expect_identical(coll2@if_missing_var, coll@if_missing_var)
})

test_that("set_collection_if_missing_var() rejects non-collection input", {
  s <- .coll_surveys()
  expect_error(
    set_collection_if_missing_var(s$d1, "skip"),
    class = "surveycore_error_not_survey_collection"
  )
})

test_that("set_collection_if_missing_var() rejects bad value (NA)", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1)
  expect_error(
    set_collection_if_missing_var(coll, NA_character_),
    class = "surveycore_error_collection_invalid_if_missing_var"
  )
})

test_that("set_collection_if_missing_var() rejects bad value (\"warn\")", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1)
  expect_error(
    set_collection_if_missing_var(coll, "warn"),
    class = "surveycore_error_collection_invalid_if_missing_var"
  )
})

test_that("set_collection_if_missing_var() rejects length > 1", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1)
  expect_error(
    set_collection_if_missing_var(coll, c("error", "skip")),
    class = "surveycore_error_collection_invalid_if_missing_var"
  )
})

test_that("set_collection_if_missing_var() rejects non-character", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1)
  expect_error(
    set_collection_if_missing_var(coll, TRUE),
    class = "surveycore_error_collection_invalid_if_missing_var"
  )
})


# ── §C15 — preservation across mutators ───────────────────────────────────

test_that("add_survey() preserves @id and @if_missing_var (append one)", {
  s <- .coll_surveys3()
  coll <- as_survey_collection(
    a = s$d1,
    b = s$d2,
    .id = "wave",
    .if_missing_var = "skip"
  )
  coll2 <- add_survey(coll, c = s$d3)
  expect_identical(coll2@id, coll@id)
  expect_identical(coll2@if_missing_var, coll@if_missing_var)
})

test_that("add_survey() preserves @id and @if_missing_var (append zero)", {
  s <- .coll_surveys()
  coll <- as_survey_collection(
    a = s$d1,
    .id = "wave",
    .if_missing_var = "skip"
  )
  coll2 <- add_survey(coll)
  expect_identical(coll2@id, coll@id)
  expect_identical(coll2@if_missing_var, coll@if_missing_var)
})

test_that("add_survey() preserves @id and @if_missing_var (multiple)", {
  s <- .coll_surveys3()
  coll <- as_survey_collection(
    a = s$d1,
    .id = "wave",
    .if_missing_var = "skip"
  )
  coll2 <- add_survey(coll, b = s$d2, c = s$d3)
  expect_identical(coll2@id, "wave")
  expect_identical(coll2@if_missing_var, "skip")
})

test_that("add_survey() preserves @id under name-repair", {
  s <- .coll_surveys3()
  coll <- as_survey_collection(
    a = s$d1,
    .id = "wave",
    .if_missing_var = "skip"
  )
  expect_warning(
    coll2 <- add_survey(coll, "a" = s$d2),
    class = "surveycore_warning_collection_duplicate_name_repaired"
  )
  expect_identical(coll2@id, "wave")
  expect_identical(coll2@if_missing_var, "skip")
})

test_that("remove_survey() preserves @id and @if_missing_var (one)", {
  s <- .coll_surveys3()
  coll <- as_survey_collection(
    a = s$d1,
    b = s$d2,
    c = s$d3,
    .id = "wave",
    .if_missing_var = "skip"
  )
  coll2 <- remove_survey(coll, "b")
  expect_identical(coll2@id, "wave")
  expect_identical(coll2@if_missing_var, "skip")
})

test_that("remove_survey() preserves @id and @if_missing_var (multiple)", {
  s <- .coll_surveys3()
  coll <- as_survey_collection(
    a = s$d1,
    b = s$d2,
    c = s$d3,
    .id = "wave",
    .if_missing_var = "skip"
  )
  coll2 <- remove_survey(coll, c("a", "c"))
  expect_identical(coll2@id, "wave")
  expect_identical(coll2@if_missing_var, "skip")
})


# ── §D8 — print snapshots ─────────────────────────────────────────────────

test_that("print() renders id: and if_missing_var: at defaults", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1)
  expect_snapshot(print(coll))
})

test_that("print() renders id: and if_missing_var: at non-default values", {
  s <- .coll_surveys()
  coll <- as_survey_collection(
    "y2018" = s$d1,
    "y2020" = s$d2,
    .id = "wave",
    .if_missing_var = "skip"
  )
  expect_snapshot(print(coll))
})

test_that("print() renders id and if_missing_var lines exactly once on multi-member", {
  s <- .coll_surveys3()
  coll <- as_survey_collection(a = s$d1, b = s$d2, c = s$d3)
  expect_snapshot(print(coll))
})


# ── set_collection_id() then dispatch reflects new @id ────────────────────

test_that("set_collection_id() then get_means(.id = NULL) reflects new id", {
  s <- .coll_surveys()
  coll <- as_survey_collection(a = s$d1, b = s$d2)
  coll2 <- set_collection_id(coll, "wave")
  res <- get_means(coll2, y1, .id = NULL)
  expect_identical(names(res)[[1L]], "wave")
})

test_that("set_collection_if_missing_var(\"skip\") flips dispatch behavior", {
  # Two surveys; only w1 has the focal variable. Default if_missing_var is
  # "error", so plain dispatch errors. After setting "skip", dispatch
  # succeeds (with a skip-message) and returns just w1.
  surveys <- list(
    w1 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 1L)
      df$focal <- rnorm(nrow(df))
      as_survey(df, ids = psu, weights = wt, strata = strata)
    },
    w2 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 2L)
      as_survey(df, ids = psu, weights = wt, strata = strata)
    }
  )
  coll <- do.call(as_survey_collection, surveys)
  coll <- set_collection_if_missing_var(coll, "skip")
  result <- suppressMessages(
    .dispatch_over_collection(
      get_means,
      coll,
      x = focal,
      .if_missing_var = NULL
    )
  )
  expect_setequal(unique(result$.survey), "w1")
})
