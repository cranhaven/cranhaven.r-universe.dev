# tests/testthat/test-collection-dispatch.R
#
# Unit tests for the internal collection-verb dispatcher and its
# supporting test infrastructure (make_test_collection,
# make_heterogeneous_collection, test_collection_invariants).
#
# Spec sections: §II.3.1 (dispatcher), §IX.2 (helpers), §IX.4 (dispatcher
# tests). Five typed conditions exercised here:
#   - surveytidy_pre_check_missing_var (internal sentinel)
#   - surveytidy_message_collection_skipped_surveys
#   - surveytidy_error_collection_verb_failed
#   - surveytidy_error_collection_verb_emptied
#   - simpleError from the .may_change_groups regression assertion

# ── helper smoke: make_test_collection ───────────────────────────────────────

test_that("make_test_collection() returns a 3-member collection with defaults", {
  coll <- make_test_collection(seed = 42)
  test_collection_invariants(coll)

  expect_length(coll@surveys, 3L)
  expect_identical(coll@id, ".survey")
  expect_identical(coll@if_missing_var, "error")
  expect_identical(coll@groups, character(0))
})

test_that("make_test_collection() mixes all three design subclasses", {
  coll <- make_test_collection(seed = 42)
  classes <- vapply(
    coll@surveys,
    function(s) class(s)[[1L]],
    character(1L)
  )
  expect_setequal(
    classes,
    c(
      "surveycore::survey_taylor",
      "surveycore::survey_replicate",
      "surveycore::survey_twophase"
    )
  )
})

# ── helper smoke: make_heterogeneous_collection ──────────────────────────────

test_that("make_heterogeneous_collection() honours its schema contract", {
  coll <- make_heterogeneous_collection(seed = 42)
  test_collection_invariants(coll)
  for (member in coll@surveys) {
    test_invariants(member)
  }

  expect_length(coll@surveys, 3L)
  expect_identical(names(coll@surveys), c("m1", "m2", "m3"))

  for (member in coll@surveys) {
    expect_true(S7::S7_inherits(member, surveycore::survey_taylor))
  }

  m1_cols <- names(coll@surveys$m1@data)
  m2_cols <- names(coll@surveys$m2@data)
  m3_cols <- names(coll@surveys$m3@data)

  expect_true(all(c("y1", "y2", "y3") %in% m1_cols))
  expect_false(any(c("y2", "y3") %in% m2_cols))
  expect_true("y1" %in% m1_cols)
  expect_false("y1" %in% m3_cols)
  expect_true("region" %in% m3_cols)

  shared <- Reduce(intersect, list(m1_cols, m2_cols, m3_cols))
  expect_true(all(c("psu", "strata", "fpc", "wt", "group") %in% shared))
})

# ── dispatcher: trivial pass-through ─────────────────────────────────────────

test_that(".dispatch_verb_over_collection ungroups via none-detection mode", {
  coll <- make_test_collection(seed = 42)
  out <- .dispatch_verb_over_collection(
    fn = dplyr::ungroup,
    verb_name = "ungroup",
    collection = coll,
    .detect_missing = "none",
    .may_change_groups = TRUE
  )
  test_collection_invariants(out)
  expect_identical(out@groups, character(0))
  expect_identical(names(out@surveys), names(coll@surveys))
  expect_identical(out@id, coll@id)
  expect_identical(out@if_missing_var, coll@if_missing_var)
})

test_that(".dispatch_verb_over_collection preserves names and order", {
  coll <- make_test_collection(seed = 42)
  out <- .dispatch_verb_over_collection(
    fn = dplyr::filter,
    verb_name = "filter",
    collection = coll,
    y1 > 0,
    .detect_missing = "pre_check",
    .may_change_groups = FALSE
  )
  test_collection_invariants(out)
  expect_identical(names(out@surveys), names(coll@surveys))
})

# ── dispatcher: empty-result error message wording ───────────────────────────

test_that("empty-result error reports stored .if_missing_var source", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll <- surveycore::set_collection_if_missing_var(coll, "skip")
  expect_snapshot(
    error = TRUE,
    .dispatch_verb_over_collection(
      fn = dplyr::filter,
      verb_name = "filter",
      collection = coll,
      ghost_col_xyz > 0,
      .detect_missing = "pre_check",
      .may_change_groups = FALSE
    )
  )
})

test_that("empty-result error reports per-call .if_missing_var source", {
  coll <- make_heterogeneous_collection(seed = 42)
  expect_snapshot(
    error = TRUE,
    .dispatch_verb_over_collection(
      fn = dplyr::filter,
      verb_name = "filter",
      collection = coll,
      ghost_col_xyz > 0,
      .if_missing_var = "skip",
      .detect_missing = "pre_check",
      .may_change_groups = FALSE
    )
  )
})

test_that("empty-result error has typed class", {
  coll <- make_heterogeneous_collection(seed = 42)
  expect_error(
    .dispatch_verb_over_collection(
      fn = dplyr::filter,
      verb_name = "filter",
      collection = coll,
      ghost_col_xyz > 0,
      .if_missing_var = "skip",
      .detect_missing = "pre_check",
      .may_change_groups = FALSE
    ),
    class = "surveytidy_error_collection_verb_emptied"
  )
})

# ── dispatcher: pre-check env-filter substeps (D1, §IX.4) ────────────────────

test_that("pre-check passes locally-bound constants through to per-survey calls", {
  coll <- make_test_collection(seed = 42)
  threshold <- 0
  out <- .dispatch_verb_over_collection(
    fn = dplyr::filter,
    verb_name = "filter",
    collection = coll,
    y1 > threshold,
    .detect_missing = "pre_check",
    .may_change_groups = FALSE
  )
  test_collection_invariants(out)
})

test_that("pre-check passes .data and .env pronouns through", {
  coll <- make_test_collection(seed = 42)
  threshold <- 0
  out <- .dispatch_verb_over_collection(
    fn = dplyr::filter,
    verb_name = "filter",
    collection = coll,
    .data$y1 > .env$threshold,
    .detect_missing = "pre_check",
    .may_change_groups = FALSE
  )
  test_collection_invariants(out)
})

test_that("pre-check accepts column references resolved by @data", {
  coll <- make_test_collection(seed = 42)
  out <- .dispatch_verb_over_collection(
    fn = dplyr::filter,
    verb_name = "filter",
    collection = coll,
    y1 > 0,
    .detect_missing = "pre_check",
    .may_change_groups = FALSE
  )
  test_collection_invariants(out)
})

test_that("pre-check flags truly missing names via the typed sentinel", {
  coll <- make_test_collection(seed = 42)
  err <- tryCatch(
    .dispatch_verb_over_collection(
      fn = dplyr::filter,
      verb_name = "filter",
      collection = coll,
      ghost_col_xyz > 0,
      .if_missing_var = "error",
      .detect_missing = "pre_check",
      .may_change_groups = FALSE
    ),
    error = function(e) e
  )
  expect_s3_class(err, "surveytidy_error_collection_verb_failed")
  expect_s3_class(err$parent, "surveytidy_pre_check_missing_var")
  expect_identical(err$parent$missing_vars, "ghost_col_xyz")
  # Sentinel class chain: explicitly excludes "rlang_error" (D1 / Issue 3).
  expect_false(inherits(err$parent, "rlang_error"))
})

test_that("global-env constants resolve via the quosure's enclosing env", {
  coll <- make_test_collection(seed = 42)
  do_filter <- function() {
    cap <- 0
    .dispatch_verb_over_collection(
      fn = dplyr::filter,
      verb_name = "filter",
      collection = coll,
      y1 > cap,
      .detect_missing = "pre_check",
      .may_change_groups = FALSE
    )
  }
  out <- do_filter()
  test_collection_invariants(out)
})

# ── dispatcher: re-raise carries parent chain ────────────────────────────────

test_that("re-raise under .if_missing_var = 'error' preserves the parent chain", {
  coll <- make_heterogeneous_collection(seed = 42)
  err <- tryCatch(
    .dispatch_verb_over_collection(
      fn = dplyr::filter,
      verb_name = "filter",
      collection = coll,
      y1 > 0,
      .if_missing_var = "error",
      .detect_missing = "pre_check",
      .may_change_groups = FALSE
    ),
    error = function(e) e
  )
  expect_s3_class(err, "surveytidy_error_collection_verb_failed")
  expect_s3_class(err$parent, "surveytidy_pre_check_missing_var")
  # Manually walk the parent chain — rlang has no exported chain accessor.
  chain <- list(err)
  cur <- err
  while (!is.null(cur$parent)) {
    cur <- cur$parent
    chain <- c(chain, list(cur))
  }
  expect_gte(length(chain), 2L)
})

# ── dispatcher: skipped-surveys typed message ────────────────────────────────

test_that("skip path emits the typed message naming every skipped survey", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll <- surveycore::set_collection_if_missing_var(coll, "skip")
  expect_snapshot(
    out <- .dispatch_verb_over_collection(
      fn = dplyr::filter,
      verb_name = "filter",
      collection = coll,
      y1 > 0,
      .detect_missing = "pre_check",
      .may_change_groups = FALSE
    )
  )
  test_collection_invariants(out)
  expect_identical(names(out@surveys), c("m1", "m2"))
})

test_that("skip-path message has typed class", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll <- surveycore::set_collection_if_missing_var(coll, "skip")
  expect_message(
    out <- .dispatch_verb_over_collection(
      fn = dplyr::filter,
      verb_name = "filter",
      collection = coll,
      y1 > 0,
      .detect_missing = "pre_check",
      .may_change_groups = FALSE
    ),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  test_collection_invariants(out)
})

# ── dispatcher: class-catch path ─────────────────────────────────────────────

test_that("class-catch handles vctrs_error_subscript_oob", {
  coll <- make_heterogeneous_collection(seed = 42)
  err <- tryCatch(
    .dispatch_verb_over_collection(
      fn = dplyr::select,
      verb_name = "select",
      collection = coll,
      "y1",
      .if_missing_var = "error",
      .detect_missing = "class_catch",
      .may_change_groups = FALSE
    ),
    error = function(e) e
  )
  expect_s3_class(err, "surveytidy_error_collection_verb_failed")
})

test_that("class-catch handles all_of() wrap (parent-walk one level)", {
  coll <- make_heterogeneous_collection(seed = 42)
  out <- .dispatch_verb_over_collection(
    fn = dplyr::select,
    verb_name = "select",
    collection = coll,
    tidyselect::all_of("y1"),
    .if_missing_var = "skip",
    .detect_missing = "class_catch",
    .may_change_groups = FALSE
  )
  test_collection_invariants(out)
  # m3 is missing y1 — it is the only one skipped.
  expect_identical(names(out@surveys), c("m1", "m2"))
})

test_that("class-catch handles rlang_error_data_pronoun_not_found", {
  coll <- make_heterogeneous_collection(seed = 42)
  err <- tryCatch(
    .dispatch_verb_over_collection(
      fn = dplyr::filter,
      verb_name = "filter",
      collection = coll,
      .data$ghost_col > 0,
      .if_missing_var = "error",
      .detect_missing = "class_catch",
      .may_change_groups = FALSE
    ),
    error = function(e) e
  )
  expect_s3_class(err, "surveytidy_error_collection_verb_failed")
})

# ── dispatcher: precedence ───────────────────────────────────────────────────

test_that("per-call .if_missing_var beats stored property (skip overrides error)", {
  coll <- make_heterogeneous_collection(seed = 42) # stored: "error"
  out <- .dispatch_verb_over_collection(
    fn = dplyr::filter,
    verb_name = "filter",
    collection = coll,
    y1 > 0,
    .if_missing_var = "skip",
    .detect_missing = "pre_check",
    .may_change_groups = FALSE
  ) |>
    suppressMessages()
  test_collection_invariants(out)
  expect_identical(names(out@surveys), c("m1", "m2"))
})

test_that("per-call .if_missing_var beats stored property (error overrides skip)", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll <- surveycore::set_collection_if_missing_var(coll, "skip")
  expect_error(
    .dispatch_verb_over_collection(
      fn = dplyr::filter,
      verb_name = "filter",
      collection = coll,
      y1 > 0,
      .if_missing_var = "error",
      .detect_missing = "pre_check",
      .may_change_groups = FALSE
    ),
    class = "surveytidy_error_collection_verb_failed"
  )
})

# ── dispatcher: @groups regression catch (.may_change_groups = FALSE) ────────

test_that(".may_change_groups = FALSE catches a per-member @groups mutation", {
  coll <- make_test_collection(seed = 42)
  bad_fn <- function(x, ...) {
    # Bypass the per-survey rename validator by writing the underlying
    # @groups attribute directly. This synthesises a malformed transition
    # — the regression catch should fire as a simpleError.
    attr(x, "groups") <- "group"
    x@data$group <- as.character(x@data$group)
    S7::validate(x)
    x
  }
  expect_error(
    .dispatch_verb_over_collection(
      fn = bad_fn,
      verb_name = "bad_fn",
      collection = coll,
      .detect_missing = "none",
      .may_change_groups = FALSE
    ),
    class = "simpleError"
  )
})

# ── dispatcher: separation of concerns ───────────────────────────────────────

test_that("dispatcher does not delegate to surveycore::.dispatch_over_collection", {
  # Inspect the deparsed function body (works under both devtools::test() and
  # R CMD check, where the source file isn't available at a fixed path).
  body_src <- paste(
    deparse(body(.dispatch_verb_over_collection)),
    collapse = "\n"
  )
  expect_false(grepl(".dispatch_over_collection", body_src, fixed = TRUE))
})


# ── dispatcher: pre-check edge cases (coverage) ──────────────────────────────

test_that("pre-check skips quosures whose only symbols are .data / .env", {
  coll <- make_test_collection(seed = 42)
  noop <- function(survey, ...) survey
  # Expression `.data` alone: all.vars returns c(".data") → empty after setdiff.
  out <- .dispatch_verb_over_collection(
    fn = noop,
    verb_name = "noop",
    collection = coll,
    .data,
    .detect_missing = "pre_check",
    .may_change_groups = FALSE
  )
  test_collection_invariants(out)
})

test_that("pre-check skips quosures whose symbols are all env-resolvable", {
  coll <- make_test_collection(seed = 42)
  noop <- function(survey, ...) survey
  some_const <- 0
  # `some_const` resolves in the quosure env → empty after env-resolve filter.
  out <- .dispatch_verb_over_collection(
    fn = noop,
    verb_name = "noop",
    collection = coll,
    some_const,
    .detect_missing = "pre_check",
    .may_change_groups = FALSE
  )
  test_collection_invariants(out)
})


# ── dispatcher: empty result without skip path (coverage) ────────────────────

test_that("empty-result error reports 'all surveys produced empty results' under .if_missing_var = 'error'", {
  coll <- make_test_collection(seed = 42)
  always_null <- function(survey, ...) NULL
  expect_error(
    suppressMessages(
      .dispatch_verb_over_collection(
        fn = always_null,
        verb_name = "noop",
        collection = coll,
        .if_missing_var = "error",
        .detect_missing = "none",
        .may_change_groups = FALSE
      )
    ),
    class = "surveytidy_error_collection_verb_emptied"
  )
})


# ── dispatcher: class-catch direct + fallthrough (coverage) ──────────────────

test_that("class-catch handles direct rlang_error_data_pronoun_not_found (no wrap)", {
  coll <- make_test_collection(seed = 42)
  bad_fn <- function(survey, ...) {
    rlang::abort(
      "data pronoun not found",
      class = "rlang_error_data_pronoun_not_found"
    )
  }
  err <- tryCatch(
    .dispatch_verb_over_collection(
      fn = bad_fn,
      verb_name = "bad",
      collection = coll,
      .if_missing_var = "error",
      .detect_missing = "class_catch",
      .may_change_groups = FALSE
    ),
    error = function(e) e
  )
  expect_s3_class(err, "surveytidy_error_collection_verb_failed")
})

test_that("class-catch re-raises unrecognized rlang_error (no parent match)", {
  coll <- make_test_collection(seed = 42)
  bad_fn <- function(survey, ...) {
    rlang::abort("vanilla rlang error")
  }
  err <- tryCatch(
    .dispatch_verb_over_collection(
      fn = bad_fn,
      verb_name = "bad",
      collection = coll,
      .if_missing_var = "error",
      .detect_missing = "class_catch",
      .may_change_groups = FALSE
    ),
    error = function(e) e
  )
  expect_s3_class(err, "rlang_error")
  # Not wrapped as the dispatcher's typed error — re-raised as-is.
  expect_false(inherits(err, "surveytidy_error_collection_verb_failed"))
})
