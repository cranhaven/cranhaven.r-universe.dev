# tests/testthat/test-collection-slice.R
#
# slice family for survey_collection — PR 2d.
#
# Spec sections: §IV.6 (slice contract — pre-flight, per-survey seeding),
# §II.3.3 (.derive_member_seed), §IX.3 (per-verb test categories).

# ── .check_slice_zero() — slice variant ──────────────────────────────────────

test_that(".check_slice_zero('slice') raises on literal integer(0)", {
  expect_error(
    .check_slice_zero("slice", rlang::quos(integer(0))),
    class = "surveytidy_error_collection_slice_zero"
  )
})

test_that(".check_slice_zero('slice') silently passes for NSE refs", {
  # NSE that references a column or n() must NOT pre-evaluate (per spec).
  # The eval fails in the empty environment → silently skip the pre-flight.
  expect_silent(.check_slice_zero("slice", rlang::quos(seq_len(n()))))
  expect_silent(.check_slice_zero("slice", rlang::quos(which(y1 > 0))))
})

test_that(".check_slice_zero('slice') passes for non-empty literal", {
  expect_silent(.check_slice_zero("slice", rlang::quos(1:5)))
  expect_silent(.check_slice_zero("slice", rlang::quos(-1L)))
})

# ── .check_slice_zero() — slice_head / slice_tail variants ───────────────────

test_that(".check_slice_zero('slice_head') raises on n = 0", {
  expect_error(
    .check_slice_zero("slice_head", n = 0L),
    class = "surveytidy_error_collection_slice_zero"
  )
})

test_that(".check_slice_zero('slice_head') raises on prop = 0", {
  expect_error(
    .check_slice_zero("slice_head", prop = 0),
    class = "surveytidy_error_collection_slice_zero"
  )
})

test_that(".check_slice_zero('slice_tail') raises on n = 0", {
  expect_error(
    .check_slice_zero("slice_tail", n = 0L),
    class = "surveytidy_error_collection_slice_zero"
  )
})

test_that(".check_slice_zero('slice_head') passes when n > 0", {
  expect_silent(.check_slice_zero("slice_head", n = 5L))
  expect_silent(.check_slice_zero("slice_head", prop = 0.1))
})

# ── .check_slice_zero() — slice_min / slice_max variants ─────────────────────

test_that(".check_slice_zero('slice_min') raises on n = 0", {
  expect_error(
    .check_slice_zero("slice_min", n = 0L),
    class = "surveytidy_error_collection_slice_zero"
  )
})

test_that(".check_slice_zero('slice_max') raises on prop = 0", {
  expect_error(
    .check_slice_zero("slice_max", prop = 0),
    class = "surveytidy_error_collection_slice_zero"
  )
})

# ── .check_slice_zero() — slice_sample variant ───────────────────────────────

test_that(".check_slice_zero('slice_sample') raises on n = 0", {
  expect_error(
    .check_slice_zero("slice_sample", n = 0L),
    class = "surveytidy_error_collection_slice_zero"
  )
})

test_that(".check_slice_zero('slice_sample') raises on prop = 0", {
  expect_error(
    .check_slice_zero("slice_sample", prop = 0),
    class = "surveytidy_error_collection_slice_zero"
  )
})

# ── .derive_member_seed() ────────────────────────────────────────────────────

test_that(".derive_member_seed() is deterministic for fixed inputs", {
  s1 <- .derive_member_seed("m1", 42L)
  s2 <- .derive_member_seed("m1", 42L)
  expect_identical(s1, s2)
})

test_that(".derive_member_seed() differs by survey name", {
  s1 <- .derive_member_seed("m1", 42L)
  s2 <- .derive_member_seed("m2", 42L)
  expect_false(identical(s1, s2))
})

test_that(".derive_member_seed() differs by user seed", {
  s1 <- .derive_member_seed("m1", 42L)
  s2 <- .derive_member_seed("m1", 99L)
  expect_false(identical(s1, s2))
})

test_that(".derive_member_seed() returns an integer in [0, 2^28)", {
  s <- .derive_member_seed("m1", 42L)
  expect_type(s, "integer")
  expect_gte(s, 0L)
  expect_lt(s, 2L^28L)
})


# ── slice.survey_collection ──────────────────────────────────────────────────

test_that("slice.survey_collection slices every member (cross-design)", {
  coll <- make_test_collection(seed = 42)
  test_collection_invariants(coll)
  for (member in coll@surveys) {
    test_invariants(member)
  }

  out <- suppressWarnings(dplyr::slice(coll, 1:3))

  test_collection_invariants(out)
  for (member in out@surveys) {
    test_invariants(member)
    expect_identical(nrow(member@data), 3L)
  }
  expect_identical(names(out@surveys), names(coll@surveys))
  expect_identical(out@id, coll@id)
  expect_identical(out@if_missing_var, coll@if_missing_var)
})

test_that("slice.survey_collection raises slice_zero on integer(0) BEFORE dispatch", {
  coll <- make_test_collection(seed = 42)
  surveys_ref <- coll@surveys

  expect_error(
    dplyr::slice(coll, integer(0)),
    class = "surveytidy_error_collection_slice_zero"
  )
  # Pre-flight runs before dispatch — collection is unchanged.
  expect_identical(coll@surveys, surveys_ref)

  expect_snapshot(
    error = TRUE,
    dplyr::slice(coll, integer(0))
  )
})

test_that("slice.survey_collection has no .if_missing_var argument", {
  fn <- get("slice.survey_collection", envir = asNamespace("surveytidy"))
  expect_false(".if_missing_var" %in% names(formals(fn)))
})


# ── slice_head / slice_tail.survey_collection ────────────────────────────────

test_that("slice_head.survey_collection takes first n rows of every member", {
  coll <- make_test_collection(seed = 42)

  out <- suppressWarnings(dplyr::slice_head(coll, n = 5L))
  test_collection_invariants(out)
  for (i in seq_along(out@surveys)) {
    member_in <- coll@surveys[[i]]
    member_out <- out@surveys[[i]]
    test_invariants(member_out)
    expect_identical(nrow(member_out@data), 5L)
    expect_identical(member_out@data$y1, head(member_in@data$y1, 5L))
  }
})

test_that("slice_tail.survey_collection takes last n rows of every member", {
  coll <- make_test_collection(seed = 42)

  out <- suppressWarnings(dplyr::slice_tail(coll, n = 5L))
  test_collection_invariants(out)
  for (i in seq_along(out@surveys)) {
    member_in <- coll@surveys[[i]]
    member_out <- out@surveys[[i]]
    expect_identical(nrow(member_out@data), 5L)
    expect_identical(member_out@data$y1, tail(member_in@data$y1, 5L))
  }
})

test_that("slice_head.survey_collection raises slice_zero on n = 0 BEFORE dispatch", {
  coll <- make_test_collection(seed = 42)
  surveys_ref <- coll@surveys

  expect_error(
    dplyr::slice_head(coll, n = 0L),
    class = "surveytidy_error_collection_slice_zero"
  )
  expect_identical(coll@surveys, surveys_ref)
})

test_that("slice_tail.survey_collection raises slice_zero on prop = 0 BEFORE dispatch", {
  coll <- make_test_collection(seed = 42)

  expect_error(
    dplyr::slice_tail(coll, prop = 0),
    class = "surveytidy_error_collection_slice_zero"
  )
})

test_that("slice_head/slice_tail.survey_collection have no .if_missing_var argument", {
  fn_h <- get("slice_head.survey_collection", envir = asNamespace("surveytidy"))
  fn_t <- get("slice_tail.survey_collection", envir = asNamespace("surveytidy"))
  expect_false(".if_missing_var" %in% names(formals(fn_h)))
  expect_false(".if_missing_var" %in% names(formals(fn_t)))
})


# ── slice_min / slice_max.survey_collection ──────────────────────────────────

test_that("slice_min.survey_collection takes smallest n by order_by", {
  coll <- make_test_collection(seed = 42)

  out <- suppressWarnings(dplyr::slice_min(coll, order_by = y1, n = 3L))
  test_collection_invariants(out)
  for (member in out@surveys) {
    expect_lte(nrow(member@data), 3L * 2L) # with_ties default → maybe more
  }
})

test_that("slice_max.survey_collection takes largest n by order_by", {
  coll <- make_test_collection(seed = 42)

  out <- suppressWarnings(
    dplyr::slice_max(coll, order_by = y1, n = 3L, with_ties = FALSE)
  )
  test_collection_invariants(out)
  for (member in out@surveys) {
    expect_identical(nrow(member@data), 3L)
  }
})

test_that("slice_min.survey_collection rejects by argument", {
  coll <- make_test_collection(seed = 42)

  expect_error(
    suppressWarnings(dplyr::slice_min(coll, order_by = y1, n = 3L, by = group)),
    class = "surveytidy_error_collection_by_unsupported"
  )
})

test_that("slice_max.survey_collection rejects by argument", {
  coll <- make_test_collection(seed = 42)

  expect_error(
    suppressWarnings(dplyr::slice_max(coll, order_by = y1, n = 3L, by = group)),
    class = "surveytidy_error_collection_by_unsupported"
  )
})

test_that("slice_min.survey_collection raises slice_zero on n = 0", {
  coll <- make_test_collection(seed = 42)

  expect_error(
    dplyr::slice_min(coll, order_by = y1, n = 0L),
    class = "surveytidy_error_collection_slice_zero"
  )
})

test_that("slice_min.survey_collection .if_missing_var = 'error' raises typed", {
  coll <- make_heterogeneous_collection(seed = 42)

  expect_error(
    suppressWarnings(dplyr::slice_min(coll, order_by = region, n = 1L)),
    class = "surveytidy_error_collection_verb_failed"
  )
})

test_that("slice_min.survey_collection .if_missing_var = 'skip' drops bad members", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  expect_message(
    out <- suppressWarnings(dplyr::slice_min(coll_skip, order_by = y3, n = 1L)),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  # m2 is missing y3 → skipped
  expect_identical(names(out@surveys), c("m1", "m3"))
})


# ── slice_sample.survey_collection ───────────────────────────────────────────

test_that("slice_sample.survey_collection samples every member", {
  coll <- make_test_collection(seed = 42)

  out <- suppressWarnings(dplyr::slice_sample(coll, n = 3L))
  test_collection_invariants(out)
  for (member in out@surveys) {
    expect_identical(nrow(member@data), 3L)
  }
})

test_that("slice_sample.survey_collection rejects by argument", {
  coll <- make_test_collection(seed = 42)

  expect_error(
    suppressWarnings(dplyr::slice_sample(coll, n = 3L, by = group)),
    class = "surveytidy_error_collection_by_unsupported"
  )
})

test_that("slice_sample.survey_collection raises slice_zero on n = 0", {
  coll <- make_test_collection(seed = 42)

  expect_error(
    dplyr::slice_sample(coll, n = 0L),
    class = "surveytidy_error_collection_slice_zero"
  )
})

test_that("slice_sample.survey_collection seed = NULL: ambient RNG (set.seed reproducible)", {
  coll <- make_test_collection(seed = 42)

  set.seed(123L)
  out1 <- suppressWarnings(dplyr::slice_sample(coll, n = 3L))
  set.seed(123L)
  out2 <- suppressWarnings(dplyr::slice_sample(coll, n = 3L))

  for (i in seq_along(out1@surveys)) {
    expect_identical(out1@surveys[[i]]@data, out2@surveys[[i]]@data)
  }
})

test_that("slice_sample.survey_collection seed = int: deterministic AND order-independent", {
  coll <- make_test_collection(seed = 42)

  out1 <- suppressWarnings(dplyr::slice_sample(coll, n = 3L, seed = 99L))
  out2 <- suppressWarnings(dplyr::slice_sample(coll, n = 3L, seed = 99L))
  # Same seed → identical results
  for (i in seq_along(out1@surveys)) {
    expect_identical(out1@surveys[[i]]@data, out2@surveys[[i]]@data)
  }

  # Reorder collection — per-survey results should still match by name
  reordered_surveys <- coll@surveys[rev(names(coll@surveys))]
  coll_rev <- surveycore::as_survey_collection(
    !!!reordered_surveys,
    .id = coll@id,
    .if_missing_var = coll@if_missing_var
  )
  out_rev <- suppressWarnings(dplyr::slice_sample(coll_rev, n = 3L, seed = 99L))

  for (nm in names(out1@surveys)) {
    expect_identical(out1@surveys[[nm]]@data, out_rev@surveys[[nm]]@data)
  }
})

test_that("slice_sample.survey_collection seed = int restores ambient .Random.seed", {
  coll <- make_test_collection(seed = 42)

  set.seed(123L)
  before_seed <- .Random.seed
  suppressWarnings(dplyr::slice_sample(coll, n = 3L, seed = 99L))
  after_seed <- .Random.seed
  expect_identical(before_seed, after_seed)
})

test_that("slice_sample.survey_collection seed = NULL + weight_by missing + skip drops bad members", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  expect_message(
    out <- suppressWarnings(
      dplyr::slice_sample(coll_skip, n = 1L, weight_by = y3)
    ),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  expect_identical(names(out@surveys), c("m1", "m3"))
})

test_that("slice_sample.survey_collection seed = int cleans up when no ambient .Random.seed exists", {
  coll <- make_test_collection(seed = 42)

  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    saved_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit(
      assign(".Random.seed", saved_seed, envir = .GlobalEnv),
      add = TRUE
    )
    rm(".Random.seed", envir = .GlobalEnv)
  }

  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  suppressWarnings(dplyr::slice_sample(coll, n = 3L, seed = 99L))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("slice_sample.survey_collection seed = int + weight_by missing raises typed error", {
  coll <- make_heterogeneous_collection(seed = 42)

  expect_error(
    suppressWarnings(
      dplyr::slice_sample(coll, n = 1L, weight_by = y3, seed = 99L)
    ),
    class = "surveytidy_error_collection_verb_failed"
  )
})

test_that("slice_sample.survey_collection seed = int + weight_by missing + skip drops bad members", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  expect_message(
    out <- suppressWarnings(
      dplyr::slice_sample(coll_skip, n = 1L, weight_by = y3, seed = 99L)
    ),
    class = "surveytidy_message_collection_skipped_surveys"
  )
  # m2 is missing y3 → skipped
  expect_identical(names(out@surveys), c("m1", "m3"))
})

test_that("slice_sample.survey_collection seed = int + skip empties collection raises typed (stored property)", {
  coll <- make_heterogeneous_collection(seed = 42)
  coll_skip <- surveycore::set_collection_if_missing_var(coll, "skip")

  expect_error(
    suppressWarnings(suppressMessages(
      dplyr::slice_sample(
        coll_skip,
        n = 1L,
        weight_by = nonexistent_col,
        seed = 99L
      )
    )),
    class = "surveytidy_error_collection_verb_emptied"
  )
})

test_that("slice_sample.survey_collection seed = int + .if_missing_var='skip' empties collection raises typed (per-call override)", {
  coll <- make_heterogeneous_collection(seed = 42)

  expect_error(
    suppressWarnings(suppressMessages(
      dplyr::slice_sample(
        coll,
        n = 1L,
        weight_by = nonexistent_col,
        seed = 99L,
        .if_missing_var = "skip"
      )
    )),
    class = "surveytidy_error_collection_verb_emptied"
  )
})


# ── per-member physical-subset warning multiplicity ──────────────────────────

test_that("slice.survey_collection fires per-member physical-subset warning", {
  coll <- make_test_collection(seed = 42)

  warning_count <- 0L
  withCallingHandlers(
    dplyr::slice(coll, 1:3),
    surveycore_warning_physical_subset = function(cnd) {
      warning_count <<- warning_count + 1L
      rlang::cnd_muffle(cnd)
    }
  )
  expect_identical(warning_count, length(coll@surveys))
})

test_that("slice_head.survey_collection fires per-member physical-subset warning", {
  coll <- make_test_collection(seed = 42)

  warning_count <- 0L
  withCallingHandlers(
    dplyr::slice_head(coll, n = 3L),
    surveycore_warning_physical_subset = function(cnd) {
      warning_count <<- warning_count + 1L
      rlang::cnd_muffle(cnd)
    }
  )
  expect_identical(warning_count, length(coll@surveys))
})

test_that("slice_min.survey_collection fires per-member physical-subset warning", {
  coll <- make_test_collection(seed = 42)

  warning_count <- 0L
  withCallingHandlers(
    dplyr::slice_min(coll, order_by = y1, n = 3L),
    surveycore_warning_physical_subset = function(cnd) {
      warning_count <<- warning_count + 1L
      rlang::cnd_muffle(cnd)
    }
  )
  expect_identical(warning_count, length(coll@surveys))
})

test_that("slice_sample.survey_collection fires per-member physical-subset warning", {
  coll <- make_test_collection(seed = 42)

  warning_count <- 0L
  withCallingHandlers(
    dplyr::slice_sample(coll, n = 3L, seed = 99L),
    surveycore_warning_physical_subset = function(cnd) {
      warning_count <<- warning_count + 1L
      rlang::cnd_muffle(cnd)
    }
  )
  expect_identical(warning_count, length(coll@surveys))
})


# ── domain preservation ──────────────────────────────────────────────────────

test_that("slice_head.survey_collection preserves domain column on every member", {
  coll <- make_test_collection(seed = 42)
  coll_filtered <- dplyr::filter(coll, y1 > 50)

  out <- suppressWarnings(dplyr::slice_head(coll_filtered, n = 3L))
  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL

  for (member in out@surveys) {
    expect_true(domain_col %in% names(member@data))
  }
})


# ── visible_vars preservation ────────────────────────────────────────────────

test_that("slice variants preserve visible_vars on every member", {
  coll <- make_test_collection(seed = 42)
  for (i in seq_along(coll@surveys)) {
    m <- coll@surveys[[i]]
    new_vars <- m@variables
    new_vars$visible_vars <- c("y1", "y2")
    attr(m, "variables") <- new_vars
    S7::validate(m)
    coll@surveys[[i]] <- m
  }

  variants <- list(
    function(c) suppressWarnings(dplyr::slice(c, 1:3)),
    function(c) suppressWarnings(dplyr::slice_head(c, n = 3L)),
    function(c) suppressWarnings(dplyr::slice_tail(c, n = 3L)),
    function(c) suppressWarnings(dplyr::slice_min(c, order_by = y1, n = 3L)),
    function(c) suppressWarnings(dplyr::slice_max(c, order_by = y1, n = 3L)),
    function(c) suppressWarnings(dplyr::slice_sample(c, n = 3L, seed = 99L))
  )

  for (variant in variants) {
    out <- variant(coll)
    for (member in out@surveys) {
      expect_identical(member@variables$visible_vars, c("y1", "y2"))
    }
  }
})
