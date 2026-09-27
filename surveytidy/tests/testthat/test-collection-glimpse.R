# tests/testthat/test-collection-glimpse.R
#
# glimpse.survey_collection — PR 3.
#
# Spec sections: §V.2 (default + .by_survey modes, id-collision pre-flight,
# `..surveycore_domain..` -> `.in_domain` display rename, type-coercion footer
# with D7 truncation at 5 cols + 80-char cap), §IX.3 (per-verb test
# categories).

# ── glimpse() default mode happy path ───────────────────────────────────────

test_that("glimpse.survey_collection default mode prints a single bound tibble", {
  coll <- make_test_collection(seed = 42)
  test_collection_invariants(coll)
  for (member in coll@surveys) {
    test_invariants(member)
  }

  out <- capture.output(result <- dplyr::glimpse(coll))

  # Output should mention combined row count = sum of per-member nrow.
  total_rows <- sum(vapply(
    coll@surveys,
    function(m) nrow(m@data),
    integer(1L)
  ))
  expect_true(any(grepl(paste0("Rows: ", total_rows), out)))

  # The prepended id column (default `.survey`) should appear as a glimpse
  # row.
  expect_true(any(grepl("\\.survey", out)))
})

test_that("glimpse.survey_collection returns invisible(x)", {
  coll <- make_test_collection(seed = 42)
  capture.output(wv <- withVisible(dplyr::glimpse(coll)))
  expect_false(wv$visible)
  expect_true(S7::S7_inherits(wv$value, surveycore::survey_collection))
})

test_that("glimpse.survey_collection respects user-set coll@id", {
  designs <- make_all_designs(seed = 42)
  coll <- surveycore::as_survey_collection(
    !!!designs,
    .id = "wave",
    .if_missing_var = "error"
  )
  out <- capture.output(dplyr::glimpse(coll))
  # Custom id "wave" should appear, default ".survey" should not.
  expect_true(any(grepl("\\$ wave", out)))
  expect_false(any(grepl("\\$ \\.survey", out)))
})

# ── glimpse() id-collision pre-flight ──────────────────────────────────────

# Build a 2-member collection where one member's @data already contains a
# column named `.survey` (the default coll@id). The pre-flight must raise
# BEFORE bind_rows would clobber the column.
.make_id_collision_collection <- function(seed = 42) {
  base <- make_survey_data(n = 80L, n_psu = 8L, n_strata = 2L, seed = seed)
  m1_data <- base
  m2_data <- base
  m2_data$.survey <- "preexisting"
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

test_that("glimpse.survey_collection raises on id-collision before binding", {
  coll <- .make_id_collision_collection(seed = 42)

  expect_error(
    dplyr::glimpse(coll),
    class = "surveytidy_error_collection_glimpse_id_collision"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::glimpse(coll)
  )
})

test_that("glimpse.survey_collection id-collision names the offending member", {
  coll <- .make_id_collision_collection(seed = 42)

  cnd <- tryCatch(
    dplyr::glimpse(coll),
    error = function(e) e
  )
  msg <- conditionMessage(cnd)
  # The error message should call out member m2 and column `.survey`.
  expect_true(grepl("m2", msg))
  expect_true(grepl("\\.survey", msg))
})

# ── glimpse() domain column rename for display ─────────────────────────────

test_that("glimpse.survey_collection renames domain column to .in_domain in display", {
  coll <- make_test_collection(seed = 42)
  # Pre-filter so domain column exists in every member's @data.
  coll_filtered <- dplyr::filter(coll, y1 > 60)

  # Sanity check: every member has the surveycore domain column.
  for (member in coll_filtered@surveys) {
    expect_true(surveycore::SURVEYCORE_DOMAIN_COL %in% names(member@data))
  }

  out <- capture.output(dplyr::glimpse(coll_filtered))
  # Display should show `.in_domain`, not the surveycore-internal column name.
  expect_true(any(grepl("\\.in_domain", out)))
  expect_false(any(grepl("\\.\\.surveycore_domain\\.\\.", out)))

  # Per-member @data is untouched: original column name is still there.
  for (member in coll_filtered@surveys) {
    expect_true(surveycore::SURVEYCORE_DOMAIN_COL %in% names(member@data))
    expect_false(".in_domain" %in% names(member@data))
  }
})

test_that("glimpse.survey_collection has no .in_domain when domain col absent", {
  coll <- make_test_collection(seed = 42)
  # Without filtering, no domain column exists.
  for (member in coll@surveys) {
    expect_false(surveycore::SURVEYCORE_DOMAIN_COL %in% names(member@data))
  }

  out <- capture.output(dplyr::glimpse(coll))
  expect_false(any(grepl("\\.in_domain", out)))
})

# ── glimpse() .by_survey = TRUE mode ───────────────────────────────────────

test_that("glimpse.survey_collection .by_survey = TRUE prints labelled blocks", {
  coll <- make_test_collection(seed = 42)

  out <- capture.output(dplyr::glimpse(coll, .by_survey = TRUE))

  # Output should contain a labelled header for each member.
  for (nm in names(coll@surveys)) {
    expect_true(any(grepl(nm, out)))
  }

  # No prepended `.survey` id column in this mode (there's no bind_rows).
  expect_false(any(grepl("^\\$ \\.survey", out)))
})

test_that("glimpse.survey_collection .by_survey = TRUE applies .in_domain rename per member", {
  coll <- make_test_collection(seed = 42)
  coll_filtered <- dplyr::filter(coll, y1 > 60)

  out <- capture.output(dplyr::glimpse(coll_filtered, .by_survey = TRUE))

  # Every member's render should show `.in_domain`, not the internal name.
  expect_true(any(grepl("\\.in_domain", out)))
  expect_false(any(grepl("\\.\\.surveycore_domain\\.\\.", out)))
})

test_that("glimpse.survey_collection .by_survey = TRUE returns invisible(x)", {
  coll <- make_test_collection(seed = 42)
  capture.output(
    wv <- withVisible(dplyr::glimpse(coll, .by_survey = TRUE))
  )
  expect_false(wv$visible)
  expect_true(S7::S7_inherits(wv$value, surveycore::survey_collection))
})

test_that("glimpse.survey_collection .by_survey = TRUE skips id-collision check", {
  # The collision check applies only to the default mode, since `.by_survey`
  # never calls bind_rows. A collection that would error in default mode
  # should glimpse cleanly under `.by_survey = TRUE`.
  coll <- .make_id_collision_collection(seed = 42)

  expect_no_error(
    capture.output(dplyr::glimpse(coll, .by_survey = TRUE))
  )
})

# ── glimpse() type-coercion footer ─────────────────────────────────────────

# Helper: build an N-member taylor collection where the user picks which
# column types to clash. Each member receives the same schema except for a
# named subset of columns, whose types are determined by `clash_spec`. Used
# by the no/1/6-conflict footer tests below.
.make_n_conflict_collection <- function(clash_spec, n_members = 2L, seed = 42) {
  set.seed(seed)
  base <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = seed)
  to_taylor <- function(df) {
    surveycore::as_survey(
      df,
      ids = psu,
      strata = strata,
      weights = wt,
      fpc = fpc
    )
  }

  members <- vector("list", n_members)
  for (i in seq_len(n_members)) {
    df <- base
    for (col in names(clash_spec)) {
      types <- clash_spec[[col]]
      type_for_member <- types[((i - 1L) %% length(types)) + 1L]
      df[[col]] <- switch(
        type_for_member,
        chr = rep(letters[i], nrow(df)),
        dbl = as.numeric(seq_len(nrow(df))),
        int = seq_len(nrow(df)),
        lgl = rep(c(TRUE, FALSE), length.out = nrow(df))
      )
    }
    members[[i]] <- to_taylor(df)
  }
  names(members) <- paste0("m", seq_len(n_members))

  surveycore::as_survey_collection(
    !!!members,
    .id = ".survey",
    .if_missing_var = "error"
  )
}

test_that("glimpse.survey_collection no type conflicts -> no footer", {
  coll <- make_test_collection(seed = 42)

  out <- capture.output(dplyr::glimpse(coll))
  # Footer keyword must be absent.
  expect_false(any(grepl("conflicting types", out)))
  expect_false(any(grepl("coerced to", out)))
})

test_that("glimpse.survey_collection one conflict -> footer with one row", {
  coll <- .make_n_conflict_collection(
    clash_spec = list(y1 = c("chr", "dbl")),
    n_members = 2L,
    seed = 42
  )

  out <- capture.output(suppressWarnings(dplyr::glimpse(coll)))
  expect_true(any(grepl("conflicting types", out)))
  expect_true(any(grepl("y1", out) & grepl("coerced to", out)))
  # No truncation marker.
  expect_false(any(grepl("more conflicting columns", out)))
})

test_that("glimpse.survey_collection six conflicts -> footer truncated at 5 + summary", {
  clash_spec <- list(
    y1 = c("chr", "dbl"),
    y2 = c("chr", "dbl"),
    y3 = c("chr", "dbl"),
    extra1 = c("chr", "dbl"),
    extra2 = c("chr", "dbl"),
    extra3 = c("chr", "dbl")
  )
  coll <- .make_n_conflict_collection(
    clash_spec = clash_spec,
    n_members = 2L,
    seed = 42
  )

  out <- capture.output(suppressWarnings(dplyr::glimpse(coll)))
  expect_true(any(grepl("conflicting types", out)))
  # First five conflicts shown.
  expect_true(any(grepl("y1.*coerced", out)))
  expect_true(any(grepl("y2.*coerced", out)))
  expect_true(any(grepl("y3.*coerced", out)))
  expect_true(any(grepl("extra1.*coerced", out)))
  expect_true(any(grepl("extra2.*coerced", out)))
  # Sixth conflict suppressed; `+ 1 more conflicting column(s)` summary present.
  expect_false(any(grepl("extra3.*coerced", out)))
  expect_true(any(grepl("\\+ 1 more conflicting column", out)))
})

test_that("glimpse.survey_collection footer line width capped at 80 chars", {
  clash_spec <- list(y1 = c("chr", "dbl"))
  coll <- .make_n_conflict_collection(
    clash_spec = clash_spec,
    n_members = 2L,
    seed = 42
  )

  out <- capture.output(suppressWarnings(dplyr::glimpse(coll)))
  # Find the row that mentions y1 + coerced.
  line <- out[grepl("y1", out) & grepl("coerced", out)]
  expect_true(length(line) >= 1L)
  expect_true(all(nchar(line) <= 80L))
})

test_that("glimpse.survey_collection footer truncates over-wide lines with ellipsis", {
  # Build a collection with many members so the per-class member list grows
  # past the 80-char cap and forces the truncation branch (line 282 in
  # collection-pull-glimpse.R).
  clash_spec <- list(y1 = c("chr", "dbl"))
  coll <- .make_n_conflict_collection(
    clash_spec = clash_spec,
    n_members = 8L,
    seed = 42
  )

  out <- capture.output(suppressWarnings(dplyr::glimpse(coll)))
  line <- out[grepl("y1", out) & grepl("coerced|\\.\\.\\.$", out)]
  expect_true(length(line) >= 1L)
  expect_true(all(nchar(line) <= 80L))
  # Truncation marker (trailing ellipsis) appears.
  expect_true(any(grepl("\\.\\.\\.$", line)))
})
