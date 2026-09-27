# tests/testthat/test-collection-if-missing-var-rename.R
#
# PR 2 rename smoke tests: verify that every collection-dispatching get_*()
# accepts `.if_missing_var` as a named argument after the rename. Behavioural
# tests for the resolved value live in test-survey-collection-dispatch.R and
# the per-function analysis test files.
#
# Note: the corresponding "rejects old `.on_missing` name" assertion is not
# enforceable at the analysis-function layer. Every renamed function carries
# `...` in its signature (to make `.id` and `.if_missing_var` named-only), so
# any unrecognised argument flows through `...` into the dispatcher rather
# than triggering R's unused-argument error. The migration-audit grep on
# `tests/testthat/` (target: zero matches outside this file) is the
# authoritative mechanism for catching residual `.on_missing` references in
# tests.

# ── Shared fixtures ──────────────────────────────────────────────────────────

.make_rename_collection <- function(n_surveys = 2L, seed = 11L) {
  surveys <- lapply(seq_len(n_surveys), function(i) {
    df <- make_survey_data(
      n = 60L,
      n_psu = 6L,
      n_strata = 2L,
      design = "taylor",
      seed = seed + i
    )
    df$grp2 <- factor(sample(c("A", "B"), nrow(df), replace = TRUE))
    df$num <- runif(nrow(df), 1, 10)
    df$denom <- runif(nrow(df), 5, 20)
    df$age <- as.integer(runif(nrow(df), 18, 80))
    df$sex <- factor(sample(c("M", "F"), nrow(df), replace = TRUE))
    as_survey(df, ids = psu, weights = wt, strata = strata)
  })
  names(surveys) <- paste0("w", seq_len(n_surveys))
  do.call(as_survey_collection, surveys)
}

# ══════════════════════════════════════════════════════════════════════════════
# Per-function rename smoke tests
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_means() accepts .if_missing_var as a named argument", {
  coll <- .make_rename_collection()
  expect_no_error(get_means(coll, y1, .if_missing_var = "skip"))
})

test_that("get_totals() accepts .if_missing_var as a named argument", {
  coll <- .make_rename_collection()
  expect_no_error(get_totals(coll, y1, .if_missing_var = "skip"))
})

test_that("get_freqs() accepts .if_missing_var as a named argument", {
  coll <- .make_rename_collection()
  expect_no_error(get_freqs(coll, grp2, .if_missing_var = "skip"))
})

test_that("get_ratios() accepts .if_missing_var as a named argument", {
  coll <- .make_rename_collection()
  expect_no_error(get_ratios(coll, num, denom, .if_missing_var = "skip"))
})

test_that("get_diffs() accepts .if_missing_var as a named argument", {
  coll <- .make_rename_collection()
  expect_no_error(
    get_diffs(coll, y1, treats = sex, .if_missing_var = "skip")
  )
})

test_that("get_corr() accepts .if_missing_var as a named argument", {
  coll <- .make_rename_collection()
  expect_no_error(get_corr(coll, c(y1, y2), .if_missing_var = "skip"))
})

test_that("get_variance() accepts .if_missing_var as a named argument", {
  coll <- .make_rename_collection()
  expect_no_error(get_variance(coll, y1, .if_missing_var = "skip"))
})

test_that("get_quantiles() accepts .if_missing_var as a named argument", {
  coll <- .make_rename_collection()
  expect_no_error(get_quantiles(coll, y1, .if_missing_var = "skip"))
})

test_that("get_covariance() accepts .if_missing_var as a named argument", {
  coll <- .make_rename_collection()
  expect_no_error(get_covariance(coll, c(y1, y2), .if_missing_var = "skip"))
})

test_that("get_t_test() accepts .if_missing_var as a named argument", {
  coll <- .make_rename_collection()
  expect_no_error(
    get_t_test(coll, y1, by = sex, .if_missing_var = "skip")
  )
})

test_that("get_pairwise() accepts .if_missing_var as a named argument", {
  coll <- .make_rename_collection()
  expect_no_error(
    get_pairwise(coll, y1, by = sex, .if_missing_var = "skip")
  )
})
