# tests/testthat/test-survey-collection-dispatch.R
#
# Dispatch tests for survey_collection: per-function happy paths, oracle
# tests (dispatch identity + NSE forwarding), .meta carry-over, .id/.if_missing_var
# behaviour, and the seven PR-2 error/warning classes (C5, C6, C7, C9, C10,
# C11, C12, C13, C14).
#
# Depends on: survey_collection class (PR 1), .dispatch_over_collection() and
# .warn_on_meta_divergence() helpers (PR 2), and per-function dispatch
# branches added to each get_*() in PR 2.

# ── Shared fixtures ──────────────────────────────────────────────────────────

.make_dispatch_collection <- function(n_surveys = 3L, seed = 42L) {
  surveys <- lapply(seq_len(n_surveys), function(i) {
    df <- make_survey_data(
      n = 120L,
      n_psu = 12L,
      n_strata = 3L,
      design = "taylor",
      seed = seed + i
    )
    df$grp2 <- factor(sample(c("A", "B"), nrow(df), replace = TRUE))
    df$outcome <- rnorm(
      nrow(df),
      mean = ifelse(df$grp2 == "A", 50, 55),
      sd = 10
    )
    df$num <- runif(nrow(df), 1, 10)
    df$denom <- runif(nrow(df), 5, 20)
    df$treat <- factor(sample(c("ctrl", "t1", "t2"), nrow(df), replace = TRUE))
    df$age <- as.integer(runif(nrow(df), 18, 80))
    df$sex <- factor(sample(c("M", "F"), nrow(df), replace = TRUE))
    as_survey(df, ids = psu, weights = wt, strata = strata)
  })
  names(surveys) <- paste0("w", seq_len(n_surveys))
  do.call(as_survey_collection, surveys)
}

# ══════════════════════════════════════════════════════════════════════════════
# Section 4 — Per-function happy paths
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_freqs() dispatches over a 3-survey collection", {
  coll <- .make_dispatch_collection()
  result <- get_freqs(coll, grp2)

  expect_s3_class(result, "tbl_df")
  expect_identical(names(result)[1], ".survey")
  expect_true(all(result$.survey %in% names(coll)))
  expect_setequal(unique(result$.survey), names(coll))
})

test_that("get_means() dispatches over a 3-survey collection", {
  coll <- .make_dispatch_collection()
  result <- get_means(coll, y1)

  expect_s3_class(result, "tbl_df")
  expect_identical(names(result)[1], ".survey")
  expect_setequal(unique(result$.survey), names(coll))
})

test_that("get_totals() dispatches over a 3-survey collection", {
  coll <- .make_dispatch_collection()
  result <- get_totals(coll, y1)

  expect_s3_class(result, "tbl_df")
  expect_identical(names(result)[1], ".survey")
  expect_setequal(unique(result$.survey), names(coll))
})

test_that("get_quantiles() dispatches over a 3-survey collection", {
  coll <- .make_dispatch_collection()
  result <- get_quantiles(coll, y1, probs = c(0.25, 0.5, 0.75))

  expect_s3_class(result, "tbl_df")
  expect_identical(names(result)[1], ".survey")
  expect_setequal(unique(result$.survey), names(coll))
})

test_that("get_ratios() dispatches over a 3-survey collection", {
  coll <- .make_dispatch_collection()
  result <- get_ratios(coll, num, denom)

  expect_s3_class(result, "tbl_df")
  expect_identical(names(result)[1], ".survey")
  expect_setequal(unique(result$.survey), names(coll))
})

test_that("get_corr() dispatches over a 3-survey collection", {
  coll <- .make_dispatch_collection()
  result <- get_corr(coll, c(y1, y2))

  expect_s3_class(result, "tbl_df")
  expect_identical(names(result)[1], ".survey")
  expect_setequal(unique(result$.survey), names(coll))
})

test_that("get_diffs() dispatches over a 3-survey collection", {
  coll <- .make_dispatch_collection()
  result <- get_diffs(coll, outcome, treats = treat)

  expect_s3_class(result, "tbl_df")
  expect_identical(names(result)[1], ".survey")
  expect_setequal(unique(result$.survey), names(coll))
})

test_that("get_t_test() dispatches over a 3-survey collection", {
  coll <- .make_dispatch_collection()
  result <- get_t_test(coll, outcome, by = grp2)

  expect_s3_class(result, "tbl_df")
  expect_identical(names(result)[1], ".survey")
  expect_setequal(unique(result$.survey), names(coll))
})

test_that("get_pairwise() dispatches over a 3-survey collection", {
  coll <- .make_dispatch_collection()
  result <- get_pairwise(coll, outcome, by = treat)

  expect_s3_class(result, "tbl_df")
  expect_identical(names(result)[1], ".survey")
  expect_setequal(unique(result$.survey), names(coll))
})

# ══════════════════════════════════════════════════════════════════════════════
# Section 4 — Custom .id
# ══════════════════════════════════════════════════════════════════════════════

test_that("custom .id renames the identifier column", {
  coll <- .make_dispatch_collection()
  result <- get_freqs(coll, grp2, .id = "wave")

  expect_identical(names(result)[1], "wave")
  expect_false(".survey" %in% names(result))
  expect_setequal(unique(result$wave), names(coll))
})

# ══════════════════════════════════════════════════════════════════════════════
# Section 4 — Single-design passthrough ignores .id / .if_missing_var
# ══════════════════════════════════════════════════════════════════════════════

test_that("single-design call silently ignores .id and .if_missing_var", {
  coll <- .make_dispatch_collection(n_surveys = 1L)
  d <- coll[["w1"]]

  plain <- get_means(d, y1)
  with_ignored <- get_means(d, y1, .id = "ignored", .if_missing_var = "skip")

  expect_identical(dim(plain), dim(with_ignored))
  expect_identical(names(plain), names(with_ignored))
})

# ══════════════════════════════════════════════════════════════════════════════
# Section 4 — .meta carry-over
# ══════════════════════════════════════════════════════════════════════════════

test_that(".meta$collection$surveys records contributing survey names", {
  coll <- .make_dispatch_collection()
  result <- get_means(coll, y1)
  meta <- attr(result, ".meta")

  expect_identical(meta$collection$surveys, names(coll))
})

test_that(".meta$collection$survey_classes is a named character vector", {
  coll <- .make_dispatch_collection()
  result <- get_means(coll, y1)
  meta <- attr(result, ".meta")

  expect_type(meta$collection$survey_classes, "character")
  expect_identical(names(meta$collection$survey_classes), names(coll))
})

test_that(".meta$per_survey preserves each survey's original .meta", {
  coll <- .make_dispatch_collection()
  result <- get_means(coll, y1)
  meta <- attr(result, ".meta")

  expect_identical(names(meta$per_survey), names(coll))
  per_survey_y1 <- get_means(coll[["w1"]], y1)
  # $call differs between dispatched and direct invocations; exclude it.
  actual <- meta$per_survey$w1
  expected <- attr(per_survey_y1, ".meta")
  actual$call <- NULL
  expected$call <- NULL
  expect_identical(actual, expected)
})

test_that("top-level .meta equals first survey's .meta for back-compat", {
  coll <- .make_dispatch_collection()
  result <- get_means(coll, y1)
  meta <- attr(result, ".meta")

  first_survey_meta <- attr(get_means(coll[["w1"]], y1), ".meta")
  # $call legitimately differs (dispatched call vs direct call); skip it.
  top_level_keys <- setdiff(names(meta), c("per_survey", "collection", "call"))
  for (k in top_level_keys) {
    expect_identical(meta[[k]], first_survey_meta[[k]])
  }
})

# ══════════════════════════════════════════════════════════════════════════════
# Section 4 — Dispatch-identity oracle (numerical regression)
# ══════════════════════════════════════════════════════════════════════════════

.oracle_dispatch <- function(fn, coll, ...) {
  per <- lapply(names(coll), function(nm) fn(coll[[nm]], ...))
  names(per) <- names(coll)
  dplyr::bind_rows(per, .id = ".survey")
}

test_that("get_means() dispatch matches per-survey bind oracle", {
  coll <- .make_dispatch_collection()
  got <- get_means(coll, y1)
  want <- .oracle_dispatch(get_means, coll, y1)

  numeric_cols <- intersect(
    names(got)[vapply(got, is.numeric, logical(1L))],
    names(want)
  )
  for (col in numeric_cols) {
    expect_equal(got[[col]], want[[col]], tolerance = 1e-12)
  }
})

test_that("get_totals() dispatch matches per-survey bind oracle", {
  coll <- .make_dispatch_collection()
  got <- get_totals(coll, y1)
  want <- .oracle_dispatch(get_totals, coll, y1)

  numeric_cols <- intersect(
    names(got)[vapply(got, is.numeric, logical(1L))],
    names(want)
  )
  for (col in numeric_cols) {
    expect_equal(got[[col]], want[[col]], tolerance = 1e-12)
  }
})

test_that("get_freqs() dispatch matches per-survey bind oracle", {
  coll <- .make_dispatch_collection()
  got <- get_freqs(coll, grp2)
  want <- .oracle_dispatch(get_freqs, coll, grp2)

  numeric_cols <- intersect(
    names(got)[vapply(got, is.numeric, logical(1L))],
    names(want)
  )
  for (col in numeric_cols) {
    expect_equal(got[[col]], want[[col]], tolerance = 1e-12)
  }
})

# ══════════════════════════════════════════════════════════════════════════════
# Section 4 — NSE-arg forwarding oracle (regression for dropped {{ }})
# ══════════════════════════════════════════════════════════════════════════════

test_that("get_means() forwards `group` via {{ }}", {
  coll <- .make_dispatch_collection()
  got <- get_means(coll, y1, group = grp2)
  want <- .oracle_dispatch(get_means, coll, y1, group = grp2)

  expect_identical(nrow(got), nrow(want))
  expect_true("grp2" %in% names(got))
})

test_that("get_ratios() forwards `numerator`, `denominator`, `group`", {
  coll <- .make_dispatch_collection()
  got <- get_ratios(coll, num, denom, group = grp2)
  want <- .oracle_dispatch(get_ratios, coll, num, denom, group = grp2)

  expect_identical(nrow(got), nrow(want))
  expect_true("grp2" %in% names(got))
})

test_that("get_diffs() forwards all four NSE args", {
  coll <- .make_dispatch_collection()
  # Note: `covariates` is a character vector in get_diffs(), not a tidy-select.
  got <- get_diffs(
    coll,
    outcome,
    treats = treat,
    group = grp2,
    covariates = c("age", "sex")
  )
  want <- .oracle_dispatch(
    get_diffs,
    coll,
    outcome,
    treats = treat,
    group = grp2,
    covariates = c("age", "sex")
  )

  expect_identical(nrow(got), nrow(want))
})

test_that("get_t_test() forwards `by`", {
  coll <- .make_dispatch_collection()
  got <- get_t_test(coll, outcome, by = grp2)
  want <- .oracle_dispatch(get_t_test, coll, outcome, by = grp2)

  expect_identical(nrow(got), nrow(want))
})

test_that("get_pairwise() forwards `by`", {
  coll <- .make_dispatch_collection()
  got <- get_pairwise(coll, outcome, by = treat)
  want <- .oracle_dispatch(get_pairwise, coll, outcome, by = treat)

  expect_identical(nrow(got), nrow(want))
})

# ══════════════════════════════════════════════════════════════════════════════
# Section 5 — Error paths (dual pattern: class= + snapshot for representative)
# ══════════════════════════════════════════════════════════════════════════════

test_that("C5: .if_missing_var = 'error' aborts when one survey lacks the variable", {
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

  expect_error(
    get_means(coll, focal, .if_missing_var = "error"),
    class = "surveycore_error_collection_missing_var"
  )
  expect_snapshot(
    error = TRUE,
    get_means(coll, focal, .if_missing_var = "error")
  )
})

test_that("C6: .if_missing_var = 'skip' with all surveys missing aborts", {
  surveys <- list(
    w1 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 1L)
      as_survey(df, ids = psu, weights = wt, strata = strata)
    },
    w2 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 2L)
      as_survey(df, ids = psu, weights = wt, strata = strata)
    }
  )
  coll <- do.call(as_survey_collection, surveys)

  expect_error(
    get_means(coll, focal, .if_missing_var = "skip"),
    class = "surveycore_error_collection_all_skipped"
  )
  expect_snapshot(
    error = TRUE,
    get_means(coll, focal, .if_missing_var = "skip")
  )
})

test_that("C7: .id collision with an existing result column aborts", {
  coll <- .make_dispatch_collection()

  expect_error(
    get_means(coll, y1, .id = "mean"),
    class = "surveycore_error_collection_id_collision"
  )
  expect_snapshot(
    error = TRUE,
    get_means(coll, y1, .id = "mean")
  )
})

test_that("C12: survey_glm() refuses survey_collection input", {
  coll <- .make_dispatch_collection()

  expect_error(
    survey_glm(coll, y1 ~ y2),
    class = "surveycore_error_collection_not_supported_by_fn"
  )
  expect_snapshot(error = TRUE, survey_glm(coll, y1 ~ y2))
})

test_that("C12: get_anova() refuses survey_collection input", {
  coll <- .make_dispatch_collection()

  expect_error(
    get_anova(coll, y1 ~ grp2),
    class = "surveycore_error_collection_not_supported_by_fn"
  )
  expect_snapshot(error = TRUE, get_anova(coll, y1 ~ grp2))
})

test_that("C13: .id rejects empty, NA, non-char, wrong length", {
  # NOTE: as of PR `feature/collection-id-if-missing-var-class`, `.id = NULL`
  # is a sentinel for "use the stored property" rather than an invalid value.
  # The other four invalid shapes still error.
  coll <- .make_dispatch_collection()

  expect_error(
    get_means(coll, y1, .id = ""),
    class = "surveycore_error_collection_invalid_id"
  )
  expect_error(
    get_means(coll, y1, .id = NA_character_),
    class = "surveycore_error_collection_invalid_id"
  )
  expect_error(
    get_means(coll, y1, .id = c("a", "b")),
    class = "surveycore_error_collection_invalid_id"
  )
  expect_error(
    get_means(coll, y1, .id = 1L),
    class = "surveycore_error_collection_invalid_id"
  )
  expect_snapshot(error = TRUE, get_means(coll, y1, .id = NA_character_))
})

test_that("C14: bind-type mismatch across surveys aborts with surveycore class", {
  surveys <- list(
    w1 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 1L)
      df$grp <- sample(1:2, nrow(df), replace = TRUE)
      as_survey(df, ids = psu, weights = wt, strata = strata)
    },
    w2 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 2L)
      df$grp <- factor(sample(c("a", "b"), nrow(df), replace = TRUE))
      as_survey(df, ids = psu, weights = wt, strata = strata)
    }
  )
  coll <- do.call(as_survey_collection, surveys)

  expect_error(
    get_means(coll, y1, group = grp),
    class = "surveycore_error_collection_bind_type_mismatch"
  )
  expect_snapshot(error = TRUE, get_means(coll, y1, group = grp))
})

test_that("C10: tidy-selected variable absent raises surveycore_error_variable_not_found", {
  d <- {
    df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 1L)
    as_survey(df, ids = psu, weights = wt, strata = strata)
  }

  expect_error(
    get_means(d, nonexistent_variable),
    class = "surveycore_error_variable_not_found"
  )
  expect_snapshot(error = TRUE, get_means(d, nonexistent_variable))

  expect_error(
    get_freqs(d, nonexistent_variable),
    class = "surveycore_error_variable_not_found"
  )
  expect_error(
    get_totals(d, nonexistent_variable),
    class = "surveycore_error_variable_not_found"
  )
  expect_error(
    get_quantiles(d, nonexistent_variable),
    class = "surveycore_error_variable_not_found"
  )
  expect_error(
    get_ratios(d, num_missing, denom_missing),
    class = "surveycore_error_variable_not_found"
  )
  expect_error(
    get_corr(d, c(y1, nonexistent_variable)),
    class = "surveycore_error_variable_not_found"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# Section — NSE-arg pre-check scope covers every NSE arg (not just focal)
# ══════════════════════════════════════════════════════════════════════════════

test_that(".if_missing_var = 'skip' drops survey missing a grouping var", {
  surveys <- list(
    w1 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 1L)
      df$grp <- factor(sample(c("A", "B"), nrow(df), replace = TRUE))
      as_survey(df, ids = psu, weights = wt, strata = strata)
    },
    w2 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 2L)
      as_survey(df, ids = psu, weights = wt, strata = strata)
    }
  )
  coll <- do.call(as_survey_collection, surveys)

  result <- suppressMessages(
    get_means(coll, y1, group = grp, .if_missing_var = "skip")
  )
  expect_setequal(unique(result$.survey), "w1")
  meta <- attr(result, ".meta")
  expect_identical(meta$collection$surveys, "w1")
  expect_false("w2" %in% names(meta$per_survey))
})

# ══════════════════════════════════════════════════════════════════════════════
# Section — Length-1 collection edge
# ══════════════════════════════════════════════════════════════════════════════

test_that("length-1 collection dispatches", {
  coll <- .make_dispatch_collection(n_surveys = 1L)
  result <- get_means(coll, y1)
  direct <- get_means(coll[["w1"]], y1)

  expect_identical(nrow(result), nrow(direct))
  expect_identical(unique(result$.survey), "w1")
})

test_that("length-1 + .if_missing_var = 'skip' + missing var → all_skipped", {
  df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 1L)
  d <- as_survey(df, ids = psu, weights = wt, strata = strata)
  coll <- as_survey_collection(only = d)

  expect_error(
    get_means(coll, absent, .if_missing_var = "skip"),
    class = "surveycore_error_collection_all_skipped"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# Section — Messages (skipped surveys info message)
# ══════════════════════════════════════════════════════════════════════════════

test_that("C9: .if_missing_var = 'skip' emits skipped-surveys message", {
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

  expect_message(
    get_means(coll, focal, .if_missing_var = "skip"),
    class = "surveycore_message_collection_skipped_surveys"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# Section — Meta divergence (C11)
# ══════════════════════════════════════════════════════════════════════════════

test_that("C11: diverging value_labels across surveys emits divergence warning", {
  surveys <- list(
    w1 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 1L)
      df$g <- 1L
      attr(df$g, "labels") <- c("Low" = 1L, "High" = 2L)
      as_survey(df, ids = psu, weights = wt, strata = strata)
    },
    w2 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 2L)
      df$g <- 1L
      attr(df$g, "labels") <- c("Alpha" = 1L, "Beta" = 2L)
      as_survey(df, ids = psu, weights = wt, strata = strata)
    }
  )
  coll <- do.call(as_survey_collection, surveys)

  expect_warning(
    get_means(coll, g),
    class = "surveycore_warning_collection_meta_divergence"
  )
  expect_snapshot({
    res <- get_means(coll, g)
    res
  })
})

test_that("C11: identical meta across surveys → no divergence warning", {
  coll <- .make_dispatch_collection()
  expect_warning(
    result <- get_means(coll, y1),
    class = "surveycore_warning_collection_meta_divergence",
    regexp = NA
  )
})

test_that("C11: absence vs presence of meta key counts as divergence", {
  surveys <- list(
    w1 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 1L)
      attr(df$y1, "label") <- "Outcome 1"
      as_survey(df, ids = psu, weights = wt, strata = strata)
    },
    w2 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 2L)
      as_survey(df, ids = psu, weights = wt, strata = strata)
    }
  )
  coll <- do.call(as_survey_collection, surveys)

  expect_warning(
    get_means(coll, y1),
    class = "surveycore_warning_collection_meta_divergence"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# Section — Heterogeneous schemas
# ══════════════════════════════════════════════════════════════════════════════

test_that("heterogeneous schemas bind_rows with NA-fills on extras", {
  surveys <- list(
    w1 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 1L)
      df$extra <- rnorm(nrow(df))
      df$shared <- factor(sample(c("a", "b"), nrow(df), replace = TRUE))
      as_survey(df, ids = psu, weights = wt, strata = strata)
    },
    w2 = {
      df <- make_survey_data(n = 60L, n_psu = 6L, n_strata = 2L, seed = 2L)
      df$shared <- factor(sample(c("a", "b"), nrow(df), replace = TRUE))
      as_survey(df, ids = psu, weights = wt, strata = strata)
    }
  )
  coll <- do.call(as_survey_collection, surveys)

  result <- get_freqs(coll, shared)
  expect_s3_class(result, "tbl_df")
  expect_setequal(unique(result$.survey), c("w1", "w2"))
})


# ══════════════════════════════════════════════════════════════════════════════
# Section — Two-tier precedence resolution (.id and .if_missing_var)
# ══════════════════════════════════════════════════════════════════════════════
#
# Eight tests covering the four .id and four .if_missing_var scenarios:
# (default-stored vs non-default-stored) × (no call-arg vs call-arg).

# ─ .id precedence ─────────────────────────────────────────────────────────

test_that(".id precedence: default stored, no call-arg → .survey", {
  coll <- .make_dispatch_collection(n_surveys = 2L)
  # Default stored .id is ".survey"; pass .id = NULL explicitly to invoke
  # the dispatcher's stored-property fallback under the PR-1 bridge.
  result <- get_means(coll, y1, .id = NULL)
  expect_identical(names(result)[[1L]], ".survey")
})

test_that(".id precedence: default stored, call-arg overrides", {
  coll <- .make_dispatch_collection(n_surveys = 2L)
  result <- get_means(coll, y1, .id = "year")
  expect_identical(names(result)[[1L]], "year")
})

test_that(".id precedence: non-default stored, no call-arg → stored", {
  surveys <- lapply(seq_len(2L), function(i) {
    df <- make_survey_data(
      n = 60L,
      n_psu = 6L,
      n_strata = 2L,
      seed = i
    )
    as_survey(df, ids = psu, weights = wt, strata = strata)
  })
  names(surveys) <- c("w1", "w2")
  coll <- do.call(
    as_survey_collection,
    c(surveys, list(.id = "wave"))
  )
  # PR-1 bridge: pass .id = NULL explicitly to invoke stored fallback.
  result <- get_means(coll, y1, .id = NULL)
  expect_identical(names(result)[[1L]], "wave")
})

test_that(".id precedence: non-default stored, call-arg overrides", {
  surveys <- lapply(seq_len(2L), function(i) {
    df <- make_survey_data(
      n = 60L,
      n_psu = 6L,
      n_strata = 2L,
      seed = i
    )
    as_survey(df, ids = psu, weights = wt, strata = strata)
  })
  names(surveys) <- c("w1", "w2")
  coll <- do.call(
    as_survey_collection,
    c(surveys, list(.id = "wave"))
  )
  result <- get_means(coll, y1, .id = "year")
  expect_identical(names(result)[[1L]], "year")
})


# ─ .if_missing_var precedence ─────────────────────────────────────────────
#
# Note: in PR 1, the analysis function signatures still expose `.if_missing_var`
# (renamed to `.if_missing_var` inside the dispatcher only). The bridge
# `.if_missing_var = .if_missing_var` at the inner call site forwards the old
# argument's value into the new dispatcher parameter. To exercise the
# dispatcher's two-tier precedence here without colliding with the
# bridge, we either (a) call through `.if_missing_var` (which forwards into
# `.if_missing_var` via the bridge) or (b) call the dispatcher directly.

.make_skip_collection <- function() {
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
  do.call(as_survey_collection, surveys)
}

test_that(".if_missing_var precedence: default stored, no call-arg → error", {
  coll <- .make_skip_collection()
  # Direct dispatcher call: .if_missing_var = NULL → fall back to
  # collection@if_missing_var (default "error").
  expect_error(
    .dispatch_over_collection(
      get_means,
      coll,
      x = focal,
      .if_missing_var = NULL
    ),
    class = "surveycore_error_collection_missing_var"
  )
})

test_that(".if_missing_var precedence: default stored, call-arg \"skip\" overrides", {
  coll <- .make_skip_collection()
  result <- suppressMessages(
    .dispatch_over_collection(
      get_means,
      coll,
      x = focal,
      .if_missing_var = "skip"
    )
  )
  expect_setequal(unique(result$.survey), "w1")
})

test_that(".if_missing_var precedence: non-default stored, no call-arg → stored", {
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
  coll <- do.call(
    as_survey_collection,
    c(surveys, list(.if_missing_var = "skip"))
  )
  # Direct dispatcher call: .if_missing_var = NULL → fall back to
  # collection@if_missing_var (set to "skip").
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

test_that(".if_missing_var precedence: non-default stored, call-arg \"error\" overrides", {
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
  coll <- do.call(
    as_survey_collection,
    c(surveys, list(.if_missing_var = "skip"))
  )
  # Direct dispatcher call: explicit "error" beats stored "skip".
  expect_error(
    .dispatch_over_collection(
      get_means,
      coll,
      x = focal,
      .if_missing_var = "error"
    ),
    class = "surveycore_error_collection_missing_var"
  )
})


# ══════════════════════════════════════════════════════════════════════════════
# Section — id_collision hint mentions set_collection_id() when stored
# ══════════════════════════════════════════════════════════════════════════════

test_that("id_collision hint mentions set_collection_id() under stored .id", {
  # Build a collection whose stored @id collides with a get_means() column.
  surveys <- lapply(seq_len(2L), function(i) {
    df <- make_survey_data(
      n = 60L,
      n_psu = 6L,
      n_strata = 2L,
      seed = i
    )
    as_survey(df, ids = psu, weights = wt, strata = strata)
  })
  names(surveys) <- c("w1", "w2")
  coll <- do.call(
    as_survey_collection,
    c(surveys, list(.id = "mean"))
  )
  expect_snapshot(
    error = TRUE,
    get_means(coll, y1, .id = NULL)
  )
})
