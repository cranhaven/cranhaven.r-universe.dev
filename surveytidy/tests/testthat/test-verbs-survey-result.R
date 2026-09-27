# tests/testthat/test-verbs-survey-result.R
#
# Tests for dplyr/tidyr verb methods on survey_result objects.
# PR 1 (passthrough verbs): sections 1, 2, 3, 3b, 3c, 4, 23, 24, 25, 26, 29
# PR 2 (meta-updating verbs): sections 5–22, 27, 28

# ── PR 1: Passthrough verbs ────────────────────────────────────────────────

# Section 1: every passthrough verb preserves class and meta for all
# types × all designs.

test_that("filter.survey_result preserves class and meta for all types × designs", {
  for (type in c("means", "freqs", "ratios")) {
    expected_class <- paste0("survey_", type)
    for (design in c("taylor", "replicate", "twophase")) {
      r <- make_survey_result(type = type, design = design)
      result <- dplyr::filter(r, n > 0)
      test_result_invariants(result, expected_class)
      expect_identical(surveycore::meta(result), surveycore::meta(r))
    }
  }
})

test_that("arrange.survey_result preserves class and meta for all types × designs", {
  for (type in c("means", "freqs", "ratios")) {
    expected_class <- paste0("survey_", type)
    for (design in c("taylor", "replicate", "twophase")) {
      r <- make_survey_result(type = type, design = design)
      result <- dplyr::arrange(r, n)
      test_result_invariants(result, expected_class)
      expect_identical(surveycore::meta(result), surveycore::meta(r))
    }
  }
})

test_that("mutate.survey_result preserves class and meta for all types × designs", {
  for (type in c("means", "freqs", "ratios")) {
    expected_class <- paste0("survey_", type)
    for (design in c("taylor", "replicate", "twophase")) {
      r <- make_survey_result(type = type, design = design)
      result <- dplyr::mutate(r, new_col = n * 2L)
      test_result_invariants(result, expected_class)
      expect_identical(surveycore::meta(result), surveycore::meta(r))
    }
  }
})

test_that("slice.survey_result preserves class and meta for all types × designs", {
  for (type in c("means", "freqs", "ratios")) {
    expected_class <- paste0("survey_", type)
    for (design in c("taylor", "replicate", "twophase")) {
      r <- make_survey_result(type = type, design = design)
      result <- dplyr::slice(r, 1)
      test_result_invariants(result, expected_class)
      expect_identical(surveycore::meta(result), surveycore::meta(r))
    }
  }
})

test_that("slice_head.survey_result preserves class and meta for all types × designs", {
  for (type in c("means", "freqs", "ratios")) {
    expected_class <- paste0("survey_", type)
    for (design in c("taylor", "replicate", "twophase")) {
      r <- make_survey_result(type = type, design = design)
      result <- dplyr::slice_head(r, n = 1)
      test_result_invariants(result, expected_class)
      expect_identical(surveycore::meta(result), surveycore::meta(r))
    }
  }
})

test_that("slice_tail.survey_result preserves class and meta for all types × designs", {
  for (type in c("means", "freqs", "ratios")) {
    expected_class <- paste0("survey_", type)
    for (design in c("taylor", "replicate", "twophase")) {
      r <- make_survey_result(type = type, design = design)
      result <- dplyr::slice_tail(r, n = 1)
      test_result_invariants(result, expected_class)
      expect_identical(surveycore::meta(result), surveycore::meta(r))
    }
  }
})

test_that("slice_min.survey_result preserves class and meta for all types × designs", {
  for (type in c("means", "freqs", "ratios")) {
    expected_class <- paste0("survey_", type)
    for (design in c("taylor", "replicate", "twophase")) {
      r <- make_survey_result(type = type, design = design)
      result <- dplyr::slice_min(r, order_by = n, n = 1)
      test_result_invariants(result, expected_class)
      expect_identical(surveycore::meta(result), surveycore::meta(r))
    }
  }
})

test_that("slice_max.survey_result preserves class and meta for all types × designs", {
  for (type in c("means", "freqs", "ratios")) {
    expected_class <- paste0("survey_", type)
    for (design in c("taylor", "replicate", "twophase")) {
      r <- make_survey_result(type = type, design = design)
      result <- dplyr::slice_max(r, order_by = n, n = 1)
      test_result_invariants(result, expected_class)
      expect_identical(surveycore::meta(result), surveycore::meta(r))
    }
  }
})

test_that("slice_sample.survey_result preserves class and meta for all types × designs", {
  for (type in c("means", "freqs", "ratios")) {
    expected_class <- paste0("survey_", type)
    for (design in c("taylor", "replicate", "twophase")) {
      r <- make_survey_result(type = type, design = design)
      set.seed(1L)
      result <- dplyr::slice_sample(r, n = 1)
      test_result_invariants(result, expected_class)
      expect_identical(surveycore::meta(result), surveycore::meta(r))
    }
  }
})

test_that("drop_na.survey_result preserves class and meta for all types × designs", {
  for (type in c("means", "freqs", "ratios")) {
    expected_class <- paste0("survey_", type)
    for (design in c("taylor", "replicate", "twophase")) {
      r <- make_survey_result(type = type, design = design)
      result <- tidyr::drop_na(r)
      test_result_invariants(result, expected_class)
      expect_identical(surveycore::meta(result), surveycore::meta(r))
    }
  }
})

# Section 2: row-changing verbs show correct row counts, including 0-row edge case.

test_that("filter.survey_result returns correct row count; 0-row result preserves class", {
  r <- make_survey_result(type = "means")

  # Non-trivial filter: keep rows where n > 0
  result_filtered <- dplyr::filter(r, n > 0)
  test_result_invariants(result_filtered, "survey_means")
  expect_lte(nrow(result_filtered), nrow(r))

  # 0-row result
  result_empty <- dplyr::filter(r, n > 1e9)
  test_result_invariants(result_empty, "survey_means")
  expect_equal(nrow(result_empty), 0L)
  expect_identical(surveycore::meta(result_empty), surveycore::meta(r))
})

test_that("slice_head.survey_result returns correct row count; 0-row result preserves class", {
  r <- make_survey_result(type = "means")

  result_one <- dplyr::slice_head(r, n = 1)
  test_result_invariants(result_one, "survey_means")
  expect_equal(nrow(result_one), 1L)

  # 0-row result
  result_empty <- dplyr::slice_head(r, n = 0)
  test_result_invariants(result_empty, "survey_means")
  expect_equal(nrow(result_empty), 0L)
  expect_identical(surveycore::meta(result_empty), surveycore::meta(r))
})

test_that("slice_tail.survey_result returns correct row count; 0-row result preserves class", {
  r <- make_survey_result(type = "means")

  result_one <- dplyr::slice_tail(r, n = 1)
  test_result_invariants(result_one, "survey_means")
  expect_equal(nrow(result_one), 1L)

  # 0-row result
  result_empty <- dplyr::slice_tail(r, n = 0)
  test_result_invariants(result_empty, "survey_means")
  expect_equal(nrow(result_empty), 0L)
  expect_identical(surveycore::meta(result_empty), surveycore::meta(r))
})

# Section 3: mutate() adds column; meta unchanged.

test_that("mutate.survey_result adds new column without modifying meta", {
  r <- make_survey_result(type = "means")
  result <- dplyr::mutate(r, sig = se < 0.1)

  test_result_invariants(result, "survey_means")
  expect_true("sig" %in% names(result))
  expect_identical(surveycore::meta(result), surveycore::meta(r))
})

# Section 3b: mutate(.keep = "none") — meta coherence maintained after column drops.

test_that("mutate.survey_result(.keep = 'none') prunes meta$group for dropped group col", {
  r <- make_survey_result(type = "means")
  # Only the new column 'sig' will remain; group, mean, se, n are all dropped
  result <- dplyr::mutate(r, sig = se < 0.1, .keep = "none")

  test_result_invariants(result, "survey_means")
  test_result_meta_coherent(result)

  # group column dropped → group entry pruned (group IS an output column)
  expect_equal(length(surveycore::meta(result)$group), 0L)
  # new column is present
  expect_true("sig" %in% names(result))
  # meta$x references the input focal variable ("y1"), not the output column;
  # it is preserved regardless of column changes
  expect_false(is.null(surveycore::meta(result)$x))
})

# Section 3c: mutate(.keep = "used") — meta coherence maintained after column drops.

test_that("mutate.survey_result(.keep = 'used') prunes meta$group for dropped group col", {
  r <- make_survey_result(type = "means")
  # .keep = "used" keeps only columns used in expressions: se and sig
  result <- dplyr::mutate(r, sig = se < 0.1, .keep = "used")

  test_result_invariants(result, "survey_means")
  test_result_meta_coherent(result)

  # group column dropped → group entry pruned (group IS an output column)
  expect_equal(length(surveycore::meta(result)$group), 0L)
  # se and sig are present
  expect_true("se" %in% names(result))
  expect_true("sig" %in% names(result))
})

# Section 4: n_respondents is not updated after filter.

test_that("n_respondents is unchanged after filter.survey_result", {
  r <- make_survey_result(type = "means")
  orig_n <- surveycore::meta(r)$n_respondents

  result <- dplyr::filter(r, n > 0)
  # Result may have fewer rows than r, but n_respondents is fixed
  expect_lte(nrow(result), nrow(r))
  expect_identical(surveycore::meta(result)$n_respondents, orig_n)
})

# Section 23: drop_na() removes rows with NAs; meta preserved (EDGE-5).

test_that("drop_na.survey_result drops rows with NAs and preserves class and meta", {
  r <- make_survey_result(type = "means")
  r_with_na <- r
  r_with_na$se[1L] <- NA_real_

  result <- tidyr::drop_na(r_with_na, se)

  test_result_invariants(result, "survey_means")
  expect_lt(nrow(result), nrow(r_with_na))
  expect_identical(surveycore::meta(result), surveycore::meta(r_with_na))
})

# Section 24: filter() with .by argument forwarded to NextMethod (EDGE-6).

test_that("filter.survey_result forwards .by to NextMethod", {
  r <- make_survey_result(type = "means")

  result <- dplyr::filter(r, n > 0, .by = group)

  test_result_invariants(result, "survey_means")
  expect_identical(surveycore::meta(result), surveycore::meta(r))
})

# Section 25: slice_min/slice_max with non-default args preserve class and meta (EDGE-7).

test_that("slice_min.survey_result with with_ties=FALSE returns exactly n rows", {
  r <- make_survey_result(type = "means")

  result <- dplyr::slice_min(r, order_by = n, n = 1, with_ties = FALSE)

  test_result_invariants(result, "survey_means")
  expect_equal(nrow(result), 1L)
  expect_identical(surveycore::meta(result), surveycore::meta(r))
})

test_that("slice_max.survey_result with na_rm=TRUE excludes NA rows from ranking", {
  r <- make_survey_result(type = "means")
  # Inject NA into the ordering column for one row
  r_with_na <- r
  r_with_na$n[1L] <- NA_integer_

  result <- dplyr::slice_max(r_with_na, order_by = n, n = 1, na_rm = TRUE)

  test_result_invariants(result, "survey_means")
  expect_identical(surveycore::meta(result), surveycore::meta(r_with_na))
})

# Section 26: slice_sample(replace=TRUE) over-sampling preserves class and meta (EDGE-8).

test_that("slice_sample.survey_result with replace=TRUE supports over-sampling", {
  r <- make_survey_result(type = "means")
  n_over <- nrow(r) + 1L

  set.seed(42L)
  result <- dplyr::slice_sample(r, n = n_over, replace = TRUE)

  test_result_invariants(result, "survey_means")
  expect_equal(nrow(result), n_over)
  expect_identical(surveycore::meta(result), surveycore::meta(r))
})

# Section 29: drop_na() with no NAs — all rows preserved; class and meta identical.

test_that("drop_na.survey_result with no NAs returns all rows unchanged", {
  r <- make_survey_result(type = "means")
  # Ensure no NAs (fixture typically has none)
  r_no_na <- r[complete.cases(r), ]
  class(r_no_na) <- class(r)
  attr(r_no_na, ".meta") <- attr(r, ".meta")

  result <- tidyr::drop_na(r_no_na)

  test_result_invariants(result, "survey_means")
  expect_equal(nrow(result), nrow(r_no_na))
  expect_identical(surveycore::meta(result), surveycore::meta(r_no_na))
})

# ── PR 2: Meta-updating verbs ──────────────────────────────────────────────

# Section 5: rename() — group key updated.

test_that("rename.survey_result updates meta$group key when group column renamed", {
  r <- make_survey_result(type = "means")
  result <- dplyr::rename(r, grp = group)

  test_result_invariants(result, "survey_means")
  test_result_meta_coherent(result)
  expect_true("grp" %in% names(surveycore::meta(result)$group))
  expect_false("group" %in% names(surveycore::meta(result)$group))
  expect_true("grp" %in% names(result))
})

# Section 6: rename() — x key updated.
# Uses result_freqs where the $x key "group" IS an output column (unlike
# result_means where $x = "y1" which is an input-only variable).

test_that("rename.survey_result updates meta$x key when focal column renamed (freqs)", {
  r <- make_survey_result(type = "freqs")
  # freqs: $x = list(group = ...) and "group" IS an output column
  result <- dplyr::rename(r, outcome = group)

  test_result_invariants(result, "survey_freqs")
  test_result_meta_coherent(result)
  expect_true("outcome" %in% names(surveycore::meta(result)$x))
  expect_false("group" %in% names(surveycore::meta(result)$x))
})

# Section 7: rename() — non-meta column rename leaves meta unchanged.

test_that("rename.survey_result leaves meta unchanged when renaming non-meta column", {
  r <- make_survey_result(type = "means")
  orig_meta <- surveycore::meta(r)

  result <- dplyr::rename(r, std_error = se)

  test_result_invariants(result, "survey_means")
  expect_identical(surveycore::meta(result), orig_meta)
  expect_true("std_error" %in% names(result))
  expect_false("se" %in% names(result))
})

# Section 8: rename() — ratios numerator$name updated.
# Note: $numerator$name = "y1" is an INPUT variable name, not an output
# column. The output columns are ratio, ci_low, ci_high, n. Renaming an
# output column does not affect $numerator$name. We verify $numerator$name
# is preserved when a non-meta output column is renamed.

test_that("rename.survey_result preserves numerator$name when renaming non-meta column", {
  r <- make_survey_result(type = "ratios")
  orig_num_name <- surveycore::meta(r)$numerator$name

  result <- dplyr::rename(r, estimate = ratio)

  test_result_invariants(result, "survey_ratios")
  expect_identical(surveycore::meta(result)$numerator$name, orig_num_name)
  expect_true("estimate" %in% names(result))
})

# Section 9: rename() — ratios denominator$name preserved when renaming non-meta column.

test_that("rename.survey_result preserves denominator$name when renaming non-meta column", {
  r <- make_survey_result(type = "ratios")
  orig_denom_name <- surveycore::meta(r)$denominator$name

  result <- dplyr::rename(r, ci_lower = ci_low)

  test_result_invariants(result, "survey_ratios")
  expect_identical(surveycore::meta(result)$denominator$name, orig_denom_name)
  expect_true("ci_lower" %in% names(result))
})

# Section 10: rename_with() — applies .fn to all columns and updates meta.

test_that("rename_with.survey_result applies .fn to all columns and updates meta$group", {
  r <- make_survey_result(type = "means")
  result <- dplyr::rename_with(r, toupper)

  test_result_invariants(result, "survey_means")
  test_result_meta_coherent(result)
  # All column names are upper-cased
  expect_true(all(names(result) == toupper(names(r))))
  # meta$group key "group" → "GROUP"
  expect_true("GROUP" %in% names(surveycore::meta(result)$group))
  expect_false("group" %in% names(surveycore::meta(result)$group))
})

# Section 11: rename_with() — .cols limits scope of rename.

test_that("rename_with.survey_result with .cols only renames selected columns", {
  r <- make_survey_result(type = "means")
  result <- dplyr::rename_with(r, toupper, .cols = c(mean, se))

  test_result_invariants(result, "survey_means")
  test_result_meta_coherent(result)
  # Only mean and se are upper-cased; group and n unchanged
  expect_true("MEAN" %in% names(result))
  expect_true("SE" %in% names(result))
  expect_true("group" %in% names(result))
  expect_true("n" %in% names(result))
  # meta$group key "group" unchanged (group not in .cols)
  expect_true("group" %in% names(surveycore::meta(result)$group))
  # meta$x key "y1" unchanged (y1 is input var name, not in .cols)
  expect_identical(
    names(surveycore::meta(result)$x),
    names(surveycore::meta(r)$x)
  )
})

# Section 12: rename_with() — invalid .fn output triggers error (parameterized, dual pattern).

test_that("rename_with.survey_result errors for all invalid .fn outputs", {
  result_means <- make_survey_result(type = "means")
  bad_fns <- list(
    "non-character output" = function(x) seq_along(x),
    "wrong-length output" = function(x) x[1],
    "NA in output" = function(x) {
      x[1] <- NA_character_
      x
    },
    "duplicate names" = function(x) rep(x[1], length(x))
  )
  for (label in names(bad_fns)) {
    fn <- bad_fns[[label]]
    expect_error(
      dplyr::rename_with(result_means, fn),
      class = "surveytidy_error_rename_fn_bad_output"
    )
    expect_snapshot(error = TRUE, dplyr::rename_with(result_means, fn))
  }
})

# Section 13: select() — group entry removed when group col dropped.

test_that("select.survey_result removes meta$group entry when group column dropped", {
  r <- make_survey_result(type = "means")
  result <- dplyr::select(r, mean, se)

  test_result_invariants(result, "survey_means")
  test_result_meta_coherent(result)
  expect_equal(length(surveycore::meta(result)$group), 0L)
  expect_false("group" %in% names(result))
})

# Section 14: select() — group col kept; estimate cols dropped; meta$group preserved.
# Note: $x for means = "y1" (input variable name, not an output column).
# $x is preserved regardless of which output columns are selected.

test_that("select.survey_result keeps meta$group when group column retained", {
  r <- make_survey_result(type = "means")
  orig_group_meta <- surveycore::meta(r)$group

  result <- dplyr::select(r, group)

  test_result_invariants(result, "survey_means")
  test_result_meta_coherent(result)
  # group column is kept; meta$group entry is preserved
  expect_identical(surveycore::meta(result)$group, orig_group_meta)
  expect_true("group" %in% names(result))
})

# Section 15: select() — kept group column preserves meta$group sub-key.

test_that("select.survey_result preserves meta$group sub-key when group retained", {
  r <- make_survey_result(type = "means")

  result <- dplyr::select(r, group, mean, se)

  test_result_invariants(result, "survey_means")
  test_result_meta_coherent(result)
  # group column is kept → meta$group$group is identical to original
  expect_identical(
    surveycore::meta(result)$group$group,
    surveycore::meta(r)$group$group
  )
  expect_false("n" %in% names(result))
})

# Section 16: select() — non-group, non-x column removal does not affect group/x meta.

test_that("select.survey_result(-se) leaves meta$group and meta$x unchanged", {
  r <- make_survey_result(type = "means")

  result <- dplyr::select(r, -se)

  test_result_invariants(result, "survey_means")
  test_result_meta_coherent(result)
  expect_identical(surveycore::meta(result)$group, surveycore::meta(r)$group)
  expect_identical(surveycore::meta(result)$x, surveycore::meta(r)$x)
  expect_false("se" %in% names(result))
})

# Section 16b: select() with inline rename syntax — meta preserved under new name.

test_that("select.survey_result with inline rename updates meta$group key", {
  r <- make_survey_result(type = "means")
  result <- dplyr::select(r, grp = group)

  test_result_invariants(result, "survey_means")
  test_result_meta_coherent(result)
  expect_true("grp" %in% names(surveycore::meta(result)$group))
  expect_false("group" %in% names(surveycore::meta(result)$group))
  expect_true("grp" %in% names(result))
})

# Section 17: rename() on result_freqs — updates $x key; empty $group path unchanged.

test_that("rename.survey_result on freqs updates $x key; empty $group is unchanged", {
  r <- make_survey_result(type = "freqs")
  # freqs: $group = list() (empty), $x = list(group = ...)
  result <- dplyr::rename(r, grp = group)

  test_result_invariants(result, "survey_freqs")
  test_result_meta_coherent(result)
  expect_true("grp" %in% names(surveycore::meta(result)$x))
  expect_false("group" %in% names(surveycore::meta(result)$x))
  expect_equal(length(surveycore::meta(result)$group), 0L)
})

# Section 18: select() on result_freqs — removes focal col; meta$group unchanged (empty).
# Note: meta$x for freqs = "group" which IS an output column.
# After dropping "group", meta$group is still empty (no change to empty list).

test_that("select.survey_result on freqs removes group col; meta$group stays empty", {
  r <- make_survey_result(type = "freqs")
  # freqs columns: group, pct, n; $group = list(), $x = list(group = ...)
  result <- dplyr::select(r, pct, n)

  test_result_invariants(result, "survey_freqs")
  test_result_meta_coherent(result)
  # group column dropped → $group stays empty (it was already empty for freqs)
  expect_equal(length(surveycore::meta(result)$group), 0L)
  expect_false("group" %in% names(result))
})

# Section 19: chained meta-updating verbs — rename then select.

test_that("rename then select on survey_means chains meta updates correctly", {
  r <- make_survey_result(type = "means")
  result <- r |>
    dplyr::rename(grp = group) |>
    dplyr::select(grp, mean)

  test_result_invariants(result, "survey_means")
  test_result_meta_coherent(result)
  expect_true("grp" %in% names(surveycore::meta(result)$group))
  expect_false("group" %in% names(surveycore::meta(result)$group))
  expect_true("grp" %in% names(result))
  expect_true("mean" %in% names(result))
})

# Section 20: rename_with() — .cols resolving to zero columns is a no-op.

test_that("rename_with.survey_result with zero-match .cols is a no-op", {
  r <- make_survey_result(type = "means")
  result <- dplyr::rename_with(r, toupper, .cols = dplyr::starts_with("zzz"))

  test_result_invariants(result, "survey_means")
  expect_identical(names(result), names(r))
  expect_identical(surveycore::meta(result), surveycore::meta(r))
})

# Section 21: rename() — identity rename is a no-op.

test_that("rename.survey_result identity rename leaves result unchanged", {
  r <- make_survey_result(type = "means")
  result <- dplyr::rename(r, group = group)

  test_result_invariants(result, "survey_means")
  expect_identical(names(result), names(r))
  expect_identical(surveycore::meta(result), surveycore::meta(r))
})

# Section 22: rename_with() — ... forwarded to .fn.

test_that("rename_with.survey_result forwards ... to .fn", {
  r <- make_survey_result(type = "means")
  # gsub renames: "mean" -> "avg"; other columns unchanged
  result <- dplyr::rename_with(r, gsub, pattern = "mean", replacement = "avg")

  test_result_invariants(result, "survey_means")
  test_result_meta_coherent(result)
  expect_true("avg" %in% names(result))
  expect_false("mean" %in% names(result))
  # meta$group key "group" unchanged (group does not contain "mean")
  expect_true("group" %in% names(surveycore::meta(result)$group))
  # meta$x key "y1" unchanged (y1 is input variable, "mean" not in $x keys)
  expect_identical(
    names(surveycore::meta(result)$x),
    names(surveycore::meta(r)$x)
  )
})

# Section 27: zero-column select() — degenerate result; invariants pass.

test_that("select.survey_result with zero matching columns returns 0-column result", {
  r <- make_survey_result(type = "means")
  result <- dplyr::select(r, dplyr::starts_with("zzz"))

  test_result_invariants(result, "survey_means")
  expect_equal(ncol(result), 0L)
  expect_equal(length(surveycore::meta(result)$group), 0L)
})

# Section 28: select(everything()) — all columns kept; meta identical to input.

test_that("select.survey_result(everything()) keeps all columns and meta unchanged", {
  r <- make_survey_result(type = "means")
  result <- dplyr::select(r, dplyr::everything())

  test_result_invariants(result, "survey_means")
  expect_identical(names(result), names(r))
  expect_identical(surveycore::meta(result), surveycore::meta(r))
})
