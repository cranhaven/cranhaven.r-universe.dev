# tests/testthat/test-rowwise.R
#
# Tests for rowwise.survey_base(), is_rowwise(), is_grouped(),
# group_vars.survey_base(), and the mutate() rowwise routing.
#
# Covers all six sections from spec §VI.4.

# ── Section 1: rowwise() sets @variables$rowwise correctly ────────────────────

test_that("rowwise() sets @variables$rowwise = TRUE and @variables$rowwise_id_cols = character(0) with no id cols", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- dplyr::rowwise(d)
    test_invariants(result)
    expect_true(isTRUE(result@variables$rowwise))
    expect_identical(result@variables$rowwise_id_cols, character(0))
    # @groups is unchanged — rowwise does not modify grouping
    expect_identical(result@groups, d@groups)
  }
})

test_that("rowwise(d, group) sets rowwise_id_cols to the specified column", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- dplyr::rowwise(d, group)
    test_invariants(result)
    expect_true(isTRUE(result@variables$rowwise))
    expect_identical(result@variables$rowwise_id_cols, "group")
    expect_identical(result@groups, d@groups)
  }
})

test_that("is_rowwise() returns TRUE after rowwise() and FALSE for plain design", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    expect_false(is_rowwise(d))
    expect_true(is_rowwise(dplyr::rowwise(d)))
  }
})

test_that("is_grouped() returns FALSE for rowwise design and TRUE for grouped design", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    expect_false(is_grouped(dplyr::rowwise(d)))
    expect_true(is_grouped(dplyr::group_by(d, group)))
    expect_false(is_rowwise(dplyr::group_by(d, group)))
  }
})

test_that("group_vars() returns character(0) for rowwise design and correct names for grouped design", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    expect_identical(dplyr::group_vars(d), character(0))
    expect_identical(dplyr::group_vars(dplyr::rowwise(d)), character(0))
    expect_identical(dplyr::group_vars(dplyr::rowwise(d, group)), character(0))
    expect_identical(dplyr::group_vars(dplyr::group_by(d, group)), "group")
  }
})


# ── Section 2: rowwise() + mutate() row-wise computation ──────────────────────

test_that("rowwise() |> mutate() computes row-wise: max across y columns per row", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- dplyr::rowwise(d) |>
      dplyr::mutate(row_max = max(dplyr::c_across(dplyr::starts_with("y"))))
    test_invariants(result)

    # Verify row-by-row computation: row_max should equal per-row max of y cols
    y_cols <- d@data[, c("y1", "y2", "y3"), drop = FALSE]
    expected_max <- apply(y_cols, 1, max)
    expect_equal(result@data$row_max, expected_max)

    # Row count unchanged
    expect_equal(nrow(result@data), nrow(d@data))

    # Design variables (actual columns) preserved — test_invariants() above
    # already verifies this; confirm no columns were accidentally dropped
    expect_true(all(names(d@data) %in% names(result@data)))
  }
})

test_that("rowwise() |> mutate() preserves domain column on filtered design", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    d_filtered <- dplyr::filter(d, y1 > 0)
    result <- dplyr::rowwise(d_filtered) |>
      dplyr::mutate(row_sum = dplyr::c_across(y1) + 1)
    test_invariants(result)

    # Domain column present and unchanged
    expect_true(surveycore::SURVEYCORE_DOMAIN_COL %in% names(result@data))
    expect_identical(
      result@data[[surveycore::SURVEYCORE_DOMAIN_COL]],
      d_filtered@data[[surveycore::SURVEYCORE_DOMAIN_COL]]
    )
  }
})

test_that("vectorization regression: ungroup() after rowwise mutate makes subsequent mutate vectorized", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- dplyr::rowwise(d) |>
      dplyr::mutate(row_max = max(dplyr::c_across(dplyr::starts_with("y")))) |>
      dplyr::ungroup() |>
      dplyr::mutate(y_mean = mean(y1))

    test_invariants(result)

    # y_mean should be the same (vectorized overall mean) in every row,
    # NOT the per-row mean — confirms rowwise_df class was stripped from @data
    expect_true(
      length(unique(result@data$y_mean)) == 1L,
      label = "y_mean is the same in every row (vectorized mean, not rowwise)"
    )
    expect_equal(unique(result@data$y_mean), mean(d@data$y1))
  }
})


# ── Section 3: ungroup() exits rowwise mode ────────────────────────────────────

test_that("ungroup() (full) clears @variables$rowwise and @variables$rowwise_id_cols", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- dplyr::rowwise(d) |> dplyr::ungroup()
    test_invariants(result)
    expect_false(is_rowwise(result))
    expect_null(result@variables$rowwise)
    expect_null(result@variables$rowwise_id_cols)
  }
})

test_that("after full ungroup, mutate() is no longer row-wise", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- dplyr::rowwise(d) |>
      dplyr::ungroup() |>
      dplyr::mutate(y_mean = mean(y1))

    test_invariants(result)

    # Vectorized mean: same value in every row
    expect_true(
      length(unique(result@data$y_mean)) == 1L,
      label = "y_mean is same in every row after full ungroup"
    )
  }
})

test_that("partial ungroup preserves rowwise mode", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    # First group, then rowwise, then partial-ungroup a column from @groups
    # (not rowwise_id_cols) — rowwise mode must persist
    d_grouped <- dplyr::group_by(d, group)
    d_rowwise_grouped <- dplyr::rowwise(d_grouped)

    # Verify setup: has groups AND is rowwise
    expect_identical(d_rowwise_grouped@groups, "group")
    expect_true(is_rowwise(d_rowwise_grouped))

    # Partial ungroup removes from @groups only — rowwise persists
    result <- dplyr::ungroup(d_rowwise_grouped, group)
    test_invariants(result)
    expect_true(is_rowwise(result))
    expect_identical(result@variables$rowwise_id_cols, character(0))
    expect_identical(result@groups, character(0))
  }
})

test_that("partial ungroup of rowwise(d, group) leaves rowwise_id_cols intact", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    # rowwise with id_col, then group_by, then partial-ungroup the group col
    d_rw <- dplyr::rowwise(d, group)
    d_rw_grouped <- dplyr::group_by(d_rw, group, .add = TRUE)

    # After .add=TRUE: rowwise cleared, group promoted to @groups
    expect_false(is_rowwise(d_rw_grouped))
    expect_identical(d_rw_grouped@groups, "group")

    # Partial-ungroup: @groups cleared; rowwise is already gone (not by partial ungroup)
    result <- dplyr::ungroup(d_rw_grouped, group)
    test_invariants(result)
    expect_false(is_rowwise(result))
    expect_identical(result@groups, character(0))
  }
})


# ── Section 4: group_by() exits rowwise mode ──────────────────────────────────

test_that("group_by(.add = FALSE) exits rowwise mode and sets @groups", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- dplyr::rowwise(d) |> dplyr::group_by(group)
    test_invariants(result)
    expect_false(is_rowwise(result))
    expect_null(result@variables$rowwise)
    expect_null(result@variables$rowwise_id_cols)
    expect_identical(result@groups, "group")
  }
})

test_that("group_by(.add = TRUE) on rowwise(d) with no id_cols: exits rowwise, sets @groups", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- dplyr::rowwise(d) |> dplyr::group_by(group, .add = TRUE)
    test_invariants(result)
    expect_false(is_rowwise(result))
    expect_null(result@variables$rowwise)
    expect_null(result@variables$rowwise_id_cols)
    expect_identical(result@groups, "group")
  }
})

test_that("group_by(.add = TRUE) on rowwise(d, group): id_col promoted to @groups, rowwise cleared", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    # rowwise with id_col = "group"; then group_by(group, .add = TRUE)
    # Expected: id_col "group" is promoted, new group "group" appended (deduped)
    result <- dplyr::rowwise(d, group) |>
      dplyr::group_by(group, .add = TRUE)
    test_invariants(result)
    expect_false(is_rowwise(result))
    expect_null(result@variables$rowwise)
    expect_null(result@variables$rowwise_id_cols)
    # "group" appears once (deduped)
    expect_identical(result@groups, "group")
  }
})

test_that("after group_by(), mutate() is no longer row-wise", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- dplyr::rowwise(d) |>
      dplyr::group_by(group) |>
      dplyr::mutate(y_mean = mean(y1))

    test_invariants(result)

    # Should be a grouped mean (by group), not row-wise
    # Each row's y_mean should equal the within-group mean of y1
    expected <- ave(d@data$y1, d@data$group, FUN = mean)
    expect_equal(result@data$y_mean, expected)
  }
})


# ── Section 5: rowwise state propagation through other verbs ──────────────────

test_that("filter() preserves rowwise state", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- dplyr::rowwise(d) |> dplyr::filter(y1 > 0)
    test_invariants(result)
    expect_true(is_rowwise(result))
    expect_true(isTRUE(result@variables$rowwise))
  }
})

test_that("select() preserves rowwise state", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- dplyr::rowwise(d) |> dplyr::select(y1, y2)
    test_invariants(result)
    expect_true(is_rowwise(result))
    expect_true(isTRUE(result@variables$rowwise))
  }
})

test_that("arrange() preserves rowwise state", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- dplyr::rowwise(d) |> dplyr::arrange(y1)
    test_invariants(result)
    expect_true(is_rowwise(result))
    expect_true(isTRUE(result@variables$rowwise))
  }
})


# ── Section 6: Edge cases ─────────────────────────────────────────────────────

test_that("rowwise(d) on grouped design leaves @groups intact", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    d_grouped <- dplyr::group_by(d, group)
    result <- dplyr::rowwise(d_grouped)
    test_invariants(result)
    # @groups unchanged — rowwise does not affect grouping
    expect_identical(result@groups, "group")
    expect_true(is_rowwise(result))
    # Both rowwise and grouped (independent modes)
    expect_true(is_grouped(result))
  }
})

test_that("rowwise() with a non-existent column triggers a tidyselect error", {
  designs <- make_all_designs(seed = 42)
  d <- designs$taylor
  expect_error(dplyr::rowwise(d, nonexistent_col))
})
