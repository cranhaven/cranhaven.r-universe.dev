# tests/testthat/test-mutate.R
#
# Behavioral tests for mutate().
# Every test that returns a survey object calls test_invariants().

# ── mutate() — happy path ─────────────────────────────────────────────────────

test_that("mutate() returns the same survey class for all design types", {
  designs <- make_all_designs()
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- mutate(d, z = y1 * 2)
    expect_true(
      inherits(result, class(d)[[1L]]),
      label = paste0(nm, ": class preserved")
    )
    test_invariants(result)
  }
})

test_that("mutate() adds the new column to @data", {
  d <- make_all_designs()$taylor
  result <- mutate(d, z = y1 * 2)
  expect_true("z" %in% names(result@data))
  expect_equal(result@data$z, result@data$y1 * 2)
  test_invariants(result)
})

test_that("mutate() preserves all existing columns", {
  d <- make_all_designs()$taylor
  result <- mutate(d, z = y1 + 1)
  original_cols <- names(d@data)
  for (col in original_cols) {
    expect_true(
      col %in% names(result@data),
      label = paste0("'", col, "' still present")
    )
  }
  test_invariants(result)
})

test_that("mutate() passes @groups through unchanged", {
  d <- make_all_designs()$taylor
  d2 <- group_by(d, group)
  result <- mutate(d2, z = y1 * 2)
  expect_identical(result@groups, d2@groups)
  test_invariants(result)
})

# ── mutate() — visible_vars ───────────────────────────────────────────────────

test_that("mutate() adds new column to visible_vars when visible_vars is set", {
  d <- make_all_designs()$taylor
  d2 <- select(d, y1, y2) # visible_vars = c("y1", "y2")
  d3 <- mutate(d2, z = y1 * 2)
  expect_true("z" %in% d3@variables$visible_vars)
  expect_true("y1" %in% d3@variables$visible_vars)
  test_invariants(d3)
})

test_that("mutate() does not change visible_vars when it is NULL", {
  d <- make_all_designs()$taylor
  expect_null(d@variables$visible_vars)
  result <- mutate(d, z = y1 * 2)
  expect_null(result@variables$visible_vars)
  test_invariants(result)
})

# ── mutate() — .keep argument ────────────────────────────────────────────────

test_that("mutate(.keep = 'none') re-attaches design variables", {
  d <- make_all_designs()$taylor
  result <- mutate(d, z = y1 * 2, .keep = "none")
  # z should be present
  expect_true("z" %in% names(result@data))
  # all design vars should be re-attached
  for (v in surveycore::.get_design_vars_flat(d)) {
    expect_true(
      v %in% names(result@data),
      label = paste0("design var '", v, "' re-attached after .keep='none'")
    )
  }
  test_invariants(result)
})

test_that("mutate(.keep = 'none') updates visible_vars to drop removed columns", {
  d <- make_all_designs()$taylor
  d2 <- select(d, y1, y2) # visible_vars = c("y1", "y2")
  d3 <- mutate(d2, z = y1 * 2, .keep = "none")
  # y2 was not used in mutation and .keep = "none" drops it
  # visible_vars should reflect what's left
  expect_false("y2" %in% (d3@variables$visible_vars %||% character(0)))
  test_invariants(d3)
})

test_that("mutate(.keep = 'used') re-attaches design variables", {
  d <- make_all_designs()$taylor
  result <- mutate(d, z = y1 * 2, .keep = "used")
  for (v in surveycore::.get_design_vars_flat(d)) {
    expect_true(
      v %in% names(result@data),
      label = paste0("design var '", v, "' present with .keep='used'")
    )
  }
  test_invariants(result)
})

# ── mutate() — design variable warning ───────────────────────────────────────

test_that("mutate() warns surveytidy_warning_mutate_weight_col when a weight column is modified", {
  d <- make_all_designs()$taylor

  expect_warning(
    result <- mutate(d, wt = wt * 1.1),
    class = "surveytidy_warning_mutate_weight_col"
  )
  expect_snapshot({
    invisible(mutate(d, wt = wt * 1.1))
  })
  test_invariants(result)
})

test_that("mutate() weight column warning names the modified variable", {
  d <- make_all_designs()$taylor
  expect_warning(
    mutate(d, wt = wt * 1.1),
    regexp = "wt"
  )
})

# ── mutate() — grouped mutate via @groups ─────────────────────────────────────

test_that("group_by() + mutate() computes group means correctly", {
  d <- make_all_designs()$taylor
  d2 <- group_by(d, group)
  d3 <- mutate(d2, group_mean_y1 = mean(y1))

  # Each row's group_mean_y1 should equal the mean of y1 within its group
  expected <- ave(d@data$y1, d@data$group, FUN = mean)
  expect_equal(d3@data$group_mean_y1, expected, tolerance = 1e-10)
  test_invariants(d3)
})

# ── mutate() — metadata tracking ─────────────────────────────────────────────

test_that("mutate() records transformation expression in @metadata for new columns", {
  d <- make_all_designs()$taylor
  result <- mutate(d, z = y1 * 2)
  expect_equal(result@metadata@transformations[["z"]], "y1 * 2")
})

test_that("mutate() does not record transformation for columns created by across()", {
  d <- make_all_designs()$taylor
  result <- mutate(d, across(c(y1, y2), ~ .x * 2))
  # across() names don't appear in mutations list — no entry expected
  # (limitation documented in spec; verify no crash)
  expect_true(is.list(result@metadata@transformations))
  test_invariants(result)
})

# ── mutate() — domain and @groups preservation ───────────────────────────────

test_that("mutate() does not alter the domain column", {
  d <- make_all_designs()$taylor
  d2 <- filter(d, y1 > 0)
  d3 <- mutate(d2, z = y1 * 2)
  expect_identical(
    d3@data[[surveycore::SURVEYCORE_DOMAIN_COL]],
    d2@data[[surveycore::SURVEYCORE_DOMAIN_COL]]
  )
  test_invariants(d3)
})

# ── mutate() — metadata attribute extraction ─────────────────────────────────

test_that("mutate() syncs variable label from haven::labelled() to @metadata", {
  d <- make_all_designs()$taylor
  result <- mutate(d, age_f = haven::labelled(y1, label = "Age in years"))
  test_invariants(result)
  expect_identical(
    result@metadata@variable_labels[["age_f"]],
    "Age in years"
  )
})

test_that("mutate() syncs value labels from haven::labelled() to @metadata", {
  d <- make_all_designs()$taylor
  result <- mutate(
    d,
    grp_lab = haven::labelled(
      y1,
      labels = c("low" = 0, "high" = 100),
      label = "Group label"
    )
  )
  test_invariants(result)
  expect_identical(
    result@metadata@variable_labels[["grp_lab"]],
    "Group label"
  )
  expect_identical(
    result@metadata@value_labels[["grp_lab"]],
    c("low" = 0, "high" = 100)
  )
})

test_that("mutate() syncs label from structure(, label =) to @metadata", {
  d <- make_all_designs()$taylor
  result <- mutate(d, y_new = structure(y1 * 2, label = "Double Y1"))
  test_invariants(result)
  expect_identical(
    result@metadata@variable_labels[["y_new"]],
    "Double Y1"
  )
})

test_that("mutate() syncs question_preface attr to @metadata", {
  d <- make_all_designs()$taylor
  result <- mutate(
    d,
    y_new = structure(y1, question_preface = "How old are you?")
  )
  test_invariants(result)
  expect_identical(
    result@metadata@question_prefaces[["y_new"]],
    "How old are you?"
  )
})

test_that("mutate() syncs note attr to @metadata", {
  d <- make_all_designs()$taylor
  result <- mutate(d, y_new = structure(y1, note = "Analyst note here"))
  test_invariants(result)
  expect_identical(
    result@metadata@notes[["y_new"]],
    "Analyst note here"
  )
})

test_that("mutate() syncs universe attr to @metadata", {
  d <- make_all_designs()$taylor
  result <- mutate(d, y_new = structure(y1, universe = "Adults 18+"))
  test_invariants(result)
  expect_identical(
    result@metadata@universe[["y_new"]],
    "Adults 18+"
  )
})

test_that("mutate() syncs missing_codes attr to @metadata", {
  d <- make_all_designs()$taylor
  result <- mutate(
    d,
    y_new = structure(y1, missing_codes = c("Refused" = 99, "DK" = 98))
  )
  test_invariants(result)
  expect_identical(
    result@metadata@missing_codes[["y_new"]],
    c("Refused" = 99, "DK" = 98)
  )
})

test_that("mutate() syncs multiple metadata attrs at once", {
  d <- make_all_designs()$taylor
  result <- mutate(
    d,
    y_new = structure(
      y1,
      label = "Age",
      question_preface = "How old are you?",
      note = "Top-coded at 90"
    )
  )
  test_invariants(result)
  expect_identical(result@metadata@variable_labels[["y_new"]], "Age")
  expect_identical(
    result@metadata@question_prefaces[["y_new"]],
    "How old are you?"
  )
  expect_identical(result@metadata@notes[["y_new"]], "Top-coded at 90")
})

test_that("mutate() syncs metadata attrs for all three design types", {
  designs <- make_all_designs()
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- mutate(d, y_lab = haven::labelled(y1, label = "Test label"))
    test_invariants(result)
    expect_identical(
      result@metadata@variable_labels[["y_lab"]],
      "Test label",
      label = paste0(nm, ": variable label synced")
    )
  }
})

test_that("mutate() clears stale metadata when column is overwritten without attrs", {
  d <- make_all_designs()$taylor
  d <- mutate(d, y_new = structure(y1, label = "Old label", note = "Old note"))
  expect_identical(d@metadata@variable_labels[["y_new"]], "Old label")
  # Overwrite without any attrs — should clear
  d2 <- mutate(d, y_new = y1 * 2)
  expect_null(d2@metadata@variable_labels[["y_new"]])
  expect_null(d2@metadata@notes[["y_new"]])
})

# ── coverage closers ─────────────────────────────────────────────────────────

# Covers mutate.R L180: rowwise(d, id_col) followed by mutate() routes to
# dplyr::rowwise(augmented_data, dplyr::all_of(id_cols)).
test_that("mutate() after rowwise(d, id_col) re-groups via id_cols (rowwise + id)", {
  d <- make_all_designs()$taylor
  d_rw <- dplyr::rowwise(d, group)
  # Sanity: rowwise_id_cols was set
  expect_identical(d_rw@variables$rowwise_id_cols, "group")
  result <- dplyr::mutate(
    d_rw,
    row_max = max(dplyr::c_across(dplyr::starts_with("y")))
  )
  test_invariants(result)
  # Row-by-row computation matches per-row max of y columns
  y_cols <- d@data[, c("y1", "y2", "y3"), drop = FALSE]
  expected_max <- apply(y_cols, 1, max)
  expect_equal(result@data$row_max, expected_max)
})

# Covers mutate.R L272: as.character(rlang::call_name(rlang::quo_get_expr(q))).
# Reached when surveytidy_recode is set on a result vector but the $fn entry
# is NULL. The public transform functions never produce that combination, so
# we attach the attr manually inline.
test_that("mutate() falls back to call_name() for fn when recode_attr$fn is NULL", {
  d <- make_all_designs()$taylor
  result <- dplyr::mutate(
    d,
    z = structure(
      y1 + 1,
      surveytidy_recode = list(
        fn = NULL,
        var = NULL,
        description = "manual attr"
      )
    )
  )
  test_invariants(result)
  trec <- result@metadata@transformations[["z"]]
  expect_true(is.list(trec))
  # call_name() of the structure(...) call returns "structure"
  expect_identical(trec$fn, "structure")
  expect_identical(trec$description, "manual attr")
})
