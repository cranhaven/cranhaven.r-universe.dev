# tests/testthat/test-rename.R
#
# Behavioral tests for rename().
# Every test that returns a survey object calls test_invariants().

# ── rename() — happy path ─────────────────────────────────────────────────────

test_that("rename() returns the same survey class for all design types", {
  designs <- make_all_designs()
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- rename(d, outcome1 = y1)
    expect_true(
      inherits(result, class(d)[[1L]]),
      label = paste0(nm, ": class preserved")
    )
    test_invariants(result)
  }
})

test_that("rename() renames the column in @data", {
  d <- make_all_designs()$taylor
  result <- rename(d, outcome1 = y1)
  expect_true("outcome1" %in% names(result@data))
  expect_false("y1" %in% names(result@data))
  test_invariants(result)
})

test_that("rename() preserves all other columns unchanged", {
  d <- make_all_designs()$taylor
  result <- rename(d, outcome1 = y1)
  other_cols <- setdiff(names(d@data), "y1")
  for (col in other_cols) {
    expect_true(
      col %in% names(result@data),
      label = paste0("'", col, "' still present")
    )
  }
  test_invariants(result)
})

test_that("rename() passes @groups through unchanged", {
  d <- make_all_designs()$taylor
  d2 <- group_by(d, group)
  result <- rename(d2, outcome1 = y1)
  expect_identical(result@groups, d2@groups)
  test_invariants(result)
})

# ── rename() — visible_vars update ───────────────────────────────────────────

test_that("rename() updates visible_vars when renaming a visible column", {
  d <- make_all_designs()$taylor
  d2 <- select(d, y1, y2) # visible_vars = c("y1", "y2")
  d3 <- rename(d2, outcome1 = y1)
  expect_identical(d3@variables$visible_vars, c("outcome1", "y2"))
  test_invariants(d3)
})

test_that("rename() leaves visible_vars NULL unchanged when NULL", {
  d <- make_all_designs()$taylor
  expect_null(d@variables$visible_vars)
  result <- rename(d, outcome1 = y1)
  expect_null(result@variables$visible_vars)
  test_invariants(result)
})

# ── rename() — design variable warning and @variables update ──────────────────

test_that("rename() warns when renaming a design variable", {
  d <- make_all_designs()$taylor
  expect_warning(
    result <- rename(d, weight = wt),
    class = "surveytidy_warning_rename_design_var"
  )
  expect_snapshot({
    invisible(rename(d, weight = wt))
  })
  test_invariants(result)
})

test_that("rename() updates @variables$weights after renaming the weight column", {
  d <- make_all_designs()$taylor
  d2 <- suppressWarnings(rename(d, weight = wt))
  expect_identical(d2@variables$weights, "weight")
  expect_false("wt" %in% names(d2@data))
  test_invariants(d2)
})

test_that("rename() updates @variables$strata after renaming the strata column", {
  d <- make_all_designs()$taylor
  d2 <- suppressWarnings(rename(d, stratum = strata))
  expect_identical(d2@variables$strata, "stratum")
  test_invariants(d2)
})

# ── rename() — three-way combined test (spec Section 3.7) ───────────────────

test_that("rename() handles design var + visible_vars simultaneously", {
  d <- make_all_designs()$taylor
  d2 <- select(d, y1, wt) # visible_vars = c("y1", "wt")
  d3 <- suppressWarnings(rename(d2, weight = wt))

  # 1. @data has "weight", not "wt"
  expect_true("weight" %in% names(d3@data))
  expect_false("wt" %in% names(d3@data))

  # 2. @variables$weights updated
  expect_identical(d3@variables$weights, "weight")

  # 3. visible_vars updated
  expect_identical(d3@variables$visible_vars, c("y1", "weight"))

  test_invariants(d3)
})

# ── rename() — @metadata key update ──────────────────────────────────────────

test_that("rename() updates @metadata variable_labels key", {
  d <- make_all_designs()$taylor
  d <- surveycore::set_var_label(d, y1 = "Outcome 1")
  d2 <- rename(d, outcome1 = y1)
  expect_null(d2@metadata@variable_labels[["y1"]])
  expect_identical(d2@metadata@variable_labels[["outcome1"]], "Outcome 1")
  test_invariants(d2)
})

test_that("rename() does not affect @metadata for non-renamed columns", {
  d <- make_all_designs()$taylor
  d <- surveycore::set_var_label(d, y2 = "Outcome 2")
  d2 <- rename(d, outcome1 = y1)
  expect_identical(d2@metadata@variable_labels[["y2"]], "Outcome 2")
  test_invariants(d2)
})

# ── rename() — domain column ──────────────────────────────────────────────────

test_that("rename() does not alter the domain column", {
  d <- make_all_designs()$taylor
  d2 <- filter(d, y1 > 0)
  d3 <- rename(d2, outcome1 = y1)
  expect_true(surveycore::SURVEYCORE_DOMAIN_COL %in% names(d3@data))
  expect_identical(
    d3@data[[surveycore::SURVEYCORE_DOMAIN_COL]],
    d2@data[[surveycore::SURVEYCORE_DOMAIN_COL]]
  )
  test_invariants(d3)
})

# Coverage: rename map containing ONLY the domain column. After stripping
# the domain col (silently blocked), the rename map is empty and we hit the
# domain-only warning branch in .apply_rename_map() (R/rename.R L52-58).
test_that("rename() warns and is a no-op when only the domain column is renamed", {
  d <- make_all_designs()$taylor
  d2 <- filter(d, y1 > 0) # creates the domain column
  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL

  expect_warning(
    result <- rename(d2, new_dom = !!domain_col),
    class = "surveytidy_warning_rename_design_var"
  )
  test_invariants(result)
  # Domain column is silently NOT renamed — name unchanged.
  expect_true(domain_col %in% names(result@data))
  expect_false("new_dom" %in% names(result@data))
  # Object otherwise unchanged.
  expect_identical(
    result@data[[domain_col]],
    d2@data[[domain_col]]
  )
})

# ── rename.R refactor regression ──────────────────────────────────────────────
# Verifies that extracting .apply_rename_map() introduced no behavioral change
# to rename(). All existing rename() contracts still hold after the refactor.

test_that("rename() refactor: basic rename still works for all design types", {
  designs <- make_all_designs(seed = 42)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- rename(d, outcome1 = y1)
    test_invariants(result)
    expect_true("outcome1" %in% names(result@data))
    expect_false("y1" %in% names(result@data))
  }
})

test_that("rename() refactor: @groups updated when renamed col is in @groups", {
  designs <- make_all_designs(seed = 42)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    d_grouped <- group_by(d, y1)
    result <- suppressWarnings(rename(d_grouped, outcome1 = y1))
    test_invariants(result)
    expect_identical(result@groups, "outcome1")
  }
})

# ── rename_with() — happy paths ───────────────────────────────────────────────

test_that("rename_with() renames selected columns for all design types", {
  designs <- make_all_designs(seed = 42)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- suppressWarnings(rename_with(
      d,
      toupper,
      .cols = starts_with("y")
    ))
    test_invariants(result)
    expect_true(all(c("Y1", "Y2", "Y3") %in% names(result@data)))
    expect_false(any(c("y1", "y2", "y3") %in% names(result@data)))
  }
})

test_that("rename_with() preserves all other columns when .cols is a subset", {
  designs <- make_all_designs(seed = 42)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    original_names <- names(d@data)
    non_y_names <- original_names[!grepl("^y", original_names)]
    result <- suppressWarnings(rename_with(
      d,
      toupper,
      .cols = starts_with("y")
    ))
    test_invariants(result)
    for (col in non_y_names) {
      expect_true(
        col %in% names(result@data),
        label = paste0(nm, ": ", col, " preserved")
      )
    }
  }
})

test_that("rename_with() updates visible_vars when renamed cols are visible", {
  designs <- make_all_designs(seed = 42)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    d2 <- select(d, y1, y2)
    result <- rename_with(d2, toupper, .cols = starts_with("y"))
    test_invariants(result)
    expect_identical(result@variables$visible_vars, c("Y1", "Y2"))
  }
})

test_that("rename_with() updates @metadata keys for renamed columns", {
  d <- make_all_designs(seed = 42)$taylor
  d <- surveycore::set_var_label(d, y1 = "Outcome 1")
  result <- rename_with(d, toupper, .cols = dplyr::all_of("y1"))
  test_invariants(result)
  expect_null(result@metadata@variable_labels[["y1"]])
  expect_identical(result@metadata@variable_labels[["Y1"]], "Outcome 1")
})

test_that("rename_with() supports formula and lambda .fn", {
  d <- make_all_designs(seed = 42)$taylor
  result_formula <- rename_with(
    d,
    ~ paste0(., "_new"),
    .cols = starts_with("y")
  )
  result_lambda <- rename_with(
    d,
    \(x) paste0(x, "_new"),
    .cols = starts_with("y")
  )
  test_invariants(result_formula)
  test_invariants(result_lambda)
  expect_true("y1_new" %in% names(result_formula@data))
  expect_identical(names(result_formula@data), names(result_lambda@data))
})

test_that("rename_with() supports extra ... args forwarded to .fn", {
  d <- make_all_designs(seed = 42)$taylor
  result <- rename_with(
    d,
    sub,
    .cols = starts_with("y"),
    pattern = "y",
    replacement = "z"
  )
  test_invariants(result)
  expect_true(all(c("z1", "z2", "z3") %in% names(result@data)))
})

test_that("rename_with() preserves domain column when .cols excludes it", {
  designs <- make_all_designs(seed = 42)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    d_filtered <- filter(d, y1 > 0)
    result <- rename_with(
      d_filtered,
      toupper,
      .cols = starts_with("y")
    )
    test_invariants(result)
    expect_true(surveycore::SURVEYCORE_DOMAIN_COL %in% names(result@data))
    expect_identical(
      result@data[[surveycore::SURVEYCORE_DOMAIN_COL]],
      d_filtered@data[[surveycore::SURVEYCORE_DOMAIN_COL]]
    )
  }
})

# ── rename_with() — design var warning ────────────────────────────────────────

test_that("rename_with() warns when .cols resolves to design variables", {
  designs <- make_all_designs(seed = 42)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    expect_warning(
      rename_with(d, toupper),
      class = "surveytidy_warning_rename_design_var"
    )
  }
})

test_that("rename_with() does NOT warn when .cols excludes design variables", {
  designs <- make_all_designs(seed = 42)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    expect_no_warning(
      rename_with(d, toupper, .cols = starts_with("y"))
    )
  }
})

test_that("rename_with() warns and preserves domain col when .cols includes it", {
  designs <- make_all_designs(seed = 42)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    d_filtered <- filter(d, y1 > 0)
    # everything() would include the domain column
    expect_warning(
      result <- rename_with(d_filtered, toupper),
      class = "surveytidy_warning_rename_design_var"
    )
    test_invariants(result)
    # Domain column is NOT renamed (blocked by .apply_rename_map)
    expect_true(surveycore::SURVEYCORE_DOMAIN_COL %in% names(result@data))
  }
})

# ── rename_with() — @groups staleness ─────────────────────────────────────────

test_that("rename_with() updates @groups when renamed col is in @groups", {
  designs <- make_all_designs(seed = 42)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    d_grouped <- group_by(d, y1)
    result <- rename_with(d_grouped, toupper, .cols = starts_with("y"))
    test_invariants(result)
    expect_identical(result@groups, "Y1")
  }
})

# ── rename_with() — error cases ───────────────────────────────────────────────

test_that("rename_with() errors when .fn returns non-character output", {
  d <- make_all_designs(seed = 42)$taylor

  expect_error(
    rename_with(d, \(x) seq_along(x), .cols = starts_with("y")),
    class = "surveytidy_error_rename_fn_bad_output"
  )
  expect_snapshot(
    error = TRUE,
    rename_with(d, \(x) seq_along(x), .cols = starts_with("y"))
  )
})

test_that("rename_with() errors when .fn returns wrong-length vector", {
  d <- make_all_designs(seed = 42)$taylor

  expect_error(
    rename_with(d, \(x) x[[1L]], .cols = starts_with("y")),
    class = "surveytidy_error_rename_fn_bad_output"
  )
  expect_snapshot(
    error = TRUE,
    rename_with(d, \(x) x[[1L]], .cols = starts_with("y"))
  )
})

test_that("rename_with() errors when .fn returns duplicate names", {
  d <- make_all_designs(seed = 42)$taylor

  expect_error(
    rename_with(d, \(x) rep("Y1", length(x)), .cols = starts_with("y")),
    class = "surveytidy_error_rename_fn_bad_output"
  )
  expect_snapshot(
    error = TRUE,
    rename_with(d, \(x) rep("Y1", length(x)), .cols = starts_with("y"))
  )
})

test_that("rename_with() errors when .fn returns name conflicting with existing column", {
  d <- make_all_designs(seed = 42)$taylor

  # Select only y1; .fn returns "y2" which conflicts with the existing y2 col
  expect_error(
    rename_with(d, \(x) "y2", .cols = dplyr::all_of("y1")),
    class = "surveytidy_error_rename_fn_bad_output"
  )
  expect_snapshot(
    error = TRUE,
    rename_with(d, \(x) "y2", .cols = dplyr::all_of("y1"))
  )
})
