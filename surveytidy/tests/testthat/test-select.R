# tests/testthat/test-select.R
#
# Behavioral tests for select(), relocate(), pull(), and glimpse().
# Also covers dplyr_reconstruct.survey_base() now that it lives in R/utils.R.
#
# Every test that returns a survey object calls test_invariants().
# Every error/warning class has both a class check and a snapshot.

# ── test helpers ──────────────────────────────────────────────────────────────

# For use in tests that need a design with a label set
.make_labeled_design <- function(seed = 42L) {
  designs <- make_all_designs(seed)
  d <- designs$taylor
  surveycore::set_var_label(d, y1 = "Outcome variable 1")
}

# ── select() — happy path ─────────────────────────────────────────────────────

test_that("select() returns the same survey class for all design types", {
  designs <- make_all_designs()
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- select(d, y1, y2)
    expect_true(
      inherits(result, class(d)[[1L]]),
      label = paste0(nm, ": class preserved")
    )
    test_invariants(result)
  }
})

test_that("select() retains user-selected outcome columns in @data", {
  d <- make_all_designs()$taylor
  result <- select(d, y1, y2)
  expect_true("y1" %in% names(result@data))
  expect_true("y2" %in% names(result@data))
  expect_false("y3" %in% names(result@data))
  test_invariants(result)
})

test_that("select() always preserves design variables in @data", {
  d <- make_all_designs()$taylor
  result <- select(d, y1)
  dv <- surveycore::.get_design_vars_flat(d)
  for (v in dv) {
    expect_true(
      v %in% names(result@data),
      label = paste0("design var '", v, "' present after select")
    )
  }
  test_invariants(result)
})

test_that("select() sets visible_vars to user selection, excluding hidden design vars", {
  d <- make_all_designs()$taylor
  result <- select(d, y1, y2)
  expect_identical(result@variables$visible_vars, c("y1", "y2"))
  # Weight col is not visible (user didn't select it)
  expect_false("wt" %in% result@variables$visible_vars)
  test_invariants(result)
})

test_that("select() normalises visible_vars to NULL for everything()", {
  d <- make_all_designs()$taylor
  result <- select(d, everything())
  expect_null(result@variables$visible_vars)
  test_invariants(result)
})

test_that("select() normalises visible_vars to NULL when user selects all final cols", {
  # When user selects all columns (including design vars), visible = final_cols
  d <- make_all_designs()$taylor
  all_names <- names(d@data)
  result <- select(d, all_of(all_names))
  expect_null(result@variables$visible_vars)
  test_invariants(result)
})

test_that("select() passes @groups through unchanged", {
  d <- make_all_designs()$taylor
  d2 <- group_by(d, group)
  result <- select(d2, y1)
  expect_identical(result@groups, d2@groups)
  test_invariants(result)
})

# ── select() — domain column survival (three-part assertion) ──────────────────

test_that("select() domain column survives: present in @data, values unchanged, not in visible_vars", {
  d <- make_all_designs()$taylor
  d2 <- filter(d, y1 > 0)
  d3 <- select(d2, y1, y2)

  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL

  # 1. domain col is still in @data
  expect_true(domain_col %in% names(d3@data))

  # 2. domain col values are unchanged
  expect_identical(d3@data[[domain_col]], d2@data[[domain_col]])

  # 3. domain col is NOT in visible_vars (user didn't select it)
  vv <- d3@variables$visible_vars %||% character(0)
  expect_false(domain_col %in% vv)

  test_invariants(d3)
})

test_that("select() before any filter() — domain col not yet in data", {
  d <- make_all_designs()$taylor
  result <- select(d, y1, y2)
  # No domain col should be added
  expect_false(surveycore::SURVEYCORE_DOMAIN_COL %in% names(result@data))
  test_invariants(result)
})

# ── select() — metadata ───────────────────────────────────────────────────────

test_that("select() preserves @metadata for retained columns", {
  d <- .make_labeled_design()
  result <- select(d, y1, y2)
  expect_identical(
    unname(surveycore::extract_var_label(result, y1)),
    "Outcome variable 1"
  )
  test_invariants(result)
})

test_that("select() deletes @metadata entries for dropped columns", {
  d <- .make_labeled_design()
  # Set a label on y3, then drop it
  d <- surveycore::set_var_label(d, y3 = "Binary outcome")
  result <- select(d, y1, y2)
  expect_null(result@metadata@variable_labels[["y3"]])
  test_invariants(result)
})

# ── select() — edge cases ─────────────────────────────────────────────────────

test_that("select() with negative selection removes the right column", {
  d <- make_all_designs()$taylor
  result <- select(d, -y3)
  expect_false("y3" %in% names(result@data))
  expect_true("y1" %in% names(result@data))
  expect_true("y2" %in% names(result@data))
  test_invariants(result)
})

test_that("select() with only design variables keeps visible_vars as those design vars", {
  d <- make_all_designs()$taylor
  # Select only the weight column (a design var)
  result <- select(d, wt)
  # visible = c("wt"), final_cols = all design vars; setequal is FALSE
  # so visible_vars = c("wt") — not NULL
  expect_identical(result@variables$visible_vars, "wt")
  test_invariants(result)
})

test_that("select() works for all three design types", {
  designs <- make_all_designs()
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- select(d, y1)
    expect_true("y1" %in% names(result@data))
    test_invariants(result)
  }
})

test_that("select() preserves weight column on survey_nonprob without explicit selection", {
  # Regression: .get_design_vars_flat() has no survey_nonprob branch and
  # returned character(0L), so the weight column was not protected and was
  # physically dropped, causing S7 validator to error.
  df <- data.frame(
    y1 = 1:5,
    y2 = letters[1:5],
    cal_wt = c(1.2, 0.8, 1.0, 1.5, 0.9)
  )
  d <- surveycore::as_survey_nonprob(df, weights = cal_wt)

  # Should not error even though cal_wt is not explicitly selected
  result <- select(d, y1)

  expect_true("cal_wt" %in% names(result@data))
  expect_false("y2" %in% names(result@data))
  expect_identical(result@variables$visible_vars, "y1")
  test_invariants(result)
})

test_that("dplyr_reconstruct() catches removed weight column on survey_nonprob", {
  # Regression: same gap — dplyr_reconstruct() used .get_design_vars_flat()
  # directly, which returned character(0L) for calibrated designs.
  df <- data.frame(y1 = 1:5, cal_wt = c(1.2, 0.8, 1.0, 1.5, 0.9))
  d <- surveycore::as_survey_nonprob(df, weights = cal_wt)

  bad <- d@data[, "y1", drop = FALSE] # cal_wt removed
  expect_error(
    dplyr_reconstruct(bad, d),
    class = "surveycore_error_design_var_removed"
  )
})

# ── dplyr_reconstruct() ───────────────────────────────────────────────────────

test_that("dplyr_reconstruct() errors when a design variable is removed", {
  d <- make_all_designs()$taylor
  bad <- d@data[, setdiff(names(d@data), d@variables$weights), drop = FALSE]
  expect_error(
    dplyr_reconstruct(bad, d),
    class = "surveycore_error_design_var_removed"
  )
  expect_snapshot(error = TRUE, dplyr_reconstruct(bad, d))
})

test_that("dplyr_reconstruct() cleans up visible_vars for columns removed by dplyr", {
  d <- make_all_designs()$taylor
  d2 <- select(d, y1, y2) # visible_vars = c("y1", "y2")
  # Simulate dplyr removing y2 from the data (e.g., inside an operation)
  smaller_data <- d2@data[, setdiff(names(d2@data), "y2"), drop = FALSE]
  result <- dplyr_reconstruct(smaller_data, d2)
  # visible_vars should drop y2 since it no longer exists
  expect_false("y2" %in% (result@variables$visible_vars %||% character(0)))
  test_invariants(result)
})

test_that("dplyr_reconstruct() preserves all design variables and class", {
  d <- make_all_designs()$taylor
  result <- dplyr_reconstruct(d@data, d)
  expect_true(inherits(result, class(d)[[1L]]))
  test_invariants(result)
})

# ── relocate() ────────────────────────────────────────────────────────────────

test_that("relocate() with visible_vars reorders visible_vars, not @data columns", {
  d <- make_all_designs()$taylor
  d2 <- select(d, y1, y2, y3) # visible_vars = c("y1", "y2", "y3")
  d3 <- relocate(d2, y3, .before = y1)

  # visible_vars reordered
  expect_identical(d3@variables$visible_vars, c("y3", "y1", "y2"))

  # @data column order unchanged from d2 (design vars appended by select)
  expect_identical(names(d3@data), names(d2@data))

  test_invariants(d3)
})

test_that("relocate() without visible_vars reorders @data directly", {
  d <- make_all_designs()$taylor
  # No select() call: visible_vars is NULL
  expect_null(d@variables$visible_vars)
  d2 <- relocate(d, y3, .before = y1)
  # @data was reordered
  y3_pos_before <- which(names(d@data) == "y3")
  y1_pos_before <- which(names(d@data) == "y1")
  y3_pos_after <- which(names(d2@data) == "y3")
  y1_pos_after <- which(names(d2@data) == "y1")
  expect_lt(y3_pos_after, y1_pos_after)
  test_invariants(d2)
})

test_that("relocate() passes @groups through unchanged", {
  d <- make_all_designs()$taylor
  d2 <- group_by(d, group)
  result <- relocate(d2, y3, .before = y1)
  expect_identical(result@groups, d2@groups)
})

# ── pull() ────────────────────────────────────────────────────────────────────

test_that("pull() returns a plain vector, not a survey object", {
  d <- make_all_designs()$taylor
  result <- pull(d, y1)
  expect_true(is.numeric(result))
  expect_false(inherits(result, "S7_object"))
})

test_that("pull() on a design variable returns the weight vector", {
  d <- make_all_designs()$taylor
  result <- pull(d, wt)
  expect_identical(result, d@data$wt)
})

test_that("pull() with name = argument returns a named vector", {
  d <- make_all_designs()$taylor
  result <- pull(d, y1, name = group)
  expect_named(result)
})

test_that("pull() on a nonexistent column errors (dplyr error; no class check)", {
  d <- make_all_designs()$taylor
  expect_error(pull(d, does_not_exist))
})

# ── glimpse() ─────────────────────────────────────────────────────────────────

test_that("glimpse() with visible_vars NULL shows all columns and returns invisible(x)", {
  d <- make_all_designs()$taylor
  expect_null(d@variables$visible_vars)
  result <- withVisible(glimpse(d))
  expect_false(result$visible)
  expect_true(inherits(result$value, "S7_object"))
})

test_that("glimpse() after select() shows only user-selected columns", {
  d <- make_all_designs()$taylor
  d2 <- select(d, y1, y2)
  # Capture glimpse output and check it only shows y1 and y2
  out <- capture.output(glimpse(d2))
  # y1 and y2 should appear in glimpse output
  expect_true(any(grepl("y1", out)))
  expect_true(any(grepl("y2", out)))
  # wt (design var, not selected) should NOT appear
  expect_false(any(grepl("^\\$ wt|Rows.*wt", out)))
})
