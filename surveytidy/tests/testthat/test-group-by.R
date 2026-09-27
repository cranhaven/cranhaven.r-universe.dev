# tests/testthat/test-group-by.R
#
# Behavioral tests for group_by() and ungroup().
# Every test that returns a survey object calls test_invariants().

# ── group_by() — happy path ───────────────────────────────────────────────────

test_that("group_by() returns the same survey class for all design types", {
  designs <- make_all_designs()
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- group_by(d, group)
    expect_true(
      inherits(result, class(d)[[1L]]),
      label = paste0(nm, ": class preserved")
    )
    test_invariants(result)
  }
})

test_that("group_by() sets @groups to the named column(s)", {
  d <- make_all_designs()$taylor
  result <- group_by(d, group)
  expect_identical(result@groups, "group")
  test_invariants(result)
})

test_that("group_by() sets @groups for multiple columns", {
  d <- make_all_designs()$taylor
  result <- group_by(d, group, strata)
  expect_identical(result@groups, c("group", "strata"))
  test_invariants(result)
})

test_that("group_by(.add = TRUE) adds to existing groups", {
  d <- make_all_designs()$taylor
  d2 <- group_by(d, group)
  d3 <- group_by(d2, strata, .add = TRUE)
  expect_true("group" %in% d3@groups)
  expect_true("strata" %in% d3@groups)
  test_invariants(d3)
})

test_that("group_by(.add = FALSE) replaces existing groups", {
  d <- make_all_designs()$taylor
  d2 <- group_by(d, group)
  d3 <- group_by(d2, strata, .add = FALSE)
  expect_false("group" %in% d3@groups)
  expect_identical(d3@groups, "strata")
  test_invariants(d3)
})

test_that("group_by() does NOT add grouped_df attribute to @data", {
  d <- make_all_designs()$taylor
  result <- group_by(d, group)
  expect_false(inherits(result@data, "grouped_df"))
})

test_that("group_by() accepts computed expressions", {
  d <- make_all_designs()$taylor
  result <- group_by(d, above_median = y1 > median(y1))
  expect_true("above_median" %in% result@groups)
  test_invariants(result)
})

test_that("group_by() passes visible_vars through unchanged", {
  d <- make_all_designs()$taylor
  # Include group in the select so group_by can find it in @data
  d2 <- select(d, y1, y2, group)
  d3 <- group_by(d2, group)
  expect_identical(d3@variables$visible_vars, d2@variables$visible_vars)
  test_invariants(d3)
})

# ── ungroup() — happy path ────────────────────────────────────────────────────

test_that("ungroup() with no args removes all groups", {
  d <- make_all_designs()$taylor
  d2 <- group_by(d, group, strata)
  d3 <- ungroup(d2)
  expect_identical(d3@groups, character(0))
  test_invariants(d3)
})

test_that("ungroup() is a no-op on an already-ungrouped object", {
  d <- make_all_designs()$taylor
  result <- ungroup(d)
  expect_identical(result@groups, character(0))
  test_invariants(result)
})

test_that("ungroup(x, col) removes only the specified column from @groups", {
  d <- make_all_designs()$taylor
  d2 <- group_by(d, group, strata)
  d3 <- ungroup(d2, group)
  expect_false("group" %in% d3@groups)
  expect_true("strata" %in% d3@groups)
  test_invariants(d3)
})

test_that("ungroup() preserves visible_vars", {
  d <- make_all_designs()$taylor
  # Include group in the select so group_by can find it in @data
  d2 <- select(d, y1, y2, group)
  d3 <- group_by(d2, group)
  d4 <- ungroup(d3)
  expect_identical(d4@variables$visible_vars, d2@variables$visible_vars)
  test_invariants(d4)
})

# ── group_by() — all design types ────────────────────────────────────────────

test_that("group_by() + ungroup() round-trips correctly for all design types", {
  designs <- make_all_designs()
  for (nm in names(designs)) {
    d <- designs[[nm]]
    d2 <- group_by(d, group)
    d3 <- ungroup(d2)
    expect_identical(d3@groups, character(0))
    test_invariants(d3)
  }
})
