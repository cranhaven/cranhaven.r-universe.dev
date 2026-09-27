# tests/testthat/test-arrange.R
#
# Behavioral tests for arrange() and the slice_*() family.
# Every test that returns a survey object calls test_invariants().

# ── arrange() — happy path ────────────────────────────────────────────────────

test_that("arrange() returns the same survey class for all design types", {
  designs <- make_all_designs()
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- arrange(d, y1)
    expect_true(
      inherits(result, class(d)[[1L]]),
      label = paste0(nm, ": class preserved")
    )
    test_invariants(result)
  }
})

test_that("arrange() sorts rows by the given column", {
  d <- make_all_designs()$taylor
  result <- arrange(d, y1)
  expect_true(all(diff(result@data$y1) >= 0))
  test_invariants(result)
})

test_that("arrange() descending order works", {
  d <- make_all_designs()$taylor
  result <- arrange(d, desc(y1))
  expect_true(all(diff(result@data$y1) <= 0))
  test_invariants(result)
})

test_that("arrange() passes @groups through unchanged", {
  d <- make_all_designs()$taylor
  d2 <- group_by(d, group)
  result <- arrange(d2, y1)
  expect_identical(result@groups, d2@groups)
  test_invariants(result)
})

test_that("arrange() passes visible_vars through unchanged", {
  d <- make_all_designs()$taylor
  d2 <- select(d, y1, y2)
  d3 <- arrange(d2, y1)
  expect_identical(d3@variables$visible_vars, d2@variables$visible_vars)
  test_invariants(d3)
})

# ── arrange() — exact row-association with domain column (spec Section 3.8) ──

test_that("arrange() keeps domain column row-associated with data rows", {
  d <- make_all_designs()$taylor
  d2 <- filter(d, y1 > mean(d@data$y1))
  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL

  original_domain <- d2@data[[domain_col]]
  original_y1 <- d2@data[["y1"]]

  d3 <- arrange(d2, y1)

  sorted_order <- order(original_y1)
  expect_identical(d3@data[[domain_col]], original_domain[sorted_order])
  test_invariants(d3)
})

# ── arrange() — .by_group ─────────────────────────────────────────────────────

test_that("arrange(.by_group = TRUE) sorts by @groups first", {
  d <- make_all_designs()$taylor
  d2 <- group_by(d, group)
  d3 <- arrange(d2, y1, .by_group = TRUE)

  # Rows within each group should be sorted by y1
  for (g in unique(d3@data$group)) {
    rows <- d3@data[d3@data$group == g, ]
    expect_true(
      all(diff(rows$y1) >= 0),
      label = paste0("group '", g, "' sorted by y1")
    )
  }
  test_invariants(d3)
})

# ── slice() ───────────────────────────────────────────────────────────────────

test_that("slice() physically removes rows and warns", {
  d <- make_all_designs()$taylor
  expect_warning(
    result <- slice(d, 1:10),
    class = "surveycore_warning_physical_subset"
  )
  expect_equal(nrow(result@data), 10L)
  test_invariants(result)
})

test_that("slice() errors on 0-row result", {
  d <- make_all_designs()$taylor
  expect_error(
    suppressWarnings(slice(d, integer(0))),
    class = "surveytidy_error_subset_empty_result"
  )
  expect_snapshot(
    error = TRUE,
    suppressWarnings(slice(d, integer(0)))
  )
})

# ── slice_head() ──────────────────────────────────────────────────────────────

test_that("slice_head() returns first n rows and warns", {
  d <- make_all_designs()$taylor
  expect_warning(
    result <- slice_head(d, n = 5L),
    class = "surveycore_warning_physical_subset"
  )
  expect_equal(nrow(result@data), 5L)
  expect_identical(result@data, head(d@data, 5L))
  test_invariants(result)
})

# ── slice_tail() ──────────────────────────────────────────────────────────────

test_that("slice_tail() returns last n rows and warns", {
  d <- make_all_designs()$taylor
  expect_warning(
    result <- slice_tail(d, n = 5L),
    class = "surveycore_warning_physical_subset"
  )
  expect_equal(nrow(result@data), 5L)
  test_invariants(result)
})

# ── slice_min() ───────────────────────────────────────────────────────────────

test_that("slice_min() returns rows with smallest values and warns", {
  d <- make_all_designs()$taylor
  expect_warning(
    result <- slice_min(d, order_by = y1, n = 5L),
    class = "surveycore_warning_physical_subset"
  )
  expect_equal(nrow(result@data), 5L)
  expect_true(max(result@data$y1) <= sort(d@data$y1)[[6L]])
  test_invariants(result)
})

# ── slice_max() ───────────────────────────────────────────────────────────────

test_that("slice_max() returns rows with largest values and warns", {
  d <- make_all_designs()$taylor
  expect_warning(
    result <- slice_max(d, order_by = y1, n = 5L),
    class = "surveycore_warning_physical_subset"
  )
  expect_equal(nrow(result@data), 5L)
  test_invariants(result)
})

# ── slice_sample() ────────────────────────────────────────────────────────────

test_that("slice_sample() returns n random rows and warns", {
  d <- make_all_designs()$taylor
  expect_warning(
    result <- slice_sample(d, n = 10L),
    class = "surveycore_warning_physical_subset"
  )
  expect_equal(nrow(result@data), 10L)
  test_invariants(result)
})

test_that("slice_sample() with weight_by = issues additional warning", {
  d <- make_all_designs()$taylor
  # Capture all warnings and check both classes are present
  warns <- character(0)
  suppressWarnings(
    withCallingHandlers(
      slice_sample(d, n = 5L, weight_by = y1),
      warning = function(w) {
        warns <<- c(warns, class(w)[[1L]])
        invokeRestart("muffleWarning")
      }
    )
  )
  expect_true("surveycore_warning_physical_subset" %in% warns)
  expect_true("surveytidy_warning_slice_sample_weight_by" %in% warns)
})

# ── slice_*() — 0-row error for all variants ─────────────────────────────────

test_that("slice_head(n=0) errors with surveytidy_error_subset_empty_result", {
  d <- make_all_designs()$taylor
  expect_error(
    suppressWarnings(slice_head(d, n = 0L)),
    class = "surveytidy_error_subset_empty_result"
  )
})

test_that("slice_tail(n=0) errors with surveytidy_error_subset_empty_result", {
  d <- make_all_designs()$taylor
  expect_error(
    suppressWarnings(slice_tail(d, n = 0L)),
    class = "surveytidy_error_subset_empty_result"
  )
})

# ── slice_*() — all three design types ───────────────────────────────────────

test_that("slice_head() works for all design types", {
  designs <- make_all_designs()
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- suppressWarnings(slice_head(d, n = 5L))
    expect_equal(nrow(result@data), 5L)
    test_invariants(result)
  }
})
