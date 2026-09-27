# tests/testthat/test-tidyr.R
#
# Behavioral tests for drop_na().
# Every test that returns a survey object calls test_invariants().

domain_col <- surveycore::SURVEYCORE_DOMAIN_COL

# ── drop_na() — happy path ────────────────────────────────────────────────────

test_that("drop_na() returns the same survey class for all design types", {
  designs <- make_all_designs()
  for (nm in names(designs)) {
    d <- designs[[nm]]
    d@data$y1[[1L]] <- NA_real_
    result <- drop_na(d, y1)
    expect_true(
      inherits(result, class(d)[[1L]]),
      label = paste0(nm, ": class preserved")
    )
    test_invariants(result)
  }
})

test_that("drop_na() preserves all rows (does not remove them)", {
  d <- make_all_designs()$taylor
  d@data$y1[[1L]] <- NA_real_
  result <- drop_na(d, y1)
  expect_equal(nrow(result@data), nrow(d@data))
  test_invariants(result)
})

test_that("drop_na() marks NA rows as out-of-domain in the domain column", {
  d <- make_all_designs()$taylor
  n <- nrow(d@data)
  d@data$y1[[1L]] <- NA_real_
  result <- drop_na(d, y1)
  expect_true(domain_col %in% names(result@data))
  # Row 1 is out-of-domain; all others are in-domain
  expect_false(result@data[[domain_col]][[1L]])
  expect_true(all(result@data[[domain_col]][-1L]))
})

test_that("drop_na() with no column spec marks rows with any NA out-of-domain", {
  d <- make_all_designs()$taylor
  d@data$y1[[1L]] <- NA_real_
  d@data$y2[[2L]] <- NA_real_
  result <- drop_na(d)
  expect_equal(nrow(result@data), nrow(d@data))
  expect_true(domain_col %in% names(result@data))
  # Rows 1 and 2 are out-of-domain
  expect_false(result@data[[domain_col]][[1L]])
  expect_false(result@data[[domain_col]][[2L]])
  # Other rows are in-domain
  expect_true(all(result@data[[domain_col]][-(1:2)]))
  test_invariants(result)
})

test_that("drop_na() stores !is.na() quosures in @variables$domain", {
  d <- make_all_designs()$taylor
  d@data$y1[[1L]] <- NA_real_
  result <- drop_na(d, y1)
  expect_true("domain" %in% names(result@variables))
  expect_true(length(result@variables$domain) >= 1L)
})

test_that("drop_na() does not issue surveycore_warning_physical_subset", {
  d <- make_all_designs()$taylor
  d@data$y1[[1L]] <- NA_real_
  expect_no_warning(drop_na(d, y1))
})

test_that("chained drop_na() calls AND the domain masks", {
  d <- make_all_designs()$taylor
  d@data$y1[[1L]] <- NA_real_
  d@data$y2[[2L]] <- NA_real_
  # Chain: first call marks row 1 out-of-domain; second call also marks row 2
  d2 <- drop_na(d, y1)
  d3 <- drop_na(d2, y2)
  expect_false(d3@data[[domain_col]][[1L]])
  expect_false(d3@data[[domain_col]][[2L]])
  expect_true(all(d3@data[[domain_col]][-(1:2)]))
  test_invariants(d3)
})

test_that("drop_na() passes @groups through unchanged", {
  d <- make_all_designs()$taylor
  d2 <- group_by(d, group)
  d2@data$y1[[1L]] <- NA_real_
  result <- drop_na(d2, y1)
  expect_identical(result@groups, d2@groups)
  test_invariants(result)
})

test_that("drop_na() passes visible_vars through unchanged", {
  d <- make_all_designs()$taylor
  d2 <- select(d, y1, y2)
  d2@data$y1[[1L]] <- NA_real_
  result <- drop_na(d2, y1)
  expect_identical(result@variables$visible_vars, d2@variables$visible_vars)
  test_invariants(result)
})

# ── drop_na() — empty domain ──────────────────────────────────────────────────

test_that("drop_na() warns with surveycore_warning_empty_domain when all rows have NA", {
  d <- make_all_designs()$taylor
  d@data$y1 <- NA_real_
  expect_warning(
    drop_na(d, y1),
    class = "surveycore_warning_empty_domain"
  )
})

test_that("drop_na() empty-domain warning snapshot", {
  d <- make_all_designs()$taylor
  d@data$y1 <- NA_real_
  expect_snapshot(
    drop_na(d, y1)
  )
})

test_that("drop_na() still returns a survey object when domain is empty", {
  d <- make_all_designs()$taylor
  d@data$y1 <- NA_real_
  result <- suppressWarnings(drop_na(d, y1))
  expect_true(inherits(result, "surveycore::survey_base"))
  expect_equal(nrow(result@data), nrow(d@data))
  expect_true(all(!result@data[[domain_col]]))
})
