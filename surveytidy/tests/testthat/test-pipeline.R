# tests/testthat/test-pipeline.R
#
# Cross-verb integration tests.
# These tests verify that state (domain column, visible_vars, @groups,
# @metadata) is correctly propagated through multi-verb pipelines.
#
# Test index:
#   1. Domain survival: filter() |> select()                   [feature/select]
#   2. visible_vars propagation: select() |> mutate() |> rename() [feature/rename]
#   3. @groups survival: group_by() through filter/select/mutate/arrange [feature/group-by]
#   4. Filter chaining: filter(A) |> filter(B) == filter(A, B)  [feature/select]
#   5. Metadata through pipeline: select() |> rename() |> mutate() [feature/rename]
#   6. Full Phase 1 prep pipeline                               [feature/group-by]

# ── Test 1: domain survival through select() ─────────────────────────────────

test_that("pipeline 1: domain column survives filter() |> select()", {
  d <- make_all_designs()$taylor
  d2 <- filter(d, y1 > 0)
  d3 <- select(d2, y1, y2)

  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL
  test_invariants(d3)

  # Domain column is still in @data
  expect_true(domain_col %in% names(d3@data))

  # Domain column values are identical (not recomputed)
  expect_identical(d3@data[[domain_col]], d2@data[[domain_col]])

  # Domain column is not in visible_vars (user didn't select it)
  vv <- d3@variables$visible_vars %||% character(0)
  expect_false(domain_col %in% vv)
})

# ── Test 2: visible_vars propagation ─────────────────────────────────────────

test_that("pipeline 2: visible_vars propagates through select() |> mutate() |> rename()", {
  d <- make_all_designs()$taylor
  d2 <- select(d, y1, y2) # visible_vars = c("y1", "y2")
  d3 <- mutate(d2, y3_new = y2 * 2) # visible_vars = c("y1", "y2", "y3_new")
  d4 <- rename(d3, outcome1 = y1) # visible_vars = c("outcome1", "y2", "y3_new")

  test_invariants(d4)
  expect_identical(d4@variables$visible_vars, c("outcome1", "y2", "y3_new"))
})

# ── Test 3: @groups survival through multiple verbs ───────────────────────────

test_that("pipeline 3: @groups survives filter() |> select() |> mutate() |> arrange()", {
  d <- make_all_designs()$taylor
  d2 <- group_by(d, group)
  d3 <- d2 |>
    filter(y1 > 0) |>
    select(y1, y2, group) |>
    mutate(z = y1^2) |>
    arrange(y1)

  test_invariants(d3)
  expect_identical(d3@groups, "group")
})

# ── Test 4: filter chaining equals single filter ──────────────────────────────

test_that("pipeline 4: filter(A) |> filter(B) equals filter(A, B) for domain values", {
  d <- make_all_designs()$taylor
  mn <- mean(d@data$y1)

  d_chained <- d |> filter(y1 > mn) |> filter(y2 > 0)
  d_single <- d |> filter(y1 > mn, y2 > 0)

  test_invariants(d_chained)
  test_invariants(d_single)

  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL
  expect_identical(
    d_chained@data[[domain_col]],
    d_single@data[[domain_col]]
  )
})

# ── Test 5: @metadata through pipeline ───────────────────────────────────────

test_that("pipeline 5: variable label persists through select() |> rename() |> mutate()", {
  d <- make_all_designs()$taylor
  d <- surveycore::set_var_label(d, y1 = "Outcome variable 1")

  d2 <- select(d, y1, y2)
  d3 <- rename(d2, outcome1 = y1)
  d4 <- mutate(d3, z = outcome1^2)

  test_invariants(d4)
  expect_identical(
    unname(surveycore::extract_var_label(d4, outcome1)),
    "Outcome variable 1"
  )
})

# ── Test 6: full Phase 1 prep pipeline ───────────────────────────────────────

test_that("pipeline 6: full prep pipeline has correct class, invariants, @groups, and domain", {
  d <- make_all_designs()$taylor
  d2 <- d |>
    filter(y1 > 0) |>
    select(y1, y2, group) |>
    group_by(group) |>
    arrange(y1)

  test_invariants(d2)

  # Class preserved
  expect_true(
    S7::S7_inherits(d2, surveycore::survey_taylor)
  )

  # @groups set correctly
  expect_identical(d2@groups, "group")

  # Domain column present and intact
  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL
  expect_true(domain_col %in% names(d2@data))
  expect_true(is.logical(d2@data[[domain_col]]))

  # visible_vars set from select()
  expect_true("y1" %in% d2@variables$visible_vars)
  expect_true("y2" %in% d2@variables$visible_vars)
  expect_true("group" %in% d2@variables$visible_vars)
})
