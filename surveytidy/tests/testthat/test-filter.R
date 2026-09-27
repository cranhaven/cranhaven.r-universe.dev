# tests/testthat/test-filter.R
#
# Behavioral tests for filter.survey_base(), dplyr_reconstruct.survey_base(),
# and subset.survey_base(). All three are defined in R/01-filter.R.
#
# test-wiring.R covers dispatch only (does dplyr route to our method?).
# This file covers what the methods actually do.

# ── filter() — happy path ─────────────────────────────────────────────────────

test_that("filter() passes test_invariants() for all design types", {
  skip_if_not_installed("dplyr")
  designs <- make_all_designs()
  for (nm in names(designs)) {
    result <- dplyr::filter(designs[[nm]], y1 > 0)
    test_invariants(result)
  }
})

test_that("filter() does not remove any rows from @data for all design types", {
  skip_if_not_installed("dplyr")
  designs <- make_all_designs()
  for (nm in names(designs)) {
    result <- dplyr::filter(designs[[nm]], y1 > 0)
    expect_equal(nrow(result@data), nrow(designs[[nm]]@data))
  }
})

test_that("filter() sets domain column to values matching the condition", {
  skip_if_not_installed("dplyr")
  d <- make_all_designs()$taylor
  result <- dplyr::filter(d, y1 > 0)
  expect_identical(
    result@data[[surveycore::SURVEYCORE_DOMAIN_COL]],
    d@data$y1 > 0
  )
})

test_that("filter() with no conditions sets all rows in-domain", {
  skip_if_not_installed("dplyr")
  d <- make_all_designs()$taylor
  result <- dplyr::filter(d)
  domain <- result@data[[surveycore::SURVEYCORE_DOMAIN_COL]]
  expect_true(all(domain))
})

test_that("filter() with multiple conditions applies logical AND within one call", {
  skip_if_not_installed("dplyr")
  d <- make_all_designs()$taylor
  result <- dplyr::filter(d, y1 > 0, y2 > 0)
  expected <- d@data$y1 > 0 & d@data$y2 > 0
  expect_identical(
    result@data[[surveycore::SURVEYCORE_DOMAIN_COL]],
    expected
  )
})

test_that("chained filter() calls AND the domain columns correctly", {
  skip_if_not_installed("dplyr")
  d <- make_all_designs()$taylor
  chained <- d |> dplyr::filter(y1 > 0) |> dplyr::filter(y2 > 0)
  combined <- dplyr::filter(d, y1 > 0, y2 > 0)
  expect_identical(
    chained@data[[surveycore::SURVEYCORE_DOMAIN_COL]],
    combined@data[[surveycore::SURVEYCORE_DOMAIN_COL]]
  )
})

test_that("chained filter() calls accumulate all conditions in @variables$domain", {
  skip_if_not_installed("dplyr")
  d <- make_all_designs()$taylor
  result <- d |> dplyr::filter(y1 > 0) |> dplyr::filter(y2 > 0)
  expect_length(result@variables$domain, 2L)
})

test_that("filter() maps NA conditions to FALSE (outside domain)", {
  skip_if_not_installed("dplyr")
  d <- make_all_designs()$taylor
  d@data$y1[1] <- NA_real_
  result <- dplyr::filter(d, y1 > 0)
  expect_false(result@data[[surveycore::SURVEYCORE_DOMAIN_COL]][[1L]])
})

test_that("filter() preserves @groups", {
  skip_if_not_installed("dplyr")
  d <- make_all_designs()$taylor
  d@groups <- c("group")
  result <- dplyr::filter(d, y1 > 0)
  expect_identical(result@groups, c("group"))
})

test_that("filter() preserves @metadata variable labels", {
  skip_if_not_installed("dplyr")
  d <- make_all_designs()$taylor
  d@metadata@variable_labels[["y1"]] <- "Outcome variable 1"
  result <- dplyr::filter(d, y1 > 0)
  expect_identical(
    result@metadata@variable_labels[["y1"]],
    "Outcome variable 1"
  )
})


# ── filter() — if_any() / if_all() ───────────────────────────────────────────

test_that("filter() supports if_any() for multi-column domain selection", {
  skip_if_not_installed("dplyr")
  designs <- make_all_designs()
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- dplyr::filter(d, dplyr::if_any(c(y1, y2), ~ .x > 60))
    test_invariants(result)
    expected <- d@data$y1 > 60 | d@data$y2 > 60
    expect_identical(result@data[[surveycore::SURVEYCORE_DOMAIN_COL]], expected)
  }
})

test_that("filter() supports if_all() for multi-column domain selection", {
  skip_if_not_installed("dplyr")
  designs <- make_all_designs()
  for (nm in names(designs)) {
    d <- designs[[nm]]
    # suppressWarnings: if_all() may yield an empty domain for some design
    # fixtures; that behaviour is tested separately.
    result <- suppressWarnings(dplyr::filter(
      d,
      dplyr::if_all(c(y1, y2), ~ .x > 40)
    ))
    test_invariants(result)
    expected <- d@data$y1 > 40 & d@data$y2 > 40
    expect_identical(result@data[[surveycore::SURVEYCORE_DOMAIN_COL]], expected)
  }
})

test_that("filter() if_any() treats NA columns as FALSE (outside domain)", {
  skip_if_not_installed("dplyr")
  df <- data.frame(
    a = c(NA_real_, 2, 3),
    b = c(1, NA_real_, 3),
    wt = c(1, 1, 1)
  )
  d <- surveycore::as_survey(df, weights = wt)
  # if_any: row 1 has NA in a, but b=1>0 → in-domain
  #         row 2 has a=2>0, but b is NA → in-domain
  #         row 3: both > 0 → in-domain
  result <- dplyr::filter(d, dplyr::if_any(c(a, b), ~ .x > 0))
  expect_identical(
    result@data[[surveycore::SURVEYCORE_DOMAIN_COL]],
    c(TRUE, TRUE, TRUE)
  )
})

test_that("chained filter() with if_any() correctly ANDs the domains", {
  skip_if_not_installed("dplyr")
  d <- make_all_designs()$taylor
  result <- d |>
    dplyr::filter(dplyr::if_any(c(y1, y2), ~ .x > 60)) |>
    dplyr::filter(group == "A")
  expected <- (d@data$y1 > 60 | d@data$y2 > 60) & (d@data$group == "A")
  expect_identical(result@data[[surveycore::SURVEYCORE_DOMAIN_COL]], expected)
})


# ── filter() — error paths ────────────────────────────────────────────────────

test_that("filter() rejects .by argument with typed error", {
  skip_if_not_installed("dplyr")
  d <- make_all_designs()$taylor
  expect_error(
    dplyr::filter(d, y1 > 0, .by = "group"),
    class = "surveytidy_error_filter_by_unsupported"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::filter(d, y1 > 0, .by = "group")
  )
})

test_that("filter() errors when a condition is non-logical", {
  skip_if_not_installed("dplyr")
  d <- make_all_designs()$taylor
  # y1 is numeric — missing comparison operator; dplyr raises the error
  expect_error(dplyr::filter(d, y1))
  expect_snapshot(error = TRUE, dplyr::filter(d, y1))
})


# ── filter() — edge cases ─────────────────────────────────────────────────────

test_that("filter() warns and marks all rows out-of-domain when no rows match", {
  skip_if_not_installed("dplyr")
  d <- make_all_designs()$taylor
  expect_warning(
    result <- dplyr::filter(d, y1 > 1e9),
    class = "surveycore_warning_empty_domain"
  )
  expect_false(any(result@data[[surveycore::SURVEYCORE_DOMAIN_COL]]))
  # Snapshot captures the warning message text
  expect_snapshot({
    invisible(dplyr::filter(d, y1 > 1e9))
  })
})


# ── filter_out() — happy path ─────────────────────────────────────────────────

test_that("filter_out() passes test_invariants() for all design types", {
  designs <- make_all_designs()
  for (nm in names(designs)) {
    result <- dplyr::filter_out(designs[[nm]], y1 > 55)
    test_invariants(result)
  }
})

test_that("filter_out() does not remove any rows from @data", {
  designs <- make_all_designs()
  for (nm in names(designs)) {
    result <- dplyr::filter_out(designs[[nm]], y1 > 55)
    expect_equal(nrow(result@data), nrow(designs[[nm]]@data))
  }
})

test_that("filter_out() sets domain column to the negation of the condition", {
  d <- make_all_designs()$taylor
  result <- dplyr::filter_out(d, y1 > 55)
  expect_identical(
    result@data[[surveycore::SURVEYCORE_DOMAIN_COL]],
    !(d@data$y1 > 55)
  )
})

test_that("filter_out() with no conditions keeps all rows in-domain", {
  d <- make_all_designs()$taylor
  result <- dplyr::filter_out(d)
  domain <- result@data[[surveycore::SURVEYCORE_DOMAIN_COL]]
  expect_true(all(domain))
})

test_that("filter_out() with multiple conditions ANDs before negating", {
  d <- make_all_designs()$taylor
  result <- dplyr::filter_out(d, y1 > 55, y2 > 0)
  # Rows where BOTH conditions hold are excluded
  expected <- !(d@data$y1 > 55 & d@data$y2 > 0)
  expect_identical(
    result@data[[surveycore::SURVEYCORE_DOMAIN_COL]],
    expected
  )
})

test_that("filter_out() is equivalent to filter() with negated condition", {
  d <- make_all_designs()$taylor
  result_out <- dplyr::filter_out(d, y1 > 55)
  result_inv <- dplyr::filter(d, !(y1 > 55))
  expect_identical(
    result_out@data[[surveycore::SURVEYCORE_DOMAIN_COL]],
    result_inv@data[[surveycore::SURVEYCORE_DOMAIN_COL]]
  )
})

test_that("filter_out() after filter() ANDs the domains correctly", {
  d <- make_all_designs()$taylor
  result <- d |> dplyr::filter(y1 > 45) |> dplyr::filter_out(y2 > 0)
  expected <- (d@data$y1 > 45) & !(d@data$y2 > 0)
  expect_identical(
    result@data[[surveycore::SURVEYCORE_DOMAIN_COL]],
    expected
  )
})

test_that("filter() after filter_out() ANDs the domains correctly", {
  d <- make_all_designs()$taylor
  result <- d |> dplyr::filter_out(y1 > 55) |> dplyr::filter(y2 > 0)
  expected <- !(d@data$y1 > 55) & (d@data$y2 > 0)
  expect_identical(
    result@data[[surveycore::SURVEYCORE_DOMAIN_COL]],
    expected
  )
})

test_that("filter_out() maps NA conditions to FALSE (row stays in-domain)", {
  d <- make_all_designs()$taylor
  d@data$y1[1] <- NA_real_
  result <- dplyr::filter_out(d, y1 > 55)
  # Row 1: NA condition → treated as FALSE → NOT excluded → stays in-domain
  expect_true(result@data[[surveycore::SURVEYCORE_DOMAIN_COL]][[1L]])
})

test_that("filter_out() preserves @groups", {
  d <- make_all_designs()$taylor
  d@groups <- c("group")
  result <- dplyr::filter_out(d, y1 > 55)
  expect_identical(result@groups, c("group"))
})

test_that("filter_out() preserves @metadata variable labels", {
  d <- make_all_designs()$taylor
  d@metadata@variable_labels[["y1"]] <- "Outcome variable 1"
  result <- dplyr::filter_out(d, y1 > 55)
  expect_identical(
    result@metadata@variable_labels[["y1"]],
    "Outcome variable 1"
  )
})


# ── filter_out() — if_any() / if_all() ───────────────────────────────────────

test_that("filter_out() supports if_any() for multi-column exclusion", {
  designs <- make_all_designs()
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- dplyr::filter_out(d, dplyr::if_any(c(y1, y2), ~ .x > 80))
    test_invariants(result)
    # Rows where ANY of y1/y2 > 80 are excluded
    expected <- !(d@data$y1 > 80 | d@data$y2 > 80)
    expect_identical(result@data[[surveycore::SURVEYCORE_DOMAIN_COL]], expected)
  }
})

test_that("filter_out() supports if_all() for multi-column exclusion", {
  designs <- make_all_designs()
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- dplyr::filter_out(d, dplyr::if_all(c(y1, y2), ~ .x > 40))
    test_invariants(result)
    # Rows where ALL of y1 AND y2 > 40 are excluded
    expected <- !(d@data$y1 > 40 & d@data$y2 > 40)
    expect_identical(result@data[[surveycore::SURVEYCORE_DOMAIN_COL]], expected)
  }
})


# ── filter_out() — error paths ────────────────────────────────────────────────

test_that("filter_out() rejects .by argument with typed error", {
  d <- make_all_designs()$taylor
  expect_error(
    dplyr::filter_out(d, y1 > 55, .by = "group"),
    class = "surveytidy_error_filter_by_unsupported"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::filter_out(d, y1 > 55, .by = "group")
  )
})

test_that("filter_out() errors when a condition is non-logical", {
  d <- make_all_designs()$taylor
  # y1 is numeric — missing comparison operator; dplyr raises the error
  expect_error(dplyr::filter_out(d, y1))
  expect_snapshot(error = TRUE, dplyr::filter_out(d, y1))
})


# ── filter_out() — edge cases ─────────────────────────────────────────────────

test_that("filter_out() warns when all rows are excluded", {
  d <- make_all_designs()$taylor
  expect_warning(
    result <- dplyr::filter_out(d, y1 > -1e9),
    class = "surveycore_warning_empty_domain"
  )
  expect_false(any(result@data[[surveycore::SURVEYCORE_DOMAIN_COL]]))
  expect_snapshot({
    invisible(dplyr::filter_out(d, y1 > -1e9))
  })
})


# ── dplyr_reconstruct() ───────────────────────────────────────────────────────

test_that("dplyr_reconstruct() preserves survey class for all design types", {
  skip_if_not_installed("dplyr")
  designs <- make_all_designs()
  for (nm in names(designs)) {
    result <- dplyr::dplyr_reconstruct(designs[[nm]]@data, designs[[nm]])
    expect_true(S7::S7_inherits(result, surveycore::survey_base))
    test_invariants(result)
  }
})

test_that("dplyr_reconstruct() errors when a design variable is removed", {
  skip_if_not_installed("dplyr")
  d <- make_all_designs()$taylor
  wt_col <- d@variables$weights
  data_no_wt <- d@data[, setdiff(names(d@data), wt_col), drop = FALSE]
  expect_error(
    dplyr::dplyr_reconstruct(data_no_wt, d),
    class = "surveycore_error_design_var_removed"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::dplyr_reconstruct(data_no_wt, d)
  )
})


# ── subset() — happy path ─────────────────────────────────────────────────────

test_that("subset() emits surveycore_warning_physical_subset for all design types", {
  designs <- make_all_designs()
  for (nm in names(designs)) {
    expect_warning(
      subset(designs[[nm]], y1 > 0),
      class = "surveycore_warning_physical_subset"
    )
  }
})

test_that("subset() warning snapshot matches expected message", {
  d <- make_all_designs()$taylor
  expect_snapshot({
    invisible(subset(d, y1 > 0))
  })
})

test_that("subset() physically removes non-matching rows from @data", {
  d <- make_all_designs()$taylor
  expected_rows <- sum(d@data$y1 > 0)
  result <- suppressWarnings(subset(d, y1 > 0))
  expect_equal(nrow(result@data), expected_rows)
})

test_that("subset() preserves all design variables in @data", {
  d <- make_all_designs()$taylor
  design_vars <- surveycore::.get_design_vars_flat(d)
  result <- suppressWarnings(subset(d, y1 > 0))
  for (v in design_vars) {
    expect_true(v %in% names(result@data), label = paste0("'", v, "' in @data"))
  }
})

test_that("subset() result passes test_invariants() for all design types", {
  designs <- make_all_designs()
  for (nm in names(designs)) {
    result <- suppressWarnings(subset(designs[[nm]], y1 > 0))
    test_invariants(result)
  }
})


# ── subset() — error paths ────────────────────────────────────────────────────

test_that("subset() errors when condition matches 0 rows", {
  d <- make_all_designs()$taylor
  # suppressWarnings isolates the error from the physical_subset warning
  expect_error(
    suppressWarnings(subset(d, y1 > 1e9)),
    class = "surveytidy_error_subset_empty_result"
  )
  # Full snapshot captures both warning and error (complete user experience)
  expect_snapshot(
    error = TRUE,
    subset(d, y1 > 1e9)
  )
})


# ── coverage closers: row_id_col collision ──────────────────────────────────

# Covers filter.R L150: when "..surveytidy_filter_id.." already exists in
# @data, the helper dot-pads the name in a while-loop until it finds a free
# name. We pre-add the colliding column via mutate() (it is not a protected
# col) and then call filter().
test_that("filter() resolves row_id_col collision when sentinel name already exists", {
  d <- make_all_designs()$taylor
  # Pre-add the default sentinel column. mutate() preserves arbitrary names.
  d2 <- dplyr::mutate(d, "..surveytidy_filter_id.." = seq_len(nrow(d@data)))
  expect_true("..surveytidy_filter_id.." %in% names(d2@data))

  # filter() must succeed despite the collision (the sentinel name is
  # padded to "...surveytidy_filter_id..." inside filter()).
  result <- dplyr::filter(d2, y1 > 0)
  test_invariants(result)

  # Pre-existing user column is preserved untouched
  expect_identical(
    result@data[["..surveytidy_filter_id.."]],
    seq_len(nrow(d@data))
  )
  # Domain mask was computed correctly
  expect_identical(
    result@data[[surveycore::SURVEYCORE_DOMAIN_COL]],
    d@data$y1 > 0
  )
})

# Covers filter.R L264: identical collision-resolution path inside filter_out()
test_that("filter_out() resolves row_id_col collision when sentinel name already exists", {
  d <- make_all_designs()$taylor
  d2 <- dplyr::mutate(d, "..surveytidy_filter_id.." = seq_len(nrow(d@data)))
  expect_true("..surveytidy_filter_id.." %in% names(d2@data))

  result <- dplyr::filter_out(d2, y1 > 55)
  test_invariants(result)

  # Pre-existing user column is preserved untouched
  expect_identical(
    result@data[["..surveytidy_filter_id.."]],
    seq_len(nrow(d@data))
  )
  # Domain mask: rows NOT matching the exclusion remain in-domain
  expect_identical(
    result@data[[surveycore::SURVEYCORE_DOMAIN_COL]],
    !(d@data$y1 > 55)
  )
})
