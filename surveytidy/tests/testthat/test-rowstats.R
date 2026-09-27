# tests/testthat/test-rowstats.R
#
# Tests for R/rowstats.R: row_means() and row_sums().
# Follows the test plan in plans/spec-rowstats.md §VII.
#
# Sections:
#  1–11   row_means()
#  12–17  row_sums()
#  18–24  Integration tests (both functions)
#  25–26  row_means() error paths (dual pattern)
#  27–28  row_sums() error paths (dual pattern)

library(dplyr)

# ── row_means() ───────────────────────────────────────────────────────────────

# 1. row_means() — happy path: correct numeric result (all 3 design types)
test_that("row_means() returns correct row means for all design types", {
  designs <- make_all_designs(seed = 42)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- mutate(d, score = row_means(c(y1, y2)))
    test_invariants(result)
    expected <- rowMeans(cbind(d@data$y1, d@data$y2))
    expect_equal(
      result@data$score,
      expected,
      label = paste0(nm, ": row_means result matches rowMeans()")
    )
  }
})

# 2. row_means() — na.rm = FALSE: NA propagates when any value is NA
test_that("row_means() propagates NA with na.rm = FALSE (default)", {
  df <- data.frame(
    a = c(1, NA, 3),
    b = c(2, 2, 4),
    wt = c(1, 1, 1)
  )
  d <- surveycore::as_survey(df, weights = wt)
  result <- mutate(d, m = row_means(c(a, b), na.rm = FALSE))
  test_invariants(result)
  expect_equal(result@data$m, c(1.5, NA_real_, 3.5))
})

# 3. row_means() — na.rm = TRUE: partial NA gives mean of non-NA values
test_that("row_means() uses non-NA values with na.rm = TRUE", {
  df <- data.frame(
    a = c(1, NA, 3),
    b = c(2, 2, 4),
    wt = c(1, 1, 1)
  )
  d <- surveycore::as_survey(df, weights = wt)
  result <- mutate(d, m = row_means(c(a, b), na.rm = TRUE))
  test_invariants(result)
  expect_equal(result@data$m, c(1.5, 2, 3.5))
})

# 4. row_means() — na.rm = TRUE: all-NA row gives NaN
test_that("row_means() returns NaN for all-NA row with na.rm = TRUE", {
  df <- data.frame(
    a = c(1, NA),
    b = c(2, NA),
    wt = c(1, 1)
  )
  d <- surveycore::as_survey(df, weights = wt)
  result <- mutate(d, m = row_means(c(a, b), na.rm = TRUE))
  test_invariants(result)
  expect_true(is.nan(result@data$m[2]))
})

# 5. row_means() — .label stored in @metadata@variable_labels after mutate()
test_that("row_means() stores .label in @metadata@variable_labels", {
  d <- make_all_designs(seed = 42)$taylor
  result <- mutate(d, score = row_means(c(y1, y2), .label = "My score"))
  test_invariants(result)
  expect_identical(
    result@metadata@variable_labels[["score"]],
    "My score"
  )
})

# 6. row_means() — .label = NULL falls back to column name
test_that("row_means() falls back to column name when .label is NULL", {
  d <- make_all_designs(seed = 42)$taylor
  result <- mutate(d, score = row_means(c(y1, y2)))
  test_invariants(result)
  expect_identical(
    result@metadata@variable_labels[["score"]],
    "score"
  )
})

# 7. row_means() — .description stored in @metadata@transformations after mutate()
test_that("row_means() stores .description in @metadata@transformations", {
  d <- make_all_designs(seed = 42)$taylor
  result <- mutate(
    d,
    score = row_means(c(y1, y2), .description = "Average of y1 and y2")
  )
  test_invariants(result)
  expect_identical(
    result@metadata@transformations[["score"]][["description"]],
    "Average of y1 and y2"
  )
})

# 8. row_means() — source_cols in @metadata@transformations matches selected cols
test_that("row_means() records source_cols in @metadata@transformations", {
  d <- make_all_designs(seed = 42)$taylor
  result <- mutate(d, score = row_means(c(y1, y2)))
  test_invariants(result)
  tr <- result@metadata@transformations[["score"]]
  expect_identical(tr[["fn"]], "row_means")
  expect_identical(tr[["source_cols"]], c("y1", "y2"))
  expect_identical(tr[["output_type"]], "vector")
})

# 9. row_means() — tidyselect helpers (starts_with, where(is.numeric))
test_that("row_means() works with starts_with() tidyselect helper", {
  d <- make_all_designs(seed = 42)$taylor
  result <- mutate(d, score = row_means(starts_with("y")))
  test_invariants(result)
  # source_cols should include y1, y2, y3 (all cols starting with "y")
  tr <- result@metadata@transformations[["score"]]
  expect_true(all(c("y1", "y2", "y3") %in% tr[["source_cols"]]))
})

# 10. row_means() — explicit column list c(y1, y2, y3)
test_that("row_means() works with explicit c(y1, y2, y3) column list", {
  d <- make_all_designs(seed = 42)$taylor
  result <- mutate(d, score = row_means(c(y1, y2, y3)))
  test_invariants(result)
  expected <- rowMeans(cbind(d@data$y1, d@data$y2, d@data$y3))
  expect_equal(result@data$score, expected)
  tr <- result@metadata@transformations[["score"]]
  expect_identical(tr[["source_cols"]], c("y1", "y2", "y3"))
})

# 11. row_means() — bad .label / .description / na.rm →
#     surveytidy_error_rowstats_bad_arg (dual pattern)
test_that("row_means() errors on bad na.rm argument", {
  d <- make_all_designs(seed = 42)$taylor

  expect_error(
    mutate(d, score = row_means(c(y1, y2), na.rm = "yes")),
    class = "surveytidy_error_rowstats_bad_arg"
  )
  expect_snapshot(
    error = TRUE,
    mutate(d, score = row_means(c(y1, y2), na.rm = "yes"))
  )
})

test_that("row_means() errors on bad .label argument", {
  d <- make_all_designs(seed = 42)$taylor

  expect_error(
    mutate(d, score = row_means(c(y1, y2), .label = 123)),
    class = "surveytidy_error_rowstats_bad_arg"
  )
  expect_snapshot(
    error = TRUE,
    mutate(d, score = row_means(c(y1, y2), .label = 123))
  )
})

test_that("row_means() errors on bad .description argument", {
  d <- make_all_designs(seed = 42)$taylor

  expect_error(
    mutate(d, score = row_means(c(y1, y2), .description = TRUE)),
    class = "surveytidy_error_rowstats_bad_arg"
  )
  expect_snapshot(
    error = TRUE,
    mutate(d, score = row_means(c(y1, y2), .description = TRUE))
  )
})

# ── row_sums() ────────────────────────────────────────────────────────────────

# 12. row_sums() — happy path: correct numeric result (all 3 design types)
test_that("row_sums() returns correct row sums for all design types", {
  designs <- make_all_designs(seed = 42)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- mutate(d, total = row_sums(c(y1, y2)))
    test_invariants(result)
    expected <- rowSums(cbind(d@data$y1, d@data$y2))
    expect_equal(
      result@data$total,
      expected,
      label = paste0(nm, ": row_sums result matches rowSums()")
    )
  }
})

# 13. row_sums() — na.rm = FALSE: NA propagates
test_that("row_sums() propagates NA with na.rm = FALSE (default)", {
  df <- data.frame(
    a = c(1, NA, 3),
    b = c(2, 2, 4),
    wt = c(1, 1, 1)
  )
  d <- surveycore::as_survey(df, weights = wt)
  result <- mutate(d, s = row_sums(c(a, b), na.rm = FALSE))
  test_invariants(result)
  expect_equal(result@data$s, c(3, NA_real_, 7))
})

# 14. row_sums() — na.rm = TRUE: partial NA gives sum of non-NA values
test_that("row_sums() sums non-NA values with na.rm = TRUE", {
  df <- data.frame(
    a = c(1, NA, 3),
    b = c(2, 2, 4),
    wt = c(1, 1, 1)
  )
  d <- surveycore::as_survey(df, weights = wt)
  result <- mutate(d, s = row_sums(c(a, b), na.rm = TRUE))
  test_invariants(result)
  expect_equal(result@data$s, c(3, 2, 7))
})

# 15. row_sums() — na.rm = TRUE: all-NA row gives 0 (not NaN)
test_that("row_sums() returns 0 for all-NA row with na.rm = TRUE", {
  df <- data.frame(
    a = c(1, NA),
    b = c(2, NA),
    wt = c(1, 1)
  )
  d <- surveycore::as_survey(df, weights = wt)
  result <- mutate(d, s = row_sums(c(a, b), na.rm = TRUE))
  test_invariants(result)
  expect_equal(result@data$s[2], 0)
})

# 16a. row_sums() — .label stored in @metadata@variable_labels
test_that("row_sums() stores .label in @metadata@variable_labels", {
  d <- make_all_designs(seed = 42)$taylor
  result <- mutate(d, total = row_sums(c(y1, y2), .label = "My total"))
  test_invariants(result)
  expect_identical(
    result@metadata@variable_labels[["total"]],
    "My total"
  )
})

# 16b. row_sums() — .label = NULL falls back to column name
test_that("row_sums() falls back to column name when .label is NULL", {
  d <- make_all_designs(seed = 42)$taylor
  result <- mutate(d, total = row_sums(c(y1, y2)))
  test_invariants(result)
  expect_identical(
    result@metadata@variable_labels[["total"]],
    "total"
  )
})

# 16c. row_sums() — .description stored and source_cols in @metadata@transformations
test_that("row_sums() stores .description and source_cols in @metadata@transformations", {
  d <- make_all_designs(seed = 42)$taylor
  result <- mutate(
    d,
    total = row_sums(c(y1, y2), .description = "Sum of y1 and y2")
  )
  test_invariants(result)
  tr <- result@metadata@transformations[["total"]]
  expect_identical(tr[["fn"]], "row_sums")
  expect_identical(tr[["source_cols"]], c("y1", "y2"))
  expect_identical(tr[["description"]], "Sum of y1 and y2")
  expect_identical(tr[["output_type"]], "vector")
})

# 17. row_sums() — bad args → surveytidy_error_rowstats_bad_arg (dual pattern)
test_that("row_sums() errors on bad na.rm argument", {
  d <- make_all_designs(seed = 42)$taylor

  expect_error(
    mutate(d, total = row_sums(c(y1, y2), na.rm = NA)),
    class = "surveytidy_error_rowstats_bad_arg"
  )
  expect_snapshot(
    error = TRUE,
    mutate(d, total = row_sums(c(y1, y2), na.rm = NA))
  )
})

test_that("row_sums() errors on bad .label argument", {
  d <- make_all_designs(seed = 42)$taylor

  expect_error(
    mutate(d, total = row_sums(c(y1, y2), .label = c("a", "b"))),
    class = "surveytidy_error_rowstats_bad_arg"
  )
  expect_snapshot(
    error = TRUE,
    mutate(d, total = row_sums(c(y1, y2), .label = c("a", "b")))
  )
})

# ── Integration tests (both functions) ────────────────────────────────────────

# 18. Both — domain column preserved through mutate() wrapping
test_that("row_means() and row_sums() preserve domain column through mutate()", {
  d <- make_all_designs(seed = 42)$taylor
  d_filtered <- filter(d, y1 > 40)
  domain_before <- d_filtered@data[[surveycore::SURVEYCORE_DOMAIN_COL]]

  result_means <- mutate(d_filtered, score = row_means(c(y1, y2)))
  test_invariants(result_means)
  expect_identical(
    result_means@data[[surveycore::SURVEYCORE_DOMAIN_COL]],
    domain_before
  )

  result_sums <- mutate(d_filtered, total = row_sums(c(y1, y2)))
  test_invariants(result_sums)
  expect_identical(
    result_sums@data[[surveycore::SURVEYCORE_DOMAIN_COL]],
    domain_before
  )
})

# 19. Both — visible_vars updated correctly after mutate() wrapping
test_that("row_means() and row_sums() add new cols to visible_vars when set", {
  d <- make_all_designs(seed = 42)$taylor
  d2 <- select(d, y1, y2)

  result_means <- mutate(d2, score = row_means(c(y1, y2)))
  test_invariants(result_means)
  expect_true("score" %in% result_means@variables$visible_vars)

  result_sums <- mutate(d2, total = row_sums(c(y1, y2)))
  test_invariants(result_sums)
  expect_true("total" %in% result_sums@variables$visible_vars)
})

# 20. Both — single column selected (degenerate case)
test_that("row_means() and row_sums() work with a single column selected", {
  d <- make_all_designs(seed = 42)$taylor

  result_means <- mutate(d, score = row_means(y1))
  test_invariants(result_means)
  expect_equal(result_means@data$score, d@data$y1)

  result_sums <- mutate(d, total = row_sums(y1))
  test_invariants(result_sums)
  expect_equal(result_sums@data$total, d@data$y1)
})

# 21. row_means() — where(is.numeric) includes a design var →
#     surveytidy_warning_rowstats_includes_design_var
test_that("row_means() warns when .cols includes a design variable", {
  d <- make_all_designs(seed = 42)$taylor
  # wt is a numeric design variable; where(is.numeric) will match it
  expect_warning(
    result <- mutate(d, score = row_means(where(is.numeric))),
    class = "surveytidy_warning_rowstats_includes_design_var"
  )
  test_invariants(result)
})

# 22. row_sums() — explicit column list that includes a weight column →
#     surveytidy_warning_rowstats_includes_design_var
test_that("row_sums() warns when .cols explicitly includes weight column", {
  d <- make_all_designs(seed = 42)$taylor
  expect_warning(
    result <- mutate(d, total = row_sums(c(y1, wt))),
    class = "surveytidy_warning_rowstats_includes_design_var"
  )
  test_invariants(result)
})

# 23. Both — warning fires but @metadata@transformations still records all
#     source_cols (including the design var column)
test_that("design-var warning fires but metadata records all source_cols", {
  d <- make_all_designs(seed = 42)$taylor
  result <- suppressWarnings(
    mutate(d, score = row_means(c(y1, wt)))
  )
  test_invariants(result)
  tr <- result@metadata@transformations[["score"]]
  # Both y1 and wt should be in source_cols
  expect_true("y1" %in% tr[["source_cols"]])
  expect_true("wt" %in% tr[["source_cols"]])
})

# 24. Both — called outside mutate() → dplyr::pick() error propagates
test_that("row_means() called outside mutate() raises an error", {
  expect_error(row_means(c(1, 2, 3)))
})

test_that("row_sums() called outside mutate() raises an error", {
  expect_error(row_sums(c(1, 2, 3)))
})

# ── row_means() error paths (dual pattern) ────────────────────────────────────

# 25. row_means() — non-numeric column selected →
#     surveytidy_error_row_means_non_numeric (dual pattern)
test_that("row_means() errors on non-numeric column selection", {
  df <- data.frame(
    a = c(1, 2, 3),
    b = c("x", "y", "z"),
    wt = c(1, 1, 1),
    stringsAsFactors = FALSE
  )
  d <- surveycore::as_survey(df, weights = wt)

  expect_error(
    mutate(d, score = row_means(c(a, b))),
    class = "surveytidy_error_row_means_non_numeric"
  )
  expect_snapshot(
    error = TRUE,
    mutate(d, score = row_means(c(a, b)))
  )
})

# 26. row_means() — 0 columns matched →
#     surveytidy_error_row_means_zero_cols (dual pattern)
test_that("row_means() errors when .cols matches 0 columns", {
  df <- data.frame(x = c(1, 2, 3), wt = c(1, 1, 1))
  d <- surveycore::as_survey(df, weights = wt)

  expect_error(
    mutate(d, score = row_means(starts_with("z"))),
    class = "surveytidy_error_row_means_zero_cols"
  )
  expect_snapshot(
    error = TRUE,
    mutate(d, score = row_means(starts_with("z")))
  )
})

# ── row_sums() error paths (dual pattern) ─────────────────────────────────────

# 27. row_sums() — non-numeric column selected →
#     surveytidy_error_row_sums_non_numeric (dual pattern)
test_that("row_sums() errors on non-numeric column selection", {
  df <- data.frame(
    a = c(1, 2, 3),
    b = c("x", "y", "z"),
    wt = c(1, 1, 1),
    stringsAsFactors = FALSE
  )
  d <- surveycore::as_survey(df, weights = wt)

  expect_error(
    mutate(d, total = row_sums(c(a, b))),
    class = "surveytidy_error_row_sums_non_numeric"
  )
  expect_snapshot(
    error = TRUE,
    mutate(d, total = row_sums(c(a, b)))
  )
})

# 28. row_sums() — 0 columns matched →
#     surveytidy_error_row_sums_zero_cols (dual pattern)
test_that("row_sums() errors when .cols matches 0 columns", {
  df <- data.frame(x = c(1, 2, 3), wt = c(1, 1, 1))
  d <- surveycore::as_survey(df, weights = wt)

  expect_error(
    mutate(d, total = row_sums(starts_with("z"))),
    class = "surveytidy_error_row_sums_zero_cols"
  )
  expect_snapshot(
    error = TRUE,
    mutate(d, total = row_sums(starts_with("z")))
  )
})
