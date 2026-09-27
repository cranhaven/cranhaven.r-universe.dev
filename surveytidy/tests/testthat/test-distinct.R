# tests/testthat/test-distinct.R
#
# Behavioral tests for distinct.survey_base().
# Covers all six sections from spec §VI.2.
# Every test block calls test_invariants() as the first assertion.

# ── Section 1: Happy paths ────────────────────────────────────────────────────

test_that("distinct() reduces rows when duplicates exist (all three designs)", {
  for (nm in c("taylor", "replicate", "twophase")) {
    # Build a minimal design with clearly duplicated non-design rows
    df_dup <- data.frame(
      psu = paste0("psu_", rep(1:5, each = 4)),
      strata = rep(c("s1", "s2"), each = 10),
      fpc = rep(c(200, 200), each = 10),
      wt = rep(10, 20),
      y1 = rep(c(1, 2, 3, 4, 5), each = 4), # 5 unique values, 4 rows each
      y2 = rep(0, 20),
      y3 = rep(0L, 20),
      group = rep("A", 20),
      stringsAsFactors = FALSE
    )

    if (nm == "replicate") {
      df_dup$repwt_1 <- df_dup$wt * 1.1
      df_dup$repwt_2 <- df_dup$wt * 0.9
      d <- surveycore::as_survey_replicate(
        df_dup,
        weights = wt,
        repweights = tidyselect::all_of(c("repwt_1", "repwt_2")),
        type = "BRR"
      )
    } else if (nm == "twophase") {
      df_dup$phase2_ind <- rep(c(TRUE, FALSE), 10)
      phase1 <- surveycore::as_survey(
        df_dup,
        ids = psu,
        weights = wt,
        strata = strata,
        fpc = fpc,
        nest = TRUE
      )
      d <- suppressWarnings(
        surveycore::as_survey_twophase(phase1, subset = phase2_ind)
      )
    } else {
      d <- surveycore::as_survey(
        df_dup,
        ids = psu,
        weights = wt,
        strata = strata,
        fpc = fpc,
        nest = TRUE
      )
    }

    result <- suppressWarnings(distinct(d))
    test_invariants(result)
    expect_lt(
      nrow(result@data),
      nrow(d@data),
      label = paste0(nm, ": row count reduced")
    )
  }
})

test_that("distinct() on a design with no duplicates returns the same row count (all three designs)", {
  designs <- make_all_designs(seed = 42)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- suppressWarnings(distinct(d))
    test_invariants(result)
    expect_equal(
      nrow(result@data),
      nrow(d@data),
      label = paste0(nm, ": row count unchanged when no duplicates")
    )
  }
})

test_that("distinct(d, col1, col2) deduplicates by specified columns; all columns retained (all three designs)", {
  designs <- make_all_designs(seed = 42)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    original_col_names <- names(d@data)

    result <- suppressWarnings(distinct(d, group, y3))
    test_invariants(result)

    # All original columns retained
    expect_identical(
      names(result@data),
      original_col_names,
      label = paste0(nm, ": all columns retained")
    )
    # At most 6 rows (3 groups × 2 y3 values)
    expect_lte(
      nrow(result@data),
      6L,
      label = paste0(nm, ": deduplicated to at most 6 rows")
    )
  }
})

# ── Section 2: Warning is always issued ──────────────────────────────────────

test_that("distinct() always issues surveycore_warning_physical_subset (all three designs)", {
  designs <- make_all_designs(seed = 42)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    expect_warning(
      distinct(d),
      class = "surveycore_warning_physical_subset",
      label = paste0(nm, ": physical_subset warning issued")
    )
    expect_warning(
      distinct(d, y1),
      class = "surveycore_warning_physical_subset",
      label = paste0(nm, ": physical_subset warning issued with ... args")
    )
  }
})

test_that("distinct() issues surveytidy_warning_distinct_design_var when ... includes a design variable (taylor)", {
  d <- make_all_designs(seed = 42)$taylor
  # Both warnings fire: physical_subset first, then design_var.
  # Nested expect_warning() catches each in turn.
  expect_warning(
    expect_warning(
      distinct(d, strata),
      class = "surveycore_warning_physical_subset"
    ),
    class = "surveytidy_warning_distinct_design_var"
  )
})

test_that("distinct() issues surveytidy_warning_distinct_design_var when ... includes a design variable (replicate)", {
  d <- make_all_designs(seed = 42)$replicate
  # wt is a protected column for all design types
  expect_warning(
    expect_warning(
      distinct(d, wt),
      class = "surveycore_warning_physical_subset"
    ),
    class = "surveytidy_warning_distinct_design_var"
  )
})

test_that("distinct() issues surveytidy_warning_distinct_design_var when ... includes a design variable (twophase)", {
  d <- make_all_designs(seed = 42)$twophase
  expect_warning(
    expect_warning(
      distinct(d, wt),
      class = "surveycore_warning_physical_subset"
    ),
    class = "surveytidy_warning_distinct_design_var"
  )
})

# ── Section 3: Column contract ────────────────────────────────────────────────

test_that("distinct() retains all @data column names (all three designs)", {
  designs <- make_all_designs(seed = 42)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    original_cols <- names(d@data)
    result <- suppressWarnings(distinct(d))
    test_invariants(result)
    expect_identical(
      names(result@data),
      original_cols,
      label = paste0(nm, ": @data column names unchanged")
    )
  }
})

test_that("distinct() does not update visible_vars (all three designs)", {
  designs <- make_all_designs(seed = 42)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    # Set visible_vars via select() first
    d_sel <- select(d, y1, y2)
    result <- suppressWarnings(distinct(d_sel))
    test_invariants(result)
    expect_identical(
      result@variables$visible_vars,
      d_sel@variables$visible_vars,
      label = paste0(nm, ": visible_vars unchanged after distinct()")
    )
  }
})

test_that("distinct() leaves @metadata unchanged (all three designs)", {
  designs <- make_all_designs(seed = 42)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- suppressWarnings(distinct(d))
    test_invariants(result)
    expect_identical(
      result@metadata,
      d@metadata,
      label = paste0(nm, ": @metadata identical after distinct()")
    )
  }
})

# ── Section 4: Domain preservation ───────────────────────────────────────────

test_that("distinct() preserves the domain column in @data (all three designs)", {
  designs <- make_all_designs(seed = 42)
  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL
  for (nm in names(designs)) {
    d <- designs[[nm]]
    # Create a filtered design so domain column exists
    d_filtered <- filter(d, y1 > 0)
    result <- suppressWarnings(distinct(d_filtered))
    test_invariants(result)
    expect_true(
      domain_col %in% names(result@data),
      label = paste0(nm, ": domain column present after distinct()")
    )
    expect_true(
      is.logical(result@data[[domain_col]]),
      label = paste0(nm, ": domain column is logical")
    )
  }
})

test_that("distinct() on a filtered design preserves the domain column without modifying it (all three designs)", {
  designs <- make_all_designs(seed = 42)
  domain_col <- surveycore::SURVEYCORE_DOMAIN_COL
  for (nm in names(designs)) {
    d <- designs[[nm]]
    d_filtered <- filter(d, y1 > mean(d@data$y1))

    # distinct() on non-domain columns should preserve existing domain values
    result <- suppressWarnings(distinct(d_filtered, group))
    test_invariants(result)
    expect_true(
      domain_col %in% names(result@data),
      label = paste0(nm, ": domain column present in result")
    )
  }
})

# ── Section 5: @groups propagation ───────────────────────────────────────────

test_that("distinct() passes @groups through unchanged (all three designs)", {
  designs <- make_all_designs(seed = 42)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    d_grouped <- group_by(d, group)
    result <- suppressWarnings(distinct(d_grouped))
    test_invariants(result)
    expect_identical(
      result@groups,
      d_grouped@groups,
      label = paste0(nm, ": @groups unchanged after distinct()")
    )
  }
})

test_that("distinct() on an ungrouped design leaves @groups as character(0) (all three designs)", {
  designs <- make_all_designs(seed = 42)
  for (nm in names(designs)) {
    d <- designs[[nm]]
    result <- suppressWarnings(distinct(d))
    test_invariants(result)
    expect_identical(
      result@groups,
      character(0),
      label = paste0(nm, ": @groups is character(0) for ungrouped design")
    )
  }
})

# ── Section 6: Edge cases ─────────────────────────────────────────────────────

test_that("distinct() on duplicate-row data collapses to 1 row and issues the physical-subset warning", {
  # Use 2 identical rows so as_survey() accepts (>=2 rows required), then

  # distinct() collapses them to 1 row.
  df_dup <- data.frame(
    psu = c("psu_1", "psu_1"),
    strata = c("s1", "s1"),
    fpc = c(100, 100),
    wt = c(10, 10),
    y1 = c(42, 42),
    y2 = c(0, 0),
    y3 = c(0L, 0L),
    group = c("A", "A"),
    stringsAsFactors = FALSE
  )
  d_dup <- suppressWarnings(
    surveycore::as_survey(
      df_dup,
      ids = psu,
      weights = wt,
      strata = strata,
      fpc = fpc,
      nest = TRUE
    )
  )

  expect_warning(
    result <- distinct(d_dup),
    class = "surveycore_warning_physical_subset"
  )
  test_invariants(result)
  expect_equal(nrow(result@data), 1L)
})

test_that("distinct() on all-identical non-design rows returns exactly 1 row", {
  df_dup <- data.frame(
    psu = paste0("psu_", 1:5),
    strata = rep("s1", 5),
    fpc = rep(100, 5),
    wt = rep(10, 5),
    y1 = rep(42, 5), # all identical
    y2 = rep(0, 5), # all identical
    y3 = rep(0L, 5), # all identical
    group = rep("A", 5), # all identical
    stringsAsFactors = FALSE
  )
  # suppressWarnings: surveycore warns on single-stratum designs
  d_dup <- suppressWarnings(
    surveycore::as_survey(
      df_dup,
      ids = psu,
      weights = wt,
      strata = strata,
      fpc = fpc,
      nest = TRUE
    )
  )

  result <- suppressWarnings(distinct(d_dup))
  test_invariants(result)
  expect_equal(nrow(result@data), 1L)
})
