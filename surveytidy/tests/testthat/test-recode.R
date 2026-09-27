# tests/testthat/test-recode.R
#
# Tests for surveytidy recode functions + mutate() label integration.
# Sections follow spec §XII.1.
library(dplyr)

# ── 1. mutate() pre-attachment ────────────────────────────────────────────────

test_that("mutate() makes variable labels available as attr(x, 'label') inside mutate [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    d2 <- d
    d2@metadata@variable_labels[["y1"]] <- "Y1 label"
    # recode function can read the label (visible because replace_when inherits it)
    result <- mutate(d2, y1_r = replace_when(y1, y1 > 90 ~ 90))
    test_invariants(result)
    expect_identical(result@metadata@variable_labels$y1_r, "Y1 label")
  }
})

test_that("mutate() makes value labels available as attr(x, 'labels') inside mutate [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    d2 <- d
    d2@metadata@value_labels[["y3"]] <- c("No" = 0L, "Yes" = 1L)
    # recode_values with .use_labels = TRUE reads attr(x, "labels") set by pre-attachment
    result <- mutate(d2, y3_r = recode_values(y3, .use_labels = TRUE))
    test_invariants(result)
    expect_true(all(result@data$y3_r %in% c("No", "Yes", NA_character_)))
  }
})

test_that("mutate() pre-attachment is a no-op when @metadata has no labels [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    # No labels in @metadata — pre-attachment should change nothing
    # Verify by confirming a plain mutate still works and @data is unchanged
    result <- mutate(d, z = y1 * 2)
    test_invariants(result)
    # No label attrs should appear on z (plain multiplication)
    expect_null(attr(result@data$z, "label", exact = TRUE))
    expect_null(attr(result@data$z, "labels", exact = TRUE))
  }
})

# ── 2. mutate() post-detection ────────────────────────────────────────────────

test_that("mutate() extracts variable_label from haven_labelled result into @metadata [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(
      d,
      cat = case_when(
        y1 > 50 ~ "high",
        .default = "low",
        .label = "Response category"
      )
    )
    test_invariants(result)
    expect_identical(result@metadata@variable_labels$cat, "Response category")
    # haven attr must be stripped from @data
    expect_null(attr(result@data$cat, "label", exact = TRUE))
  }
})

test_that("mutate() extracts value_labels from haven_labelled result into @metadata [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(
      d,
      cat = case_when(
        y1 > 50 ~ 1L,
        .default = 0L,
        .value_labels = c("High" = 1L, "Low" = 0L)
      )
    )
    test_invariants(result)
    expect_identical(
      result@metadata@value_labels$cat,
      c("High" = 1L, "Low" = 0L)
    )
    # haven attr must be stripped
    expect_null(attr(result@data$cat, "labels", exact = TRUE))
  }
})

test_that("mutate() strips haven attrs from @data after mutation [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(
      d,
      cat = case_when(
        y1 > 50 ~ "high",
        .default = "low",
        .label = "Category",
        .value_labels = c("High" = "high", "Low" = "low")
      )
    )
    test_invariants(result)
    # All haven attrs must be absent from @data columns
    for (col in names(result@data)) {
      expect_null(attr(result@data[[col]], "label", exact = TRUE))
      expect_null(attr(result@data[[col]], "labels", exact = TRUE))
    }
  }
})

test_that("mutate() clears @metadata labels when labelled column is overwritten with non-labelled output [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    # First: give y3 a label and value_labels
    d2 <- d
    d2@metadata@variable_labels[["y3"]] <- "Binary outcome"
    d2@metadata@value_labels[["y3"]] <- c("No" = 0L, "Yes" = 1L)
    # Second: overwrite y3 with a plain mutate (no recode function)
    result <- mutate(d2, y3 = as.integer(y3 * 2))
    test_invariants(result)
    # Old labels should be cleared since the new output carries no surveytidy_recode attr
    expect_null(result@metadata@variable_labels[["y3"]])
    expect_null(result@metadata@value_labels[["y3"]])
  }
})

# ── 3. case_when() ────────────────────────────────────────────────────────────

test_that("case_when() with no label args produces output identical to dplyr::case_when() [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(d, cat = case_when(y1 > 50 ~ "high", .default = "low"))
    test_invariants(result)
    expect_null(attr(result@data$cat, "surveytidy_recode"))
    expect_null(attr(result@data$cat, "label"))
    expect_null(attr(result@data$cat, "labels"))
    expect_identical(
      result@data$cat,
      dplyr::case_when(d@data$y1 > 50 ~ "high", .default = "low")
    )
  }
})

test_that("case_when() .label stores variable label in @metadata [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(
      d,
      cat = case_when(
        y1 > 50 ~ "high",
        .default = "low",
        .label = "Response category"
      )
    )
    test_invariants(result)
    expect_identical(result@metadata@variable_labels$cat, "Response category")
  }
})

test_that("case_when() .value_labels stores value labels in @metadata [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(
      d,
      cat = case_when(
        y1 > 50 ~ 1L,
        .default = 0L,
        .value_labels = c("High" = 1L, "Low" = 0L)
      )
    )
    test_invariants(result)
    expect_identical(
      result@metadata@value_labels$cat,
      c("High" = 1L, "Low" = 0L)
    )
  }
})

test_that("case_when() .factor = TRUE returns factor with levels from .value_labels [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(
      d,
      cat = case_when(
        y1 > 50 ~ "high",
        .default = "low",
        .factor = TRUE,
        .value_labels = c("High" = "high", "Low" = "low")
      )
    )
    test_invariants(result)
    expect_true(is.factor(result@data$cat))
    expect_identical(levels(result@data$cat), c("High", "Low"))
  }
})

test_that("case_when() .factor = TRUE without .value_labels uses formula appearance order [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(
      d,
      cat = case_when(
        y1 > 50 ~ "high",
        .default = "low",
        .factor = TRUE
      )
    )
    test_invariants(result)
    expect_true(is.factor(result@data$cat))
    expect_identical(levels(result@data$cat), c("high", "low"))
  }
})

test_that("case_when() error: .label not scalar -> surveytidy_error_recode_label_not_scalar", {
  d <- make_all_designs(seed = 42)$taylor
  expect_error(
    mutate(d, cat = case_when(y1 > 50 ~ "high", .label = c("a", "b"))),
    class = "surveytidy_error_recode_label_not_scalar"
  )
  expect_snapshot(
    error = TRUE,
    mutate(d, cat = case_when(y1 > 50 ~ "high", .label = c("a", "b")))
  )
})

test_that("case_when() error: .value_labels unnamed -> surveytidy_error_recode_value_labels_unnamed", {
  d <- make_all_designs(seed = 42)$taylor
  expect_error(
    mutate(d, cat = case_when(y1 > 50 ~ 1L, .value_labels = c(1L, 0L))),
    class = "surveytidy_error_recode_value_labels_unnamed"
  )
  expect_snapshot(
    error = TRUE,
    mutate(d, cat = case_when(y1 > 50 ~ 1L, .value_labels = c(1L, 0L)))
  )
})

test_that("case_when() error: .factor = TRUE + .label -> surveytidy_error_recode_factor_with_label", {
  d <- make_all_designs(seed = 42)$taylor
  expect_error(
    mutate(
      d,
      cat = case_when(y1 > 50 ~ "high", .factor = TRUE, .label = "bad")
    ),
    class = "surveytidy_error_recode_factor_with_label"
  )
  expect_snapshot(
    error = TRUE,
    mutate(d, cat = case_when(y1 > 50 ~ "high", .factor = TRUE, .label = "bad"))
  )
})

test_that("case_when() domain column preserved through mutate [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    d_filtered <- filter(d, y1 > 40)
    result <- mutate(
      d_filtered,
      cat = case_when(y1 > 50 ~ "high", .default = "low")
    )
    test_invariants(result)
    expect_identical(
      result@data[[surveycore::SURVEYCORE_DOMAIN_COL]],
      d_filtered@data[[surveycore::SURVEYCORE_DOMAIN_COL]]
    )
  }
})

# ── 4. replace_when() ─────────────────────────────────────────────────────────

test_that("replace_when() with no label args produces plain output [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(d, y1_r = replace_when(y1, y1 > 90 ~ 90))
    test_invariants(result)
    expect_null(attr(result@data$y1_r, "surveytidy_recode"))
    expect_null(attr(result@data$y1_r, "label"))
    expect_null(attr(result@data$y1_r, "labels"))
  }
})

test_that("replace_when() .label sets variable label in @metadata [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(
      d,
      y1_r = replace_when(
        y1,
        y1 > 90 ~ 90,
        .label = "Winsorized y1"
      )
    )
    test_invariants(result)
    expect_identical(result@metadata@variable_labels$y1_r, "Winsorized y1")
  }
})

test_that("replace_when() inherits variable label from x when .label is NULL [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    # Set a label on y1 via @metadata
    d2 <- d
    d2@metadata@variable_labels[["y1"]] <- "Original y1 label"
    result <- mutate(d2, y1_r = replace_when(y1, y1 > 90 ~ 90))
    test_invariants(result)
    expect_identical(result@metadata@variable_labels$y1_r, "Original y1 label")
  }
})

test_that("replace_when() .value_labels merges with x labels [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    d2 <- d
    d2@metadata@value_labels[["y3"]] <- c("No" = 0L, "Yes" = 1L)
    result <- mutate(
      d2,
      y3_r = replace_when(
        y3,
        y3 == 1L ~ 2L,
        .value_labels = c("Maybe" = 2L)
      )
    )
    test_invariants(result)
    vl <- result@metadata@value_labels$y3_r
    # "No" = 0L is preserved (0 still appears in result)
    expect_true("No" %in% names(vl))
    # "Yes" = 1L is pruned because value 1 no longer appears after the recode
    expect_false("Yes" %in% names(vl))
    # User-supplied "Maybe" = 2L is present (the new value)
    expect_true("Maybe" %in% names(vl))
  }
})

test_that("replace_when() error: .label not scalar -> surveytidy_error_recode_label_not_scalar", {
  d <- make_all_designs(seed = 42)$taylor
  expect_error(
    mutate(d, y1_r = replace_when(y1, y1 > 90 ~ 90, .label = c("a", "b"))),
    class = "surveytidy_error_recode_label_not_scalar"
  )
})

test_that("replace_when() error: .value_labels unnamed -> surveytidy_error_recode_value_labels_unnamed", {
  d <- make_all_designs(seed = 42)$taylor
  expect_error(
    mutate(d, y3_r = replace_when(y3, y3 == 1L ~ 2L, .value_labels = c(2L))),
    class = "surveytidy_error_recode_value_labels_unnamed"
  )
})

# ── 4b. replace_when() stale value label pruning ─────────────────────────────

test_that("replace_when() preserves all inherited labels when no value is eliminated [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    x <- haven::labelled(
      as.integer(round(d@data$y1 %% 4)) + 1L,
      labels = c("Low" = 1L, "Med-Low" = 2L, "Med-High" = 3L, "High" = 4L)
    )
    # Winsorize values above 90 — no value is eliminated from the 1-4 range
    result <- replace_when(x, x > 90 ~ 90L)
    vl <- attr(result, "labels", exact = TRUE)
    expect_true(all(c("Low", "Med-Low", "Med-High", "High") %in% names(vl)))
  }
})

test_that("replace_when() drops inherited labels for values no longer in result [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    x <- haven::labelled(
      as.integer(round(d@data$y1 %% 4)) + 1L,
      labels = c("Low" = 1L, "Med-Low" = 2L, "Med-High" = 3L, "High" = 4L)
    )
    # Collapse 4 -> 3 so value 4 no longer appears
    result <- replace_when(x, x == 4L ~ 3L)
    vl <- attr(result, "labels", exact = TRUE)
    expect_false("High" %in% names(vl))
    expect_true(all(c("Low", "Med-Low", "Med-High") %in% names(vl)))
  }
})

test_that("replace_when() preserves .value_labels entries even when value absent from result [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    x <- haven::labelled(
      as.integer(round(d@data$y1 %% 4)) + 1L,
      labels = c("Low" = 1L, "Med-Low" = 2L, "Med-High" = 3L, "High" = 4L)
    )
    # Collapse 4 -> 3; user explicitly supplies a label for 4 in .value_labels
    result <- replace_when(
      x,
      x == 4L ~ 3L,
      .value_labels = c("Something else" = 4L)
    )
    vl <- attr(result, "labels", exact = TRUE)
    # User-supplied label for 4 must survive even though 4 is gone from result
    expect_true("Something else" %in% names(vl))
  }
})

test_that("replace_when() auto-prunes stale labels with no .value_labels supplied [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    x <- haven::labelled(
      as.integer(round(d@data$y1 %% 4)) + 1L,
      labels = c("Low" = 1L, "Med-Low" = 2L, "Med-High" = 3L, "High" = 4L)
    )
    # Collapse 4 -> 3, no .value_labels
    result <- replace_when(x, x == 4L ~ 3L)
    vl <- attr(result, "labels", exact = TRUE)
    expect_false("High" %in% names(vl))
    expect_true(all(c("Low", "Med-Low", "Med-High") %in% names(vl)))
  }
})

test_that("replace_when() prunes inherited NA-value label when NA no longer in result [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    # Build a labelled vector that includes NA as a labelled value
    vals <- as.integer(round(d@data$y1 %% 3)) + 1L
    vals[1L] <- NA_integer_
    x <- haven::labelled(
      vals,
      labels = c("Low" = 1L, "Mid" = 2L, "High" = 3L, "Missing" = NA_integer_)
    )
    # Replace NA with 0 so NA no longer appears in result
    result <- replace_when(x, is.na(x) ~ 0L)
    vl <- attr(result, "labels", exact = TRUE)
    expect_false("Missing" %in% names(vl))
  }
})

# ── 5. if_else() ──────────────────────────────────────────────────────────────

test_that("if_else() with no label args produces output identical to dplyr::if_else() [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(d, cat = if_else(y1 > 50, "high", "low"))
    test_invariants(result)
    expect_null(attr(result@data$cat, "surveytidy_recode"))
    expect_identical(
      result@data$cat,
      dplyr::if_else(d@data$y1 > 50, "high", "low")
    )
  }
})

test_that("if_else() .label and .value_labels set metadata via post-detection [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(
      d,
      cat = if_else(
        y3 == 1L,
        true = 1L,
        false = 0L,
        .label = "Binary outcome",
        .value_labels = c("Yes" = 1L, "No" = 0L)
      )
    )
    test_invariants(result)
    expect_identical(result@metadata@variable_labels$cat, "Binary outcome")
    expect_identical(result@metadata@value_labels$cat, c("Yes" = 1L, "No" = 0L))
  }
})

test_that("if_else() error: .label not scalar -> surveytidy_error_recode_label_not_scalar", {
  d <- make_all_designs(seed = 42)$taylor
  expect_error(
    mutate(d, cat = if_else(y1 > 50, "high", "low", .label = c("a", "b"))),
    class = "surveytidy_error_recode_label_not_scalar"
  )
})

test_that("if_else() error: .value_labels unnamed -> surveytidy_error_recode_value_labels_unnamed", {
  d <- make_all_designs(seed = 42)$taylor
  expect_error(
    mutate(
      d,
      cat = if_else(
        y3 == 1L,
        1L,
        0L,
        .value_labels = c(1L, 0L)
      )
    ),
    class = "surveytidy_error_recode_value_labels_unnamed"
  )
})

# ── 6. na_if() ────────────────────────────────────────────────────────────────

test_that("na_if() .update_labels = TRUE removes label entry for y from value_labels [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    d2 <- d
    d2@metadata@value_labels[["y3"]] <- c("No" = 0L, "Yes" = 1L)
    result <- mutate(d2, y3_na = na_if(y3, 0L, .update_labels = TRUE))
    test_invariants(result)
    vl <- result@metadata@value_labels$y3_na
    expect_false("No" %in% names(vl))
    expect_true("Yes" %in% names(vl))
  }
})

test_that("na_if() .update_labels = FALSE retains label entry for y [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    d2 <- d
    d2@metadata@value_labels[["y3"]] <- c("No" = 0L, "Yes" = 1L)
    result <- mutate(d2, y3_na = na_if(y3, 0L, .update_labels = FALSE))
    test_invariants(result)
    vl <- result@metadata@value_labels$y3_na
    expect_true("No" %in% names(vl))
    expect_true("Yes" %in% names(vl))
  }
})

test_that("na_if() y as a vector removes all matching label entries [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    d2 <- d
    d2@metadata@value_labels[["y3"]] <- c("No" = 0L, "Yes" = 1L)
    result <- mutate(d2, y3_na = na_if(y3, c(0L, 1L), .update_labels = TRUE))
    test_invariants(result)
    # Both label entries removed; metadata entry should be NULL
    expect_null(result@metadata@value_labels$y3_na)
  }
})

test_that("na_if() x with no labels returns plain vector [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(d, y1_na = na_if(y1, 0))
    test_invariants(result)
    expect_null(attr(result@data$y1_na, "labels"))
    expect_null(attr(result@data$y1_na, "label"))
  }
})

test_that("na_if() error: .update_labels not logical -> surveytidy_error_na_if_update_labels_not_scalar", {
  d <- make_all_designs(seed = 42)$taylor
  expect_error(
    mutate(d, y3_na = na_if(y3, 0L, .update_labels = "yes")),
    class = "surveytidy_error_na_if_update_labels_not_scalar"
  )
  expect_snapshot(
    error = TRUE,
    mutate(d, y3_na = na_if(y3, 0L, .update_labels = "yes"))
  )
  expect_error(
    mutate(d, y3_na = na_if(y3, 0L, .update_labels = c(TRUE, FALSE))),
    class = "surveytidy_error_na_if_update_labels_not_scalar"
  )
})

# ── 7. recode_values() ────────────────────────────────────────────────────────

test_that("recode_values() happy path with explicit from/to [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(
      d,
      y3_r = recode_values(
        y3,
        from = c(0L, 1L),
        to = c("no", "yes")
      )
    )
    test_invariants(result)
    expect_true(all(result@data$y3_r %in% c("no", "yes", NA_character_)))
  }
})

test_that("recode_values() .use_labels = TRUE reads attr(x, 'labels') [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    d2 <- d
    d2@metadata@value_labels[["y3"]] <- c("No" = 0L, "Yes" = 1L)
    result <- mutate(d2, y3_r = recode_values(y3, .use_labels = TRUE))
    test_invariants(result)
    expect_true(all(result@data$y3_r %in% c("No", "Yes", NA_character_)))
  }
})

test_that("recode_values() .use_labels = TRUE with no labels -> surveytidy_error_recode_use_labels_no_attrs", {
  d <- make_all_designs(seed = 42)$taylor
  expect_error(
    mutate(d, y3_r = recode_values(y3, .use_labels = TRUE)),
    class = "surveytidy_error_recode_use_labels_no_attrs"
  )
})

test_that("recode_values() .unmatched = 'error' with unmatched values -> surveytidy_error_recode_unmatched_values", {
  d <- make_all_designs(seed = 42)$taylor
  # y3 has values 0 and 1; from only contains 0 — value 1 is unmatched
  expect_error(
    mutate(
      d,
      y3_r = recode_values(
        y3,
        from = 0L,
        to = "no",
        .unmatched = "error"
      )
    ),
    class = "surveytidy_error_recode_unmatched_values"
  )
})

test_that("recode_values() .factor = TRUE returns factor with correct levels [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(
      d,
      y3_f = recode_values(
        y3,
        from = c(0L, 1L),
        to = c("no", "yes"),
        .factor = TRUE,
        .value_labels = c("No" = "no", "Yes" = "yes")
      )
    )
    test_invariants(result)
    expect_true(is.factor(result@data$y3_f))
    expect_identical(levels(result@data$y3_f), c("No", "Yes"))
  }
})

test_that("recode_values() .factor = TRUE + .label -> surveytidy_error_recode_factor_with_label", {
  d <- make_all_designs(seed = 42)$taylor
  expect_error(
    mutate(
      d,
      y3_f = recode_values(
        y3,
        from = c(0L, 1L),
        to = c("no", "yes"),
        .factor = TRUE,
        .label = "bad"
      )
    ),
    class = "surveytidy_error_recode_factor_with_label"
  )
})

test_that("recode_values() no formulas + from = NULL + .use_labels = FALSE -> surveytidy_error_recode_from_to_missing", {
  d <- make_all_designs(seed = 42)$taylor
  # No formulas in ..., no from, and .use_labels = FALSE — no map supplied
  expect_error(
    mutate(d, y3_r = recode_values(y3, .use_labels = FALSE)),
    class = "surveytidy_error_recode_from_to_missing"
  )
})

# ── 7b. recode_values() formula interface ────────────────────────────────────

test_that("recode_values() formula interface matches dplyr [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(
      d,
      y3_r = recode_values(y3, 0L ~ "no", 1L ~ "yes")
    )
    test_invariants(result)
    expect_identical(
      result@data$y3_r,
      dplyr::recode_values(d@data$y3, 0L ~ "no", 1L ~ "yes")
    )
  }
})

test_that("recode_values() formula interface with default [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(
      d,
      y3_r = recode_values(y3, 1L ~ "yes", default = "other")
    )
    test_invariants(result)
    expect_identical(
      result@data$y3_r,
      dplyr::recode_values(d@data$y3, 1L ~ "yes", default = "other")
    )
  }
})

test_that("recode_values() formula interface with .unmatched = 'error' on unmatched values", {
  d <- make_all_designs(seed = 42)$taylor
  # y3 has values 0L and 1L; formula only matches 1L — 0L is unmatched
  expect_error(
    mutate(
      d,
      y3_r = recode_values(y3, 1L ~ "yes", .unmatched = "error")
    ),
    class = "surveytidy_error_recode_unmatched_values"
  )
})

test_that("recode_values() formula interface + .label sets variable label [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(
      d,
      y3_r = recode_values(
        y3,
        0L ~ "no",
        1L ~ "yes",
        .label = "Y3 recoded"
      )
    )
    test_invariants(result)
    expect_identical(result@metadata@variable_labels$y3_r, "Y3 recoded")
  }
})

test_that("recode_values() formula interface + .value_labels sets value labels [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(
      d,
      y3_r = recode_values(
        y3,
        0L ~ 10L,
        1L ~ 20L,
        .value_labels = c("Low" = 10L, "High" = 20L)
      )
    )
    test_invariants(result)
    expect_identical(
      result@metadata@value_labels$y3_r,
      c("Low" = 10L, "High" = 20L)
    )
  }
})

test_that("recode_values() formula interface + .factor = TRUE uses formula RHS order [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(
      d,
      y3_f = recode_values(y3, 1L ~ "yes", 0L ~ "no", .factor = TRUE)
    )
    test_invariants(result)
    expect_true(is.factor(result@data$y3_f))
    # Levels follow formula RHS order, not sorted data order
    expect_identical(levels(result@data$y3_f), c("yes", "no"))
  }
})

test_that("recode_values() .use_labels = TRUE combined with formulas errors", {
  d <- make_all_designs(seed = 42)$taylor
  d@metadata@value_labels[["y3"]] <- c("No" = 0L, "Yes" = 1L)
  expect_error(
    mutate(
      d,
      y3_r = recode_values(y3, 0L ~ "no", .use_labels = TRUE)
    ),
    class = "surveytidy_error_recode_use_labels_with_formulas"
  )
  expect_snapshot(
    error = TRUE,
    mutate(
      d,
      y3_r = recode_values(y3, 0L ~ "no", .use_labels = TRUE)
    )
  )
})

test_that("recode_values() .use_labels = TRUE + .factor = TRUE uses label-derived levels [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    d2 <- d
    d2@metadata@value_labels[["y3"]] <- c("No" = 0L, "Yes" = 1L)
    result <- mutate(
      d2,
      y3_f = recode_values(y3, .use_labels = TRUE, .factor = TRUE)
    )
    test_invariants(result)
    expect_true(is.factor(result@data$y3_f))
    expect_true(all(levels(result@data$y3_f) %in% c("No", "Yes")))
  }
})

# ── 8. replace_values() ───────────────────────────────────────────────────────

test_that("replace_values() happy path with no labels [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(d, y3_r = replace_values(y3, from = 0L, to = NA_integer_))
    test_invariants(result)
    expect_null(attr(result@data$y3_r, "labels"))
    expect_null(attr(result@data$y3_r, "label"))
  }
})

test_that("replace_values() .value_labels merges with x labels [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    d2 <- d
    d2@metadata@value_labels[["y3"]] <- c("No" = 0L, "Yes" = 1L)
    result <- mutate(
      d2,
      y3_r = replace_values(
        y3,
        from = 0L,
        to = 9L,
        .value_labels = c("Missing" = 9L)
      )
    )
    test_invariants(result)
    vl <- result@metadata@value_labels$y3_r
    expect_true("Yes" %in% names(vl))
    expect_true("Missing" %in% names(vl))
  }
})

test_that("replace_values() inherits variable label from x when .label is NULL [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    d2 <- d
    d2@metadata@variable_labels[["y3"]] <- "Binary outcome"
    result <- mutate(d2, y3_r = replace_values(y3, from = 0L, to = 9L))
    test_invariants(result)
    expect_identical(result@metadata@variable_labels$y3_r, "Binary outcome")
  }
})

test_that("replace_values() error: .label not scalar -> surveytidy_error_recode_label_not_scalar", {
  d <- make_all_designs(seed = 42)$taylor
  expect_error(
    mutate(
      d,
      y3_r = replace_values(y3, from = 0L, to = 9L, .label = c("a", "b"))
    ),
    class = "surveytidy_error_recode_label_not_scalar"
  )
})

test_that("replace_values() error: .value_labels unnamed -> surveytidy_error_recode_value_labels_unnamed", {
  d <- make_all_designs(seed = 42)$taylor
  expect_error(
    mutate(
      d,
      y3_r = replace_values(y3, from = 0L, to = 9L, .value_labels = c(9L))
    ),
    class = "surveytidy_error_recode_value_labels_unnamed"
  )
})

# ── 8b. replace_values() stale value label pruning ───────────────────────────

test_that("replace_values() preserves all inherited labels when no value is eliminated [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    x <- haven::labelled(
      as.integer(round(d@data$y1 %% 4)) + 1L,
      labels = c("Low" = 1L, "Med-Low" = 2L, "Med-High" = 3L, "High" = 4L)
    )
    # Replace value 99 (absent) with 0 — no existing value is eliminated
    result <- replace_values(x, from = 99L, to = 0L)
    vl <- attr(result, "labels", exact = TRUE)
    expect_true(all(c("Low", "Med-Low", "Med-High", "High") %in% names(vl)))
  }
})

test_that("replace_values() drops inherited labels for values no longer in result [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    x <- haven::labelled(
      as.integer(round(d@data$y1 %% 4)) + 1L,
      labels = c("Low" = 1L, "Med-Low" = 2L, "Med-High" = 3L, "High" = 4L)
    )
    # Collapse 4 -> 3 so value 4 no longer appears
    result <- replace_values(x, from = 4L, to = 3L)
    vl <- attr(result, "labels", exact = TRUE)
    expect_false("High" %in% names(vl))
    expect_true(all(c("Low", "Med-Low", "Med-High") %in% names(vl)))
  }
})

test_that("replace_values() preserves .value_labels entries even when value absent from result [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    x <- haven::labelled(
      as.integer(round(d@data$y1 %% 4)) + 1L,
      labels = c("Low" = 1L, "Med-Low" = 2L, "Med-High" = 3L, "High" = 4L)
    )
    # Collapse 4 -> 3; user explicitly supplies a label for 4 in .value_labels
    result <- replace_values(
      x,
      from = 4L,
      to = 3L,
      .value_labels = c("Something else" = 4L)
    )
    vl <- attr(result, "labels", exact = TRUE)
    # User-supplied label for 4 must survive even though 4 is gone from result
    expect_true("Something else" %in% names(vl))
  }
})

test_that("replace_values() auto-prunes stale labels with no .value_labels supplied [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    x <- haven::labelled(
      as.integer(round(d@data$y1 %% 4)) + 1L,
      labels = c("Low" = 1L, "Med-Low" = 2L, "Med-High" = 3L, "High" = 4L)
    )
    # Collapse 4 -> 3, no .value_labels
    result <- replace_values(x, from = 4L, to = 3L)
    vl <- attr(result, "labels", exact = TRUE)
    expect_false("High" %in% names(vl))
    expect_true(all(c("Low", "Med-Low", "Med-High") %in% names(vl)))
  }
})

test_that("replace_values() prunes inherited NA-value label when NA no longer in result [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    # Build a labelled vector that includes NA as a labelled value
    vals <- as.integer(round(d@data$y1 %% 3)) + 1L
    vals[1L] <- NA_integer_
    x <- haven::labelled(
      vals,
      labels = c("Low" = 1L, "Mid" = 2L, "High" = 3L, "Missing" = NA_integer_)
    )
    # Replace NA with 0 so NA no longer appears in result
    result <- replace_values(x, from = NA_integer_, to = 0L)
    vl <- attr(result, "labels", exact = TRUE)
    expect_false("Missing" %in% names(vl))
  }
})

# ── 9. Domain preservation ────────────────────────────────────────────────────

test_that("domain column preserved through mutate + each recode function [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    d_filtered <- filter(d, y1 > 40)
    domain_before <- d_filtered@data[[surveycore::SURVEYCORE_DOMAIN_COL]]

    result <- mutate(
      d_filtered,
      cat = case_when(y1 > 50 ~ "high", .default = "low", .label = "Category")
    )
    test_invariants(result)
    expect_identical(
      result@data[[surveycore::SURVEYCORE_DOMAIN_COL]],
      domain_before
    )
  }
})

# ── 10. Backward compatibility (shadowing) ────────────────────────────────────

test_that("case_when() with no surveytidy args is identical to dplyr::case_when()", {
  x <- 1:10
  st_out <- case_when(x > 5 ~ "high", .default = "low")
  dp_out <- dplyr::case_when(x > 5 ~ "high", .default = "low")
  expect_identical(st_out, dp_out)
  expect_null(attr(st_out, "surveytidy_recode"))
})

test_that("if_else() with no surveytidy args is identical to dplyr::if_else()", {
  x <- 1:10
  st_out <- if_else(x > 5, "high", "low")
  dp_out <- dplyr::if_else(x > 5, "high", "low")
  expect_identical(st_out, dp_out)
  expect_null(attr(st_out, "surveytidy_recode"))
})

test_that("na_if() with no surveytidy args is identical to dplyr::na_if()", {
  x <- c(1, 2, NA, 4)
  st_out <- na_if(x, 2)
  dp_out <- dplyr::na_if(x, 2)
  expect_identical(st_out, dp_out)
  expect_null(attr(st_out, "surveytidy_recode"))
})

# ── 11. .description argument (all 6 functions) ───────────────────────────────

test_that(".description is stored in @metadata@transformations$description for all 6 functions [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    # case_when
    r1 <- mutate(
      d,
      cat = case_when(
        y1 > 50 ~ "high",
        .default = "low",
        .description = "High vs. low y1"
      )
    )
    test_invariants(r1)
    expect_identical(
      r1@metadata@transformations$cat$description,
      "High vs. low y1"
    )

    # replace_when
    r2 <- mutate(
      d,
      cat = replace_when(
        y1,
        y1 > 90 ~ 90,
        .description = "replace_when desc"
      )
    )
    test_invariants(r2)
    expect_identical(
      r2@metadata@transformations$cat$description,
      "replace_when desc"
    )

    # if_else
    r3 <- mutate(
      d,
      cat = if_else(
        y1 > 50,
        "high",
        "low",
        .description = "if_else desc"
      )
    )
    test_invariants(r3)
    expect_identical(
      r3@metadata@transformations$cat$description,
      "if_else desc"
    )

    # na_if
    r4 <- mutate(d, cat = na_if(y1, 0, .description = "na_if desc"))
    test_invariants(r4)
    expect_identical(r4@metadata@transformations$cat$description, "na_if desc")

    # recode_values
    r5 <- mutate(
      d,
      cat = recode_values(
        y3,
        from = c(0L, 1L),
        to = c("no", "yes"),
        .description = "recode_values desc"
      )
    )
    test_invariants(r5)
    expect_identical(
      r5@metadata@transformations$cat$description,
      "recode_values desc"
    )

    # replace_values
    r6 <- mutate(
      d,
      cat = replace_values(
        y3,
        from = 0L,
        to = NA_integer_,
        .description = "replace_values desc"
      )
    )
    test_invariants(r6)
    expect_identical(
      r6@metadata@transformations$cat$description,
      "replace_values desc"
    )
  }
})

test_that(".description = NULL stores description = NULL in transformations record [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(
      d,
      cat = case_when(
        y1 > 50 ~ "high",
        .default = "low",
        .label = "Category"
      )
    )
    test_invariants(result)
    expect_null(result@metadata@transformations$cat$description)
  }
})

test_that("no surveytidy args -> no surveytidy_recode attr on @data (backward compat, all shadowed fns)", {
  d <- make_all_designs(seed = 42)$taylor

  r1 <- mutate(d, cat = dplyr::case_when(y1 > 50 ~ "high", .default = "low"))
  test_invariants(r1)
  expect_null(attr(r1@data$cat, "surveytidy_recode"))

  r2 <- mutate(d, cat = if_else(y1 > 50, "high", "low"))
  test_invariants(r2)
  expect_null(attr(r2@data$cat, "surveytidy_recode"))

  r3 <- mutate(d, cat = na_if(y1, 0))
  test_invariants(r3)
  expect_null(attr(r3@data$cat, "surveytidy_recode"))
})

test_that(".description not character(1) -> surveytidy_error_recode_description_not_scalar", {
  d <- make_all_designs(seed = 42)$taylor
  expect_error(
    mutate(d, cat = case_when(y1 > 50 ~ "high", .description = c("a", "b"))),
    class = "surveytidy_error_recode_description_not_scalar"
  )
})

# ── 12. Error snapshots ───────────────────────────────────────────────────────

test_that("error snapshots for all recode error classes", {
  d <- make_all_designs(seed = 42)$taylor

  expect_snapshot(
    error = TRUE,
    mutate(d, cat = case_when(y1 > 50 ~ "high", .label = c("a", "b")))
  )
  expect_snapshot(
    error = TRUE,
    mutate(d, cat = case_when(y1 > 50 ~ 1L, .value_labels = c(1L, 0L)))
  )
  expect_snapshot(
    error = TRUE,
    mutate(d, cat = case_when(y1 > 50 ~ "hi", .factor = TRUE, .label = "x"))
  )
  expect_snapshot(
    error = TRUE,
    mutate(d, cat = recode_values(y3, from = 1L, to = 2L, .use_labels = TRUE))
  )
  expect_snapshot(
    error = TRUE,
    mutate(d, cat = recode_values(y3, .use_labels = FALSE))
  )
  expect_snapshot(
    error = TRUE,
    mutate(
      d,
      cat = recode_values(
        y3,
        from = 99L,
        to = "other",
        .unmatched = "error"
      )
    )
  )
  expect_snapshot(
    error = TRUE,
    mutate(d, cat = case_when(y1 > 50 ~ "high", .description = c("a", "b")))
  )
})

# ── 2b. mutate() step 1 — design variable warnings ───────────────────────────

test_that("mutate() warns surveytidy_warning_mutate_structural_var when mutating strata [taylor]", {
  d <- make_all_designs(seed = 42)$taylor
  expect_warning(
    result <- mutate(d, strata = paste0(strata, "_mod")),
    class = "surveytidy_warning_mutate_structural_var"
  )
  expect_snapshot(
    mutate(d, strata = paste0(strata, "_mod"))
  )
  test_invariants(result)
})

test_that("mutate() warns surveytidy_warning_mutate_structural_var when mutating PSU [taylor]", {
  d <- make_all_designs(seed = 42)$taylor
  expect_warning(
    result <- mutate(d, psu = paste0(psu, "_mod")),
    class = "surveytidy_warning_mutate_structural_var"
  )
  test_invariants(result)
})

test_that("mutate() warns surveytidy_warning_mutate_structural_var when mutating FPC [taylor]", {
  d <- make_all_designs(seed = 42)$taylor
  expect_warning(
    result <- mutate(d, fpc = fpc * 1.1),
    class = "surveytidy_warning_mutate_structural_var"
  )
  test_invariants(result)
})

test_that("mutate() warns surveytidy_warning_mutate_structural_var when mutating repweights [replicate]", {
  d <- make_all_designs(seed = 42)$replicate
  repwt1 <- d@variables$repweights[[1]]
  expect_warning(
    result <- mutate(d, !!repwt1 := .data[[repwt1]] * 1.1),
    class = "surveytidy_warning_mutate_structural_var"
  )
  test_invariants(result)
})

test_that("mutate() warns surveytidy_warning_mutate_weight_col when mutating weight [all designs]", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    expect_warning(
      result <- mutate(d, wt = wt * 1.1),
      class = "surveytidy_warning_mutate_weight_col"
    )
    test_invariants(result)
  }
})

# ── 11. Coverage gap: non-literal RHS in case_when() .factor branch ──────────

test_that("case_when() .factor = TRUE with non-literal RHS uses unique result values for levels [all designs]", {
  # Covers R/case-when.R:242 — the all_literal == FALSE branch where
  # formula_values is derived from unique(result) rather than from the
  # syntactic literals on the RHS of each formula.
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(
      d,
      cat = case_when(
        y1 > 50 ~ paste0("hi", "gh"),
        .default = "low",
        .factor = TRUE
      )
    )
    test_invariants(result)
    expect_true(is.factor(result@data$cat))
    # Levels come from unique non-NA values in result; "low" is the .default
    expect_setequal(levels(result@data$cat), c("high", "low"))
  }
})

# ── 12. Coverage gap: recode_values() rethrows non-unmatched errors ──────────

test_that("recode_values() rethrows errors that aren't vctrs_error_combine_unmatched", {
  # Covers R/recode-values.R:339 — `stop(e)` re-throws errors thrown by
  # dplyr::recode_values() that are NOT vctrs_error_combine_unmatched (e.g.,
  # incompatible from/to lengths produce vctrs_error_incompatible_size).
  d <- make_all_designs(seed = 42)$taylor

  # from has 2 elements but to has 3 — vctrs_error_incompatible_size, which
  # doesn't match the unmatched-values branch and falls through to stop(e).
  expect_error(
    mutate(
      d,
      y3_r = recode_values(
        y3,
        from = c(0L, 1L),
        to = c("a", "b", "c")
      )
    ),
    class = "vctrs_error_incompatible_size"
  )
})

# ── 13. Coverage gap: replace_values() .label argument propagates ────────────

test_that("replace_values() .label sets variable label in @metadata [all designs]", {
  # Covers R/replace-values.R:161 — the !is.null(.label) branch where the
  # caller-supplied .label takes precedence over attr(x, "label").
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    result <- mutate(
      d,
      y3_r = replace_values(y3, from = 0L, to = 9L, .label = "Recoded outcome")
    )
    test_invariants(result)
    expect_identical(
      result@metadata@variable_labels$y3_r,
      "Recoded outcome"
    )
  }
})
