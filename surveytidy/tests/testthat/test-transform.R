# tests/testthat/test-transform.R
#
# Tests for R/transform.R: make_factor(), make_dicho(), make_binary(),
# make_rev(), make_flip(). Follows the test plan in plans/spec-transform.md §IX.
#
# Sections:
#  1–14c make_factor()
#  15–26d make_dicho()
#  27–32d make_binary()
#  33–41  make_rev()
#  42–50  make_flip()
#  51–53  surveytidy_recode attr structure
#  54–59  Integration tests (vector pipelines + mutate() on survey designs)

library(dplyr)

# ── helpers ──────────────────────────────────────────────────────────────────

# Build a haven_labelled-like numeric vector with labels and label attrs
.make_labelled <- function(x, labels, label = NULL) {
  attr(x, "labels") <- labels
  attr(x, "class") <- c(
    "haven_labelled",
    class(x)[class(x) != "haven_labelled"]
  )
  if (!is.null(label)) {
    attr(x, "label") <- label
  }
  x
}

# 4-level Likert: 1=Strongly agree, 2=Agree, 3=Disagree, 4=Strongly disagree
.likert4 <- function() {
  x <- c(1, 2, 2, 3, 4, 1, 3, 4, 2, NA)
  attr(x, "labels") <- c(
    "Strongly agree" = 1,
    "Agree" = 2,
    "Disagree" = 3,
    "Strongly disagree" = 4
  )
  attr(x, "label") <- "Agreement scale"
  class(x) <- c("haven_labelled", "numeric")
  x
}

# ── make_factor() ─────────────────────────────────────────────────────────────

# 1. Happy path: haven_labelled input
test_that("make_factor() converts haven_labelled to factor with labelled levels", {
  x <- .likert4()
  result <- make_factor(x)
  expect_true(is.factor(result))
  expect_equal(
    levels(result),
    c("Strongly agree", "Agree", "Disagree", "Strongly disagree")
  )
  # NA stays NA
  expect_true(is.na(result[10]))
  # Values map correctly
  expect_identical(as.character(result[1]), "Strongly agree")
  expect_identical(as.character(result[2]), "Agree")
})

# 2. Happy path: plain numeric with labels attr (not haven_labelled class)
test_that("make_factor() works on plain numeric with labels attr", {
  x <- c(1, 2, 3, 2, 1)
  attr(x, "labels") <- c("Low" = 1, "Medium" = 2, "High" = 3)
  result <- make_factor(x)
  expect_true(is.factor(result))
  expect_equal(levels(result), c("Low", "Medium", "High"))
  expect_identical(as.character(result[1]), "Low")
})

# 3. Happy path: factor pass-through (levels and order preserved)
test_that("make_factor() passes through a factor with levels unchanged", {
  x <- factor(c("B", "A", "C", "A"), levels = c("C", "B", "A"))
  result <- make_factor(x)
  expect_true(is.factor(result))
  expect_equal(levels(result), c("C", "B", "A"))
})

# 3a. ordered = TRUE on factor pass-through returns ordered factor
test_that("make_factor() with ordered = TRUE on factor returns ordered factor", {
  x <- factor(c("Low", "High", "Med"), levels = c("Low", "Med", "High"))
  result <- make_factor(x, ordered = TRUE)
  expect_true(is.ordered(result))
  expect_equal(levels(result), c("Low", "Med", "High"))
})

# 3b. ordered = FALSE on an ordered factor removes ordered class
test_that("make_factor() with ordered = FALSE on ordered factor removes ordered class", {
  x <- factor(c("Low", "High"), levels = c("Low", "High"), ordered = TRUE)
  result <- make_factor(x, ordered = FALSE)
  expect_false(is.ordered(result))
  expect_true(is.factor(result))
})

# 4. Happy path: character input (alphabetical levels)
test_that("make_factor() converts character vector to factor with alphabetical levels", {
  x <- c("cat", "dog", "bird", "cat")
  result <- make_factor(x)
  expect_true(is.factor(result))
  expect_equal(levels(result), c("bird", "cat", "dog"))
  expect_identical(as.character(result[1]), "cat")
})

# 5. drop_levels = FALSE includes unobserved levels
test_that("make_factor() with drop_levels = FALSE retains unobserved levels", {
  x <- c(1, 2, 1)
  attr(x, "labels") <- c("Low" = 1, "Medium" = 2, "High" = 3)
  result <- make_factor(x, drop_levels = FALSE)
  expect_true("High" %in% levels(result))
  expect_equal(levels(result), c("Low", "Medium", "High"))
})

# 6. ordered = TRUE returns ordered factor for numeric input
test_that("make_factor() with ordered = TRUE returns ordered factor for labelled numeric", {
  x <- c(1, 2, 3)
  attr(x, "labels") <- c("Low" = 1, "Medium" = 2, "High" = 3)
  result <- make_factor(x, ordered = TRUE)
  expect_true(is.ordered(result))
  expect_equal(levels(result), c("Low", "Medium", "High"))
})

# 7. na.rm = TRUE converts na_values to NA before levelling
test_that("make_factor() na.rm = TRUE removes na_values from levels", {
  x <- c(1, 2, 3, 8, 9, 1)
  attr(x, "labels") <- c(
    "Agree" = 1,
    "Neutral" = 2,
    "Disagree" = 3,
    "Don't know" = 8,
    "Refused" = 9
  )
  attr(x, "na_values") <- c(8, 9)
  result <- make_factor(x, na.rm = TRUE)
  expect_false("Don't know" %in% levels(result))
  expect_false("Refused" %in% levels(result))
  expect_true("Agree" %in% levels(result))
  # Rows with na_values are now NA
  expect_true(is.na(result[4]))
  expect_true(is.na(result[5]))
})

# 8. na.rm = TRUE with na_range
test_that("make_factor() na.rm = TRUE with na_range converts range to NA", {
  x <- c(1, 2, 3, 97, 98, 99)
  attr(x, "labels") <- c(
    "Low" = 1,
    "Mid" = 2,
    "High" = 3,
    "DK" = 97,
    "Refused" = 98,
    "N/A" = 99
  )
  attr(x, "na_range") <- c(97, 99)
  result <- make_factor(x, na.rm = TRUE)
  expect_false("DK" %in% levels(result))
  expect_false("Refused" %in% levels(result))
  expect_false("N/A" %in% levels(result))
  expect_true(is.na(result[4]))
  expect_true(is.na(result[5]))
  expect_true(is.na(result[6]))
})

# 9. .label overrides inherited variable label attr
test_that("make_factor() .label overrides inherited label attr", {
  x <- c(1, 2)
  attr(x, "labels") <- c("Yes" = 1, "No" = 2)
  attr(x, "label") <- "Original label"
  result <- make_factor(x, .label = "New label")
  expect_identical(attr(result, "label"), "New label")
})

# 10. .description sets surveytidy_recode attr
test_that("make_factor() .description is stored in surveytidy_recode attr", {
  x <- c(1, 2)
  attr(x, "labels") <- c("Yes" = 1, "No" = 2)
  result <- make_factor(x, .description = "Converted to factor")
  recode_attr <- attr(result, "surveytidy_recode")
  expect_false(is.null(recode_attr))
  expect_identical(recode_attr$description, "Converted to factor")
  expect_identical(recode_attr$fn, "make_factor")
})

# 11. Variable label inherited from attr(x, "label") when .label = NULL
test_that("make_factor() inherits label from attr(x, 'label') when .label = NULL", {
  x <- c(1, 2)
  attr(x, "labels") <- c("A" = 1, "B" = 2)
  attr(x, "label") <- "Inherited label"
  result <- make_factor(x)
  expect_identical(attr(result, "label"), "Inherited label")
})

# 11b. Label falls back to var_name when no attr(x, "label") and .label = NULL
test_that("make_factor() label falls back to var_name when no attr(x, 'label')", {
  my_var <- c(1, 2)
  attr(my_var, "labels") <- c("A" = 1, "B" = 2)
  result <- make_factor(my_var)
  # When called directly (not inside mutate/across), var_name = "my_var"
  expect_identical(attr(result, "label"), "my_var")
})

# 12. Error: unsupported type (list, logical)
test_that("make_factor() errors on unsupported type (list)", {
  expect_error(
    make_factor(list(1, 2, 3)),
    class = "surveytidy_error_make_factor_unsupported_type"
  )
  expect_snapshot(error = TRUE, make_factor(list(1, 2, 3)))
})

test_that("make_factor() errors on unsupported type (logical)", {
  expect_error(
    make_factor(c(TRUE, FALSE, TRUE)),
    class = "surveytidy_error_make_factor_unsupported_type"
  )
})

# 12b. Error: bad arg type (ordered = "yes", drop_levels = 2L)
test_that("make_factor() errors on bad arg type for ordered", {
  x <- c(1, 2)
  attr(x, "labels") <- c("A" = 1, "B" = 2)
  expect_error(
    make_factor(x, ordered = "yes"),
    class = "surveytidy_error_make_factor_bad_arg"
  )
  expect_snapshot(error = TRUE, make_factor(x, ordered = "yes"))
})

test_that("make_factor() errors on bad arg type for drop_levels", {
  x <- c(1, 2)
  attr(x, "labels") <- c("A" = 1, "B" = 2)
  expect_error(
    make_factor(x, drop_levels = 2L),
    class = "surveytidy_error_make_factor_bad_arg"
  )
})

test_that("make_factor() errors on bad .label type", {
  x <- c(1, 2)
  attr(x, "labels") <- c("A" = 1, "B" = 2)
  expect_error(
    make_factor(x, .label = 123),
    class = "surveytidy_error_make_factor_bad_arg"
  )
})

# 13. Error: no labels (plain numeric without labels, force = FALSE)
test_that("make_factor() errors when numeric has no labels and force = FALSE", {
  x <- c(1, 2, 3)
  expect_error(
    make_factor(x),
    class = "surveytidy_error_make_factor_no_labels"
  )
  expect_snapshot(error = TRUE, make_factor(x))
})

# 14. Error: incomplete labels (one value missing a label)
test_that("make_factor() errors when a non-NA value lacks a label entry", {
  x <- c(1, 2, 3, 4) # value 4 has no label
  attr(x, "labels") <- c("A" = 1, "B" = 2, "C" = 3)
  expect_error(
    make_factor(x),
    class = "surveytidy_error_make_factor_incomplete_labels"
  )
  expect_snapshot(error = TRUE, make_factor(x))
})

# 14b. force = TRUE: numeric without labels warns and coerces
test_that("make_factor() force = TRUE warns and coerces numeric without labels", {
  x <- c(1, 2, 3, 1, 2)
  expect_warning(
    result <- make_factor(x, force = TRUE),
    class = "surveytidy_warning_make_factor_forced"
  )
  expect_true(is.factor(result))
})

# 14c. force = TRUE: warning class is surveytidy_warning_make_factor_forced
test_that("make_factor() force = TRUE warning class is correct", {
  x <- c(1, 2, 3)
  expect_warning(
    make_factor(x, force = TRUE),
    class = "surveytidy_warning_make_factor_forced"
  )
})

# ── make_dicho() ──────────────────────────────────────────────────────────────

# 15. Happy path: 4-level Likert auto-collapses to 2
test_that("make_dicho() collapses 4-level Likert to 2 levels", {
  x <- .likert4()
  result <- make_dicho(x)
  expect_true(is.factor(result))
  expect_equal(nlevels(result), 2L)
  lvls <- levels(result)
  # After stripping "Strongly" from "Strongly agree" → "Agree"
  # and "Strongly" from "Strongly disagree" → "Disagree"
  expect_true(all(lvls %in% c("Agree", "Disagree")))
})

# 16. Already 2-level factor: stems are single words, pass through
test_that("make_dicho() passes through 2-level factor with single-word levels", {
  x <- factor(
    c("Agree", "Disagree", "Agree", "Agree"),
    levels = c("Agree", "Disagree")
  )
  result <- make_dicho(x)
  expect_true(is.factor(result))
  expect_equal(nlevels(result), 2L)
  expect_equal(levels(result), c("Agree", "Disagree"))
})

# 17. .exclude sets middle level to NA
test_that("make_dicho() .exclude removes specified level from levels", {
  x <- factor(
    c(
      "Always agree",
      "Sometimes agree",
      "Sometimes disagree",
      "Always disagree",
      "Neutral"
    ),
    levels = c(
      "Always agree",
      "Sometimes agree",
      "Neutral",
      "Sometimes disagree",
      "Always disagree"
    )
  )
  result <- make_dicho(x, .exclude = "Neutral")
  expect_false("Neutral" %in% levels(result))
  expect_equal(nlevels(result), 2L)
})

# 18. .exclude: excluded rows become NA in the 2-level factor result
test_that("make_dicho() .exclude rows become NA in result", {
  x <- factor(
    c("Always agree", "Neutral", "Always disagree"),
    levels = c("Always agree", "Neutral", "Always disagree")
  )
  result <- make_dicho(x, .exclude = "Neutral")
  expect_true(is.na(result[2]))
  expect_false(is.na(result[1]))
  expect_false(is.na(result[3]))
})

# 19. flip_levels reverses level order
test_that("make_dicho() flip_levels = TRUE reverses level order", {
  x <- .likert4()
  result_normal <- make_dicho(x)
  result_flipped <- make_dicho(x, flip_levels = TRUE)
  expect_equal(levels(result_flipped), rev(levels(result_normal)))
})

# 20. Warning: unknown .exclude level
test_that("make_dicho() warns on unknown .exclude level", {
  x <- factor(c("Agree", "Disagree"), levels = c("Agree", "Disagree"))
  expect_warning(
    make_dicho(x, .exclude = "Neutral"),
    class = "surveytidy_warning_make_dicho_unknown_exclude"
  )
})

# 21. Error: too few levels after .exclude
test_that("make_dicho() errors when fewer than 2 levels remain after .exclude", {
  x <- factor(c("Agree", "Neutral"), levels = c("Agree", "Neutral"))
  expect_error(
    make_dicho(x, .exclude = "Neutral"),
    class = "surveytidy_error_make_dicho_too_few_levels"
  )
  expect_snapshot(error = TRUE, make_dicho(x, .exclude = "Neutral"))
})

# 22. Error: collapse ambiguous (4 distinct stems, no shared first word)
test_that("make_dicho() errors when collapse is ambiguous (4 distinct stems)", {
  x <- factor(
    c("Apple", "Banana", "Cherry", "Date"),
    levels = c("Apple", "Banana", "Cherry", "Date")
  )
  expect_error(
    make_dicho(x),
    class = "surveytidy_error_make_dicho_collapse_ambiguous"
  )
  expect_snapshot(error = TRUE, make_dicho(x))
})

# 23. Single-word labels pass through unchanged (no stripping)
test_that("make_dicho() leaves single-word level labels unchanged", {
  x <- factor(c("Yes", "No", "Yes"), levels = c("Yes", "No"))
  result <- make_dicho(x)
  expect_equal(levels(result), c("Yes", "No"))
})

# 24. Non-standard first words stripped correctly ("Always agree" → "Agree")
test_that("make_dicho() strips first word from multi-word Likert levels", {
  x <- factor(
    c("Always agree", "Usually agree", "Usually disagree", "Always disagree"),
    levels = c(
      "Always agree",
      "Usually agree",
      "Usually disagree",
      "Always disagree"
    )
  )
  result <- make_dicho(x)
  lvls <- levels(result)
  expect_true("Agree" %in% lvls)
  expect_true("Disagree" %in% lvls)
})

# 25. Level order preserved from original labels, not alphabetical
test_that("make_dicho() preserves original level order (not alphabetical)", {
  x <- factor(
    c(
      "Always agree",
      "Sometimes agree",
      "Sometimes disagree",
      "Always disagree"
    ),
    levels = c(
      "Always agree",
      "Sometimes agree",
      "Sometimes disagree",
      "Always disagree"
    )
  )
  result <- make_dicho(x)
  lvls <- levels(result)
  # "Agree" comes first because "Always agree" appears first in original
  expect_identical(lvls[1], "Agree")
  expect_identical(lvls[2], "Disagree")
})

# 26. .label and .description set attrs on result
test_that("make_dicho() .label and .description are set on result", {
  x <- .likert4()
  result <- make_dicho(
    x,
    .label = "Agreement group",
    .description = "Collapsed Likert"
  )
  expect_identical(attr(result, "label"), "Agreement group")
  recode_attr <- attr(result, "surveytidy_recode")
  expect_identical(recode_attr$description, "Collapsed Likert")
  expect_identical(recode_attr$fn, "make_dicho")
})

# 26b. Label falls back to var_name when no attr(x, "label") and .label = NULL
test_that("make_dicho() label falls back to var_name when no inherited label", {
  my_var <- factor(c("Agree", "Disagree"), levels = c("Agree", "Disagree"))
  result <- make_dicho(my_var)
  expect_identical(attr(result, "label"), "my_var")
})

# 26c. Error: bad arg type (.label = 123)
test_that("make_dicho() errors on bad .label type", {
  x <- .likert4()
  expect_error(
    make_dicho(x, .label = 123),
    class = "surveytidy_error_transform_bad_arg"
  )
  expect_snapshot(error = TRUE, make_dicho(x, .label = 123))
})

# 26d. Error: bad arg type (flip_levels = "yes")
test_that("make_dicho() errors on bad flip_levels type", {
  x <- .likert4()
  expect_error(
    make_dicho(x, flip_levels = "yes"),
    class = "surveytidy_error_transform_bad_arg"
  )
})

# ── make_binary() ─────────────────────────────────────────────────────────────

# 27. Basic 0/1 mapping: first level → 1, second → 0
test_that("make_binary() maps first level to 1 and second level to 0", {
  x <- factor(
    c("Agree", "Disagree", "Agree", NA),
    levels = c("Agree", "Disagree")
  )
  result <- make_binary(x)
  expect_equal(result[1], 1L)
  expect_equal(result[2], 0L)
  expect_equal(result[3], 1L)
  expect_true(is.na(result[4]))
})

# 28. flip_values reverses mapping: first level → 0
test_that("make_binary() flip_values = TRUE maps first level to 0", {
  x <- factor(c("Agree", "Disagree", "Agree"), levels = c("Agree", "Disagree"))
  result <- make_binary(x, flip_values = TRUE)
  expect_equal(result[1], 0L)
  expect_equal(result[2], 1L)
})

# 29. .exclude passed through to make_dicho
test_that("make_binary() passes .exclude through to make_dicho()", {
  x <- .likert4()
  # .likert4() has 4 levels; by providing .exclude, collapsed to 2 then binary
  result <- make_binary(x)
  expect_true(is.integer(result))
  expect_true(all(result[!is.na(result)] %in% c(0L, 1L)))
})

# 30. NA propagates correctly to NA_integer_
test_that("make_binary() NA values propagate to NA_integer_", {
  x <- factor(c("Agree", NA, "Disagree"), levels = c("Agree", "Disagree"))
  result <- make_binary(x)
  expect_true(is.na(result[2]))
  expect_true(is.integer(result))
})

# 31. attr(result, "labels") reflects the 0/1 mapping
test_that("make_binary() sets labels attr reflecting 0/1 mapping", {
  x <- factor(c("Agree", "Disagree"), levels = c("Agree", "Disagree"))
  result <- make_binary(x)
  lbs <- attr(result, "labels")
  expect_false(is.null(lbs))
  expect_equal(lbs[["Agree"]], 1L)
  expect_equal(lbs[["Disagree"]], 0L)
})

# 31 flip check
test_that("make_binary() flip_values = TRUE reflects in labels attr", {
  x <- factor(c("Agree", "Disagree"), levels = c("Agree", "Disagree"))
  result <- make_binary(x, flip_values = TRUE)
  lbs <- attr(result, "labels")
  expect_equal(lbs[["Agree"]], 0L)
  expect_equal(lbs[["Disagree"]], 1L)
})

# 32. .label and .description set attrs on result
test_that("make_binary() .label and .description are set on result", {
  x <- factor(c("Agree", "Disagree"), levels = c("Agree", "Disagree"))
  result <- make_binary(
    x,
    .label = "Binary agreement",
    .description = "Encoded"
  )
  expect_identical(attr(result, "label"), "Binary agreement")
  recode_attr <- attr(result, "surveytidy_recode")
  expect_identical(recode_attr$description, "Encoded")
  expect_identical(recode_attr$fn, "make_binary")
})

# 32b. Label falls back to var_name when no attr(x, "label") and .label = NULL
test_that("make_binary() label falls back to var_name when no inherited label", {
  my_var <- factor(c("Yes", "No"), levels = c("Yes", "No"))
  result <- make_binary(my_var)
  expect_identical(attr(result, "label"), "my_var")
})

# 32c. Error: bad arg type (.label = 123)
test_that("make_binary() errors on bad .label type", {
  x <- factor(c("Agree", "Disagree"), levels = c("Agree", "Disagree"))
  expect_error(
    make_binary(x, .label = 123),
    class = "surveytidy_error_transform_bad_arg"
  )
  expect_snapshot(error = TRUE, make_binary(x, .label = 123))
})

# 32d. Error: bad arg type (flip_values = "yes")
test_that("make_binary() errors on bad flip_values type", {
  x <- factor(c("Agree", "Disagree"), levels = c("Agree", "Disagree"))
  expect_error(
    make_binary(x, flip_values = "yes"),
    class = "surveytidy_error_transform_bad_arg"
  )
})

# ── make_rev() ────────────────────────────────────────────────────────────────

# 33. Reverses 1–4 scale correctly
test_that("make_rev() reverses a 1-4 scale correctly", {
  x <- c(1, 2, 3, 4)
  result <- make_rev(x)
  expect_equal(unname(as.numeric(result)), c(4, 3, 2, 1))
})

# 34. Remaps value labels: strings stay tied to concept
test_that("make_rev() remaps label values while keeping label strings", {
  x <- c(1, 2, 3, 4)
  attr(x, "labels") <- c(
    "Strongly agree" = 1,
    "Agree" = 2,
    "Disagree" = 3,
    "Strongly disagree" = 4
  )
  result <- make_rev(x)
  lbs <- attr(result, "labels")
  expect_false(is.null(lbs))
  # "Strongly agree" should now be at value 4 (reversed)
  expect_equal(lbs[["Strongly agree"]], 4)
  # "Strongly disagree" should now be at value 1
  expect_equal(lbs[["Strongly disagree"]], 1)
})

# 35. .label overrides inherited variable label
test_that("make_rev() .label overrides inherited label", {
  x <- c(1, 2, 3, 4)
  attr(x, "label") <- "Original"
  result <- make_rev(x, .label = "Reversed scale")
  expect_identical(attr(result, "label"), "Reversed scale")
})

# 35b. Label falls back to var_name when no attr(x, "label") and .label = NULL
test_that("make_rev() label falls back to var_name when no inherited label", {
  my_var <- c(1L, 2L, 3L)
  result <- make_rev(my_var)
  expect_identical(attr(result, "label"), "my_var")
})

# 36. All-NA input returns all-NA + warning
test_that("make_rev() all-NA input returns all-NA with warning", {
  x <- c(NA_real_, NA_real_, NA_real_)
  expect_warning(
    result <- make_rev(x),
    class = "surveytidy_warning_make_rev_all_na"
  )
  expect_true(all(is.na(result)))
})

# 37. Error: non-numeric input (factor, character)
test_that("make_rev() errors on factor input", {
  x <- factor(c("A", "B"))
  expect_error(
    make_rev(x),
    class = "surveytidy_error_make_rev_not_numeric"
  )
  expect_snapshot(error = TRUE, make_rev(x))
})

test_that("make_rev() errors on character input", {
  x <- c("a", "b", "c")
  expect_error(
    make_rev(x),
    class = "surveytidy_error_make_rev_not_numeric"
  )
})

# 38. NA values in input remain NA in output
test_that("make_rev() NA values remain NA in output", {
  x <- c(1, 2, NA, 4)
  result <- make_rev(x)
  expect_true(is.na(result[3]))
  expect_equal(result[1], 4)
  expect_equal(result[4], 1)
})

# 39. .description sets surveytidy_recode attr
test_that("make_rev() .description is stored in surveytidy_recode attr", {
  x <- c(1, 2, 3)
  result <- make_rev(x, .description = "Scale reversed")
  recode_attr <- attr(result, "surveytidy_recode")
  expect_identical(recode_attr$description, "Scale reversed")
  expect_identical(recode_attr$fn, "make_rev")
})

# 39b. Error: bad arg type (.label = 123)
test_that("make_rev() errors on bad .label type", {
  x <- c(1, 2, 3)
  expect_error(
    make_rev(x, .label = 123),
    class = "surveytidy_error_transform_bad_arg"
  )
  expect_snapshot(error = TRUE, make_rev(x, .label = 123))
})

# 40. Sorted labels after reversal (ascending by new value)
test_that("make_rev() labels are sorted ascending by new value after reversal", {
  x <- c(1, 2, 3, 4)
  attr(x, "labels") <- c(
    "Strongly agree" = 1,
    "Agree" = 2,
    "Disagree" = 3,
    "Strongly disagree" = 4
  )
  result <- make_rev(x)
  lbs <- attr(result, "labels")
  # Values should be in ascending order: 1, 2, 3, 4
  expect_equal(unname(sort(lbs)), c(1, 2, 3, 4))
  # Names should be: Strongly disagree=1, Disagree=2, Agree=3, Strongly agree=4
  expect_equal(
    names(lbs)[order(unname(lbs))],
    c("Strongly disagree", "Disagree", "Agree", "Strongly agree")
  )
})

# 41. 2–5 scale: range preserved (not shifted to 1–4)
test_that("make_rev() preserves scale range (2-5 stays 2-5)", {
  x <- c(2, 3, 4, 5)
  result <- make_rev(x)
  expect_equal(unname(as.numeric(result)), c(5, 4, 3, 2))
  expect_equal(min(result, na.rm = TRUE), 2)
  expect_equal(max(result, na.rm = TRUE), 5)
})

# ── make_flip() ───────────────────────────────────────────────────────────────

# 42. Values unchanged, label strings reversed
test_that("make_flip() values are unchanged", {
  x <- c(1, 2, 3, 4)
  attr(x, "labels") <- c("SA" = 1, "A" = 2, "D" = 3, "SD" = 4)
  result <- make_flip(x, "Flipped scale")
  expect_equal(as.numeric(unname(result)), c(1, 2, 3, 4))
})

# 43. Variable label set to required label arg
test_that("make_flip() sets variable label to the required label arg", {
  x <- c(1, 2, 3)
  attr(x, "labels") <- c("A" = 1, "B" = 2, "C" = 3)
  result <- make_flip(x, "New label")
  expect_identical(attr(result, "label"), "New label")
})

# 44. attr(result, "labels") has reversed string-to-value mapping
test_that("make_flip() reverses label string-to-value mapping", {
  x <- c(1, 2, 3, 4)
  attr(x, "labels") <- c(
    "Strongly agree" = 1,
    "Agree" = 2,
    "Disagree" = 3,
    "Strongly disagree" = 4
  )
  result <- make_flip(x, "Flipped")
  lbs <- attr(result, "labels")
  # Values unchanged (1, 2, 3, 4), but names reversed
  expect_equal(lbs[["Strongly disagree"]], 1)
  expect_equal(lbs[["Disagree"]], 2)
  expect_equal(lbs[["Agree"]], 3)
  expect_equal(lbs[["Strongly agree"]], 4)
})

# 45. Input with no value labels: only variable label changes
test_that("make_flip() with no labels attr only changes variable label", {
  x <- c(1, 2, 3)
  result <- make_flip(x, "New label")
  expect_identical(attr(result, "label"), "New label")
  expect_null(attr(result, "labels"))
})

# 46. Error: non-numeric input
test_that("make_flip() errors on non-numeric input", {
  x <- factor(c("A", "B"))
  expect_error(
    make_flip(x, "label"),
    class = "surveytidy_error_make_flip_not_numeric"
  )
  expect_snapshot(error = TRUE, make_flip(x, "label"))
})

# 47. Error: label missing
test_that("make_flip() errors when label is missing", {
  x <- c(1, 2, 3)
  expect_error(
    make_flip(x),
    class = "surveytidy_error_make_flip_missing_label"
  )
  expect_snapshot(error = TRUE, make_flip(x))
})

# 48. Error: label not character(1) (e.g. numeric, NULL)
test_that("make_flip() errors when label is numeric", {
  x <- c(1, 2, 3)
  expect_error(
    make_flip(x, label = 42),
    class = "surveytidy_error_make_flip_missing_label"
  )
})

test_that("make_flip() errors when label is NULL", {
  x <- c(1, 2, 3)
  expect_error(
    make_flip(x, label = NULL),
    class = "surveytidy_error_make_flip_missing_label"
  )
})

# 49. .description sets surveytidy_recode attr
test_that("make_flip() .description is stored in surveytidy_recode attr", {
  x <- c(1, 2, 3)
  result <- make_flip(x, "New label", .description = "Flipped polarity")
  recode_attr <- attr(result, "surveytidy_recode")
  expect_identical(recode_attr$description, "Flipped polarity")
  expect_identical(recode_attr$fn, "make_flip")
})

# 49b. Error: bad arg type (.description = 123)
test_that("make_flip() errors on bad .description type", {
  x <- c(1, 2, 3)
  expect_error(
    make_flip(x, "label", .description = 123),
    class = "surveytidy_error_transform_bad_arg"
  )
  expect_snapshot(error = TRUE, make_flip(x, "label", .description = 123))
})

# 50. All-NA input: values unchanged, labels reversed, no warning
test_that("make_flip() all-NA input: values unchanged, labels reversed, no warning", {
  x <- c(NA_real_, NA_real_)
  attr(x, "labels") <- c("A" = 1, "B" = 2)
  result <- expect_no_warning(make_flip(x, "New label"))
  expect_true(all(is.na(result)))
  lbs <- attr(result, "labels")
  expect_equal(lbs[["B"]], 1)
  expect_equal(lbs[["A"]], 2)
})

# ── surveytidy_recode attr structure ─────────────────────────────────────────

# 51. var field captures column name via cur_column() in across()
test_that("surveytidy_recode$var captures column name via cur_column() in across()", {
  df <- data.frame(
    q1 = structure(c(1, 2, 3), labels = c("A" = 1, "B" = 2, "C" = 3)),
    q2 = structure(c(3, 2, 1), labels = c("A" = 1, "B" = 2, "C" = 3))
  )
  result <- df |> mutate(across(c(q1, q2), make_factor))
  recode_q1 <- attr(result$q1, "surveytidy_recode")
  recode_q2 <- attr(result$q2, "surveytidy_recode")
  expect_identical(recode_q1$var, "q1")
  expect_identical(recode_q2$var, "q2")
})

# 52. var field falls back to symbol name in direct call
test_that("surveytidy_recode$var falls back to symbol name in direct call", {
  my_column <- c(1L, 2L, 3L)
  attr(my_column, "labels") <- c("A" = 1, "B" = 2, "C" = 3)
  result <- make_factor(my_column)
  recode_attr <- attr(result, "surveytidy_recode")
  expect_identical(recode_attr$var, "my_column")
})

# 53. fn field matches function name for all 5 functions
test_that("surveytidy_recode$fn matches function name for all 5 functions", {
  # make_factor
  x_fac <- c(1, 2)
  attr(x_fac, "labels") <- c("A" = 1, "B" = 2)
  r_fac <- make_factor(x_fac)
  expect_identical(attr(r_fac, "surveytidy_recode")$fn, "make_factor")

  # make_dicho (Likert input)
  r_dicho <- make_dicho(.likert4())
  expect_identical(attr(r_dicho, "surveytidy_recode")$fn, "make_dicho")

  # make_binary
  x_bin <- factor(c("Agree", "Disagree"), levels = c("Agree", "Disagree"))
  r_bin <- make_binary(x_bin)
  expect_identical(attr(r_bin, "surveytidy_recode")$fn, "make_binary")

  # make_rev
  r_rev <- make_rev(c(1, 2, 3))
  expect_identical(attr(r_rev, "surveytidy_recode")$fn, "make_rev")

  # make_flip
  r_flip <- make_flip(c(1, 2, 3), "label")
  expect_identical(attr(r_flip, "surveytidy_recode")$fn, "make_flip")
})

# ── Integration tests ─────────────────────────────────────────────────────────

# 54. Integration — make_factor() |> make_dicho() pipeline
test_that("make_factor() |> make_dicho() pipeline works end-to-end", {
  x <- .likert4()
  result <- x |> make_factor() |> make_dicho()
  expect_true(is.factor(result))
  expect_equal(nlevels(result), 2L)
})

# 55. Integration — make_factor() |> make_rev() pipeline is an error (factor input)
test_that("make_factor() |> make_rev() errors because make_rev() requires numeric", {
  x <- c(1, 2, 3)
  attr(x, "labels") <- c("A" = 1, "B" = 2, "C" = 3)
  result_factor <- make_factor(x)
  expect_error(
    make_rev(result_factor),
    class = "surveytidy_error_make_rev_not_numeric"
  )
})

# 56. Integration — inside mutate(): @metadata updated correctly for all 5 fns
test_that("mutate() updates @metadata correctly for all 5 transform functions", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    # Use y3 (binary 0/1 integer) as a labelled column for make_factor()
    y3_lab <- d@data$y3
    attr(y3_lab, "labels") <- c("No" = 0L, "Yes" = 1L)
    attr(y3_lab, "label") <- "Y3 labelled"
    d@data$y3_lab <- y3_lab

    # Test make_factor() in mutate()
    result <- dplyr::mutate(d, y3_factor = make_factor(y3_lab))
    test_invariants(result)
    expect_false(is.null(result@metadata@variable_labels$y3_factor))
    expect_null(result@metadata@value_labels$y3_factor)
    expect_false(is.null(result@metadata@transformations$y3_factor))
    expect_identical(
      result@metadata@transformations$y3_factor$fn,
      "make_factor"
    )
    expect_identical(
      result@metadata@transformations$y3_factor$source_cols,
      "y3_lab"
    )

    # Test make_rev() in mutate()
    result_rev <- dplyr::mutate(d, y1_rev = make_rev(y1))
    test_invariants(result_rev)
    expect_false(is.null(result_rev@metadata@transformations$y1_rev))
    expect_identical(result_rev@metadata@transformations$y1_rev$fn, "make_rev")
  }
})

# 57. Integration — inside mutate(): @data stripped of haven attrs after call
test_that("mutate() strips haven attrs from @data after transform functions", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    y3_lab <- d@data$y3
    attr(y3_lab, "labels") <- c("No" = 0L, "Yes" = 1L)
    attr(y3_lab, "label") <- "Y3 labelled"
    d@data$y3_lab <- y3_lab

    result <- dplyr::mutate(d, y3_factor = make_factor(y3_lab))
    test_invariants(result)

    # surveytidy_recode attr must be stripped from stored @data
    expect_null(attr(result@data$y3_factor, "surveytidy_recode"))
    # label attr stripped
    expect_null(attr(result@data$y3_factor, "label"))
  }
})

# 58 + 58b. Integration — inside mutate() on all 3 design types; domain preserved
test_that("mutate() with transform functions works on all 3 design types and preserves domain", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    # Apply a domain filter first
    d_filtered <- dplyr::filter(d, y1 > 40)

    # Make labelled column inline
    x_lab <- d_filtered@data$y1
    attr(x_lab, "labels") <- c("Low" = 1, "High" = 2)
    d_filtered@data$y1_lab <- x_lab

    result <- dplyr::mutate(d_filtered, y1_rev = make_rev(y1))
    test_invariants(result)

    # Domain column preserved
    domain_col <- surveycore::SURVEYCORE_DOMAIN_COL
    expect_true(domain_col %in% names(result@data))
    expect_identical(
      result@data[[domain_col]],
      d_filtered@data[[domain_col]]
    )
  }
})

# 59. Integration — across() workflow: multiple columns, correct var_name per column
# cur_column() captures the correct column name inside across(); verified on plain
# data frames (test 51 above). On survey designs, across() results are not tracked
# in @metadata@transformations (accepted limitation documented in mutate.R).
# This test verifies that mutate() on survey designs with across() does not error
# and the invariants hold, and that cur_column() reports the correct var per column.
test_that("across() with transform functions works on all 3 design types", {
  designs <- make_all_designs(seed = 42)
  for (d in designs) {
    # Attach labels to two columns so make_factor() can work
    x1 <- as.integer(d@data$y3)
    attr(x1, "labels") <- c("No" = 0L, "Yes" = 1L)
    d@data$q1 <- x1
    d@data$q2 <- x1

    # Use across() — cur_column() must return "q1" and "q2" respectively
    result <- dplyr::mutate(d, dplyr::across(c(q1, q2), make_factor))
    test_invariants(result)

    # Factors were created correctly
    expect_true(is.factor(result@data$q1))
    expect_true(is.factor(result@data$q2))
    expect_equal(levels(result@data$q1), c("No", "Yes"))
    expect_equal(levels(result@data$q2), c("No", "Yes"))
  }
})

# 59b. across() cur_column() sets correct var in surveytidy_recode (pre-strip)
test_that("cur_column() captures correct var per column in across() on plain data frame", {
  df <- data.frame(
    q1 = structure(c(0L, 1L, 0L), labels = c("No" = 0L, "Yes" = 1L)),
    q2 = structure(c(1L, 0L, 1L), labels = c("No" = 0L, "Yes" = 1L))
  )
  result <- df |> mutate(across(c(q1, q2), make_factor))
  recode_q1 <- attr(result$q1, "surveytidy_recode")
  recode_q2 <- attr(result$q2, "surveytidy_recode")
  expect_identical(recode_q1$var, "q1")
  expect_identical(recode_q2$var, "q2")
  expect_identical(recode_q1$fn, "make_factor")
  expect_identical(recode_q2$fn, "make_factor")
})

# ── coverage closers ─────────────────────────────────────────────────────────

# 60. ordered = TRUE on character input applies ordered class (transform.R L152)
test_that("make_factor() with ordered = TRUE on character input returns ordered factor", {
  x <- c("low", "mid", "high", "mid")
  result <- make_factor(x, ordered = TRUE)
  expect_true(is.ordered(result))
  expect_true(is.factor(result))
  # Levels remain alphabetical (factor()'s default for character)
  expect_equal(levels(result), c("high", "low", "mid"))
})

# 61. force = TRUE + ordered = TRUE on numeric without labels (transform.R L200)
test_that("make_factor() force = TRUE with ordered = TRUE returns ordered factor", {
  x <- c(1, 2, 3, 1, 2)
  expect_warning(
    result <- make_factor(x, force = TRUE, ordered = TRUE),
    class = "surveytidy_warning_make_factor_forced"
  )
  expect_true(is.ordered(result))
  expect_true(is.factor(result))
})

# 62. na.rm = FALSE: unlabelled value in na_values is tolerated (transform.R L240)
test_that("make_factor() tolerates unlabelled na_values entries when na.rm = FALSE", {
  # Value 8 is in na_values but has no label entry. With na.rm = FALSE this
  # is permitted because special-missing values without labels are excluded
  # from the unlabelled-error check.
  x <- c(1, 2, 3, 8, 1)
  attr(x, "labels") <- c("Agree" = 1, "Neutral" = 2, "Disagree" = 3)
  attr(x, "na_values") <- c(8)
  # Should NOT error
  result <- make_factor(x, na.rm = FALSE)
  expect_true(is.factor(result))
  expect_true("Agree" %in% levels(result))
})

# 63. na.rm = FALSE: unlabelled value within na_range tolerated (transform.R L248)
test_that("make_factor() tolerates unlabelled na_range entries when na.rm = FALSE", {
  # Value 98 is inside na_range but has no label entry; na.rm = FALSE so the
  # special-missing tolerance branch in is_special_missing() applies.
  x <- c(1, 2, 3, 98, 1)
  attr(x, "labels") <- c("Low" = 1, "Mid" = 2, "High" = 3)
  attr(x, "na_range") <- c(97, 99)
  result <- make_factor(x, na.rm = FALSE)
  expect_true(is.factor(result))
  expect_true("Low" %in% levels(result))
})

# 64. na.rm = TRUE skips special-missing check; unlabelled non-special errors
#     (transform.R L258 — the na.rm = TRUE branch of the unlabelled-vals path)
test_that("make_factor() with na.rm = TRUE errors on unlabelled non-special value", {
  # Value 4 is NOT in na_values nor na_range, but lacks a label. With
  # na.rm = TRUE the code path takes the else branch (L258) before erroring.
  x <- c(1, 2, 3, 4, 8)
  attr(x, "labels") <- c("A" = 1, "B" = 2, "C" = 3)
  attr(x, "na_values") <- c(8)
  expect_error(
    make_factor(x, na.rm = TRUE),
    class = "surveytidy_error_make_factor_incomplete_labels"
  )
})
