# tests/testthat/test-collection-joins.R
#
# Join-verb error stubs for survey_collection — PR 4.
#
# Spec sections: §V.3 (Joins V8 — error template), §VII.1
# (`surveytidy_error_collection_verb_unsupported`).
#
# All six join verbs (`left_join`, `right_join`, `inner_join`, `full_join`,
# `semi_join`, `anti_join`) error unconditionally when dispatched on a
# `survey_collection`. The error class is the same; the verb name is
# interpolated into the message.

# Small lookup data frame — every join verb takes a y argument.
.collection_join_y <- function() {
  data.frame(group = c("A", "B", "C"), label = letters[1:3])
}

# ── left_join ─────────────────────────────────────────────────────────────

test_that("left_join.survey_collection raises verb_unsupported", {
  coll <- make_test_collection(seed = 42)
  y <- .collection_join_y()

  expect_error(
    dplyr::left_join(coll, y, by = "group"),
    class = "surveytidy_error_collection_verb_unsupported"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::left_join(coll, y, by = "group")
  )
})

# ── right_join ────────────────────────────────────────────────────────────

test_that("right_join.survey_collection raises verb_unsupported", {
  coll <- make_test_collection(seed = 42)
  y <- .collection_join_y()

  expect_error(
    dplyr::right_join(coll, y, by = "group"),
    class = "surveytidy_error_collection_verb_unsupported"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::right_join(coll, y, by = "group")
  )
})

# ── inner_join ────────────────────────────────────────────────────────────

test_that("inner_join.survey_collection raises verb_unsupported", {
  coll <- make_test_collection(seed = 42)
  y <- .collection_join_y()

  expect_error(
    dplyr::inner_join(coll, y, by = "group"),
    class = "surveytidy_error_collection_verb_unsupported"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::inner_join(coll, y, by = "group")
  )
})

# ── full_join ─────────────────────────────────────────────────────────────

test_that("full_join.survey_collection raises verb_unsupported", {
  coll <- make_test_collection(seed = 42)
  y <- .collection_join_y()

  expect_error(
    dplyr::full_join(coll, y, by = "group"),
    class = "surveytidy_error_collection_verb_unsupported"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::full_join(coll, y, by = "group")
  )
})

# ── semi_join ─────────────────────────────────────────────────────────────

test_that("semi_join.survey_collection raises verb_unsupported", {
  coll <- make_test_collection(seed = 42)
  y <- .collection_join_y()

  expect_error(
    dplyr::semi_join(coll, y, by = "group"),
    class = "surveytidy_error_collection_verb_unsupported"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::semi_join(coll, y, by = "group")
  )
})

# ── anti_join ─────────────────────────────────────────────────────────────

test_that("anti_join.survey_collection raises verb_unsupported", {
  coll <- make_test_collection(seed = 42)
  y <- .collection_join_y()

  expect_error(
    dplyr::anti_join(coll, y, by = "group"),
    class = "surveytidy_error_collection_verb_unsupported"
  )
  expect_snapshot(
    error = TRUE,
    dplyr::anti_join(coll, y, by = "group")
  )
})
