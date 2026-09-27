# tests/testthat/test-collection-pipeline.R
#
# Cross-verb integration test for survey_collection — PR 4.
#
# Pipes a `survey_collection` through one verb from each of the four
# verb-family PRs (2a — data-mask, 2b — tidyselect, 2c — grouping, 2d —
# slicing) and asserts:
#
# 1. The result is a well-formed `survey_collection` (collection invariants).
# 2. Every surviving member is a well-formed `survey_base` (per-member
#    invariants).
# 3. The rendered `print()` output is stable (snapshot).
#
# This is the load-bearing assertion that all four verb-family PRs compose
# correctly. Spec sections: §V.1, §V.2, §IX.3.

test_that("survey_collection composes through filter -> select -> group_by -> slice_head", {
  coll <- make_test_collection(seed = 42)

  # PR 2a (data-mask): filter — restricts each member to a domain.
  # PR 2b (tidyselect): select — picks a subset of columns; design vars
  #                              remain protected on each member.
  # PR 2c (grouping): group_by — sets coll@groups and propagates to members.
  # PR 2d (slicing): slice_head — keeps the first 5 rows of each member.
  # slice_head fires `surveycore_warning_physical_subset` per member; that
  # behaviour is exercised in the per-member slice tests, so suppress here.
  result <- suppressWarnings(
    coll |>
      dplyr::filter(y1 > 40) |>
      dplyr::select(y1, y2, group) |>
      dplyr::group_by(group) |>
      dplyr::slice_head(n = 5)
  )

  # Dual invariants — collection-level then per-member.
  test_collection_invariants(result)
  for (member in result@surveys) {
    test_invariants(member)
  }

  # Sanity: groups propagated; visible_vars set on each member.
  expect_identical(result@groups, "group")
  for (member in result@surveys) {
    expect_identical(member@groups, "group")
    expect_true("group" %in% names(member@data))
    expect_identical(
      sort(member@variables$visible_vars),
      sort(c("y1", "y2", "group"))
    )
  }

  # Print snapshot — captures @id, @if_missing_var, @groups, member summary.
  expect_snapshot(print(result))
})
