test_that("get_groups() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("jsonlite", quietly = TRUE))
  expect_true(requireNamespace("httr2", quietly = TRUE))
})

test_that("get_groups()", {
  expect_snapshot(head(RcensusPkg::get_groups(
      dataset = "acs/acs5",
      vintage = 2019
    ))
  )
})
