test_that("get_dataset_names() namespaces", {
  expect_true(requireNamespace("data.table", quietly = TRUE))
  expect_true(requireNamespace("jsonlite", quietly = TRUE))
  expect_true(requireNamespace("httr2", quietly = TRUE))
})

test_that("get_dataset_names() acs5", {
  testthat::skip(message = "Census Bureau request takes well over 10 seconds in most cases.")
  expect_snapshot({
    acs5_datasets_ls <- RcensusPkg::get_dataset_names(
      vintage = 2020,
      filter_name_str = "acs5/"
    )
  })
  expect_snapshot(acs5_datasets_ls$data)
})
