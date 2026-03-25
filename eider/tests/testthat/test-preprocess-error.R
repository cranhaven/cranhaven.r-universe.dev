test_that("preprocess_table errors with duplicate columns", {
  df <- read_one_table("../data/preproc.csv")

  mock_preproc_spec <- list(
    grouping_column = "id",
    preprocess = list(
      on = c("id", "group"),
      retain_min = "start_date",
      retain_max = "start_date"
    )
  )

  expect_error(
    preprocess_table(df, mock_preproc_spec),
    regexp = "multiple"
  )
})
