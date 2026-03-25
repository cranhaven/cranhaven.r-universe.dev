test_that("run_pipeline errors on duplicate feature names", {
  ae2_table_path <- "../data/ae2.csv"

  all_table_filenames <- c(ae2_table_path)

  expect_error(
    run_pipeline(
      data_sources = list(ae2 = ae2_table_path),
      feature_filenames = c(
        "../spec/test_join1.json",
        "../spec/test_join1.json"
      )
    ),
    regexp = "Duplicate feature"
  )
})

test_that("run_pipeline errors on duplicate response names", {
  ae2_table_path <- "../data/ae2.csv"

  all_table_filenames <- c(ae2_table_path)

  expect_error(
    run_pipeline(
      data_sources = list(ae2 = ae2_table_path),
      response_filenames = c(
        "../spec/test_join1.json",
        "../spec/test_join1.json"
      )
    ),
    regexp = "Duplicate response"
  )
})
