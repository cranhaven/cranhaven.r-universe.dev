ae2_table_path <- "../data/ae2.csv"
cutoff_date <- lubridate::ymd("2023-03-18")

test_that("featurise_time_since errors if a non-date column is given", {
  all_tables <- read_data(list(ae2 = ae2_table_path))

  expect_error(
    featurise(
      all_tables,
      json_to_feature("../spec/test_time_since_error.json")
    ),
    regexp = "type 'date'"
  )
})
