test_that("get_peak_table correctly returns the table", {

  directory <- "exttestdata"
  peak_table_name <- "102623_peaktable_coculture_simple.csv"
  meta_data_name <- "102623_metadata_correct.csv"
  data <- import_data(test_path(directory, peak_table_name),
    test_path(directory, meta_data_name),
    format = "Progenesis"
  )

  peak_table <- get_peak_table(data)
  expect_equal(class(peak_table), c("data.table", "data.frame"))
  expect_equal(nrow(peak_table), 1303)
})

test_that("get_meta_data correctly returns the table", {
  directory <- "exttestdata"
  peak_table_name <- "102623_peaktable_coculture_simple.csv"
  meta_data_name <- "102623_metadata_correct.csv"
  data <- import_data(test_path(directory, peak_table_name),
    test_path(directory, meta_data_name),
    format = "Progenesis"
  )

  metadata <- get_meta_data(data)
  expect_equal(class(metadata), c("data.table", "data.frame"))
  expect_equal(nrow(metadata), 18)
})

test_that("get_raw_data correctly returns the table", {
  directory <- "exttestdata"
  peak_table_name <- "102623_peaktable_coculture_simple.csv"
  meta_data_name <- "102623_metadata_correct.csv"
  data <- import_data(test_path(directory, peak_table_name),
    test_path(directory, meta_data_name),
    format = "Progenesis"
  )

  raw_data <- get_raw_data(data)
  expect_equal(class(raw_data), c("data.table", "data.frame"))
  expect_equal(nrow(raw_data), 1306)
})
