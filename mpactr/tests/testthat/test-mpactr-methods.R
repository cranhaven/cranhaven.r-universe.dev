test_that("mpactr class setup works properly", {
  directory <- "exttestdata"
  peak_table_name <- "102623_peaktable_coculture_simple.csv"
  meta_data_name <- "102623_metadata_correct.csv"
  meta <- data.table(read_csv(test_path(directory, meta_data_name),
                              show_col_types = FALSE))
  pt_list <- progenesis_formatter(test_path(directory, peak_table_name))

  mpactr_class <- mpactr$new(
    pt_list,
    meta
  )
  mpactr_class$setup()

  expect_true(all(c("kmd", "mz", "rt") %in%
                    colnames(mpactr_class$get_peak_table())))
  expect_equal(nrow(mpactr_class$get_peak_table()), 1303)
})
