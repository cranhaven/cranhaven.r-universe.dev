test_that("mpactr class initialize works correctly", {

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

  expect_true(all(class(mpactr_class) == c("mpactr", "R6")))
  expect_equal(length(mpactr_class$get_meta_data()), 3)
  expect_equal(length(mpactr_class$get_peak_table()), 21)
  expect_error(mpactr$new(peak_table_path = 2, meta_data_path = 5), NULL)
})

test_that("mpactr isMultipleTechReps correctly
  determines if there are technical replicates", {
            directory <- "exttestdata"
            peak_table_name <- "102623_peaktable_coculture_simple.csv"
            meta_data_name <- "102623_metadata_correct.csv"
            meta <- data.table(read_csv(test_path(directory, meta_data_name),
                                        show_col_types = FALSE))
            pt_list <- progenesis_formatter(test_path(directory,
                                                      peak_table_name))

            mpactr_class <- mpactr$new(
              pt_list,
              meta
            )
            expect_true(mpactr_class$isMultipleTechReps())
          })
