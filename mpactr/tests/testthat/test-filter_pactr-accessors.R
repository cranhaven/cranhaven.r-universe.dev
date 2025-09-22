test_that("filter_summary returns an error when an incorrect fitler argument
 is provided", {
            peak_table_name <- "102623_peaktable_coculture_simple.csv"
            meta_data_name <- "102623_metadata_correct.csv"
            data <- import_data(test_path("exttestdata",
                                          peak_table_name),
              test_path("exttestdata", meta_data_name),
              format = "Progenesis"
            )

            expect_error(filter_summary(data,
                                        filter = "cv"),
                         "`filter` must be one of mpactr's")
          })

test_that("filter_summary returns an error when the fitler
 argument provided has not yet been run (e.g., not in the log)", {
            directory <- "exttestdata"
            peak_table_name <- "102623_peaktable_coculture_simple.csv"
            meta_data_name <- "102623_metadata_correct.csv"
            er <-  "`filter` replicability has not yet been applied to the data"
            data <- import_data(test_path(directory, peak_table_name),
              test_path(directory, meta_data_name),
              format = "Progenesis"
            )

            data_mpactr <- filter_mispicked_ions(data,
                                                 ringwin = 0.5,
                                                 isowin = 0.01,
                                                 trwin = 0.005,
                                                 max_iso_shift = 3,
                                                 merge_peaks = TRUE)

            expect_error(filter_summary(data_mpactr,
                                        filter = "replicability"),
                         er)
          })

test_that(" returns the correct fitler summary list", {
  directory <- "exttestdata"
  peak_table_name <- "102623_peaktable_coculture_simple.csv"
  meta_data_name <- "102623_metadata_correct.csv"

  data <- import_data(test_path("exttestdata", peak_table_name),
    test_path("exttestdata", meta_data_name),
    format = "Progenesis"
  )

  data_mpactr <- filter_mispicked_ions(data,
                                       ringwin = 0.5,
                                       isowin = 0.01,
                                       trwin = 0.005,
                                       max_iso_shift = 3,
                                       merge_peaks = TRUE)

  mispicked_summary <- filter_summary(data_mpactr, filter = "mispicked")

  expect_type(mispicked_summary, "list")
  expect_equal(length(mispicked_summary), 2)
  expect_equal(length(mispicked_summary$passed_ions), 1233)
})

test_that("get_similar_ions returns error if 
check_mismatched_peaks has not been called", {
            directory <- "exttestdata"
            peak_table_name <- "102623_peaktable_coculture_simple.csv"
            meta_data_name <- "102623_metadata_correct.csv"

            data <- import_data(test_path(directory, peak_table_name),
              test_path(directory, meta_data_name),
              format = "Progenesis"
            )
            er <- "The mispicked filter has not yet been"
            expect_error(get_similar_ions(data), er)
          })

test_that("get_similar_ions correctly returns the
 check_mismatched_peaks list", {
            directory <- "exttestdata"
            peak_table_name <- "102623_peaktable_coculture_simple.csv"
            meta_data_name <- "102623_metadata_correct.csv"
            data <- import_data(test_path(directory, peak_table_name),
              test_path(directory, meta_data_name),
              format = "Progenesis"
            )

            data_mpactr <- filter_mispicked_ions(data,
                                                 ringwin = 0.5,
                                                 isowin = 0.01,
                                                 trwin = 0.005,
                                                 max_iso_shift = 3,
                                                 merge_peaks = TRUE)

            mispicked_groups <- get_similar_ions(data_mpactr)

            expect_equal(class(mispicked_groups), c("data.table", "data.frame"))
            expect_equal(length(mispicked_groups), 2)
            expect_equal(names(mispicked_groups), c("main_ion", "similar_ions"))
          })

test_that("get_group_averages calculates a group table", {

  directory <- "exttestdata"
  peak_table_name <- "102623_peaktable_coculture_simple.csv"
  meta_data_name <- "102623_metadata_correct.csv"
  data <- import_data(test_path(directory, peak_table_name),
    test_path(directory, meta_data_name),
    format = "Progenesis"
  )

  data_mpactr <- filter_mispicked_ions(data,
                                       ringwin = 0.5,
                                       isowin = 0.01,
                                       trwin = 0.005,
                                       max_iso_shift = 3,
                                       merge_peaks = TRUE)


  avgs <- get_group_averages(data_mpactr)
  expect_equal(class(avgs), c("data.table", "data.frame"))
  expect_equal(nrow(avgs), (1233 * 6))
})


test_that("get_cv_data returns the cv table if filter cv has been run", {
  directory <- "exttestdata"
  peak_table_name <- "102623_peaktable_coculture_simple.csv"
  meta_data_name <- "102623_metadata_correct.csv"
  data <- import_data(test_path(directory, peak_table_name),
    test_path(directory, meta_data_name),
    format = "Progenesis"
  )

  data_mpactr <- filter_mispicked_ions(data,
                                       ringwin = 0.5,
                                       isowin = 0.01,
                                       trwin = 0.005,
                                       max_iso_shift = 3,
                                       merge_peaks = TRUE) |>
    filter_group(group_to_remove = "Blanks")

  expect_error(get_cv_data(data_mpactr), "The cv filter has not yet")

  data_mpactr |>
    filter_cv(cv_threshold = 0.2)

  cv <- get_cv_data(data_mpactr)

  expect_equal(class(cv), c("data.table", "data.frame"))
})
