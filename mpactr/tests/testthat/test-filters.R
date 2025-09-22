test_that("filter mismatch ions wrapper works
 as expected when merge_peaks is TRUE", {
            directory <- "exttestdata"
            peak_table_name <- "102623_peaktable_coculture_simple.csv"
            meta_data_name <- "102623_metadata_correct.csv"
            data <- import_data(test_path(directory, peak_table_name),
              test_path(directory, meta_data_name),
              format = "Progenesis"
            )

            data_mpactr_copy <- filter_mispicked_ions(data,
              ringwin = 0.5, isowin = 0.01, trwin = 0.005, max_iso_shift = 3,
              merge_peaks = TRUE, merge_method = "sum", copy_object = TRUE
            )

            data_mpactr <- filter_mispicked_ions(data,
              ringwin = 0.5, isowin = 0.01, trwin = 0.005, max_iso_shift = 3,
              merge_peaks = TRUE, merge_method = "sum", copy_object = FALSE
            )

            expected_cut_ions <- read_csv(test_path(directory,
                                                    "cut_ions.csv"),
                                          col_names = c("V1"),
                                          show_col_types = FALSE)
            expected_cut_ions <- as.character(expected_cut_ions$V1)

            expect_equal(nrow(data_mpactr$mpactr_data$get_peak_table()),
                         nrow((data_mpactr$mpactr_data$get_peak_table())))
            log_mm <- "check_mismatched_peaks"
            expect_equal(data_mpactr$logger[[log_mm]][["cut_ions"]],
                         expected_cut_ions)
            expect_equal(data_mpactr_copy$logger[[log_mm]][["cut_ions"]],
                         expected_cut_ions)

            expect_equal(nrow(data_mpactr$mpactr_data$get_peak_table()), 1233)

            dat <- import_data(peak_table = get_peak_table(data_mpactr_copy),
                               meta_data = get_meta_data(data_mpactr_copy),
                               format = "None")
            filtered_data <- dat |>
              filter_mispicked_ions(merge_peaks = TRUE,
                                    merge_method = "sum",
                                    copy_object = TRUE)
            expect_equal(nrow(get_peak_table(dat)),
                         nrow(get_peak_table(filtered_data)))
          })

test_that("filter mismatch ions wrapper works 
as expected when merge_peaks is FALSE", {
            directory <- "exttestdata"
            peak_table_name <- "102623_peaktable_coculture_simple.csv"
            meta_data_name <- "102623_metadata_correct.csv"
            data <- import_data(test_path(directory, peak_table_name),
              test_path(directory, meta_data_name),
              format = "Progenesis"
            )

            data_mpactr_copy <- filter_mispicked_ions(data,
              ringwin = 0.5, isowin = 0.01, trwin = 0.005, max_iso_shift = 3,
              merge_peaks = FALSE, merge_method = "sum", copy_object = TRUE
            )


            data_mpactr <- filter_mispicked_ions(data,
              ringwin = 0.5, isowin = 0.01, trwin = 0.005, max_iso_shift = 3,
              merge_peaks = FALSE
            )

            expect_equal(nrow(data_mpactr$mpactr_data$get_peak_table()), 1303)
            expect_equal(nrow(data_mpactr_copy$mpactr_data$get_peak_table()),
                         1303)
          })

test_that("group filter wrapper works as expected", {
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
    merge_peaks = TRUE,
    merge_method = "sum"
  )
  data_mpactr <- filter_group(data_mpactr, 0.01, "Blanks", FALSE)
  expect_equal(nrow(data_mpactr$mpactr_data$get_peak_table()), 1233)

  data_mpactr_copy <- filter_group(data_mpactr, 0.01, "Blanks", TRUE,
    copy_object = TRUE
  )

  data_mpactr <- filter_group(data_mpactr, 0.01, "Blanks", TRUE)
  expect_equal(nrow(data_mpactr$mpactr_data$get_peak_table()), 484)

  expect_equal(
    nrow(data_mpactr$mpactr_data$get_peak_table()),
    nrow(data_mpactr_copy$mpactr_data$get_peak_table())
  )

  log_name <- "group_filter-failing_list"
  expect_true(all(!(data_mpactr$logger[[log_name]]$Blanks
                    %in% data_mpactr$mpactr_data$get_peak_table()$Compound)))

  expect_true(all(!(data_mpactr_copy$logger[[log_name]]$Blanks %in%
                      data_mpactr_copy$mpactr_data$get_peak_table()$Compound)))
})

test_that("filter cv filter wrapper works as expected", {
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
    merge_peaks = TRUE,
    merge_method = "sum"
  )
  data_mpactr <- filter_group(data_mpactr, 0.01, "Blanks", TRUE)
  data_mpactr_copy <- filter_cv(data_mpactr, 0.2, copy_object = TRUE)
  data_mpactr <- filter_cv(data_mpactr, 0.2)


  expect_equal(length(data_mpactr$logger[["list_of_summaries"]]$
                        replicability$get_failed_ions()), 33)
  expect_equal(length(data_mpactr_copy$logger[["list_of_summaries"]]$
                        replicability$get_failed_ions()), 33)

  meta <- get_meta_data(data)[c(1, 4), ]
  df <- get_raw_data(data)
  df <- cbind(df[, 1:3], df[, meta$Injection, with = FALSE])

  data_no_replicates <- import_data(peak_table = df,
                                    meta_data = meta,
                                    format = "None")
  expect_error(filter_cv(data_no_replicates, cv_threshold = 0.1),
               "There are no technical replicates")
})

test_that("filter insource ions wrapper works as expected", {
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
    merge_peaks = TRUE,
    merge_method = "sum"
  )
  data_mpactr <- filter_group(data_mpactr, 0.01, "Blanks", TRUE)

  data_mpactr_copy <- filter_insource_ions(data_mpactr,
                                           cluster_threshold = 0.95,
                                           copy_object = TRUE)

  data_mpactr <- filter_insource_ions(data_mpactr, cluster_threshold = 0.95)

  insource_ion_expected_list <- c(
    38, 204, 214, 993, 270, 1003, 271, 294, 331, 349, 382,
    447, 498, 1233, 644, 1307, 677, 675, 689,
    690, 688, 758, 985, 982, 981, 1297, 1311
  )

  expect_true(length(data_mpactr$logger[["list_of_summaries"]]$
                       insource$get_failed_ions()) == 27)
  expect_true(length(data_mpactr_copy$logger[["list_of_summaries"]]$
                       insource$get_failed_ions()) == 27)

  expect_true(all(!(insource_ion_expected_list %in%
                      data_mpactr$mpactr_data$get_peak_table()$Compound)))
  expect_true(all(!(insource_ion_expected_list %in% data_mpactr_copy$
                      mpactr_data$get_peak_table()$Compound)))
})

test_that("filters abort if the filter has already been run", {
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
    merge_peaks = TRUE,
    merge_method = "sum"
  )
  data_mpactr <- filter_group(data_mpactr, 0.01, "Blanks", TRUE)
  data_mpactr <- filter_cv(data_mpactr, cv_threshold = 0.2)
  data_mpactr <- filter_insource_ions(data_mpactr, cluster_threshold = 0.95)

  expect_error(filter_mispicked_ions(data_mpactr,
                                     ringwin = 0.5,
                                     isowin = 0.01,
                                     trwin = 0.005,
                                     max_iso_shift = 3,
                                     merge_peaks = TRUE,
                                     merge_method = "sum"))
  expect_error(filter_group(data_mpactr, 0.01, "Blanks", TRUE))
  expect_error(filter_cv(data_mpactr, cv_threshold = 0.2))
  expect_error(filter_insource_ions(data_mpactr, cluster_threshold = 0.95))
})
