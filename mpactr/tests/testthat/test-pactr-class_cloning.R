test_that("The clone function properly deep clones the mpactr object", {
  directory <- "exttestdata"
  peak_table_name <- "102623_peaktable_coculture_simple.csv"
  meta_data_name <- "102623_metadata_correct.csv"
  mpactr_object <- import_data(test_path(directory, peak_table_name),
    test_path(directory, meta_data_name),
    format = "Progenesis"
  )

  mpactr_object <- filter_mispicked_ions(mpactr_object,
    ringwin = 0.5, isowin = 0.01, trwin = 0.005, max_iso_shift = 3,
    merge_peaks = TRUE
  )

  mpactr_object_clone <- clone(mpactr_object)

  mpactr_object_clone <- filter_group(mpactr_object_clone,
                                      0.01, "Coculture", TRUE)
  expect_false(nrow(get_peak_table(mpactr_object)) ==
                 nrow(get_peak_table(mpactr_object_clone)))

  identical(mpactr_object, mpactr_object_clone)
  expect_false(address(mpactr_object) == address(mpactr_object_clone))
  expect_false(address(mpactr_object$mpactr_data) ==
                 address(mpactr_object_clone$mpactr_data))
  expect_false(address(mpactr_object$logger) ==
                 address(mpactr_object_clone$logger))
  address(mpactr_object$mpactr_data$get_peak_table()) ==
    address(mpactr_object_clone$mpactr_data$get_peak_table())
})
