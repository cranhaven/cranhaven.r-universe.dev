
test_that("CFS_format returns correct structure", {

  # locate file
  raw_data <- system.file("extdata", "dt.samples.csv", package = "growthTrendR")

  # file existence tests
  expect_true(nzchar(raw_data))
  expect_true(file.exists(raw_data))

  # read data
  dt.samples <- data.table::fread(raw_data)

  # run function
  dt.samples_trt <- suppressMessages(suppressWarnings(
    CFS_format(
    data    = list(dt.samples, 39:68),
    usage   = 1,
    out.csv = NULL
  )
)
)
  # class tests
  expect_s3_class(dt.samples_trt, "cfs_format")

  # structure tests
  expect_named(dt.samples_trt, c("tr_all_long", "tr_all_wide", "complete_vars"))

  # element classes
  expect_true(is.list(dt.samples_trt$tr_all_long))
  expect_s3_class(dt.samples_trt$tr_all_wide, "data.table")
  expect_s3_class(dt.samples_trt$complete_vars, "data.table")

  # row consistency
  expect_equal(nrow(dt.samples_trt$tr_all_wide), nrow(dt.samples))


  expect_true(all(c("species", "uid_site", "site_id", "latitude","longitude", "uid_tree", "uid_sample",
                    "sample_id", "radius_id","uid_radius") %in% names(dt.samples_trt$tr_all_wide)))


})



test_that("CFS_freq returns correct structure", {


  # loading processed data
  dt.samples_trt <- get_test_samples_trt()

  dt.freq <- CFS_freq(dt.samples_trt$tr_all_wide, freq.label_data = "demo-samples", freq.uid_level = "uid_radius")
  expect_s3_class(dt.freq, "cfs_freq")
  # element classes
  expect_true(is.list(dt.freq$freq.parms))
  expect_s3_class(dt.freq$dist_uids, "data.table")


})

test_that("generate_report returns correct structure", {


  # loading processed data
  dt.samples_trt <- get_test_samples_trt()
  # genereate data summary report
  generate_report(dt.samples_trt)
  expect_error(generate_report(dt.samples_trt), NA)

})
