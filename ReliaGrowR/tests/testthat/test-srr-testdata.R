#' @srrstats {G5.1} The data set is created within and used to test the package.
#' The data set is exported so that users can confirm tests and run examples.
#' @srrstats {G5.10} All unit tests run as part of continuous integration.

test_that("testdata dataset has expected structure", {
  data("testdata", package = "ReliaGrowR")

  # check class
  expect_s3_class(testdata, "data.frame")

  # check dimensions
  expect_equal(nrow(testdata), 25)
  expect_equal(ncol(testdata), 7)

  # check column names
  expected_cols <- c(
    "LRU", "Cum_ETI", "Failure_Count",
    "Cum_MTBF", "Report_No", "Flag", "Cause"
  )
  expect_equal(names(testdata), expected_cols)

  # check column types
  expect_type(testdata$LRU, "character") # could be factor, depends how saved
  expect_type(testdata$Cum_ETI, "double")
  expect_type(testdata$Failure_Count, "integer") # or "double"
  expect_type(testdata$Cum_MTBF, "double")
  expect_type(testdata$Report_No, "double") # or "double"
  expect_type(testdata$Flag, "double") # or factor
  expect_type(testdata$Cause, "character") # or factor

  # sanity checks on values
  expect_true(all(testdata$Cum_ETI >= 0))
  expect_true(all(testdata$Failure_Count >= 0))
  expect_true(all(testdata$Cum_MTBF > 0))
  expect_true(all(testdata$LRU %in% c("G1", "G2")))
  expect_true(all(testdata$Cause %in% c("D", "M", "R", "NR")))
})
