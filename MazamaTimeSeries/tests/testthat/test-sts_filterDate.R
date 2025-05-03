# NOTE: The following tests are taken from AirSensor

test_that("character date formats work", {

  # IF start and end are specified as numeric or character:
  #   assume the timezone of the sts object

  start_PDT <- ISOdatetime(2018, 08, 02, 07, 00, 00, tz = "UTC")
  end_PDT <- ISOdatetime(2018, 08, 03, 06, 59, 00, tz = "UTC")

  expect_identical(
    c(start_PDT, end_PDT),
    range( sts_filterDate(example_sts, 20180802, 20180803)$data$datetime )
  )

  expect_identical(
    c(start_PDT, end_PDT),
    range( sts_filterDate(example_sts, "20180802", "20180803")$data$datetime )
  )

  expect_identical(
    c(start_PDT, end_PDT),
    range( sts_filterDate(example_sts, "2018-08-02", 20180803)$data$datetime )
  )

  expect_identical(
    c(start_PDT, end_PDT),
    range( sts_filterDate(example_sts, "2018-08-02 00", 20180803)$data$datetime )
  )

  expect_identical(
    c(start_PDT, end_PDT),
    range( sts_filterDate(example_sts, "2018-08-02 00:00:00", 20180803)$data$datetime )
  )

})

test_that("character date formats work with timezones", {

  # IF start and end are specified as numeric or character
  # AND a timezone is specified:
  #   use the timezone to interpret the dates

  # UTC times
  start_HST <- ISOdatetime(2018, 08, 02, 10, 00, 00, tz = "UTC")
  end_HST <- ISOdatetime(2018, 08, 03, 09, 59, 00, tz = "UTC")

  start_PDT <- ISOdatetime(2018, 08, 02, 07, 00, 00, tz = "UTC")
  end_PDT <- ISOdatetime(2018, 08, 03, 06, 59, 00, tz = "UTC")

  start_MDT <- ISOdatetime(2018, 08, 02, 06, 00, 00, tz = "UTC")
  end_MDT <- ISOdatetime(2018, 08, 03, 05, 59, 00, tz = "UTC")

  start_EDT <- ISOdatetime(2018, 08, 02, 04, 00, 00, tz = "UTC")
  end_EDT <- ISOdatetime(2018, 08, 03, 03, 59, 00, tz = "UTC")

  start_UTC <- ISOdatetime(2018, 08, 02, 00, 00, 00, tz = "UTC")
  end_UTC <- ISOdatetime(2018, 08, 02, 23, 59, 00, tz = "UTC")

  expect_identical(
    c(start_PDT, end_PDT),
    range( sts_filterDate(example_sts, 20180802, 20180803)$data$datetime )
  )

  expect_identical(
    c(start_HST, end_HST),
    range( sts_filterDate(example_sts, 20180802, 20180803, timezone = "Pacific/Honolulu")$data$datetime )
  )

  expect_identical(
    c(start_PDT, end_PDT),
    range( sts_filterDate(example_sts, 20180802, 20180803, timezone = "America/Los_Angeles")$data$datetime )
  )

  expect_identical(
    c(start_MDT, end_MDT),
    range( sts_filterDate(example_sts, 20180802, 20180803, timezone = "America/Denver")$data$datetime )
  )

  expect_identical(
    c(start_EDT, end_EDT),
    range( sts_filterDate(example_sts, 20180802, 20180803, timezone = "America/New_York")$data$datetime )
  )

  expect_identical(
    c(start_UTC, end_UTC),
    range( sts_filterDate(example_sts, 20180802, 20180803, timezone = "UTC")$data$datetime )
  )

})

test_that("POSIXct date formats work", {

  # Assume that anyone passing in a POSIXct object knows what they are doing.

  # IF start and end are specified as POSIXct
  #   accept them at face value, ignoring the sensor timezone

  # UTC times
  start_HST <- ISOdatetime(2018, 08, 02, 10, 00, 00, tz = "UTC")
  end_HST <- ISOdatetime(2018, 08, 03, 09, 59, 00, tz = "UTC")

  start_PDT <- ISOdatetime(2018, 08, 02, 07, 00, 00, tz = "UTC")
  end_PDT <- ISOdatetime(2018, 08, 03, 06, 59, 00, tz = "UTC")

  start_MDT <- ISOdatetime(2018, 08, 02, 06, 00, 00, tz = "UTC")
  end_MDT <- ISOdatetime(2018, 08, 03, 05, 59, 00, tz = "UTC")

  start_EDT <- ISOdatetime(2018, 08, 02, 04, 00, 00, tz = "UTC")
  end_EDT <- ISOdatetime(2018, 08, 03, 03, 59, 00, tz = "UTC")

  start_UTC <- ISOdatetime(2018, 08, 02, 00, 00, 00, tz = "UTC")
  end_UTC <- ISOdatetime(2018, 08, 02, 23, 59, 00, tz = "UTC")

  # Local times
  start_Hawaii <- ISOdatetime(2018, 08, 02, 00, 00, 00, tz = "Pacific/Honolulu")
  end_Hawaii <- ISOdatetime(2018, 08, 02, 23, 59, 00, tz = "Pacific/Honolulu")

  start_LA <- ISOdatetime(2018, 08, 02, 00, 00, 00, tz = "America/Los_Angeles")
  end_LA <- ISOdatetime(2018, 08, 02, 23, 59, 00, tz = "America/Los_Angeles")

  start_Denver <- ISOdatetime(2018, 08, 02, 00, 00, 00, tz = "America/Denver")
  end_Denver <- ISOdatetime(2018, 08, 02, 23, 59, 00, tz = "America/Denver")

  start_NYC <- ISOdatetime(2018, 08, 02, 00, 00, 00, tz = "America/New_York")
  end_NYC <- ISOdatetime(2018, 08, 02, 23, 59, 00, tz = "America/New_York")

  expect_identical(
    c(start_HST, end_HST),
    range( sts_filterDate(example_sts, start_Hawaii, end_Hawaii)$data$datetime )
  )

  expect_identical(
    c(start_PDT, end_PDT),
    range( sts_filterDate(example_sts, start_LA, end_LA)$data$datetime )
  )

  expect_identical(
    c(start_MDT, end_MDT),
    range( sts_filterDate(example_sts, start_Denver, end_Denver)$data$datetime )
  )

  expect_identical(
    c(start_EDT, end_EDT),
    range( sts_filterDate(example_sts, start_NYC, end_NYC)$data$datetime )
  )

  expect_identical(
    c(start_UTC, end_UTC),
    range( sts_filterDate(example_sts, start_UTC, end_UTC)$data$datetime )
  )

})

test_that("POSIXct date formats work with timezones", {

  # Assume that anyone passing in a POSIXct object knows what they are doing.

  # IF start and end are specified as POSIXct
  # AND a timezone is specified:
  #   accept them at face value, ignoring the incoming timezone

  # UTC times
  start_HST <- ISOdatetime(2018, 08, 02, 10, 00, 00, tz = "UTC")
  end_HST <- ISOdatetime(2018, 08, 03, 09, 59, 00, tz = "UTC")

  start_PDT <- ISOdatetime(2018, 08, 02, 07, 00, 00, tz = "UTC")
  end_PDT <- ISOdatetime(2018, 08, 03, 06, 59, 00, tz = "UTC")

  start_MDT <- ISOdatetime(2018, 08, 02, 06, 00, 00, tz = "UTC")
  end_MDT <- ISOdatetime(2018, 08, 03, 05, 59, 00, tz = "UTC")

  start_EDT <- ISOdatetime(2018, 08, 02, 04, 00, 00, tz = "UTC")
  end_EDT <- ISOdatetime(2018, 08, 03, 03, 59, 00, tz = "UTC")

  start_UTC <- ISOdatetime(2018, 08, 02, 00, 00, 00, tz = "UTC")
  end_UTC <- ISOdatetime(2018, 08, 02, 23, 59, 00, tz = "UTC")

  # Local times
  start_Hawaii <- ISOdatetime(2018, 08, 02, 00, 00, 00, tz = "Pacific/Honolulu")
  end_Hawaii <- ISOdatetime(2018, 08, 02, 23, 59, 00, tz = "Pacific/Honolulu")

  start_LA <- ISOdatetime(2018, 08, 02, 00, 00, 00, tz = "America/Los_Angeles")
  end_LA <- ISOdatetime(2018, 08, 02, 23, 59, 00, tz = "America/Los_Angeles")

  start_Denver <- ISOdatetime(2018, 08, 02, 00, 00, 00, tz = "America/Denver")
  end_Denver <- ISOdatetime(2018, 08, 02, 23, 59, 00, tz = "America/Denver")

  start_NYC <- ISOdatetime(2018, 08, 02, 00, 00, 00, tz = "America/New_York")
  end_NYC <- ISOdatetime(2018, 08, 02, 23, 59, 00, tz = "America/New_York")

  expect_identical(
    c(start_HST, end_HST),
    range( sts_filterDate(example_sts, start_Hawaii, end_Hawaii, timezone = "Pacific/Honolulu")$data$datetime )
  )

  expect_identical(
    c(start_PDT, end_PDT),
    range( sts_filterDate(example_sts, start_LA, end_LA, timezone = "America/Los_Angeles")$data$datetime )
  )

  expect_identical(
    c(start_MDT, end_MDT),
    range( sts_filterDate(example_sts, start_Denver, end_Denver, timezone = "America/Denver")$data$datetime )
  )

  expect_identical(
    c(start_EDT, end_EDT),
    range( sts_filterDate(example_sts, start_NYC, end_NYC, timezone = "America/New_York")$data$datetime )
  )

  expect_identical(
    c(start_UTC, end_UTC),
    range( sts_filterDate(example_sts, start_UTC, end_UTC, timezone = "UTC")$data$datetime )
  )

})
