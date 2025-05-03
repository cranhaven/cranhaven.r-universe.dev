# NOTE:  mts objects have an hourly time axis

test_that("character date formats work", {

  # IF start and end are specified as numeric or character:
  #   assume the timezone of the mts object

  start_PDT <- ISOdatetime(2019, 07, 02, 07, 00, 00, tz = "UTC")
  end_PDT <- ISOdatetime(2019, 07, 03, 06, 00, 00, tz = "UTC")

  expect_identical(
    c(start_PDT, end_PDT),
    range( mts_filterDatetime(example_mts, 20190702, 20190703)$data$datetime )
  )

  expect_identical(
    c(start_PDT, end_PDT),
    range( mts_filterDatetime(example_mts, "20190702", "20190703")$data$datetime )
  )

  expect_identical(
    c(start_PDT, end_PDT),
    range( mts_filterDatetime(example_mts, "2019-07-02", 20190703)$data$datetime )
  )

  expect_identical(
    c(start_PDT, end_PDT),
    range( mts_filterDatetime(example_mts, "2019-07-02 00", 20190703)$data$datetime )
  )

  expect_identical(
    c(start_PDT, end_PDT),
    range( mts_filterDatetime(example_mts, "2019-07-02 00:00:00", 20190703)$data$datetime )
  )

})

test_that("character date formats work with timezones", {

  # IF start and end are specified as numeric or character
  # AND a timezone is specified:
  #   use the timezone to interpret the dates

  # UTC times
  start_HST <- ISOdatetime(2019, 07, 02, 10, 00, 00, tz = "UTC")
  end_HST <- ISOdatetime(2019, 07, 03, 09, 00, 00, tz = "UTC")

  start_PDT <- ISOdatetime(2019, 07, 02, 07, 00, 00, tz = "UTC")
  end_PDT <- ISOdatetime(2019, 07, 03, 06, 00, 00, tz = "UTC")

  start_MDT <- ISOdatetime(2019, 07, 02, 06, 00, 00, tz = "UTC")
  end_MDT <- ISOdatetime(2019, 07, 03, 05, 00, 00, tz = "UTC")

  start_EDT <- ISOdatetime(2019, 07, 02, 04, 00, 00, tz = "UTC")
  end_EDT <- ISOdatetime(2019, 07, 03, 03, 00, 00, tz = "UTC")

  start_UTC <- ISOdatetime(2019, 07, 02, 00, 00, 00, tz = "UTC")
  end_UTC <- ISOdatetime(2019, 07, 02, 23, 00, 00, tz = "UTC")

  expect_identical(
    c(start_PDT, end_PDT),
    range( mts_filterDatetime(example_mts, 20190702, 20190703)$data$datetime )
  )

  expect_identical(
    c(start_HST, end_HST),
    range( mts_filterDatetime(example_mts, 20190702, 20190703, timezone = "Pacific/Honolulu")$data$datetime )
  )

  expect_identical(
    c(start_PDT, end_PDT),
    range( mts_filterDatetime(example_mts, 20190702, 20190703, timezone = "America/Los_Angeles")$data$datetime )
  )

  expect_identical(
    c(start_MDT, end_MDT),
    range( mts_filterDatetime(example_mts, 20190702, 20190703, timezone = "America/Denver")$data$datetime )
  )

  expect_identical(
    c(start_EDT, end_EDT),
    range( mts_filterDatetime(example_mts, 20190702, 20190703, timezone = "America/New_York")$data$datetime )
  )

  expect_identical(
    c(start_UTC, end_UTC),
    range( mts_filterDatetime(example_mts, 20190702, 20190703, timezone = "UTC")$data$datetime )
  )

})

test_that("POSIXct date formats work", {

  # Assume that anyone passing in a POSIXct object knows what they are doing.

  # IF start and end are specified as POSIXct
  #   accept them at face value, ignoring the sensor timezone

  # UTC times
  start_HST <- ISOdatetime(2019, 07, 02, 10, 00, 00, tz = "UTC")
  end_HST <- ISOdatetime(2019, 07, 03, 09, 00, 00, tz = "UTC")

  start_PDT <- ISOdatetime(2019, 07, 02, 07, 00, 00, tz = "UTC")
  end_PDT <- ISOdatetime(2019, 07, 03, 06, 00, 00, tz = "UTC")

  start_MDT <- ISOdatetime(2019, 07, 02, 06, 00, 00, tz = "UTC")
  end_MDT <- ISOdatetime(2019, 07, 03, 05, 00, 00, tz = "UTC")

  start_EDT <- ISOdatetime(2019, 07, 02, 04, 00, 00, tz = "UTC")
  end_EDT <- ISOdatetime(2019, 07, 03, 03, 00, 00, tz = "UTC")

  start_UTC <- ISOdatetime(2019, 07, 02, 00, 00, 00, tz = "UTC")
  end_UTC <- ISOdatetime(2019, 07, 02, 23, 00, 00, tz = "UTC")

  # Local times
  start_Hawaii <- ISOdatetime(2019, 07, 02, 00, 00, 00, tz = "Pacific/Honolulu")
  end_Hawaii <- ISOdatetime(2019, 07, 03, 00, 00, 00, tz = "Pacific/Honolulu")

  start_LA <- ISOdatetime(2019, 07, 02, 00, 00, 00, tz = "America/Los_Angeles")
  end_LA <- ISOdatetime(2019, 07, 03, 00, 00, 00, tz = "America/Los_Angeles")

  start_Denver <- ISOdatetime(2019, 07, 02, 00, 00, 00, tz = "America/Denver")
  end_Denver <- ISOdatetime(2019, 07, 03, 00, 00, 00, tz = "America/Denver")

  start_NYC <- ISOdatetime(2019, 07, 02, 00, 00, 00, tz = "America/New_York")
  end_NYC <- ISOdatetime(2019, 07, 03, 00, 00, 00, tz = "America/New_York")

  expect_identical(
    c(start_HST, end_HST),
    range( mts_filterDatetime(example_mts, start_Hawaii, end_Hawaii)$data$datetime )
  )

  expect_identical(
    c(start_PDT, end_PDT),
    range( mts_filterDatetime(example_mts, start_LA, end_LA)$data$datetime )
  )

  expect_identical(
    c(start_MDT, end_MDT),
    range( mts_filterDatetime(example_mts, start_Denver, end_Denver)$data$datetime )
  )

  expect_identical(
    c(start_EDT, end_EDT),
    range( mts_filterDatetime(example_mts, start_NYC, end_NYC)$data$datetime )
  )

  expect_identical(
    c(start_UTC, end_UTC),
    range( mts_filterDatetime(example_mts, start_UTC, end_UTC + lubridate::minutes(1))$data$datetime )
  )

})

test_that("POSIXct date formats work with timezones", {

  # Assume that anyone passing in a POSIXct object knows what they are doing.

  # IF start and end are specified as POSIXct
  # AND a timezone is specified:
  #   accept them at face value, ignoring the incoming timezone

  # UTC times
  start_HST <- ISOdatetime(2019, 07, 02, 10, 00, 00, tz = "UTC")
  end_HST <- ISOdatetime(2019, 07, 03, 09, 00, 00, tz = "UTC")

  start_PDT <- ISOdatetime(2019, 07, 02, 07, 00, 00, tz = "UTC")
  end_PDT <- ISOdatetime(2019, 07, 03, 06, 00, 00, tz = "UTC")

  start_MDT <- ISOdatetime(2019, 07, 02, 06, 00, 00, tz = "UTC")
  end_MDT <- ISOdatetime(2019, 07, 03, 05, 00, 00, tz = "UTC")

  start_EDT <- ISOdatetime(2019, 07, 02, 04, 00, 00, tz = "UTC")
  end_EDT <- ISOdatetime(2019, 07, 03, 03, 00, 00, tz = "UTC")

  start_UTC <- ISOdatetime(2019, 07, 02, 00, 00, 00, tz = "UTC")
  end_UTC <- ISOdatetime(2019, 07, 02, 23, 00, 00, tz = "UTC")

  # Local times
  start_Hawaii <- ISOdatetime(2019, 07, 02, 00, 00, 00, tz = "Pacific/Honolulu")
  end_Hawaii <- ISOdatetime(2019, 07, 03, 00, 00, 00, tz = "Pacific/Honolulu")

  start_LA <- ISOdatetime(2019, 07, 02, 00, 00, 00, tz = "America/Los_Angeles")
  end_LA <- ISOdatetime(2019, 07, 03, 00, 00, 00, tz = "America/Los_Angeles")

  start_Denver <- ISOdatetime(2019, 07, 02, 00, 00, 00, tz = "America/Denver")
  end_Denver <- ISOdatetime(2019, 07, 03, 00, 00, 00, tz = "America/Denver")

  start_NYC <- ISOdatetime(2019, 07, 02, 00, 00, 00, tz = "America/New_York")
  end_NYC <- ISOdatetime(2019, 07, 03, 00, 00, 00, tz = "America/New_York")

  expect_identical(
    c(start_HST, end_HST),
    range( mts_filterDatetime(example_mts, start_Hawaii, end_Hawaii, timezone = "Pacific/Honolulu")$data$datetime )
  )

  expect_identical(
    c(start_PDT, end_PDT),
    range( mts_filterDatetime(example_mts, start_LA, end_LA, timezone = "America/Los_Angeles")$data$datetime )
  )

  expect_identical(
    c(start_MDT, end_MDT),
    range( mts_filterDatetime(example_mts, start_Denver, end_Denver, timezone = "America/Denver")$data$datetime )
  )

  expect_identical(
    c(start_EDT, end_EDT),
    range( mts_filterDatetime(example_mts, start_NYC, end_NYC, timezone = "America/New_York")$data$datetime )
  )

  expect_identical(
    c(start_UTC, end_UTC),
    range( mts_filterDatetime(example_mts, start_UTC, end_UTC + lubridate::minutes(1), timezone = "UTC")$data$datetime )
  )

})

test_that("includeEnd works", {

  startdate <- min(example_mts$data$datetime)
  enddate <- max(example_mts$data$datetime)
  new_mts <-
    mts_filterDatetime(
      example_mts,
      startdate = startdate,
      enddate = enddate,
      includeEnd = TRUE
    )

  expect_identical(
    c(startdate, enddate),
    range( new_mts$data$datetime )
  )

})

