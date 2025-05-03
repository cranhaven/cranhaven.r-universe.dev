# NOTE:  mts objects have an hourly time axis

test_that("basic trimming works", {

  startdate <- 20190702
  enddate <- 20190706

  UTC_days <- mts_filterDate(
    example_mts,
    startdate = startdate,
    enddate = enddate,
    timezone = "UTC"
  )

  # From MazamaSpatialUtils::SimpleTimezones
  utc_offset <- 7

  # mts_trimDate gets timezone from mts' meta
  local_days <- mts_trimDate(UTC_days)

  expect_equal(
    min(local_days$data$datetime),
    lubridate::parse_date_time(startdate, orders = "ymd", tz = "UTC") + lubridate::hours(7)
  )

  expect_equal(
    max(local_days$data$datetime),
    lubridate::parse_date_time(enddate, orders = "ymd", tz = "UTC") - lubridate::hours(24-7) - lubridate::hours(1)
  )

})

test_that("single-monitor objects are handled", {

  my_mts <- mts_select(example_mts, "e0d610f7f219e39f_5080")

  # Failure would happen here
  trim_EDT <- mts_trimDate(my_mts, timezone = "America/New_York")

  expect_equal(
    ncol(my_mts$data),
    ncol(trim_EDT$data)
  )

})

test_that("trimEmptyDays works", {

  my_mts <- example_mts

  trim_EDT <- mts_trimDate(my_mts, timezone = "America/New_York")

  expect_equal(
    min(trim_EDT$data$datetime),
    ISOdatetime(2019, 07, 02, 04, 00, 00, tz = "UTC")
  )

  expect_equal(
    max(trim_EDT$data$datetime),
    ISOdatetime(2019, 07, 08, 03, 00, 00, tz = "UTC")
  )

  my_mts$data[1:50,-1] <- as.numeric(NA)

  trim_EDT <- mts_trimDate(my_mts, timezone = "America/New_York")

  expect_equal(
    min(trim_EDT$data$datetime),
    ISOdatetime(2019, 07, 04, 04, 00, 00, tz = "UTC")
  )

  expect_equal(
    max(trim_EDT$data$datetime),
    ISOdatetime(2019, 07, 08, 03, 00, 00, tz = "UTC")
  )

})
