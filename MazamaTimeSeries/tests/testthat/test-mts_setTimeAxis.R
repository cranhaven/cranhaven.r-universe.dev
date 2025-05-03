# NOTE:  mts objects have an hourly time axis
# NOTE:
# NOTE:  > range(example_mts$data$datetime)
# NOTE:  [1] "2019-07-01 07:00:00 UTC" "2019-07-08 06:00:00 UTC"

test_that("missing times throws an error", {

  expect_error(
    mts <- mts_setTimeAxis(example_mts)
  )

})

test_that("single-sided trim", {

  startdate <- MazamaCoreUtils::parseDatetime(20190703, timezone = "UTC")
  enddate <- MazamaCoreUtils::parseDatetime(20190705, timezone = "UTC")

  mts <- mts_setTimeAxis(
    example_mts,
    startdate = startdate
  )

  expect_equal(
    range(mts$data$datetime),
    c(startdate, max(example_mts$data$datetime))
  )

  mts <- mts_setTimeAxis(
    example_mts,
    enddate = enddate
  )

  expect_equal(
    range(mts$data$datetime),
    c(min(example_mts$data$datetime), enddate)
  )

})

test_that("single-sided extend", {

  startdate <- MazamaCoreUtils::parseDatetime(20190615, timezone = "UTC")
  enddate <- MazamaCoreUtils::parseDatetime(20190715, timezone = "UTC")

  mts <- mts_setTimeAxis(
    example_mts,
    startdate = startdate
  )

  expect_equal(
    range(mts$data$datetime),
    c(startdate, max(example_mts$data$datetime))
  )

  mts <- mts_setTimeAxis(
    example_mts,
    enddate = enddate
  )

  expect_equal(
    range(mts$data$datetime),
    c(min(example_mts$data$datetime), enddate)
  )

})

test_that("timezone is ignored for POSIXct input", {

  startdate <- MazamaCoreUtils::parseDatetime(20190615, timezone = "UTC")
  enddate <- MazamaCoreUtils::parseDatetime(20190715, timezone = "UTC")

  mts <- mts_setTimeAxis(
    example_mts,
    startdate = startdate,
    enddate = enddate,
    timezone = "America/Los_Angeles"
  )

  expect_equal(
    range(mts$data$datetime),
    c(startdate, enddate)
  )

})

test_that("timezone is used for numeric input", {

  # Should be "UTC"
  time_axis_timezone <- lubridate::tz(example_mts$data$datetime[1])

  startdate <- 20190615
  enddate <- 20190715
  timezone <- "UTC"

  mts <- mts_setTimeAxis(
    example_mts,
    startdate = startdate,
    enddate = enddate,
    timezone = timezone
  )

  s <- MazamaCoreUtils::parseDatetime(startdate, timezone = timezone)
  e <- MazamaCoreUtils::parseDatetime(enddate, timezone = timezone)

  expect_equal(
    range(mts$data$datetime),
    lubridate::with_tz(c(s, e), tz = time_axis_timezone) # Retain original data$datetime timezone
  )

})

test_that("timezone is used for character input", {

  # Should be "UTC"
  time_axis_timezone <- lubridate::tz(example_mts$data$datetime[1])

  startdate <- "2019-06-15"
  enddate <- "2019-07-15 00:00"
  timezone <- "UTC"

  mts <- mts_setTimeAxis(
    example_mts,
    startdate = startdate,
    enddate = enddate,
    timezone = timezone
  )

  s <- MazamaCoreUtils::parseDatetime(startdate, timezone = timezone)
  e <- MazamaCoreUtils::parseDatetime(enddate, timezone = timezone)

  expect_equal(
    range(mts$data$datetime),
    lubridate::with_tz(c(s, e), tz = time_axis_timezone) # Retain original data$datetime timezone
  )

})

test_that("original timezone is retained", {

  # Should be "UTC"
  time_axis_timezone <- lubridate::tz(example_mts$data$datetime[1])

  startdate <- 20190615
  enddate <- 20190715
  timezone <- "America/Los_Angeles"

  mts <- mts_setTimeAxis(
    example_mts,
    startdate = startdate,
    enddate = enddate,
    timezone = timezone
  )

  s <- MazamaCoreUtils::parseDatetime(startdate, timezone = timezone)
  e <- MazamaCoreUtils::parseDatetime(enddate, timezone = timezone)

  expect_equal(
    range(mts$data$datetime),
    lubridate::with_tz(c(s, e), tzone = time_axis_timezone) # Retain original data$datetime timezone
  )

})

test_that("meta$timezone is used when 'timezone' is missing", {

  # Should be "UTC"
  time_axis_timezone <- lubridate::tz(example_mts$data$datetime[1])

  # Should be "America/Los_Angeles"
  meta_timezone <- unique(example_mts$meta$timezone)

  startdate <- 20190615
  enddate <- 20190715

  mts <- mts_setTimeAxis(
    example_mts,
    startdate = startdate,
    enddate = enddate
  )

  s <- MazamaCoreUtils::parseDatetime(startdate, timezone = meta_timezone)
  e <- MazamaCoreUtils::parseDatetime(enddate, timezone = meta_timezone)

  expect_equal(
    range(mts$data$datetime),
    lubridate::with_tz(c(s, e), tz = time_axis_timezone) # Retain original data$datetime timezone
  )

})


