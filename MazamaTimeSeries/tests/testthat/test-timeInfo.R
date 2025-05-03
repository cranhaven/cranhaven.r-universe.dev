test_that("timeInfo() works", {

  # Thompson Falls example from PWFSLSmoke::timeInfo()
  starttime <- ISOdatetime(2018, 11, 03, 23, 00, 00, tz = "America/Denver")
  endtime <- ISOdatetime(2018, 11, 04, 23, 00, 00, tz = "America/Denver")

  timeInfo <- timeInfo(
    time = seq(starttime, endtime, by = "hour"),
    longitude = -115.3237,
    latitude = 47.59439,
    timezone = "America/Denver"
  )

  # LST
  expect_equal(
    timeInfo$localStandardTime_UTC[1:4],
    c(
      ISOdatetime(2018, 11, 03, 22, 00, 00, tz = "UTC"),
      ISOdatetime(2018, 11, 03, 23, 00, 00, tz = "UTC"),
      ISOdatetime(2018, 11, 04, 00, 00, 00, tz = "UTC"),
      ISOdatetime(2018, 11, 04, 01, 00, 00, tz = "UTC")
    )
  )

  # Local time (not duplicated 1 am)
  expect_equal(
    strftime(timeInfo$localTime[1:4], "%Y-%m-%d %H:%M", tz = "America/Denver"),
    c(
      "2018-11-03 23:00",  # MDT
      "2018-11-04 00:00",  # MDT
      "2018-11-04 01:00",  # MDT
      "2018-11-04 01:00"   # MST
    )
  )

  # masks
  expect_equal(
    timeInfo$day,
    c(F,F,F,F,F,F,F,F,F,T,T,T,T,T,T,T,T,T,T,F,F,F,F,F,F,F)
  )

  expect_equal(
    timeInfo$night,
    c(T,T,T,T,T,T,T,T,T,F,F,F,F,F,F,F,F,F,F,T,T,T,T,T,T,T)
  )

  expect_equal(
    timeInfo$morning,
    c(F,F,F,F,F,F,F,F,F,T,T,T,T,T,F,F,F,F,F,F,F,F,F,F,F,F)
  )

  expect_equal(
    timeInfo$afternoon,
    c(F,F,F,F,F,F,F,F,F,F,F,F,F,F,T,T,T,T,T,F,F,F,F,F,F,F)
  )

})
