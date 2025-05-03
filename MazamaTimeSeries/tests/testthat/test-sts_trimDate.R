test_that("tidy dataframes", {
  
  startdate <- 20180808
  enddate <- 20180815
  
  UTC_week <- sts_filterDate(
    example_sts,
    startdate = startdate,
    enddate = enddate,
    timezone = "UTC"
  )
  
  # From MazamaSpatialUtils::SimpleTimezones
  utc_offset <- 7
  
  # sts_trimDate gets timezone from sts' meta
  local_week <- sts_trimDate(UTC_week)
  
  expect_equal(
    min(local_week$data$datetime),
    lubridate::parse_date_time(startdate, orders = "ymd", tz = "UTC") + lubridate::hours(7)
  )
  
  expect_equal(
    max(local_week$data$datetime),
    lubridate::parse_date_time(enddate, orders = "ymd", tz = "UTC") - lubridate::hours(24-7) - lubridate::minutes(1)
  )
  
  

})
