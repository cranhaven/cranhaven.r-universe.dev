#context('AQI Test')
#
#test_that("daily_aqi creates an aqi dataframe", {
#  expect_named(daily_aqi(aqs_api_email = Sys.getenv("aqs_api_email"), aqs_api_key = Sys.getenv("aqs_api_key"), fips_list = c('06001'), metric_list = c(42101, 44201), year_min = 2015, year_max = 2015),
#               c('state_code', 'county_code', 'county_name', 'date', 'aqi', 'co', 'ozone', 'no2', 'pm25', 'pm10'))
#})
#
#test_that("daily_aqi errors out on incorrect input", {
#  expect_error(daily_aqi(year_min = 2016, year_max = 2015))
#  expect_error(daily_aqi(fips_list = 06001, year_min = 2015, year_max = 2015))
#  expect_error(daily_aqi(fips_list = c('06001'), year_min = 2014, year_max = '2015'))
#  expect_error(daily_aqi(metric_list = c("81102", "88101", "42101", "44201", "42602"), year_min = 2015, year_max = 2015))
#  expect_error(daily_aqi(metric_list = c("PM10", "PM2.5", "CO", "NO2", "Ozone"), year_min = 2015, year_max = 2015))
#  expect_error(daily_aqi(metrics_list = c(42101, 44201), year_min = 2015, year_max = 2015))
#})
