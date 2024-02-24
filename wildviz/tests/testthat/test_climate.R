#context('Climate Test')
#
#test_that("daily_climate_counties creates a climate dataframe", {
#  expect_named(daily_climate_counties(fips_list = c('06001'), var = c('tmax', 'tmin'), date_min = '2015-01-01', date_max = '2015-01-01'),
#               c('fips', 'date', 'tmax', 'tmin'))
#  expect_warning(daily_climate_counties(fips_list = c('06001'), var = c('tmax', 'tmin'), date_min = '2015-01-01', date_max = '2015-01-01'))
#})
#
#test_that("daily_climate_counties errors out on incorrect input", {
#  expect_error(daily_climate_counties(date_min = '2015-01-01', date_max = '2015-01-01'))
#  expect_error(daily_climate_counties(fips_list = 06001, date_min = '2015-01-01', date_max = '2015-01-01'))
#  expect_error(daily_climate_counties(fips_list = c('06001'), date_min = '2015-01-02', date_max = '2015-01-01'))
#  expect_error(daily_climate_counties(fips_list = c('06001'), date_min = 2015-01-01, date_max = '2015-01-01'))
#})
