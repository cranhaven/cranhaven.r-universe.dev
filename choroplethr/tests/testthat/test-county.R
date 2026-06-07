

test_that("default parameters returns ggplot", {
  data(df_pop_county)
  expect_message(expect_s3_class(county_choropleth(df_pop_county), "ggplot"))
})

test_that("continuous scale returns ggplot", {
  data(df_pop_county)
  expect_message(expect_s3_class(county_choropleth(df_pop_county, num_colors=1), "ggplot"))
})

test_that("west coast zoom returns ggplot", {
  data(df_pop_county)
  expect_message(expect_s3_class(county_choropleth(df_pop_county, state_zoom=c("california", "oregon", "washington")), "ggplot"))
})

test_that("nyc county zoom returns ggplot", {
  data(df_pop_county)
  nyc_county_fips = c(36005, 36047, 36061, 36081, 36085)
  expect_message(expect_s3_class(county_choropleth(df_pop_county, num_colors=1, county_zoom=nyc_county_fips, add_state_outline = FALSE), 'ggplot'))
})
