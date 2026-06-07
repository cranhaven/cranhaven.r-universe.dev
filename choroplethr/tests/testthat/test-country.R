df_world = choroplethr::df_country_demographics

# Baseline
test_that("default parameters returns ggplot", {
  expect_s3_class(country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population'), 'ggplot')
})

test_that("different projections return ggplot", {
  expect_s3_class(country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'albers'), 'ggplot')
  expect_s3_class(country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'mercator'), 'ggplot')
  expect_s3_class(country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', projection = 'robinson'), 'ggplot')
})


test_that("zoom returns ggplot", {
  expect_s3_class(country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', 
                               zoom=c("USA", "MEX", "CAN")), "ggplot")
  expect_s3_class(country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', 
                               continent_zoom=c('North America')), "ggplot")
  expect_s3_class(country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', 
                               zoom=c("USA", "MEX", "CAN"),
                               continent_zoom=c('North America')), "ggplot")
})

