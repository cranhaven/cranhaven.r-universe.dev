
df_world = choroplethr::df_country_demographics
df_pop_state = choroplethr::df_pop_state
df_county_demographics = choroplethr::df_county_demographics

# Baseline
test_that("basic parameters returns ggplot", {
  expect_s3_class(country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population'), 'ggplot')
  expect_s3_class(state_choropleth(df = df_pop_state, value.name = 'value'), 'ggplot')
  expect_message(expect_s3_class(county_choropleth(df = df_county_demographics, geoid.name = 'region', value.name = 'population'), 'ggplot'))
})

# Continuous data with divergent and convergent scales
test_that("continuous data returns continous scale with num colors = 0 or 1", {
  p0 = country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', num_colors=0)
  p1 = country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', num_colors=1)
  expect_s3_class(p0$scales$scales[[1]], "ScaleContinuous")
  expect_s3_class(p1$scales$scales[[1]], "ScaleContinuous")
})

test_that("continuous data returns discrete scale with num colors > 1", {
  p = country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', num_colors = 7)
  expect_s3_class(p$scales$scales[[1]], "ScaleDiscrete")
})

test_that("categorical data returns discrete scale and warns that num_colors ignored", {
  expect_message(country_choropleth(df = df_world, geoid.name = 'region', value.name = 'region_type', num_colors = 7))
})

test_that("custom colors work", {
  expect_s3_class(country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', num_colors = 6,
                     custom.colors = c('grey', 'green', 'blue', 'violet', 'brown', 'orange')), 'ggplot')
  
  expect_message(expect_s3_class(country_choropleth(df = df_world, geoid.name = 'region', value.name = 'region_type', 
                               custom.colors = c('grey', 'green', 'blue', 'violet', 'brown', 'orange')), 'ggplot'))
  
  expect_error(country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', num_colors = 3,
                               custom.colors = c('grey', 'green', 'blue', 'violet', 'brown', 'orange')))
  
  expect_message(expect_error(country_choropleth(df = df_world, geoid.name = 'region', value.name = 'region_type', 
                               custom.colors = c('grey', 'green', 'blue'))))
})


test_that("zoom returns ggplot", {
  expect_s3_class(country_choropleth(df = df_world, geoid.name = 'region', value.name = 'population', zoom = c('USA', 'CAN', 'MEX')), 'ggplot')
})


