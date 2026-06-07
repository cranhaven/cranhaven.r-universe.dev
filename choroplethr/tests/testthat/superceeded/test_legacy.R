#state

test_that("default parameters returns ggplot", {
  data(df_pop_state)
  expect_s3_class(state_choropleth(df_pop_state), "ggplot")
})

test_that("title and legend work", {
  data(df_pop_state)
  p = state_choropleth(df_pop_state, title="test title", legend = 'test legend')
  expect_equal(p$labels$title, "test title")
  expect_equal(p$labels$fill, "test legend")
})

test_that("num colors = 0 or = 1 returns continuous scale with continuous variable", {
  data(df_pop_state)
  p0 = state_choropleth(df_pop_state, num_colors=0)
  p1 = state_choropleth(df_pop_state, num_colors=1)
  expect_s3_class(p0$scales$scales[[1]], "ScaleContinuous")
  expect_s3_class(p1$scales$scales[[1]], "ScaleContinuous")
})

test_that("west coast zoom returns ggplot", {
  data(df_pop_state)
  expect_s3_class(state_choropleth(df_pop_state, zoom=c("california", "oregon", "washington"), num_colors=1), "ggplot")
})

test_that("error on invalid zoom", {
  data(df_pop_state)
  expect_error(state_choropleth(df_pop_state, zoom="asdf"))  
})

# test_that("less than full states emits warning", {
#   data(df_pop_state)
#   df = df_pop_state[df_pop_state$region != "new york", ]
#   expect_warning(state_choropleth(df))  
# })

test_that("less than full states works", {
  data(df_pop_state)
  df = df_pop_state[df_pop_state$region != "new york", ]
  expect_s3_class(
    suppressWarnings(state_choropleth(df)), 
    "ggplot")  
})

test_that("any duplicate regions trigger an error", {
  data(df_pop_state)
  df = rbind(df_pop_state, data.frame(region = "new york", value=1))
  expect_error(state_choropleth(df))
})

# test_that("state reference map returns ggplot", {
#   data(df_pop_state)
#   data(continental_us_states)
#   expect_s3_class(state_choropleth(df_pop_state, 
#                              zoom=continental_us_states,
#                              reference_map=TRUE), "ggplot")
# })

# num colors

# test_that("state num_colors = 10 throws exception", {
#   data(df_pop_state, package="choroplethr")
#   expect_error(state_choropleth(df_pop_state, num_colors=10))
# })

test_that("county float num_colors throws exception", {
  data(df_pop_state, package="choroplethr")
  expect_error(state_choropleth(df_pop_state, num_colors=1.5))
})

test_that("zip string num_colors throws exception", {
  data(df_pop_state, package="choroplethr")
  expect_error(state_choropleth(df_pop_state, num_colors="hello"))
})

test_that("state num_colors = 0 returns ggplot", {
  data(df_pop_state, package="choroplethr")
  expect_s3_class(state_choropleth(df_pop_state, num_colors=0), "ggplot")
})

test_that("country num_colors=1 returns ggplot", {
  data(df_pop_state, package="choroplethr")
  expect_s3_class(state_choropleth(df_pop_state, num_colors=1), "ggplot")
})

test_that("county num_colors=5 returns ggplot", {
  data(df_pop_state, package="choroplethr")
  expect_s3_class(state_choropleth(df_pop_state, num_colors=5), "ggplot")
})

# county

test_that("default parameters returns ggplot", {
  data(df_pop_county)
  expect_warning(expect_s3_class(county_choropleth(df_pop_county), "ggplot"))
})

# test_that("setting title returns ggplot", {
#   data(df_pop_county)
#   expect_s3_class(county_choropleth(df_pop_county, title="test title"), "ggplot")
# })

# test_that("setting legend returns ggplot", {
#   data(df_pop_county)
#   expect_s3_class(county_choropleth(df_pop_county, legend="test legend"), "ggplot")
# })

# test_that("continuous scale returns ggplot", {
#   data(df_pop_county)
#   expect_s3_class(county_choropleth(df_pop_county, num_colors=1), "ggplot")
# })

test_that("west coast zoom returns ggplot", {
  data(df_pop_county)
  expect_warning(expect_s3_class(county_choropleth(df_pop_county, state_zoom=c("california", "oregon", "washington")), "ggplot"))
})

test_that("nyc county zoom returns ggplot", {
  data(df_pop_county)
  nyc_county_fips = c(36005, 36047, 36061, 36081, 36085)
  expect_warning(county_choropleth(df_pop_county, num_colors=1, county_zoom=nyc_county_fips))
})

test_that("error on invalid zoom", {
  data(df_pop_county)
  expect_error(expect_warning(county_choropleth(df_pop_county, state_zoom="asdf")))  
})

test_that("less than full counties emits warning", {
  data(df_pop_county)
  df = df_pop_county[df_pop_county$region <= 10000, ]
  expect_warning(county_choropleth(df))  
})

test_that("less than full states works", {
  data(df_pop_county)
  df = df_pop_county[df_pop_county$region <= 10000, ]
  expect_s3_class(suppressWarnings(county_choropleth(df)), "ggplot")  
})

# test_that("county reference map returns ggplot", {
#   data(df_pop_county)
#   data(continental_us_states)
#   expect_s3_class(county_choropleth(df_pop_county, 
#                               state_zoom=continental_us_states,
#                               reference_map=TRUE), "ggplot")
# })

# country 


data('df_country_demographics')

# Baseline
test_that("default parameters returns ggplot", {
  expect_s3_class(country_choropleth(df = df_country_demographics, geoid.name = 'iso_a3', value.name = 'population'), 'ggplot')
})

# 
# test_that("default parameters returns ggplot", {
#   expect_s3_class(country_choropleth(df), "ggplot")
# })
# 
# test_that("setting title returns ggplot", {
#   df = get_test_df()
#   expect_s3_class(country_choropleth(df, title="test title"), "ggplot")
# })
# 
# test_that("setting legend returns ggplot", {
#   df = get_test_df()
#   expect_s3_class(country_choropleth(df, legend="test legend"), "ggplot")
# })
# 
# test_that("continuous scale returns ggplot", {
#   df = get_test_df()
#   expect_s3_class(country_choropleth(df, num_colors=1), "ggplot")
# })
# 
# test_that("west coast zoom returns ggplot", {
#   df = get_test_df()
#   expect_s3_class(country_choropleth(df, num_colors=2, zoom=c("united states of america", "mexico", "canada")), "ggplot")
# })
# 
# test_that("error on invalid zoom", {
#   df = get_test_df()
#   expect_error(country_choropleth(df, zoom="asdf"))  
# })
# 
# test_that("less than full countries emits warning", {
#   df = get_test_df()
#   df = df[df$region != "angola", ]
#   expect_warning(country_choropleth(df))  
# })
# 
# test_that("less than full countries works", {
#   df = get_test_df()
#   df = df[df$region != "angola", ]
#   expect_s3_class(suppressWarnings(country_choropleth(df)), "ggplot")
# })
# 
# test_that("regions not on map emit warning", {
#   df = get_test_df()
#   df = rbind(df, data.frame(region="asdf", value=1))
#   expect_warning(country_choropleth(df))  
# })
# 
# test_that("duplicate regions emit error", {
#   df = get_test_df()
#   df = rbind(df, data.frame(region="angola", value=1))
#   expect_error(country_choropleth(df))  
# })

# acs
# library(testthat)

# test_that('get_acs_data returns a list with data and a map title', {
#   expect_type(get_acs_data(variable = 'B19013_001', map = 'county', endyear = 2012, span = 5), 'list')
# })
# 
# test_that('User can not specify both variable and tableId', {
#   expect_error(get_acs_data(variable = 'B19013_001', tableId = 'B00001', map = 'county', endyear = 2012, span = 5))
# })
# 
# test_that('Table B01001 has more than one variable, so an error should be thrown, unless column_idx is given.', {
#   expect_error(get_acs_data(tableId = 'B01001', map = 'county', endyear = 2012, span = 5))
#   expect_type(get_acs_data(tableId = 'B01001', column_idx = 1, map = 'county', endyear = 2012, span = 5), 'list')
# })
# 
# test_that('...Unless a column Idx is specified', {
#   expect_error(get_acs_data(tableId = 'B01001', map = 'county', endyear = 2012, span = 5))
# })


# test_that("state_choropleth_acs returns a ggplot2 with parameters set", {
#   expect_s3_class(state_choropleth_acs(variable = "B19013_001", endyear = 2012, num_colors=1, zoom=c("new york", "new jersey", "connecticut")), "ggplot") 
# })
# 
# test_that("county_choropleth_acs returns a ggplot2 with parameters set", {
#   expect_s3_class(suppressWarnings(
#     county_choropleth_acs(variable = "B19013_001", endyear = 2012, num_colors=1, state_zoom=c("new york", "new jersey", "connecticut"))), 
#     "ggplot")
# })


