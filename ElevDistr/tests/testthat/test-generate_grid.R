test_that("output type is correct", {
  temp <- generate_grid(8.728898, 46.93756, 10, 0.0025)
  expect_type(temp, "list")
  expect_s3_class(temp$df, "data.frame")
  expect_type(temp$df$longitude, "double")
  expect_type(temp$df$latitude, "double")
  expect_type(temp$lonLength, "integer")
  expect_type(temp$latLength, "integer")
})

test_that("check input length", {
  expect_error(generate_grid(c(1,2), 46.93756, 10, 0.0025), "must be of length 1")
  expect_error(generate_grid(8.728898, c(1,2), 10, 0.0025), "must be of length 1")
  expect_error(generate_grid(8.728898, 46.93756, c(1,2), 0.0025), "must be of length 1")
  expect_error(generate_grid(8.728898, 46.93756, 10, c(1,2)), "must be of length 1")
})

test_that("check input type", {
  expect_error(generate_grid("8.728898", 46.93756, 10, 0.0025), "must be numeric and finite")
  expect_error(generate_grid(NaN, 46.93756, 10, 0.0025), "must be numeric and finite")
  expect_error(generate_grid(8.728898, "46.93756", 10, 0.0025), "must be numeric and finite")
  expect_error(generate_grid(8.728898, NaN, 10, 0.0025), "must be numeric and finite")
  expect_error(generate_grid(8.728898, 46.93756, "10", 0.0025), "must be numeric and finite")
  expect_error(generate_grid(8.728898, 46.93756, NaN, 0.0025), "must be numeric and finite")
  expect_error(generate_grid(8.728898, 46.93756, 10, "0.0025"), "must be numeric and finite")
  expect_error(generate_grid(8.728898, 46.93756, 10, NaN), "must be numeric and finite")
})
