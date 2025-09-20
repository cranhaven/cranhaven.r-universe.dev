test_that("output type is correct", {
  temp <- get_nearest_point(8.65, 46.87, pointsAboveTreeline)
  expect_type(temp, "list")
  expect_type(temp$lon, "double")
  expect_type(temp$lat, "double")
})

test_that("check input length", {
  longitude <- seq(0, 10)
  latitude <- seq(40, 50)
  temp <- data.frame(longitude, latitude)

  expect_error(get_nearest_point(c(1,2), 46.87, temp), "must be of length 1")
  expect_error(get_nearest_point(8.65, c(1,2), temp), "must be of length 1")
  expect_error(get_nearest_point(8.65, 46.87, as.data.frame(temp[,1])), "needs to have at least two columns")
})

test_that("check input type", {
  longitude <- seq(0, 10)
  latitude <- seq(40, 50)
  temp <- data.frame(longitude, latitude)

  expect_error(get_nearest_point("8.65", 46.87, temp), "must be numeric and finite")
  expect_error(get_nearest_point(NaN, 46.87, temp), "must be numeric and finite")
  expect_error(get_nearest_point(8.65, "46.87", temp), "must be numeric and finite")
  expect_error(get_nearest_point(8.65, NaN, temp), "must be numeric and finite")
  expect_error(get_nearest_point(8.65, 46.87, as.list(temp)), "must be a data frame")

  temp[,1] <- as.character(temp[,1])
  expect_error(get_nearest_point(8.65, 46.87, temp), "must be numeric and finite")

  temp <- data.frame(longitude, latitude)
  temp[1,1] <- NA
  expect_error(get_nearest_point(8.65, 46.87, temp), "must be numeric and finite")

  temp[1,1] <- NaN
  expect_error(get_nearest_point(8.65, 46.87, temp), "must be numeric and finite")
})

test_that("check coordinate value", {
  expect_error(get_nearest_point(-181, 46.87, temp), "lon must be from -180 to 180")
  expect_error(get_nearest_point(181, 46.87, temp), "lon must be from -180 to 180")
  expect_error(get_nearest_point(8.65, -91, temp), "lat must be from -90 to 90")
  expect_error(get_nearest_point(8.65, 91, temp), "lat must be from -90 to 90")
})
