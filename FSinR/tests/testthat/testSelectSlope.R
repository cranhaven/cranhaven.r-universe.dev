context("Select Slope")

data1 <- get(load('../data/example.RData'))
data2 <- get(load("../data/example2.RData"))
data3 <- get(load("../data/continuous.RData"))

test_that("Results are correct", {
  expect_setequal(selectSlope(0.2)(data1, 'clase', cramer())$featuresSelected, c('x3', 'x2'))
  expect_setequal(selectSlope(0.1)(data1, 'clase', cramer())$featuresSelected, c('x3'))
})

test_that("Name is set", {
  expect_equal(attr(selectSlope(),'name'),"Select Slope");
  expect_equal(attr(selectSlope(),'shortName'),"selectSlope");
})