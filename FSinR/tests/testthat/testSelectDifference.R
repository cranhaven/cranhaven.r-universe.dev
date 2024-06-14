context("Select Difference")

data1 <- get(load('../data/example.RData'))
data2 <- get(load("../data/example2.RData"))

test_that("Results are correct", {
  expect_setequal(selectDifference(0.6)(data2, 'clase', symmetricalUncertain())$featuresSelected, c('x6', 'x3', 'x2', 'x4', 'x5', 'x1'))
  expect_setequal(selectDifference(0.3)(data2, 'clase', cramer())$featuresSelected, c('x6', 'x3', 'x2', 'x4', 'x5'))
  expect_setequal(selectDifference(0.2)(data1, 'clase', cramer())$featuresSelected, c('x3', 'x2'))
  
})

test_that("Name is set", {
  expect_equal(attr(selectDifference(),'name'),"Select Difference");
  expect_equal(attr(selectDifference(),'shortName'),"selectDifference");
})