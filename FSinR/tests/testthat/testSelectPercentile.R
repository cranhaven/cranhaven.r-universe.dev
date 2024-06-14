context("Select Percentile")

data2 <- get(load("../data/example2.RData"))

test_that("Results are correct", {
  expect_setequal(selectPercentile(20)(data2, 'clase', giniIndex())$featuresSelected, c('x6'))
  expect_setequal(selectPercentile(30)(data2, 'clase', giniIndex())$featuresSelected, c('x6', 'x2'))
  expect_setequal(selectPercentile(50)(data2, 'clase', giniIndex())$featuresSelected, c('x6', 'x2', 'x3'))
})

test_that("Name is set", {
  expect_equal(attr(selectPercentile(),'name'),"Select Percentile");
  expect_equal(attr(selectPercentile(),'shortName'),"selectPercentile");
})