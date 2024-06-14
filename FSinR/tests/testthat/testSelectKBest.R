context("Select K Best")

data1 <- get(load('../data/example.RData'))
data2 <- get(load("../data/example2.RData"))
data3 <- get(load("../data/continuous.RData"))

test_that("Results are correct", {
  expect_setequal(selectKBest(1)(data1, 'clase', IEPConsistency())$featuresSelected, c('x3'))
  expect_setequal(selectKBest(1)(data1, 'clase', giniIndex())$featuresSelected, c('x2'))
  expect_setequal(selectKBest(4)(data2, 'clase', giniIndex())$featuresSelected, c('x6', 'x2', 'x3', 'x4'))
  expect_setequal(selectKBest(3)(data3, 'clase', determinationCoefficient())$featuresSelected, c('x3', 'x1', 'x2'))
})

test_that("Name is set", {
  expect_equal(attr(selectKBest(),'name'),"Select K Best");
  expect_equal(attr(selectKBest(),'shortName'),"selectKBest");
})