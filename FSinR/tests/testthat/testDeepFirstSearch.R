context("Deep First Search")

data1 <- get(load('../data/example.RData'))
data2 <- get(load("../data/example2.RData"))
data3 <- get(load("../data/continuous.RData"))

test_that("Results are correct", {
  expect_equal(deepFirst()(data2, 'clase', IEConsistency())$bestFeatures[1,], c('x1' = 1, 'x2' = 1, 'x3' = 1, 'x4' = 1, 'x5' = 1, 'x6' = 0))
  expect_identical(deepFirst()(data1, 'clase', roughsetConsistency())$bestFeatures[1,], c('x1' = 1, 'x2' =  1, 'x3' = 1))
  expect_identical(deepFirst()(data1, 'clase', giniIndex())$bestFeatures[1,], c('x1' = 1, 'x2' =  1, 'x3' = 1))
  expect_equal(deepFirst()(data3, 'clase', determinationCoefficient())$bestFeatures[1,], c('x1' = 1, 'x2' = 1, 'x3' = 1, 'x4' = 1, 'x5' = 1, 'x6' = 1, 'x7' = 1))
})

test_that("Names are set", {
  expect_equal(attr(deepFirst(),'name'),"Deep First Search");
  expect_equal(attr(deepFirst(),'shortName'),"deepFirstSearch");
})