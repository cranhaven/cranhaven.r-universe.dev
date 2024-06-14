context("Search")

data2 <- get(load("../data/example2.RData"))
data3 <- get(load("../data/continuous.RData"))

test_that("Sequential search", {
  expect_equal(sequentialFloatingBackwardSelection()(data2, 'clase', binaryConsistency())$bestFeatures[1,], c('x1' = 0, 'x2' = 0, 'x3' = 0, 'x4' = 0, 'x5' = 0, 'x6' = 1))
  expect_identical(sequentialFloatingBackwardSelection()(data3, 'clase', determinationCoefficient())$bestFeatures[1,], c('x1' = 1, 'x2' = 1, 'x3' = 1, 'x4' = 1, 'x5' = 1, 'x6' = 1, 'x7' = 1))
})

test_that("Names are set", {
  expect_equal(attr(sequentialFloatingBackwardSelection(),'name'),"Sequential Floating Backward Selection");
  expect_equal(attr(sequentialFloatingBackwardSelection(),'shortName'),"sfbs");
})