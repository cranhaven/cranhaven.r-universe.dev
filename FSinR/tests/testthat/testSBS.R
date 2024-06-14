context("SBS")

data2 <- get(load("../data/example2.RData"))
data3 <- get(load("../data/continuous.RData"))

test_that("Returns correct results", {
  expect_identical(sequentialBackwardSelection()(data3, 'clase', determinationCoefficient())$bestFeatures[1,], c('x1' = 1, 'x2' = 1, 'x3' = 1, 'x4' = 1, 'x5' = 1, 'x6' = 1, 'x7' = 1))
  expect_equal(sequentialBackwardSelection()(data2, 'clase', IEPConsistency())$bestFeatures[1,], c('x1' = 0, 'x2' = 0, 'x3' = 0, 'x4' = 0, 'x5' = 0, 'x6' = 1))
})

test_that("Name is set", {
  expect_equal(attr(sequentialBackwardSelection(),'name'),"Sequential Backward Selection");
  expect_equal(attr(sequentialBackwardSelection(),'shortName'),"sbs");
})