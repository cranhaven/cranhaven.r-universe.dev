context("SFS")

data1 <- get(load('../data/example.RData'))
data2 <- get(load("../data/example2.RData"))
data3 <- get(load("../data/continuous.RData"))

test_that("Returns correct results", {
  expect_identical(sequentialForwardSelection()(data1, 'clase', IEConsistency())$bestFeatures[1,], c('x1' = 0,'x2' =  1, 'x3' = 1))
  expect_equal(sequentialForwardSelection()(data2, 'clase', symmetricalUncertain())$bestFeatures[1,], c('x1' = 0, 'x2' = 0, 'x3' = 0, 'x4' = 0, 'x5' = 0, 'x6' = 1))
  expect_identical(sequentialForwardSelection()(data3, 'clase', determinationCoefficient())$bestFeatures[1,], c('x1' = 1, 'x2' = 1, 'x3' = 1, 'x4' = 1, 'x5' = 1, 'x6' = 1, 'x7' = 1))
})

test_that("Name is set", {
  expect_equal(attr(sequentialForwardSelection(),'name'),"Sequential Forward Selection");
  expect_equal(attr(sequentialForwardSelection(),'shortName'),"sfs");
})