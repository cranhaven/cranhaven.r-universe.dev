context("Linear Consistency-Constrained")

dataset <- read.csv(file = "../data/50-35-15-0-lessthan-4.csv")

test_that("Feature set is ordered correctly", {
  orderedFeatures <- orderFeatures(dataset, 'y', c('x1', 'x2', 'x3', 'x4'), symmetricalUncertain())
  expect_equal(orderedFeatures, c('x1', 'x2', 'x3', 'x4'));
  invertedSymmetricalUncertain <- symmetricalUncertain()
  attr(invertedSymmetricalUncertain,'target') <- "minimize"
  orderedFeatures <- orderFeatures(dataset, 'y', c('x1', 'x2', 'x3', 'x4'), invertedSymmetricalUncertain)
  expect_equal(orderedFeatures, c('x4', 'x3', 'x2', 'x1'));
})

test_that("Algorithm exits if value is lower than threshold", {
  expect_identical(LCC()(dataset[,-(1:3)], 'y', symmetricalUncertain(), giniIndex())$bestFeatures[1,], c('x4' = 1))
})

test_that("Algorithm performs correctly", {
  expect_identical(LCC(threshold = 0.7)(dataset, 'y', symmetricalUncertain(), giniIndex())$bestFeatures[1,], c('x1' = 1, 'x2' = 0, 'x3' = 0, 'x4' = 0))
  expect_identical(LCC(threshold = 0.9)(dataset, 'y', symmetricalUncertain(), giniIndex())$bestFeatures[1,], c('x1' = 1, 'x2' = 1, 'x3' = 0, 'x4' = 0))
})

test_that("Name is set", {
  expect_equal(attr(LCC(),'name'),"Linear Consistency-Constrained");
  expect_equal(attr(LCC(),'shortName'),"LCC");
})