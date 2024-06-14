context("Gini Index")

data1 <- get(load('../data/example.RData'))
data2 <- get(load("../data/example2.RData"))

test_that("Gini functions work", {
  expect_equal(giniIndex()(data1, 'clase', 'x1'), 0.3888, tolerance = 1e-3)
  expect_equal(giniIndex()(data1, 'clase', 'x2'), 0.5833, tolerance = 1e-4)
  expect_equal(giniIndex()(data1, 'clase', 'x3'), 0.5555, tolerance = 1e-3)
  expect_equal(giniIndex()(data2, 'clase', c('x2', 'x3')), 0.7777, tolerance = 1e-3)
  expect_equal(giniIndex()(data2, 'clase', c('x1', 'x3')), 0.5555, tolerance = 1e-3)
  expect_equal(giniIndex()(data2, 'clase', c('x1', 'x3', 'x5', 'x6')), 1)
})

test_that("Name is correct", {
  expect_equal(attr(giniIndex(),'name'),"Gini Index");
  expect_equal(attr(giniIndex(),'shortName'),"giniIndex");
})