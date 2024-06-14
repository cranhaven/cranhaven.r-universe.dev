context("Binary Consistency")

data1 <- read.csv(file = "../data/example_1.csv")
data2 <- read.csv(file = "../data/example_2.csv")
data3 <- read.csv(file = "../data/example_3.csv")

test_that("Returns 0 on empty set in > 1 class dataset", {
  expect_equal(binaryConsistency()(data2, 'class', c()), 0)
})

test_that("Returns 1 on empty set in a 1 class only dataset", {
  expect_equal(binaryConsistency()(data3, 'class', c()), 1)
})

test_that("Returns 1 on full set", {
  expect_equal(binaryConsistency()(data2, 'class', c('x1','x2','x3','x4','x5','x6')),1)
})

test_that("Results are correct", {
  expect_equal(binaryConsistency()(data1, 'class', c('x1', 'x2', 'x3')), 0)
  expect_equal(binaryConsistency()(data2, 'class', 'x6'), 1)
  expect_equal(binaryConsistency()(data2, 'class', c('x5', 'x6')), 1)
})

test_that("It has correct name", {
  expect_equal(attr(binaryConsistency(), 'name'),"Binary Consistency")
  expect_equal(attr(binaryConsistency(), 'shortName'),"binaryConsistency")
})
