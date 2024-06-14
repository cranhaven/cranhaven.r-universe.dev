context("IE Consistency")

data1 <- read.csv(file = "../data/example_1.csv")
data2 <- read.csv(file = "../data/example_2.csv")
data3 <- read.csv(file = "../data/example_3.csv")

test_that("Returns majority class proportion on empty set", {
  classCount <- sort(table(data2['class']),decreasing=TRUE)
  expect_equal(IEConsistency()(data2, 'class', c()), as.numeric(classCount[1] / sum(classCount)))
})

test_that("Returns 1 on empty set in a 1 class only dataset", {
  expect_equal(IEConsistency()(data3, 'class', c()), 1)
})

test_that("Returns 1 on full set", {
  expect_equal(IEConsistency()(data2, 'class', c('x1','x2','x3','x4','x5','x6')),1)
})

test_that("Results are correct", {
  expect_equal(IEConsistency()(data1, 'class', 'x1'), 0.5)
  expect_equal(IEConsistency()(data1, 'class', c('x1', 'x2')), 0.6666, tolerance=1e-3)
  expect_equal(IEConsistency()(data2, 'class', c('x1', 'x2', 'x3', 'x4', 'x5', 'x6')), 1, tolerance=1e-4)
})

test_that("It has correct name", {
  expect_equal(attr(IEConsistency(), 'name'),"Inconsistent Examples Consistency")
  expect_equal(attr(IEConsistency(), 'shortName'),"IEConsistency")
})