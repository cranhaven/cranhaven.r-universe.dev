context("IEP Consistency measures")

data1 <- read.csv(file = "../data/example_1.csv")
data2 <- read.csv(file = "../data/example_2.csv")
data3 <- read.csv(file = "../data/example_3.csv")

test_that("Returns 4/15 on empty set", { #Automatizar esto
  expect_equal(IEPConsistency()(data2, 'class', c()), 4/15)
})

test_that("Returns 1 on empty set in a 1 class only dataset", {
  expect_equal(IEPConsistency()(data3, 'class', c()), 1)
})

test_that("Returns 1 on full set", {
  expect_equal(IEPConsistency()(data2, 'class', c('x1','x2','x3','x4','x5','x6')),1)
})


test_that("Results are correct", {
  expect_equal(IEPConsistency()(data1, 'class', c('x1', 'x2', 'x3')), 0.8666, tolerance=1e-3)
  expect_equal(IEPConsistency()(data1, 'class', c('x1', 'x3')), 0.7333, tolerance=1e-3)
  expect_equal(IEPConsistency()(data2, 'class', 'x6'), 1)
})

test_that("It has correct name", {
  expect_equal(attr(IEPConsistency(), 'name'),"Inconsistent Examples Pairs Consistency")
  expect_equal(attr(IEPConsistency(), 'shortName'),"IEPConsistency")
})