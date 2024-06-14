context("Mutual Information")

data1 <- read.csv(file = "../data/example_1.csv")
data2 <- read.csv(file = "../data/example_2.csv")

test_that("Results are correct", {
  expect_equal(mutualInformation()(data2, 'class', 'x1'), 0)
  expect_equal(mutualInformation()(data2, 'class', 'x2'), 0.4591, tolerance = 1e-3)
  expect_equal(mutualInformation()(data1, 'class', c('x1', 'x2')),0.4591, tolerance = 1e-3)
  expect_equal(mutualInformation()(data1, 'class', c('x2', 'x3')), 1)
})

test_that("It has correct name", {
  expect_equal(attr(mutualInformation(), 'name'),"Mutual Information")
  expect_equal(attr(mutualInformation(), 'shortName'),"mutualInformation")
})
