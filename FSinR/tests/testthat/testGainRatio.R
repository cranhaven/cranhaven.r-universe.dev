context("Gain Ratio")

data1 <- read.csv(file = "../data/example_1.csv")
data2 <- read.csv(file = "../data/example_2.csv")

test_that("Results are correct", {
  expect_equal(gainRatio()(data1, 'class', 'x1'), 0) #1
  expect_equal(gainRatio()(data1, 'class', 'x2'), 0.5)
  expect_equal(gainRatio()(data2, 'class', c('x1', 'x2')), 0.5)
  expect_equal(gainRatio()(data1, 'class', c('x1', 'x2', 'x3')), 0.6853, tolerance = 1e-3)
})

test_that("It has correct name", {
  expect_equal(attr(gainRatio(), 'name'),"Gain Ratio")
  expect_equal(attr(gainRatio(), 'shortName'),"gainRatio")
})
