context("Symmetrical Uncertain")

data1 <- read.csv(file = "../data/example_1.csv")
data2 <- read.csv(file = "../data/example_2.csv")

test_that("Results are correct", {
  expect_equal(symmetricalUncertain()(data1, 'class', 'x1'), 0)
  expect_equal(symmetricalUncertain()(data1, 'class', 'x3'), 0.4398, tolerance = 1e-3)
  expect_equal(symmetricalUncertain()(data2, 'class', c('x1', 'x2', 'x3', 'x4', 'x5', 'x6')), 0.7216, tolerance = 1e-3)
  expect_equal(symmetricalUncertain()(data2, 'class', 'x6'), 1)
})

test_that("It has correct name", {
  expect_equal(attr(symmetricalUncertain(), 'name'),"Symmetrical Uncertain")
  expect_equal(attr(symmetricalUncertain(), 'shortName'),"symmetricalUncertain")
})
