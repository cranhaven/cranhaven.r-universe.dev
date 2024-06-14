context("Cramer")

data1 <- get(load('../data/example.RData'))
data2 <- get(load("../data/example2.RData"))

test_that("Results are correct", {
  expect_equal(cramer()(data2, 'clase', c('x1', 'x2', 'x3', 'x4', 'x5', 'x6')), c(x1 = 0, x2 = 0.7071, x3 = 0.7453, x4 = 0.4714, x5 = 0.4714, x6 = 1), tolerance = 1e-4)
  expect_equal(cramer()(data1, 'clase', 'x1'), 0)
  expect_equal(cramer()(data1, 'clase', 'x2'), 0.7071, tolerance = 1e-5)
  expect_equal(cramer()(data1, 'clase', 'x3'), 0.7453, tolerance = 1e-4)
  expect_equal(cramer()(data1, 'clase', c('x1', 'x2', 'x3')), c(x1 = 0, x2 = 0.7071, x3 = 0.7453), tolerance = 1e-4)
})

test_that("It has correct name", {
  expect_equal(attr(cramer(), 'name'),"Cramer")
  expect_equal(attr(cramer(), 'shortName'),"cramer")
})