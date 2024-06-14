context("Chi Squared")

data1 <- get(load('../data/example.RData'))
data2 <- get(load("../data/example2.RData"))

test_that("Results are correct", {
  expect_equal(chiSquared()(data1, 'clase', 'x1'), 0)
  expect_equal(chiSquared()(data2, 'clase', 'x2'), 3) #0
  expect_equal(chiSquared()(data2, 'clase', 'x5'), 1.3333, tolerance = 1e-4)
  expect_equal(chiSquared()(data2, 'clase', 'x6'), 12)
  expect_equal(chiSquared()(data2, 'clase', c('x1', 'x2', 'x3', 'x4', 'x5', 'x6')), c(x1 = 0, x2 = 3, x3 = 3.333333, x4 = 1.333333, x5 = 1.333333, x6 = 12), tolerance = 1e-5)
})

test_that("It has correct name", {
  expect_equal(attr(chiSquared(), 'name'),"Chi Squared")
  expect_equal(attr(chiSquared(), 'shortName'),"chiSquared")
  
})

