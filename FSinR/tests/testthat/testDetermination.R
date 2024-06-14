context("Determination")

data3 <- get(load("../data/continuous.RData"))

test_that("Results are correct", {
  expect_equal(determinationCoefficient()(data3, 'clase', 'x1'), 0.1002, tolerance = 1e-3)
  expect_equal(determinationCoefficient()(data3, 'clase', 'x6'), 0.0032, tolerance = 1e-2)
  expect_equal(determinationCoefficient()(data3, 'clase', c('x3', 'x5')), 0.1541, tolerance = 1e-3)
  expect_equal(determinationCoefficient()(data3, 'clase', c('x2', 'x1')), 0.1112, tolerance = 1e-3)
  expect_equal(determinationCoefficient()(data3, 'clase', c('x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7')), 0.5568, tolerance = 1e-4)
  expect_equal(determinationCoefficient()(mtcars, 'mpg', c('cyl', 'disp', 'hp', 'drat')), 0.7825119, tolerance = 1e-7)
})

test_that("It has correct name", {
  expect_equal(attr(determinationCoefficient(), 'name'),"Determination Coefficient")
  expect_equal(attr(determinationCoefficient(), 'shortName'),"determinationCoefficient")
})

