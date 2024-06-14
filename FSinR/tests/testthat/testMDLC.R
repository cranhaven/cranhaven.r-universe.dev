context("MDLC measure")

test_that("Returns 0 on empty set", {
  expect_equal(MDLC()(iris, 'Species', c()), 0)
})

test_that("Performs correctly", {
  expect_equal(MDLC()(iris,'Species',c('Sepal.Length')), 39.71941, tolerance = 1e-3)
  expect_equal(MDLC()(iris,'Species',c('Sepal.Length', 'Petal.Width')), -93.08945, tolerance = 1e-3)
  expect_equal(MDLC()(iris,'Species',c('Sepal.Length', 'Petal.Width', 'Petal.Length')), -121.0862, tolerance = 1e-3)
  expect_equal(MDLC()(iris,'Species',c('Sepal.Length', 'Petal.Width', 'Petal.Length', 'Sepal.Width')), -34.14408, tolerance = 1e-3)
})

test_that("Names are set", {
  expect_equal(attr(MDLC(),'name'),"MDLC");
  expect_equal(attr(MDLC(),'shortName'),"MDLC");
})