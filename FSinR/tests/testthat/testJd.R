context("Jd")

test_that("Returns 0 on empty feature set", {
  expect_equal(Jd()(ToothGrowth, 'supp', c()), 0)
})

test_that("Stops on datasets with less or more than 2 classes", {
  expect_error(Jd()(iris, 'Species', c('Petal.Length')), 'Data set is required to have only 2 classes')
})

test_that("Returns correct results", {
  expect_equal(Jd()(ToothGrowth,'supp',c('len')), 0.2445502, tolerance = 1e-6)
  expect_equal(Jd()(ToothGrowth,'supp',c('dose')), 0, tolerance = 1e-6)
  expect_equal(Jd()(ToothGrowth,'supp',c('len','dose')), 0.7765059, tolerance = 1e-6)
})

test_that("Name is set", {
  expect_equal(attr(Jd(),'name'),"Jd");
  expect_equal(attr(Jd(),'shortName'),"Jd");
})