context("F-score measure")

test_that("Returns 0 on empty feature set", {
  expect_equal(fscore()(ToothGrowth, 'supp', c()), 0)
})

test_that("Stops on datasets with less or more than 2 classes", {
  expect_error(fscore()(iris, 'Species', c('Petal.Length')), 'Data set is required to have only 2 classes')
})

test_that("Performs correctly", {
  expect_equal(fscore()(ToothGrowth, 'supp', c('len')), 0.06113754)
  expect_equal(fscore()(ToothGrowth, 'supp', c('dose')), 0)
  expect_equal(fscore()(ToothGrowth, 'supp', c('len','dose')), c(len = 0.06113754, dose = 0))
})

test_that("Name is set", {
  expect_equal(attr(fscore(),'name'),"F-score");
  expect_equal(attr(fscore(),'shortName'),"fscore");
})