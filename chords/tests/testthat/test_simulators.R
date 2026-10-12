library(chords)
context("testing simulator")

test_that("simulator", {
  expect_is(makeSparseMatrix(2, 2e3, 1)!=0, 'Matrix')
  expect_is(makeWeightMatrix(c(weak=10, strong=2), 2e3)!=0, 'Matrix')
})
