context("Tabu Search")

test_that("Tabu search works", {
  result <- tabu(iter = 20)(iris, 'Species', giniIndex())
  expect_true(sum(result$bestFeatures) == 3)
  expect_equal(result$bestFitness,1)
})

test_that("Name is set", {
  expect_equal(attr(tabu(),'name'),"Tabu Search");
  expect_equal(attr(tabu(),'shortName'),"tabu");
})