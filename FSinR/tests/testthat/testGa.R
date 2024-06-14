context("Genetic Algorithm")

test_that("Name is set", {
  expect_equal(attr(geneticAlgorithm(),'name'),"Genetic Algorithm");
  expect_equal(attr(geneticAlgorithm(),'shortName'),"ga");
})