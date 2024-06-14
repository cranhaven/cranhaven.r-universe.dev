context("Simmulated Annealing")

test_that("Name is set", {
  expect_equal(attr(simulatedAnnealing(),'name'),"Simmulated Annealing");
  expect_equal(attr(simulatedAnnealing(),'shortName'),"sa");
})