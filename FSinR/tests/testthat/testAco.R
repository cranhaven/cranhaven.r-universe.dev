context("Ant Colony Optimization")

test_that("Name is set", {
  expect_equal(attr(antColony(),'name'),"Ant Colony Optimization");
  expect_equal(attr(antColony(),'shortName'),"aco");
})