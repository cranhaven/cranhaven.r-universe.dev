context("Hill Climbing")

test_that("Name is set", {
  expect_equal(attr(hillClimbing(),'name'),"Hill Climbing");
  expect_equal(attr(hillClimbing(),'shortName'),"hc");
})