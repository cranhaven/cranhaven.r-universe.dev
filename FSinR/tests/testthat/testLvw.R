context("Las Vegas")

test_that("Name is set", {
  expect_equal(attr(LasVegas(),'name'),"Las Vegas");
  expect_equal(attr(LasVegas(),'shortName'),"lv");
})