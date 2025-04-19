test_that("gives vector", {
  expect_type(restaurants <- ushuaia_restaurants(), "character")
  expect_gt(length(restaurants), 100)
})
