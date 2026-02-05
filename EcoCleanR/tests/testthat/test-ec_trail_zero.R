test_that("ec_trail_zero removes trailing zeros", {
  expect_equal(ec_trail_zero(12.700000), "12.7")
  expect_equal(ec_trail_zero(45.000000), "45")
})
