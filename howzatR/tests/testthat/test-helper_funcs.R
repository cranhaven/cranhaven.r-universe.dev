
# calc_int_div & calc_mod -------------------------------------------------

test_that("calc_int_div & calc_mod works", {
  expect_equal(calc_mod(value = 5.5, divisor = 1), 0.5)
  expect_equal(calc_int_div(value = 5.5, divisor = 1), 5)
})
