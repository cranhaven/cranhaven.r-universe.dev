### Test whether or not IEEE floating point is working on the C/Fortran side.

test_that('IEEE floating point, Fortran bind(C)', {
  expect_equal(.C(glinvtestfloatIEEE01_, -Inf, NaN, NAOK=T)[[2]], 0.0)
  expect_equal(.C(glinvtestfloatIEEE01_, Inf, NaN, NAOK=T)[[2]], Inf)
})

test_that('IEEE floating point, .Call(), Rmath.h', {
  expect_equal(.Call(glinvtestfloatIEEE02, -Inf), 0.0)
  expect_equal(.Call(glinvtestfloatIEEE02,  Inf), Inf)
})

test_that('IEEE floating point, .C(), Rmath.h', {
  expect_equal(.C(glinvtestfloatIEEE03, -Inf, NaN, NAOK=T)[[2]], 0.0)
  expect_equal(.C(glinvtestfloatIEEE03,  Inf, NaN, NAOK=T)[[2]], Inf)
})

