context("generics")

test_that("print", {
  r = c(0.8, sqrt(0.5), sqrt(0.5))
  N = c(100, 100, 100)
  x = p_value(0.5, r, N)
  expect_output(print(x),
                "p-value for corrected correlation coefficients")

  x = p_value(c(0.25, 0.5), r, N)
  expect_output(print(x),
                "p-values for corrected correlation coefficients")

  expect_output(print(cc(r, N)),
                "Confidence curve for corrected correlation coefficients")
})

test_that("plot", {
  r = c(0.8, sqrt(0.5), sqrt(0.5))
  N = c(100, 100, 100)
  x = p_value(c(0.25, 0.5), r, N)
  expect_equal(x, plot(x))
  expect_equal(x, lines(x))

  r = c(0.8, sqrt(0.5), sqrt(0.5))
  N = c(100, 100, 100)
  x = p_value(c(0.25, 0.5), r, N)
  expect_equal(x, plot(x, level = NULL))
  expect_equal(x, lines(x, level = NULL))

  r = c(0.8, sqrt(0.5), sqrt(0.5))
  N = c(100, 100, 100)
  x = p_value(0.5, r, N)
  expect_error(plot(x))
  expect_error(lines(x))

  cc_ = cc(r, N, by = 0.5)
  expect_equal(cc_, plot(cc_))
  expect_equal(cc_, lines(cc_))
})
