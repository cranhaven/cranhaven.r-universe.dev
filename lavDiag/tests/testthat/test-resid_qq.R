test_that("resid_qq returns ggplot for single- and multi-group", {
  p1 <- resid_qq(.fit_cont(n = 100))
  expect_true(inherits(p1, "ggplot"))

  p2 <- resid_qq(.fit_ord(n = 200))
  expect_true(inherits(p2, "ggplot"))
})
