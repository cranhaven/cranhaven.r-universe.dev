x <- KKM_test(RDWW, nsim = 1000, nc0 = 1000)
test_that("KKM_test works", {
  expect_true(x$pvalue_exact<0.05)
  })
