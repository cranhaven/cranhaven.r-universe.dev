x <- Boik_test(MVGH, nsim = 10000)
test_that("Boik_test works", {
  expect_true(x$pvalue_exact<0.05)
})
