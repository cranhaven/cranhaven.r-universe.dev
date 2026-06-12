x <- Piepho_test(MVGH, nsim = 1000)
test_that("Piepho_test works", {
  expect_true(x$pvalue_exact<0.05)
})
