x <- Franck_test(CNV, nsim = 1000, Elapsed_time = FALSE)
test_that("Franck_test works", {
  expect_true(x$pvalue_exact<0.05)
  expect_true(x$pvalue_appro<0.05)
})
