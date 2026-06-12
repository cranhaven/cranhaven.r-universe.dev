x <- Malik_test(IDCP, nsim = 1000, Elapsed_time = FALSE)
test_that("Malik_test works", {
  expect_true(x$pvalue_exact<0.05)
})

