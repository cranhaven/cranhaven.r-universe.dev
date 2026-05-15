test_that("resid_cor returns residual correlation table with cor and z/se", {
  fit1 <- .fit_cont(n = 60)
  r1 <- resid_cor(fit1)
  expect_s3_class(r1, "data.frame")
  expect_true(all(c("pair","cor") %in% names(r1)))
  expect_true(any(c("z","se") %in% names(r1)))

  fit2 <- .fit_ord(n = 100)
  r2 <- resid_cor(fit2)
  expect_s3_class(r2, "data.frame")
  expect_true(all(c("pair","cor") %in% names(r2)))
  expect_true(any(c("z","se") %in% names(r2)))
})
