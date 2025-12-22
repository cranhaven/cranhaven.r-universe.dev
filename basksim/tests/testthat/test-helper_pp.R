test_that("beta_borrow_pp works", {
  design <- setup_cpp(k = 4, p0 = 0.2)
  n <- 20
  r <- c(10, 12, 14, 16)

  weight_mat <- get_weights_cpp(n = 20, a = 1, b = 1)
  res1 <- beta_borrow_pp(design = design, n = 20, r = r, weights = weight_mat)
  res2 <- val_borrow_cpp(design = design, n = 20, r = r, a = 1, b = 1)
  res3 <- beta_borrow_pp(design = design, n = 20, r = rev(r),
    weights = weight_mat)

  expect_equal(unname(res1), unname(res2))
  expect_equal(res1, res3[, 4:1])
})
