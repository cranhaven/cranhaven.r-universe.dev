test_that("beta_borrow_fujikawa works", {
  design <- setup_fujikawa(k = 4, p0 = 0.2)
  r <- c(9, 14, 1, 5)

  weight_mat <- get_weights_jsd(design, n = 20, epsilon = 1, tau = 0,
    logbase = 2)
  res1 <- beta_borrow_fujikawa(design = design, n = 20, r = r,
    weights = weight_mat)
  res2 <- val_borrow_fujikawa(design = design, n = 20, r = r, epsilon = 1,
    tau = 0, logbase = 2)
  res3 <- beta_borrow_fujikawa(design = design, n = 20, r = rev(r),
    weights = weight_mat)

  expect_equal(unname(res1), unname(res2))
  expect_equal(res1, res3[, 4:1])
})
