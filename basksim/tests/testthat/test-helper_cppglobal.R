test_that("diff_all works", {
  res1 <- diff_all(n = 20, r = c(0, 10, 20), epsilon = 1)
  res2 <- diff_all(n = 20, r = c(5, 5, 5), epsilon = 1)

  expect_equal(res1, 0)
  expect_equal(res2, 1)
})

test_that("beta_borrow_cpp works", {
  design <- setup_cpp(k = 4, p0 = 0.2)
  r <- c(17, 15, 9, 5)

  weight_mat <- get_weights_cpp(n = 20, a = 1, b = 1)
  res1 <- beta_borrow_cppglobal(design = design, n = 20, r = r,
    weights_pair = weight_mat, epsilon = 1.5)
  res2 <- val_borrow_cpp(design = design, n = 20, r = r, a = 1, b = 1,
    globalweight_fun = diff_all, globalweight_params = list(epsilon = 1.5))
  res3 <- beta_borrow_cppglobal(design = design, n = 20, r = rev(r),
    weights_pair = weight_mat, epsilon = 1.5)

  expect_equal(unname(res1), unname(res2))
  expect_equal(res1, res3[, 4:1])
})
