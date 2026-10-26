test_that("prepare_cost_matrix handles NA, rectangles, and maximize flip", {
  M <- matrix(c(4, NA, 3,
                2,  0, 5), nrow = 2, byrow = TRUE)

  out <- prepare_cost_matrix(M, maximize = TRUE)

  expect_identical(out$n, 2L)
  expect_identical(out$m, 3L)

  mask <- as.integer(out$mask)
  expect_equal(mask, c(0L,1L,0L, 0L,0L,0L))

  v <- as.numeric(out$cost)
  expect_true(is.infinite(v[2]))
  expect_equal(v[c(1,3,4,5,6)], c(1,2,3,5,0), tolerance = 1e-12)
  expect_equal(out$cmax, 5)
})
