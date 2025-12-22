test_that("get_weights_cpp works", {
  wmat <- get_weights_cpp(n = 5, tune_a = 1, tune_b = 1)
  w1 <- 1 / (1 + exp(1 + 1 * log(5^(1/4) * abs(0/5 - 1/5))))
  w2 <- 1 / (1 + exp(1 + 1 * log(5^(1/4) * abs(0/5 - 2/5))))

  expect_equal(wmat[1, 2], w1)
  expect_equal(wmat[1, 3], w2)
  expect_true(isSymmetric(wmat))
  expect_equal(dim(wmat), c(6, 6))
})

test_that("get_weights_mml works", {
  design <- setup_mml(k = 4, p0 = 0.2, shape1 = 1, shape2 = 1)
  wmat <- get_weights_mml(design = design, n = 20)

  expect_true(isSymmetric(wmat))
  expect_true(all(wmat >= 0) & all(wmat <= 1))
  expect_equal(dim(wmat), c(21, 21))
})

test_that("get_weights_jsd works", {
  design <- setup_jsdglobal(k = 4, p0 = 0.15, shape1 = 1, shape2 = 1)
  wmat <- get_weights_jsd(design = design, n = 20, epsilon = 1.5, tau = 0,
    logbase = 2)

  shape <- rbind(1 + c(12, 7), 1 + (20 - c(12, 7)))
  w1 <- jsd_global(shape, epsilon = 1.5)

  expect_true(isSymmetric(wmat))
  expect_true(all(wmat >= 0) & all(wmat <= 1))
  expect_equal(wmat[13, 8], w1)
  expect_equal(dim(wmat), c(21, 21))
})
