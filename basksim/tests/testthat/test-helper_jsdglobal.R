test_that("jsd_global works", {
  shape1 <- matrix(c(1, 1, 1, 10, 10, 10), ncol = 3, byrow = TRUE)
  jsdglobal1 <- jsd_global(shape1, 1)

  # (1 - JSD) is equal to 1 when all distributions are equal
  expect_equal(jsdglobal1, 1)

  shape2 <- matrix(c(1 + 4, 1 + 6, 1 + 2, 1 + 8), ncol = 2)
  jsdglobal2 <- jsd_global(shape2, 2)

  # Compare with results from baskexact
  expect_equal(jsdglobal2, 0.560588865)

  shape3 <- matrix(c(1, 100, 100, 1), ncol = 2, byrow = TRUE)
  jsdglobal3 <- jsd_global(shape3, 1)

  # (1 - JSD) is 0 when distributions are very different
  expect_equal(jsdglobal3, 0)

  # Compare with validator function jsd_global4
  r <- c(5, 1, 10, 7)
  shape4 <- rbind(1 + r, 1 + (20 - r))

  jsdglobal4 <- jsd_global(shape4, 2)
  jsdglobal5 <- jsd_global4(shape4, 2)

  expect_equal(jsdglobal4, jsdglobal5)

  # Compare with weights from get_weights_jsd
  design <- setup_jsdglobal(k = 4, p0 = 0.15)
  jsdmat <- get_weights_jsd(design = design, n = 20, epsilon = 1.5, tau = 0,
    logbase = 2)
  r2 <- c(5, 7)
  shape5 <- rbind(1 + r2, 1 + 20 - r2)
  jsdglobal6 <- jsd_global(shape5, 1.5)
  jsdglobal7 <- jsdmat[1 + 5, 1 + 7]

  expect_equal(jsdglobal6, jsdglobal7)
})

test_that("beta_borrow_jsdglobal works", {
  design1 <- setup_fujikawa(k = 3, p0 = 0.2)

  weights1 <- get_weights_jsd(design1, n = 20, epsilon = 2, tau = 1,
    logbase = 2)
  res1 <- beta_borrow_jsdglobal(design1, n = 20, r = c(1, 3, 5),
    weights_pair = weights1, eps_all = 0)
  res2 <- beta_borrow_fujikawa(design1, n = 20, r = c(1, 3, 5),
    weights = weights1)

  # Results are identical for fujikawa and jsd global when no information
  # is shared
  expect_equal(res1, res2)

  weights2 <- get_weights_jsd(design1, n = 20, epsilon = 2, tau = 0,
    logbase = 2)
  res3 <- beta_borrow_jsdglobal(design1, n = 20, r = c(3, 3, 3),
    weights_pair = weights2, eps_all = 0)
  res4 <- beta_borrow_fujikawa(design1, n = 20, r = c(3, 3, 3),
    weights = weights2)

  # Results differ only by the amount of prior information that is not shared
  expect_true(all(res3 + 2 == res4))

  # Sharing works correctly with 4 baskets
  design2 <- setup_jsdglobal(k = 4, p0 = 0.15)

  r1 <- c(12, 7, 3, 18)
  weights3 <- get_weights_jsd(design2, n = 20, epsilon = 1.5, tau = 0,
    logbase = 2)
  res5 <- beta_borrow_jsdglobal(design2, n = 20, r = r1,
    weights_pair = weights3, eps_all = 1.2)
  res6 <- val_borrow_jsdglobal(design2, n = 20, r = r1, epsilon = 1.5, tau = 0,
    logbase = 2, eps_all = 1.2)
  res7 <- beta_borrow_jsdglobal(design2, n = 20, r = rev(r1),
    weights_pair = weights3, eps_all = 1.2)

  expect_equal(unname(res5), unname(res6))
  expect_equal(res5, res7[, 4:1])

  # Compare with results from baskexact
  res8 <- beta_borrow_jsdglobal(design2, n = 20, r = r1,
    weights_pair = weights3, eps_all = 0)
  res9 <- rbind(c(17.23864, 12.83330, 6.842622, 20.380510),
    c(13.55264, 22.74386, 22.979734, 3.947204))

  expect_equal(unname(res8), res9, tolerance = 1e-7)
  r2 <- c(9, 9, 5, 17)
  res10 <- beta_borrow_jsdglobal(design2, n = 20, r = r2,
    weights_pair = weights3, eps_all = 1.2)

  s11 <- 1 + r2[1] +
    (jsd_global(rbind(1 + r2[1:2], 1 + 20 - r2[1:2]), eps = 1.5) * r2[2] +
    jsd_global(rbind(1 + r2[c(1, 3)], 1 + 20 - r2[c(1, 3)]), eps = 1.5) * r2[3] +
    jsd_global(rbind(1 + r2[c(1, 4)], 1 + 20 - r2[c(1, 4)]), eps = 1.5) * r2[4]) *
    jsd_global(rbind(1 + r2, 1 + 20 - r2), eps = 1.2)
  s21 <- 1 + r2[2] +
    (jsd_global(rbind(1 + r2[1:2], 1 + 20 - r2[1:2]), eps = 1.5) * r2[1] +
    jsd_global(rbind(1 + r2[2:3], 1 + 20 - r2[2:3]), eps = 1.5) * r2[3] +
    jsd_global(rbind(1 + r2[c(2, 4)], 1 + 20 - r2[c(2, 4)]), eps = 1.5) * r2[4]) *
    jsd_global(rbind(1 + r2, 1 + 20 - r2), eps = 1.2)
  s31 <- 1 + r2[3] +
    (jsd_global(rbind(1 + r2[c(1, 3)], 1 + 20 - r2[c(1, 3)]), eps = 1.5) * r2[1] +
    jsd_global(rbind(1 + r2[2:3], 1 + 20 - r2[2:3]), eps = 1.5) * r2[2] +
    jsd_global(rbind(1 + r2[3:4], 1 + 20 - r2[3:4]), eps = 1.5) * r2[4]) *
    jsd_global(rbind(1 + r2, 1 + 20 - r2), eps = 1.2)
  s41 <- 1 + r2[4] +
    (jsd_global(rbind(1 + r2[c(1, 4)], 1 + 20 - r2[c(1, 4)]), eps = 1.5) * r2[1] +
    jsd_global(rbind(1 + r2[c(2, 4)], 1 + 20 - r2[c(2, 4)]), eps = 1.5) * r2[2] +
    jsd_global(rbind(1 + r2[3:4], 1 + 20 - r2[3:4]), eps = 1.5) * r2[3]) *
    jsd_global(rbind(1 + r2, 1 + 20 - r2), eps = 1.2)

  s12 <- 1 + 20 - r2[1] +
    (jsd_global(rbind(1 + r2[1:2], 1 + 20 - r2[1:2]), eps = 1.5) *
        (20 - r2[2]) +
    jsd_global(rbind(1 + r2[c(1, 3)], 1 + 20 - r2[c(1, 3)]), eps = 1.5) *
        (20 - r2[3]) +
    jsd_global(rbind(1 + r2[c(1, 4)], 1 + 20 - r2[c(1, 4)]), eps = 1.5) *
        (20 - r2[4])) *
    jsd_global(rbind(1 + r2, 1 + 20 - r2), eps = 1.2)
  s22 <- 1 + 20 - r2[2] +
    (jsd_global(rbind(1 + r2[1:2], 1 + 20 - r2[1:2]), eps = 1.5) *
        (20 - r2[1]) +
    jsd_global(rbind(1 + r2[2:3], 1 + 20 - r2[2:3]), eps = 1.5) *
        (20 - r2[3]) +
    jsd_global(rbind(1 + r2[c(2, 4)], 1 + 20 - r2[c(2, 4)]), eps = 1.5) *
        (20 - r2[4])) *
    jsd_global(rbind(1 + r2, 1 + 20 - r2), eps = 1.2)
  s32 <- 1 + 20 - r2[3] +
    (jsd_global(rbind(1 + r2[c(1, 3)], 1 + 20 - r2[c(1, 3)]), eps = 1.5) *
        (20 - r2[1]) +
    jsd_global(rbind(1 + r2[2:3], 1 + 20 - r2[2:3]), eps = 1.5) *
        (20 - r2[2]) +
    jsd_global(rbind(1 + r2[3:4], 1 + 20 - r2[3:4]), eps = 1.5) *
        (20 - r2[4])) *
    jsd_global(rbind(1 + r2, 1 + 20 - r2), eps = 1.2)
  s42 <- 1 + 20 - r2[4] +
    (jsd_global(rbind(1 + r2[c(1, 4)], 1 + 20 - r2[c(1, 4)]), eps = 1.5) *
        (20 - r2[1]) +
    jsd_global(rbind(1 + r2[c(2, 4)], 1 + 20 - r2[c(2, 4)]), eps = 1.5) *
        (20 - r2[2]) +
    jsd_global(rbind(1 + r2[3:4], 1 + 20 - r2[3:4]), eps = 1.5) *
        (20 - r2[3])) *
    jsd_global(rbind(1 + r2, 1 + 20 - r2), eps = 1.2)

  res11 <- rbind(c(s11, s21, s31, s41), c(s12, s22, s32, s42))

  expect_equal(unname(res10), res11)
})
