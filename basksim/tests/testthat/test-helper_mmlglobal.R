test_that("weight_mmlglobal works", {
  design <- setup_mmlglobal(k = 4, p0 = 0.2)
  n <- 20
  r <- c(10, 12, 14, 16)
  res <- weight_mmlglobal(design = design, n = n, r = r)

  # Compare with the StudyPrior package (not avaible on cran)
  # val1 <- StudyPrior::binom.PP.EB(x = r[-1], n = rep(n, 3), X = r[1], N = n,
  #   p.prior.a = 1, p.prior.b = 1, mix = TRUE)
  # val2 <- StudyPrior::binom.PP.EB(x = r[-2], n = rep(n, 3), X = r[2], N = n,
  #   p.prior.a = 1, p.prior.b = 1, mix = TRUE)
  # val3 <- StudyPrior::binom.PP.EB(x = r[-3], n = rep(n, 3), X = r[3], N = n,
  #   p.prior.a = 1, p.prior.b = 1, mix = TRUE)
  # val4 <- StudyPrior::binom.PP.EB(x = r[-4], n = rep(n, 3), X = r[4], N = n,
  #   p.prior.a = 1, p.prior.b = 1, mix = TRUE)
  #
  # val_res <- cbind(t(attr(val1, "pars")), t(attr(val2, "pars")),
  #   t(attr(val3, "pars")), t(attr(val4, "pars")))
  # val_res[1, ] <- val_res[1, ] + r
  # val_res[2, ] <- val_res[2, ] + n - r

  val_res <- matrix(c(23, 39.87804, 43.87857, 31, 19, 25.71951, 19.87857, 11),
    nrow = 2, byrow = TRUE)

  expect_equal(res, val_res, tolerance = 1e-04)
})
