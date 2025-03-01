test_that("bpt_rp", {
  # set some parameters
  m <- 10 # number of iterations for MLE optimization
  t <- seq(100, 200, by = 10) # time intervals
  y <- 304 # cut-off year for estimating probablity

  # fix the random seed
  set.seed(42)

  # sample data for testing
  data <- rgamma(30, 3, 0.01)

  # fit renewal model
  suppressWarnings(  # suppressing warnings from stats::nlm: NA/Inf replaced by maximum positive value
    res <- marp::bpt_rp(data, t, m, y)
  )

  # check result
  expect_equal(res$par1, 292.945125794581)
  expect_equal(res$par2, 0.718247184450307)
  expect_equal(res$logL, -194.409960016141)
  expect_equal(res$AIC, 392.819920032282)
  expect_equal(res$BIC, 395.622314795606)
  expect_equal(res$mu_hat, 292.945125794581)
  expect_equal(res$pr_hat, 0.617945229960928)
  expect_true(all.equal(
    res$haz_hat,
    c(
      -5.79111270000214 ,
      -5.67659735477847 ,
      -5.58924173392941 ,
      -5.52166084269556 ,
      -5.46879213452752,
      -5.42707296810344 ,
      -5.39393880031248 ,
      -5.36750674126544 ,
      -5.34637016391776 ,
      -5.32946197009786,
      -5.31596149148995
    )
  ))
})
