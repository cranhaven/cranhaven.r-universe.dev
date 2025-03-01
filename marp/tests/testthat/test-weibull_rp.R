test_that("weibull_rp", {
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
    res <- marp::weibull_rp(data, t, m, y)
  )

  # check result
  expect_equal(res$par1, 330.801103808081)
  expect_equal(res$par2, 1.80101338777944)
  expect_equal(res$logL, -193.923776766673)
  expect_equal(res$AIC, 391.847553533345)
  expect_equal(res$BIC, 394.64994829667)
  expect_equal(res$mu_hat, 294.169028302998)
  expect_equal(res$pr_hat, 0.307802775397902)
  expect_true(all.equal(res$haz_hat, c(-6.17145785493305 , -6.09511312491812 , -6.02541584706030 , -5.96130056661968 , -5.90193908878337, -5.84667487506097, -5.79497865560229  , -5.74641752189826 , -5.70063286718822 , -5.65732429911070, -5.61623768360296)))
})
