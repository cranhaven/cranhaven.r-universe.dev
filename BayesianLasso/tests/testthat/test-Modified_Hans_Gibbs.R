test_that("Modified_Hans_Gibbs returns valid output", {
  set.seed(123)
  X <- matrix(rnorm(100), 20, 5)
  y <- rnorm(20)
  beta_init <- rep(1, 5)
  
  out <- Modified_Hans_Gibbs(
    X = X,
    y = y,
    a1 = 0.01,
    b1 = 0.01,
    u1 = 0.01,
    v1 = 0.01,
    nsamples = 10,
    beta_init = beta_init,
    lambda_init = 0.1,
    sigma2_init = 1,
    verbose = 0
  )
  
  expect_type(out, "list")
  expect_true("mBeta" %in% names(out))
  expect_equal(nrow(out$mBeta), 10)
})
