test_that("Modified_PC_Gibbs returns valid output", {
  set.seed(123)
  X <- matrix(rnorm(100), 20, 5)
  y <- rnorm(20)
  
  out <- Modified_PC_Gibbs(
    X = X,
    y = y,
    a1 = 0.01,
    b1 = 0.01,
    u1 = 0.01,
    v1 = 0.01,
    nsamples = 10,
    lambda_init = 0.1,
    sigma2_init = 1,
    verbose = 0
  )
  
  expect_type(out, "list")
  expect_true("mBeta" %in% names(out))
  expect_equal(nrow(out$mBeta), 10)
})


test_that("normalize function works correctly", {
  set.seed(1)
  X <- matrix(rnorm(100 * 5), 100, 5)
  y <- as.vector(X %*% c(1, -1, 0.5, 0, 0) + rnorm(100))
  
  norm_result <- normalize(y, X)
  
  expect_type(norm_result, "list")
  expect_named(norm_result, c("vy", "mX", "mu.y", "sigma2.y", "mu.x", "sigma2.x"))
  expect_equal(length(norm_result$vy), 100)
  expect_equal(dim(norm_result$mX), dim(X))
})
