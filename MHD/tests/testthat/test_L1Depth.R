test_that('L1Depth is correct', {

  n <- 50
  xInd <- seq_len(n)
  set.seed(1)
  p <- 2
  for (p in 2:3) {
    data <- cbind(
      MASS::mvrnorm(n, rep(0, p), 
              diag(seq_len(p), nrow=p)),
      rexp(n))

    distM <- as.matrix(dist(data))

    a <- LpAve(distM)
    expect_equal(a, colMeans(distM))
    expect_equal(LpAve(distM, p=2), colMeans(distM^2)^(1/2))

    ind <- 1:10
    dataX <- distM[, ind, drop=FALSE]
    expect_equal(LpAve(dataX), a[ind])
  }
})

