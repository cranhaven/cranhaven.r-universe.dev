library(testthat)
library(foreach)
library(doParallel)
registerDoParallel(cores=2)

test_that('PairwiseDistance is right', {

  n <- 50
  xInd <- seq_len(n)
  set.seed(1)
  p <- 3
  data <- cbind(
    MASS::mvrnorm(n, rep(0, p), 
            diag(seq_len(p), nrow=p)),
    rexp(n))

  distM <- dist(data)
  distM2 <- PairwiseDistance(data, Lp)
  distM2p <- PairwiseDistance(data, Lp, PARALLEL=TRUE)
  expect_equal(as.matrix(distM), as.matrix(distM2))
  expect_equal(distM2, distM2p)

})


test_that('PairwiseDistance2 is right', {

  p <- 2
  for (n in c(2, 4, 50)) {
    xInd <- seq_len(n)
    set.seed(1)
    data <- cbind(
      MASS::mvrnorm(n, rep(0, p), 
              diag(seq_len(p), nrow=p)),
      rexp(n))

    # same data
    distM <- dist(data)
    distM2 <- PairwiseDistance2(data, data, Lp)
    distM2p <- PairwiseDistance2(data, data, Lp, PARALLEL=TRUE)
    expect_equal(unname(as.matrix(distM)), as.matrix(distM2))
    expect_equal(distM2, distM2p)

    # different data
    nr <- floor(n / 2)
    subDist <- PairwiseDistance2(data[seq_len(nr), , drop=FALSE], 
                                 data[seq(nr + 1, n), , drop=FALSE], 
                                 Lp)

    expect_equal(subDist, as.matrix(distM2)[seq_len(nr), seq(nr + 1, n), drop=FALSE])
  }

})


test_that('Lp distance is right', {

  n <- 10
  x <- c(0, 0)
  Y <- matrix(rnorm(10), n / 2, 2)
  expect_equal(Lp(x, Y), sqrt(rowSums(Y^2)))
  expect_equal(Lp(x, Y, 10), (rowSums(Y^10))^(1/10))
  expect_true(all(Lp(x, Y, 1) >= Lp(x, Y, 10)))
  
})
