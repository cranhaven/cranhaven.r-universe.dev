library(testthat)
library(ddalpha)
test_that('Different MHD implementations are the same. They are closed to the exact depth', {

  n <- 50
  xInd <- seq_len(n)
  set.seed(1)
  for (p in 2:3) {
    data <- cbind(
      MASS::mvrnorm(n, rep(0, p), 
              diag(seq_len(p), nrow=p)),
      rexp(n))

    depths <- ddalpha::depth.halfspace(data, data, exact=TRUE)

    distM <- as.matrix(dist(data))
    dataInd <- seq_len(nrow(distM))
    # myDepth <- MHalfDepth(distM, xInd, dataInd)
    # myDepth2 <- MHD2(distM, xInd, dataInd)
    myDepth3 <- MHD_cpp(distM)
    
    expect_equal(cor(depths, myDepth3), 1, scale=1, tol=0.15)
    # expect_equal(myDepth, myDepth3)
    # expect_equal(myDepth2, myDepth3)
  }
})


test_that('When there are a lot of anchor points, the result is closer to the true Tukeys depth', {
  
  n <- 20
  xInd <- seq_len(n)
  set.seed(1)
  p <- 2
  data <- MASS::mvrnorm(n, rep(0, p), 
            diag(seq_len(p), nrow=p))

  depths <- ddalpha::depth.halfspace(data, data, exact=TRUE)

  grid <- seq(-5, 5, length.out=30)
  anc <- as.matrix(expand.grid(grid, grid))
  # nAnc <- nrow(anc)
  # anc <- data
  distM <- as.matrix(PairwiseDistance(data, Lp))
  distM2 <- PairwiseDistance2(data, anc, Lp, p=2)
  
  myDepth <- MHD_cpp(distM)
  myDepth2 <- MHD_cpp(distM2)
  expect_true(Lp(depths, myDepth) >= Lp(depths, myDepth2))
  expect_equal(depths, myDepth2)
  
})


test_that('The implementation with user-specified eval points work', {

  n <- 50
  set.seed(1)
  p <- 2
  data <- MASS::mvrnorm(n, rep(0, p), 
            diag(seq_len(p), nrow=p))

  # depths <- ddalpha::depth.halfspace(data, data, exact=TRUE)

  distM <- as.matrix(dist(data))
  myDepth_all <- MHD_cpp(distM)

  xInd <- 1:3
  ancX <- distM[, xInd, drop=FALSE]
  myDepth <- MHD2_cpp(distM, ancX)
  expect_equal(myDepth_all[xInd], myDepth)

  x <- matrix(c(0, 0), 1, p)
  ancX <- PairwiseDistance2(data, x, Lp)
  myDepth0 <- MHD2_cpp(distM, ancX)
  expect_equal(myDepth0, 0.5, scale=1, tolerance=0.1)
})


test_that('MHD_cpp and MHD2_cpp agrees', {
  
  n <- 50
  set.seed(1)
  p <- 2
  data <- MASS::mvrnorm(n, rep(0, p), 
            diag(seq_len(p), nrow=p))

  distM <- as.matrix(dist(data))
  myDepth1 <- MHD_cpp(distM)
  myDepth2 <- MHD2_cpp(distM, distM)
  expect_equal(myDepth1, myDepth2)

  grid <- seq(-5, 5, length.out=30)
  anc <- as.matrix(expand.grid(grid, grid))
  distM2 <- PairwiseDistance2(data, anc, Lp, p=2)
  myDepth3 <- MHD_cpp(distM2)
  myDepth4 <- MHD2_cpp(distM2, t(distM2))
  expect_equal(myDepth3, myDepth4)

})


test_that('MHD true depth is working', {

  n <- 50
  xInd <- seq_len(n)
  set.seed(1)
  p <- 3
  mfdName <- 'AffInv'

  for (p in 2:4) {
  for (mfdName in c('Euclidean', 'Sphere', 'AffInv')) {
    mfd <- manifold::createM(mfdName)
    dInt <- manifold::calcIntDim(mfd, geomPar=p)

    data <- t(manifold::rmfd(mfd, n, dInt))
    anchors <- t(manifold::rmfd(mfd, 3 * n, dInt))

    Dist <- function(a, B) {
      manifold::distance(mfd, a, t(B))
    }
    # profvis::profvis({
    datAnc <- PairwiseDistance2(data, anchors, Dist)
    # })
    ancX <- t(datAnc)

    depthOnly <- MHD2_cpp(datAnc, ancX)

    # print(system.time({
    depthObj <- MHD(mfd, data, anchors)
    # }))

    expect_equal(depthOnly, depthObj$depthSamp)

    # Out-of-sample deepest depth is larger than the in-sample version
    expect_true(depthObj$depthDeepest >= max(depthObj$depthSamp))

    # TODO: test API of nloptr etc
  }
  }
})


test_that('MHD jiggle= and XEval= are working', {

  n <- 50
  xInd <- seq_len(n)
  set.seed(1)
  p <- 2
  mfdName <- 'AffInv'

  mfd <- manifold::createM(mfdName)
  dInt <- manifold::calcIntDim(mfd, geomPar=p)

  data <- t(manifold::rmfd(mfd, n, dInt))
  anchors <- t(manifold::rmfd(mfd, n, dInt))

  Dist <- function(a, B) {
    manifold::distance(mfd, a, t(B))
  }

  o <- manifold::origin(mfd, dInt)

  # print(system.time({
  obj <- MHD(mfd, data, anchors, XEval=t(o))
  # }))
  # print(system.time({
  objJig <- MHD(mfd, data, anchors, jiggle=2, XEval=t(o))
  # }))

  expect_true(all(obj$depthSamp >= objJig$depthSamp))

  expect_true(all(obj$depthX >= objJig$depthX))

})


test_that('MHD_tree works', {

  dat <- kdetrees::apicomplexa
  res0 <- MHD_tree(dat, jiggle=0)
  res2 <- MHD_tree(dat, jiggle=2)

  expect_true(all(res0$depthSamp >= res2$depthSamp))
})
