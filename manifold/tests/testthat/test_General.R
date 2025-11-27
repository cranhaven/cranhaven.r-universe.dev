library(testthat)

mfdList <- listAvailMfd()
mfdFinite <- mfdList[vapply(mfdList, is.finiteDim, TRUE)]
# mfdFinite <- mfdFinite[!mfdFinite %in% 'SO']

test_that('random variable generation works', {
  # mm <- mfdList[1]
  # n <- 10
  # p <- 4

  set.seed(2)
  for (n in 1:3) {
  for (p in 2:4) {
  for (mm in mfdFinite) {
    mfd <- createM(mm)
    d <- calcIntDim(mfd, geomPar=p)

    samp <- rmfd(mfd, n, d)
    samp1 <- rmfd(mfd, 100, d, p=samp[, 1], totalVar = 0.1)

    expect_identical(dim(samp), c(calcAmbDim(mfd, dimIntrinsic=d), as.integer(n)))
    
    expect_equal(distance(mfd, samp, samp), rep(0, n), scale=1, tolerance=1e-6)

    expect_equal(distance(mfd, frechetMean(mfd, samp1, mu0=samp[, 1]), samp[, 1, drop=FALSE]), 
                 0, scale=1, tolerance=0.1)
  }
  }
  }
})


test_that('Frechet median works', {

  mfdName <- 'Euclidean'
  mfd <- createM(mfdName)
  n <- c(5, 11, 51) # Should be even
  for (nn in n) {
    x <- rnorm(nn)
    fMed <- c(frechetMedian(mfd, matrix(x, 1, nn)))
    med <- median(x)
    expect_equal(fMed, med)
  }

})


test_that('multistart Frechet median works', {

  mfdName <- 'Sphere'
  mfd <- createM(mfdName)
  o <- origin(mfd, 1)
  v0 <- c(-2.5, -1, -0.5, 0, 1)
  x <- rieExp(mfd, 
              o, 
              rbind(0, v0))
  allMedian <- apply(x, 2, function(y) {
    frechetMedian(mfd, x, mu0=y)
  })
  # plot(x[1, ], x[2, ])
  # points(allMedian[1, ], allMedian[2, ], col='red')
  multStartMedian <- frechetMedian(mfd, x, mu0=x)
  expect_equal(multStartMedian, rieExp(mfd, o, rbind(0, median(v0))))


})


test_that('multistart Frechet mean works', {

  mfdName <- 'Sphere'
  mfd <- createM(mfdName)
  o <- origin(mfd, 1)
  v0 <- c(-2.5, -1, 0, 1)
  x <- rieExp(mfd, 
              o, 
              rbind(0, v0))
  allMean <- apply(x, 2, function(y) {
    frechetMean(mfd, x, mu0=y)
  })
  multStartMean <- frechetMean(mfd, x, mu0=x)
  expect_equal(multStartMean, rieExp(mfd, o, rbind(0, mean(v0))))


})


test_that('metric, norm, distance, exp, log follows the recycling rule', {

  for (n in c(1, 20)) {

  for (p in 2:3) {
  for (mm in mfdFinite) {
    mfd <- createM(mm)
    d <- calcIntDim(mfd, geomPar=p)
    dAmb <- calcAmbDim(mfd, geomPar=p)
    dTan <- calcTanDim(mfd, geomPar=p)
    samp <- rmfd(mfd, n, d)
    samp0 <- rmfd(mfd, 0, d)

    # Test length 0 case
    expect_equal(rieLog(mfd, samp0, samp0), matrix(0, dTan, 0))
    expect_equal(rieLog(mfd, samp0, samp), matrix(0, dTan, 0))
    expect_equal(rieLog(mfd, samp, samp0), matrix(0, dTan, 0))
    expect_equal(rieExp(mfd, samp0, rieLog(mfd, samp[, 1], samp0)), matrix(0, dAmb, 0))
    expect_equal(rieExp(mfd, samp0, rieLog(mfd, samp[, 1], samp)), matrix(0, dAmb, 0))
    expect_equal(rieExp(mfd, samp, rieLog(mfd, samp[, 1], samp0)), matrix(0, dAmb, 0))
    expect_equal(distance(mfd, samp0, samp0), numeric(0))
    expect_equal(distance(mfd, samp0, samp), numeric(0))
    expect_equal(distance(mfd, samp, samp0), numeric(0))
    expect_equal(norm(mfd, samp0, samp0), numeric(0))
    expect_equal(norm(mfd, samp, samp0), numeric(0))
    expect_equal(metric(mfd, samp, samp0, samp0), numeric(0))
    expect_equal(metric(mfd, samp, samp, samp0), numeric(0))
    expect_equal(metric(mfd, samp, samp0, samp), numeric(0))

    # Positive length case
    V1 <- rieLog(mfd, samp[, 1], samp)
    V2 <- rieLog(mfd, matrix(samp[, 1], nrow(samp), n), samp)
    expect_equal(V1, V2)
    V3 <- rieLog(mfd, samp, samp[, 1])
    V4 <- rieLog(mfd, samp, matrix(samp[, 1], nrow(samp), n))
    expect_equal(V3, V4)


    if (n > 1) {
      X <- samp
      V <- rieLog(mfd, samp[, 2], X)
      expect_equivalent(X, rieExp(mfd, samp[, 2], V))
      VV <- rieLog(mfd, samp, X[, 2])
      expect_equivalent(matrix(X[, 2], nrow=nrow(samp), ncol=ncol(samp)), 
                        rieExp(mfd, samp, VV))
    }

    in1 <- metric(mfd, U=V1, V=V1[, 1])
    in2 <- metric(mfd, U=V1, V=matrix(V1[, 1], nrow(V1), ncol(V1)))
    expect_equivalent(in1, in2)

    norm1 <- norm(mfd, samp[, 1], V1)
    norm2 <- norm(mfd, matrix(samp[, 1], nrow(samp), ncol(V1)), V1)
    expect_equal(norm1, norm2)

    norm3 <- norm(mfd, samp, V1[, 1])
    norm4 <- norm(mfd, samp, matrix(V1[, 1], nrow(V1), ncol(samp)))
    expect_equal(norm3, norm4)

    X1 <- samp
    X2 <- samp[, rev(seq_len(ncol(samp))), drop=FALSE]
    d0 <- distance(mfd, X1, X1) # M-M works
    expect_equal(max(d0), 0, tolerance=1e-6, scale=1)

    d1 <- distance(mfd, X1, X2[, 1])
    d2 <- distance(mfd, X1, matrix(X2[, 1], nrow(X1), ncol(X1)))
    expect_equal(d1, d2) 

    d3 <- distance(mfd, X1[, 1], X2[, 1])
    expect_equivalent(d3, d1[1])
  }
  }
  }
})


test_that('Parametrization and its inverse works', {

  mfdList <- listAvailMfd()
  mfdFinite <- mfdList[vapply(mfdList, is.finiteDim, TRUE)]

  n <- 20
  set.seed(1)
  p <- 2
  # mfdName <- 'Sphere'
  # mfdName <- 'AffInv'
  # mfdName <- 'SO'

  for (p in 2:4) {
  for (mfdName in mfdFinite) {

    mfd <- createM(mfdName)
    dInt <- calcIntDim(mfd, geomPar=p)

    data <- t(rmfd(mfd, n, dInt))

    basis <- basisTan(mfd, data[1, ])
    expect_equal(crossprod(basis), diag(nrow=dInt))
    V <- t(rieLog(mfd, data[1, ], t(data))) # rows are the tangent vectors
    coord <- V %*% basis
    expect_equal(coord %*% t(basis), V)
    if (mfdName == 'Sphere') {
      expect_equal(c(data[1, , drop=FALSE] %*% basis), rep(0, dInt))
    }

    expect_equal(ncol(coord), dInt)
    # The covariance of the coordinates should be full rank

    singVal <- svd(coord)$dInt
    expect_true(all(singVal > 1e-8))

  }
  } 
})


