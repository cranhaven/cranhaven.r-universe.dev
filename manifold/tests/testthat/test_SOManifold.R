# devtools::load_all()
library(testthat)


test_that('Helper functions for SO(n) works', {
  
  # isSkewSym
  expect_true(isSkewSym(0))
  expect_false(isSkewSym(1))
  expect_true(isSkewSym(matrix(c(0, 1, -1, 0), 2, 2)))
  expect_false(isSkewSym(matrix(c(0, 1, 1, 0), 2, 2)))
  expect_true(isSkewSym(0.01, tol=0.1))
  expect_false(isSkewSym(0.1, tol=0.01))

  # isOrth
  expect_true(isOrth(1))
  expect_false(isOrth(0))
  expect_true(isOrth(diag(2)))
  expect_true(isOrth(diag(c(1, -1))))
  expect_false(isOrth(diag(2) + 0.1))

  # isSO
  expect_true(isSO(diag(2)))
  expect_false(isSO(diag(c(1, -1))))

  # NearestOrth
  set.seed(1)
  d <- 3
  A <- matrix(rnorm(d^2), d, d)
  O1 <- NearestOrth(A)
  expect_true(isOrth(O1))

  # NearestSO
  set.seed(1)
  d <- 3
  A <- matrix(rnorm(d^2), d, d)
  B <- diag(d)
  SO1 <- project.SO(p=cbind(c(A), c(B)))
  expect_true(isSO(matrix(SO1[, 1], d, d)))
  expect_equal(SO1[, 2], c(B))

  # MakeSkewSym
  set.seed(1)
  v <- c(1, 2, 3) / 10
  A <- matrix(c(0, -v[1], -v[2],
                v[1], 0, -v[3],
                v[2], v[3], 0), 3, 3, byrow=TRUE)
  SSv <- MakeSkewSym(v)

  expect_equal(SSv, A)
  expect_equal(SSv + t(SSv), matrix(0, 3, 3))

  m <- 5
  pts <- seq(0, 1, length.out=m)
  v0 <- function(x) rep(0, length(x))
  v1 <- function(x) x * 2 
  v2 <- function(x) exp(- (x * 2 - 1/4)^2 * 3) - exp(- 3 / 16)
  F1 <- MakeSkewSymFunc(list(v1))
  F2 <- MakeSkewSymFunc(list(v0, v1, v2))
  Fpts1 <- sapply(F1, function(f) f(pts))
  Fpts2 <- sapply(F2, function(f) f(pts))
  Fmat1 <- lapply(seq_len(m), function(i) matrix(Fpts1[i, ], 2, 2))
  Fmat2 <- lapply(seq_len(m), function(i) matrix(Fpts2[i, ], 3, 3))

  # MakeSkewSymFunc
  expect_equal(sapply(Fmat1, `[`, 2, 1), v1(pts))
  expect_equal(sapply(Fmat1, `[`, 1, 2), -v1(pts))
  expect_equal(sapply(Fmat2, `[`, 3, 1), v1(pts))
  expect_equal(sapply(Fmat2, `[`, 1, 3), -v1(pts))
  expect_equal(sapply(Fmat2, `[`, 3, 2), v2(pts))
  expect_equal(sapply(Fmat2, `[`, 2, 3), -v2(pts))
  # Output is skew symmetric
  expect_true(all(sapply(Fmat1, isSkewSym)))
  expect_true(all(sapply(Fmat2, isSkewSym)))

  R1 <- MakeSOMat(v)
  # R2 <- MakeSOMat(c(0, 0, 0))
  R2 <- MakeSOMat(rep(0, 3))
  R3 <- MakeSOMat(rnorm(3))

  # MakeSOMat
  expect_equal(R1, ExpM(A)) # Make SO(n) matrix works
  expect_equal(R2, diag(3))
  expect_equal(crossprod(MakeSOMat(rnorm(6))), diag(4)) # orthogonal
  expect_equal(crossprod(MakeSOMat(rnorm(10))), diag(5)) # orthonormal

  # distSO
  # expect_equal(distSO(matrix(R1), matrix(R3)), 
  #              rotations::rot.dist(rotations::as.SO3(R1), 
  #                                  rotations::as.SO3(R3), 
  #                                  method='intrinsic'))
  expect_equal(distSO(matrix(R2), matrix(R2)), 0)
  # A <- crossprod(R1)
  # LogM(A, 'Eigen')
})


test_that('rieLog.SO, rieExp.SO, distance.SO works', {
  
  mfd <- structure(list(), class='SO')
  x <- c(1, 2, 1) / 5         
  y <- c(-2, 1, -1) / 5
  X <- MakeSOMat(x)
  Y <- MakeSOMat(y)

  # Example from matlab
  z <- matrix(c(1, 1, 0, 0, 0, 2, 0, 0, -1), 3, 3)
  Z <- matrix(c(2.718281828459046, 1.718281828459045, 1.086161269630488, 0, 1, 1.264241117657115, 0, 0, 0.367879441171442), 3, 3)
  A <- cbind(as.numeric(X), as.numeric(Y))

  w <- matrix(c(0, 1, 0, -1, 0, 2, 0, -2, 0) / 5, 3, 3, byrow=TRUE)
  W <- matrix(c(0.980331119030009, 0.193399683419916, 0.039337761939982, -0.193399683419915, 0.901655595150045, 0.386799366839831, 0.039337761939982, -0.386799366839831, 0.921324476120036), 3, 3, byrow=TRUE)

  expect_equal(ExpM(z), Z)
  # expect_equal(ExpM_old(z), Z)
  expect_equal(LogM(Z), z)
  # expect_equal(LogM_old(Z), z)
  expect_equal(ExpM(w), W)
  # expect_equal(ExpM_old(w), W)
  expect_equal(ExpMSO3(w), W)
  expect_equal(LogM(W), w)
  # expect_equal(LogM_old(W), w)
  expect_equal(LogMSO3(W), w)
  expect_equal(ExpMSO3(matrix(0, 3, 3)), diag(3))

  # An SPD example
  Z <- cov(matrix(rnorm(200), 20, 10))
  expect_equal(LogMSPD(Z), LogM(Z))
  # expect_equal(ExpM_old(LogMSPD(Z)), Z)

  # microbenchmark::microbenchmark(
    # armaSPD = LogMSPD(Z), 
    # arma = LogM(Z),
    # old = LogM_old(Z),
    # times=100
    # )
  # Z1 <- Z
  # Z1[1, 2] <- Z1[1, 2] + 1e-5
  # microbenchmark::microbenchmark(
    # armaSPD = ExpM(Z), 
    # arma = ExpM(Z1),
    # old = ExpM_old(Z),
    # times=1000
    # )
 
  expect_equal(LogM(X), LogMSO3(X))
  expect_equal(LogM(Y), LogMSO3(Y))
  expect_equal(rieLog.SO(mfd, as.numeric(X), as.numeric(X)), 
               matrix(0, 3, 1))
  expect_equal(c(rieLog.SO(mfd, as.numeric(diag(3)), as.numeric(X))), 
               matrix(LogM(X), ncol=1)[lower.tri(diag(3))])
  expect_equal(rieLog.SO(mfd, as.numeric(diag(3)), A), unname(cbind(x, y)))

  v1 <- c(-1, -0.5, -1)
  expect_equal(rieExp.SO(V=as.numeric(v1)), 
               matrix(ExpM(MakeSkewSym(v1)), ncol=1))
  expect_equal(rieExp.SO(V=cbind(as.numeric(v1), as.numeric(v1)))[, 1, drop=FALSE], 
               matrix(ExpM(MakeSkewSym(v1)), ncol=1))

  expect_equal(rieExp.SO(V=rieLog.SO(X=A)), A)
  expect_equal(rieExp.SO(mfd, as.numeric(X), rieLog.SO(mfd, as.numeric(X), A)), A)
  expect_equal(rieLog.SO(X=rieExp.SO(V=rieLog.SO(X=A))), unname(cbind(x, y)))

  expect_equal(rieExp(mfd, V=as.numeric(w[lower.tri(w)])), rieExp.SO(V=as.numeric(w[lower.tri(w)])))
  expect_equal(rieLog(mfd, X=as.numeric(W)), rieLog.SO(X=as.numeric(W)))

  set.seed(1)
  for (d in 2:3) {
    p0 <- if (d == 2) 1 else if (d == 3) 3
  for (i in 1:10) {
    mu <- rieExp.SO(V=rnorm(p0))
    Z <- matrix(rieExp.SO(p=mu, V=rnorm(p0)), d, d)
    expect_true(isSO(Z))
    V <- rieLog.SO(p=mu, X=as.numeric(Z))
    expect_equal(nrow(V), p0)
  }
  }

  # distance.SO
  expect_equal(distance.SO(mfd, as.numeric(X), as.numeric(X)), 0)
  expect_equal(distance.SO(mfd, as.numeric(X), as.numeric(X)), 0)
  expect_equal(distance.SO(mfd, as.numeric(X), as.numeric(diag(3)))^2, sum(x^2))
  expect_equal(distance.SO(mfd, as.numeric(X), as.numeric(Y))^2, 
               sum(rieLog.SO(p=as.numeric(X), X=as.numeric(Y))^2))
})


test_that('calcIntDim.SO, calcGeomPar.SO, calcTanDim.SO work', {

  mfd <- structure(1, class='SO')
  for (n in 2:4) {
    X <- diag(n)
    ambient <- length(X)
    intrinsic <- n * (n - 1) / 2
    tangent <- intrinsic

    expect_equal(calcGeomPar(mfd, dimTangent=tangent), n)
    expect_equal(calcIntDim(mfd, dimAmbient=ambient), intrinsic)
    expect_equal(calcIntDim(mfd, dimTangent=tangent), intrinsic)
    expect_equal(calcTanDim(mfd, dimAmbient=ambient), tangent)
    expect_equal(calcTanDim(mfd, dimIntrinsic=intrinsic), tangent)
    expect_error(calcGeomPar(mfd, 1, 2))
    expect_error(calcIntDim(mfd, 1, 2))
    expect_error(calcTanDim(mfd, 1, 2))
  }
})


test_that('Axis-angle representation works', {
  
  mfd <- createM('SO')
  v1 <- c(0, 0, 0)
  v2 <- c(pi, 0, 0)
  v3 <- Normalize(c(1/3, 1/3, 1/3))

  x1 <- c(rieExp(mfd, V=v1))
  x2 <- rieExp(mfd, V=v2)
  x3 <- rieExp(mfd, V=v3)

  # results
  r1 <- axisAngleRep(mfd, x1)
  r2 <- axisAngleRep(mfd, x2)
  r3 <- axisAngleRep(mfd, x3)

  # expected
  e1 <- c(0, 1, 0, 0)
  e2 <- c(pi, 0, 0, 1)
  e3 <- c(1, v3[1], -v3[2], v3[3])

  expect_equivalent(r1, e1)
  expect_equivalent(r2, e2)
  expect_equivalent(r3, e3)
  expect_equivalent(axisAngleRep(mfd, cbind(x1, x2, x3)), cbind(e1, e2, e3))
})
