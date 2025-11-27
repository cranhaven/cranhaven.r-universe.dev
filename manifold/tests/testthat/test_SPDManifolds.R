# devtools::load_all()
library(testthat)

test_that('Helper functions for SPD(n) works', {
  
  # isSPD
  expect_true(isSPD(1))
  expect_true(isSPD(1e-10))
  expect_false(isSPD(0))

  # isSO
  expect_true(isSO(diag(2)))
  expect_false(isSO(diag(c(1, -1))))

  # NearestSPSD
  set.seed(1)
  d <- 3
  A <- matrix(rnorm(d^2), d, d)
  A <- A + t(A)
  B <- NearestSPSD(A)
  expect_true(isSPSD(B))
  expect_true(isSPD(B + diag(1e-13, d)))

  # MakeSym
  set.seed(1)
  v <- c(1, 2, 3) / 10
  A <- matrix(c(v[1], v[2], v[2], v[3]), 2, 2, byrow=TRUE)
  A1 <- matrix(c(v[1] * sqrt(2), v[2], v[2], v[3] * sqrt(2)), 2, 2, byrow=TRUE)
  Sv <- MakeSym(lowerTri=v)
  expect_equal(Sv, A)
  expect_true(isSymmetric(Sv))

  Sv1 <- MakeSym(lowerTri=v, doubleDiag=TRUE)
  expect_equal(Sv1, A1)

  # TransposeAsMatrix
  n <- 3
  T <- TransposeAsMatrix(n)
  expect_true(all(sapply(seq_len(10), function(i) {
    A <- matrix(rnorm(n^2), n, n)
    identical(t(A), matrix(T %*% c(A), n, n))
  })))
})


test_that('project.SPD and projectTangent.SPD works', {
  n <- 3
  A <- matrix(rnorm(n^2), n, n)
  A <- crossprod(A)
  expect_equal(project.SPD(NULL, c(A)), matrix(A))

  B <- MakeSym(matrix(rnorm(n^2), n, n))
  expect_equal(projectTangent.SPD(X=matrix(B)), matrix(B))

  C <- B
  C[2] <- C[2] + 1e-5
  expect_equal(projectTangent.SPD(X=matrix(C)), matrix((C + t(C)) / 2))
})


test_that('calcIntDim.SPD, calcGeomPar.SPD, calcTanDim.SPD work', {

  mfd <- structure(1, class='SPD')
  for (n in 2:4) {
    X <- diag(n)
    ambient <- length(X)
    intrinsic <- n * (n + 1) / 2
    tangent <- ambient

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


test_that('rieLog.LogEu, rieExp.LogEu, distance.LogEu, metric.LogEu, norm.LogEu work', {
  
  for (n in 1:3) {
    mfd <- structure(list(), class='SPD')
    A <- matrix(rnorm(n ^ 2), n, n)
    O <- eigen(crossprod(A))$vectors

    ## rieLog.LogEu
    # scalar
    expect_equal(rieLog.LogEu(X=1), matrix(0))
    expect_equal(rieLog.LogEu(p=2, X=2), matrix(0))
    expect_equal(rieLog.LogEu(X=exp(1)), matrix(1))

    # matrix
    expect_equal(rieLog.LogEu(X=c(diag(n))), matrix(diag(0, n)))
    expect_equal(rieLog.LogEu(X=cbind(c(diag(n)), c(diag(n) * 2))), 
                 cbind(c(diag(0, n)), c(diag(log(2), n))))
    expect_equal(rieLog.LogEu(p=c(diag(2, n)), X=c(diag(2, n))), matrix(diag(0, n)))

    v <- exp(rnorm(n))
    expect_equal(rieLog.LogEu(X=c(O %*% diag(v, n) %*% t(O))), matrix(O %*% diag(log(v), n) %*% t(O)))

    ## rieExp.LogEu
    # scalar
    expect_equal(rieExp.LogEu(V=0), matrix(1))
    expect_equal(rieExp.LogEu(p=exp(-2), V=2), matrix(1))

    # matrix
    expect_equal(rieExp.LogEu(V=c(diag(0, n))), matrix(diag(1, n)))
    expect_equal(rieExp.LogEu(p=c(diag(2, n)), V=c(diag(-log(2), n))), 
                 matrix(diag(1, n)))
    expect_equal(rieExp.LogEu(V=c(O %*% diag(log(v), n) %*% t(O))), 
                 matrix(O %*% diag(v, n) %*% t(O)))

    ## distance.LogEu
    # scalar
    expect_equal(distance.LogEu(X=1, Y=1), 0)
    expect_equal(distance.LogEu(X=1, Y=2), log(2) - log(1))
    expect_equal(distance.LogEu(X=1, Y=2, assumeLogRep = TRUE), 1)

    # matrix
    M1 <- O %*% diag(v, n) %*% t(O)
    M2 <- O %*% diag(2 * v, n) %*% t(O)
    M3 <- diag(2, n)
    M4 <- diag(1, n)
    d34 <- sqrt(sum((log(diag(M3)) - log(diag(M4)))^2))
    expect_equal(distance.LogEu(X=cbind(c(M1), c(M2)), Y=cbind(c(M2), c(M1))), 
                 rep(log(2) * sqrt(n), 2))
    expect_equal(distance.LogEu(X=cbind(c(M3), c(M4)), Y=cbind(c(M4), c(M3))), 
                 rep(d34, 2))

    ## metric.LogEu
    U <- matrix(1:2, nrow=4, ncol=2, byrow=TRUE)
    V <- matrix(3:4, nrow=4, ncol=2, byrow=TRUE)
    expect_equal(metric.LogEu(mfd, U=U, V=V), c(12, 32))

    u <- -1
    v <- 1
    expect_equal(metric.LogEu(mfd, U=u, V=v), -1)

    ## norm.LogEu : read code
  }
})


# logmvec, expmvec, SqrtM, affineCenter
test_that('Helper functions for LogEu and AffInv work', {

  # logmvec and expmvec: Look at code
  expect_equal(logmvec(exp(1), 1), matrix(1))
  expect_equal(expmvec(0, 1), matrix(1))

  # SqrtM
  expect_equal(SqrtM(1), matrix(1))
  expect_equal(SqrtM(diag(1, 3)), diag(1, 3))
  expect_equal(SqrtM(diag(2, 3)), diag(sqrt(2), 3))
  a <- SqrtM(matrix(c(41, 12, 12, 34), 2, 2))
  b <- 1/5 * matrix(c(9 + 16 * sqrt(2), -12 + 12 * sqrt(2),
                      -12 + 12 * sqrt(2), 16 + 9 * sqrt(2)), 2, 2)
  expect_equal(a, b)
  expect_equal(SqrtM(diag(0.1, 2), -1), diag(10, 2))

  # affineCenter
  expect_equal(affineCenter(1, 2, d=1), matrix(2))
  expect_equal(affineCenter(c(diag(2)), c(diag(c(3, 2))), d=2), diag(c(3, 2)))
  expect_equal(affineCenter(s2=c(1, 3, 3, 10), S1HalfInv=diag(2), d=2), 
               matrix(c(1, 3, 3, 10), 2, 2))
  expect_equal(affineCenter(s2=c(1, 3, 3, 10), S1HalfInv=diag(1/2, 2), d=2), 
               matrix(c(1, 3, 3, 10) / 4, 2, 2))
})


test_that('rieLog.AffInv, rieExp.AffInv, distance.AffInv, metric.AffInv, norm.AffInv work', {
  
  for (n in c(1, 3, 100)) {
    mfd <- structure(list(), class='SPD')
    set.seed(n)
    A <- matrix(rnorm(n ^ 2), n, n)
    B <- matrix(rnorm(n ^ 2), n, n)
    BSym <- crossprod(B)
    C <- matrix(rnorm(n ^ 2), n, n)
    CSym <- crossprod(C)
    bv <- c(BSym)
    cv <- c(CSym)

    ## rieLog.AffInv
    # scalar
    expect_equal(rieLog.AffInv(p=1, X=1), matrix(0))
    expect_equal(rieLog.AffInv(p=2, X=2), matrix(0))
    expect_equal(rieLog.AffInv(p=1, X=exp(1)), matrix(1))

    # matrix
    expect_equal(cbind(rieLog.AffInv(p=bv, X=cv), rieLog.AffInv(p=cv, X=bv)), 
                 rieLog.AffInv(p=cbind(bv, cv), X=cbind(cv, bv)))
    expect_equal(rieLog.AffInv(p=c(diag(n)), X=c(diag(n))), matrix(diag(0, n)))
    expect_equal(rieLog.AffInv(p=c(diag(n)), X=cbind(c(diag(n)), c(diag(n) * 2))), 
                 cbind(c(diag(0, n)), c(diag(log(2), n))))
    expect_equal(rieLog.AffInv(p=c(diag(2, n)), X=c(diag(2, n))), matrix(diag(0, n)))


    ## rieExp.AffInv
    # scalar
    expect_equal(rieExp.AffInv(p=1, V=0), matrix(1))
    expect_equal(rieExp.AffInv(p=4, V=log(4)), matrix(16))

    # matrix
    expect_equal(rieExp.AffInv(p=c(diag(1, n)), V=c(diag(0, n))), matrix(diag(1, n)))
    expect_equal(rieExp.AffInv(p=c(diag(1/2, n)), V=c(diag(log(2), n))), 
                 matrix(diag(1, n)))
    expect_equal(cbind(rieExp.AffInv(p=bv, V=cv), rieExp.AffInv(p=cv, V=bv)),
                 rieExp.AffInv(p=cbind(bv, cv), V=cbind(cv, bv)))

    ## distance.AffInv : check code
    # scalar
    expect_equal(distance.AffInv(X=1, Y=1), 0)
    expect_equal(distance.AffInv(X=1, Y=4), log(4) - log(1))

    # rieLog.AffInv and rieExp.AffInv are inverse of each other
    expect_equal(cv, c(rieExp.AffInv(p=bv, V=rieLog.AffInv(p=bv, X=cv))))
    expect_equal(unname(cbind(cv, bv)), 
                 cbind(rieExp.AffInv(p=cbind(bv, cv), 
                                  V=rieLog.AffInv(p=cbind(bv, cv), 
                                               X=cbind(cv, bv)))))
    
    L <- rieLog.AffInv(p=bv, X=cv)
    expect_equal(c(L), c(rieLog.AffInv(p=bv, X=rieExp.AffInv(p=bv, V=c(L)))))

    # matrix
    # affine invariant property
    expect_equal(distance.AffInv(X=bv, Y=cv), 
                 distance.AffInv(X=c(A %*% BSym %*% t(A)), Y=c(A %*% CSym %*% t(A))))
    # symmetric
    expect_equal(distance.AffInv(X=bv, Y=cv), 
                 distance.AffInv(X=cv, Y=bv))

    expect_equal(distAffInv11(matrix(bv, n, n), matrix(cv, n, n)), 
                 distAffInv11_2(matrix(bv, n, n), matrix(cv, n, n)))

    expect_equal(distAffInv1m(matrix(bv, n, n), cbind(bv, cv)),
                 c(0, distAffInv11_2(matrix(bv, n, n), matrix(cv, n, n))))

    expect_equal(distance.AffInv(X=bv, Y=cbind(bv, cv)),
                 c(0, distAffInv11_2(matrix(bv, n, n), matrix(cv, n, n))))

    expect_equal(distance.AffInv(X=cbind(bv, cv), Y=cbind(bv, cv)),
                 c(0, 0))


    # microbenchmark::microbenchmark(
      # asymDist = distAffInv11(matrix(bv, n, n), matrix(cv, n, n)),
      # symDist = distAffInv11_2(matrix(bv, n, n), matrix(cv, n, n)), # Use the symmetric version
      # times=10
      # )

    # microbenchmark::microbenchmark(
      # mine = distance.AffInv(X=bv, Y=cv), 
      # joris = pdSpecEst::pdDist(matrix(bv, n, n), matrix(cv, n, n), 'Riemannian'),
      # times = 10
      # )

    # microbenchmark::microbenchmark(
      # mine = LogM_old(matrix(cv, n, n)), 
      # joris = pdSpecEst::Logm(diag(nrow=n), matrix(cv, n, n)),
      # times = 10
      # )


    ## metric.AffInv
    U <- matrix(1:2, nrow=4, ncol=2, byrow=TRUE)
    V <- matrix(3:4, nrow=4, ncol=2, byrow=TRUE)
    expect_equal(metric.AffInv(mfd, U=U, V=V), c(12, 32))

    u <- -1
    v <- 1
    expect_equal(metric.AffInv(mfd, U=u, V=v), -1)

    ## norm.AffInv : read code
  }
})
