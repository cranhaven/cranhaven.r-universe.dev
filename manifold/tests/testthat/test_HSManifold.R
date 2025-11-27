# devtools::load_all()
library(testthat)

test_that('methods work for L2', {

  mfd <- structure(1, class='L2')
  p <- 40
  X <- rep(1, p)
  Y <- rep(2, p)
  ambient <- length(X)
  intrinsic <- ambient
  tangent <- ambient

  expect_equal(calcGeomPar(mfd, dimTangen=tangent), p)
  expect_equal(calcIntDim(mfd, dimAmbient=ambient), intrinsic)
  expect_equal(calcIntDim(mfd, dimTangent=tangent), intrinsic)
  expect_equal(calcTanDim(mfd, dimAmbient=ambient), tangent)
  expect_equal(calcTanDim(mfd, dimIntrinsic=intrinsic), tangent)

  expect_equal(rieExp.L2(mfd, X, Y), matrix(X + Y))
  expect_equal(rieExp(mfd, X, Y), matrix(X + Y))
  expect_equal(rieLog.L2(mfd, X, Y), matrix(Y - X))
  expect_equal(rieLog(mfd, X, Y), matrix(Y - X))

  expect_equal(metric.L2(mfd, U=X, V=Y), sum(X * Y) / (p - 1))
  expect_equal(metric(mfd, U=X, V=Y), sum(X * Y) / (p - 1))
  expect_equal(norm.L2(mfd, U=X - Y), distance.L2(mfd, X, Y))
  expect_equal(norm(mfd, U=X - Y), distance(mfd, X, Y))
  expect_equal(distance.L2(mfd, X, Y), sqrt(sum((X - Y)^2) / (p - 1)))
  expect_equal(distance(mfd, X, Y), sqrt(sum((X - Y)^2) / (p - 1)))
  expect_equal(project.L2(mfd, cbind(X, Y)), cbind(X, Y))
  expect_equal(project(mfd, cbind(X, Y)), cbind(X, Y))
  expect_equal(projectTangent.L2(mfd, X, cbind(X, Y)), cbind(X, Y))
  expect_equal(projectTangent(mfd, X, cbind(X, Y)), cbind(X, Y))
})
