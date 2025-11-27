# devtools::load_all()
library(testthat)

test_that('calcIntDim.Sphere, calcGeomPar.Sphere, calcTanDim.Sphere work', {

  mfd <- structure(1, class='Sphere')
  for (n in 1:4) {
    X <- rep(1 / sqrt(n + 1), n + 1)
    ambient <- length(X)
    intrinsic <- ambient - 1
    tangent <- ambient

    expect_equal(calcGeomPar(mfd, dimTangen=tangent), n)
    expect_equal(calcIntDim(mfd, dimAmbient=ambient), intrinsic)
    expect_equal(calcIntDim(mfd, dimTangent=tangent), intrinsic)
    expect_equal(calcTanDim(mfd, dimAmbient=ambient), tangent)
    expect_equal(calcTanDim(mfd, dimIntrinsic=intrinsic), tangent)
  }
})


test_that('runifSphere works', {

  set.seed(1)
  n <- 200
  for (dimAmbient in c(1, 5)) {
    X <- runifSphere(n, dimAmbient)
    expect_equal(colMeans(t(X)), rep(0, dimAmbient), scale=1, tolerance=0.1)
  }

})
