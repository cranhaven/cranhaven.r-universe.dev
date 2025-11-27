# devtools::load_all()
library(testthat)

test_that('calcIntDim.Euclidean, calcGeomPar.Euclidean, calcTanDim.Euclidean work', {

  mfd <- structure(1, class='Euclidean')
  for (n in 1:4) {
    X <- rep(1, n)
    ambient <- length(X)
    intrinsic <- ambient
    tangent <- ambient

    expect_equal(calcGeomPar(mfd, dimTangen=tangent), n)
    expect_equal(calcIntDim(mfd, dimAmbient=ambient), intrinsic)
    expect_equal(calcIntDim(mfd, dimTangent=tangent), intrinsic)
    expect_equal(calcTanDim(mfd, dimAmbient=ambient), tangent)
    expect_equal(calcTanDim(mfd, dimIntrinsic=intrinsic), tangent)
  }
})


