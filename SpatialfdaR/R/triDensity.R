triDensity <- function(pts, tri, logIntensFn, nquad=5) {
  #  Given a function defining un-normalized log density values
  #  compute the normalization value and the vector
  #  intDensVec containing probability of
  #  a location appearing within each triangle.
  #  Argument nquad is the numerical quadrature order
  #  for computing the integral of the probability surface
  #  over a triangle.
  ntri = dim(tri)[1]
  intDensityVec = rep(0,ntri)
  for (i in 1:ntri) {
    trii = tri[i,]
    vi = matrix(0,3,2)
    for (k in 1:3) vi[k,] = pts[trii[k],]
    triquadList = triquad(nquad,vi)
    X  = triquadList$X
    Y  = triquadList$Y
    Wx = triquadList$Wx
    Wy = triquadList$Wy
    logIntensMat = logIntensFn(X,Y)
    intDensityVec[i] = t(Wx) %*% exp(logIntensMat) %*% Wy
  }
  C = sum(intDensityVec)
  intDensityVec = intDensityVec/C
  intDensityVec
}
