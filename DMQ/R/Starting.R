
Starting <- function(vY) {

  dBeta  = 0.95
  dAlpha = 0.05
  dGamma = 0.10
  dPhi   = 0.94

  vPn = c(beta = dBeta,
          alpha = dAlpha,
          phi = dPhi,
          gamma = dGamma)

  return(vPn)
}
