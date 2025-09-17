################################################################################
#                                                                              #
#           DCSmooth Package: Spectral Density Estimation via Buehlmann        #
#                                                                              #
################################################################################

### Nonparametric estimation of variance factor (experimental)

#-------------------Estimation of Autocovariance Function----------------------#

acfMatrix = function(Y)
{
  nX = dim(Y)[1]; nT = dim(Y)[2]
  Y = as.matrix(Y - mean(Y))
  Y_1 = Y[nX:1, ]# top right matrix

  acfMat_diag = acfMatrix_quarter2(Y)
  acfMat_off  = acfMatrix_quarter2(Y_1)

  acfMat_out = matrix(NA, nrow = 2*nX - 1, ncol = 2*nT - 1)
  acfMat_out[nX:1, nT:1] = acfMat_diag
  acfMat_out[nX:1, nT:(2*nT - 1)] = acfMat_off
  acfMat_out[nX:(2*nX - 1), nT:1] = acfMat_off
  acfMat_out[nX:(2*nX - 1), nT:(2*nT - 1)] = acfMat_diag

  return(acfMat_out)
}

#---------------------Estimation of Spectral Density---------------------------#

specDens = function(Y, omega)
{
  nX = dim(as.matrix(Y))[1]; nT = dim(as.matrix(Y))[2]
  Y = Y - mean(Y)
  acfMat = acfMatrix(Y)

  # initial values
  hVec = matrix(NA, 21, 2)
  hVec[1, ] = trunc(0.5*c(nX, nT))

  # global step
  for (g in 2:6)
  {
    hVecInfl = hVec[g - 1, ] / c(nX^(2/21), nT^(2/21))
    hVecInfl = trunc(hVecInfl) + 1
    hVec[g, ] = globalBndwEst(acfMat, hLag = hVecInfl)
    if (all(hVec[g, ] == hVec[g - 1, ])) { break() }
  }

  # local step
  hVecInfl = hVec[g, ] / c(nX^(2/21), nT^(2/21))
  hVecInfl = trunc(hVecInfl) + 1
  hOpt = localBndwEst(acfMat, hVecInfl, omega)

  specDensOut = specDensEst(acfMat, drv = c(0, 0), hLag = hOpt, omega = omega)
  
  return(list(specDens = specDensOut, cf = specDensOut*(2*pi)^2, h = hOpt))
}

specDensEst = function(acfMat, drv, hLag, omega)
{
  nX = dim(as.matrix(acfMat))[1]/2 + 0.5
  nT = dim(as.matrix(acfMat))[2]/2 + 0.5
  Lx = hLag[1]; Lt = hLag[2]; drvX = drv[1]; drvT = drv[2]

  # absolute bandwidths

  # compute matrix of bartlett weights multiplied with "fourier factor"
  w = (1 - (0:Lx)/max(Lx, 1)) %*% t(1 - (0:Lt)/max(Lt, 1)) *
    complex(argument = -(0:Lx) * omega[1]) %*%
    t(complex(argument = - (0:Lt) * omega[2])) *
    matrix((0:Lx)^drvX, Lx + 1, Lt + 1) *
    matrix((0:Lt)^drvT, Lx + 1, Lt + 1, byrow = TRUE)

  if (Lx > 1 & Lt > 1) {
    W = rbind(cbind(w[Lx:1, Lt:1, drop = FALSE], w[Lx:1, 2:Lt, drop = FALSE]),
              cbind(w[2:Lx, Lt:1, drop = FALSE], w[2:Lx, 2:Lt, drop = FALSE]))
  } else if (Lx == 1 & Lt > 1) {
    W = cbind(w[Lx:1, Lt:1, drop = FALSE], w[Lx:1, 2:Lt, drop = FALSE])
  } else if (Lx > 1 & Lt == 1) {
    W = rbind(w[Lx:1, Lt:1, drop = FALSE], w[2:Lx, Lt:1, drop = FALSE])
  } else if (Lx == 1 & Lt == 1) {
    W = 1
  }

  # calculate f. Note that f(-kX, - kT) = f(kX, kT), off diagonal sub matrices
  # are different etc.
  # note that w is (Lx + 1)x(Lt + 1) but the outer values are all zeroes,
  # thus summation over them is not needed.
  fOut = sum(W * acfMat[(nX - Lx + 1):(nX + Lx - 1),
                        (nT - Lt + 1):(nT + Lt - 1)])

  return((2*pi)^(-2) * Re(fOut))
}

#-----------------------Estimation of Bandwidths-------------------------------#

localBndwEst = function(acfMat, hLag, omega)
{
  nX = dim(as.matrix(acfMat))[1]/2 + 0.5
  nT = dim(as.matrix(acfMat))[2]/2 + 0.5
  mu_2w = 4/9

  if(all(hLag > 1)) {
    f00 = specDensEst(acfMat, drv = c(0, 0), hLag = hLag, omega = omega)
    f10 = specDensEst(acfMat, drv = c(1, 0), hLag = hLag, omega = omega)
    f01 = specDensEst(acfMat, drv = c(0, 1), hLag = hLag, omega = omega)

    Lx = (4 * abs(f10^3/f01) * 1/f00^2 * nX*nT/mu_2w)^0.25
    Lt = Lx * abs(f01/f10)
  }
  else if (hLag[2] == 1)
  {
    f00 = specDensEst(acfMat, drv = c(0, 0), hLag = hLag, omega = omega)
    f10 = specDensEst(acfMat, drv = c(1, 0), hLag = hLag, omega = omega)

    Lx = (2 * (f10/f00)^2 * nX*nT/sqrt(mu_2w))^(1/3)
    Lt = 0
  }
  else if (hLag[1] == 1)
  {
    f00 = specDensEst(acfMat, drv = c(0, 0), hLag = hLag, omega = omega)
    f01 = specDensEst(acfMat, drv = c(0, 1), hLag = hLag, omega = omega)

    Lx = 0
    Lt = (2 * (f01/f00)^2 * nX*nT/sqrt(mu_2w))
  }

  hOut = c(trunc(Lx) + 1, trunc(Lt) + 1)
  hOut = pmin(hOut, c(nX - 1, nT - 1))
  return(hOut)
}

globalBndwEst = function(acfMat, hLag)
{
  nX = dim(as.matrix(acfMat))[1]/2 + 0.5
  nT = dim(as.matrix(acfMat))[2]/2 + 0.5
  mu_2w = 4/9

  if(all(hLag > 1))
  {
    F00 = specIntEst00(acfMat)
    F10 = specIntEstDrv(acfMat, drv1 = c(1, 0), drv2 = c(1, 0), hLag = hLag)
    F01 = specIntEstDrv(acfMat, drv1 = c(0, 1), drv2 = c(0, 1), hLag = hLag)
    F1001 = specIntEstDrv(acfMat, drv1 = c(1, 0), drv2 = c(0, 1), hLag = hLag)

    Fx = F10 * (sqrt(F10/F01) + F1001/F01)/F00
    Ft = F01 * (sqrt(F01/F10) + F1001/F10)/F00

    Lx = (2 * Fx * nX*nT/mu_2w)^0.25
    Lt = (2 * Ft * nX*nT/mu_2w)^0.25
  }
  else if (hLag[2] == 1)
  {
    F00 = specIntEst00(acfMat)
    F10 = specIntEstDrv(acfMat, drv1 = c(1, 0), drv2 = c(1, 0), hLag = hLag)

    Fx = F10/F00

    Lx = (2 * Fx * nX*nT/sqrt(mu_2w))^(1/3)
    Lt = 0
  }
  else if (hLag[1] == 1)
  {
    F00 = specIntEst00(acfMat)
    F01 = specIntEstDrv(acfMat, drv1 = c(0, 1), drv2 = c(0, 1), hLag = hLag)

    Ft = F01/F00

    Lx = 0
    Lt = (2 * Ft * nX*nT/mu_2w)^(1/3)
  }

  hLag = c(trunc(Lx) + 1, trunc(Lt) + 1)
  hLag = pmin(hLag, c(nX - 1, nT - 1))
  return(hLag)
}

#----------------------Estimation of Integral over SD--------------------------#

# Function for spectral density
specIntEst00 = function(acfMat)
{
  iOut = sum(acfMat^2)

  return((2*pi)^(-2) * iOut * 0.5)
}

# Function for derivatives of Spectral Density
specIntEstDrv = function(acfMat, drv1, drv2, hLag)
{
  nX = dim(as.matrix(acfMat))[1]/2 + 0.5
  nT = dim(as.matrix(acfMat))[2]/2 + 0.5
  Lx = hLag[1]; Lt = hLag[2]

  # compute matrix of bartlett weights without "fourier factor"
  w = ((1 - (0:Lx)/max(Lx, 1)) %*% t(1 - (0:Lt)/max(Lt, 1)))^2 *
    matrix((0:Lx)^(drv1[1] + drv2[1]), Lx + 1, Lt + 1) *
    matrix((0:Lt)^(drv1[2] + drv2[2]), Lx + 1, Lt + 1, byrow = TRUE)

  if (Lx > 1 & Lt > 1) {
    W = rbind(cbind(w[Lx:1, Lt:1, drop = FALSE], w[Lx:1, 2:Lt, drop = FALSE]),
              cbind(w[2:Lx, Lt:1, drop = FALSE], w[2:Lx, 2:Lt, drop = FALSE]))
  } else if (Lx == 1 & Lt > 1) {
    W = cbind(w[Lx:1, Lt:1, drop = FALSE], w[Lx:1, 2:Lt, drop = FALSE])
  } else if (Lx > 1 & Lt == 1) {
    W = rbind(w[Lx:1, Lt:1, drop = FALSE], w[2:Lx, Lt:1, drop = FALSE])
  } else if (Lx == 1 & Lt == 1) {
    W = 1
  }

  # calculate F. Note that f(-kX, - kT) = f(kX, kT) etc.
  FOut = sum(W * acfMat[(nX - Lx + 1):(nX + Lx - 1),
                        (nT - Lt + 1):(nT + Lt - 1)]^2)

  return((2*pi)^(-2) * FOut)
}