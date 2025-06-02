#' A function rather aimed at developers
#' @noRd

EMP = function (specIn){
  hdat=cbind(Re(specIn), Im(specIn))
  pspec=hdat[,1]**2+hdat[,2]**2
  maxi=which.max(pspec)
  ph0Initial = -atan2(hdat[maxi,2],hdat[maxi,1])
  ph1Initial=0.005

  optimRes=stats::optim(par=c(ph0Initial,ph1Initial),fn=entropyP, specDat=hdat)
  bestPh=optimRes$par

  nn=dim(hdat)[1]
  angles=bestPh[1]+bestPh[2]*c(1:nn)/nn

  dat3col=cbind(hdat, angles)
  phasedDat=t(apply(dat3col, 1, phaseCorr2))

  return(phasedDat[,1])
}
