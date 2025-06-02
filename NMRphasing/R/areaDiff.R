#' A function rather aimed at developers
#' @noRd

areaDiff = function(phasePara, specDat) {
  n=dim(specDat)[1]
  phases=phasePara[1]+phasePara[2]*c(1:n)/n

  dat3col=cbind(specDat, phases)
  phasedDat=t(apply(dat3col, 1, phaseCorr2))
  sum(abs(phasedDat[,1]))-sum(phasedDat[,1])
}
