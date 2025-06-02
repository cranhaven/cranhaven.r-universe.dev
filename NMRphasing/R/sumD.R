#' A function rather aimed at developers
#' @noRd

sumD = function(phasePara, specDat) {

  n=dim(specDat)[1]
  phases=phasePara[1]+phasePara[2]*c(1:n)/n

  dat3col=cbind(specDat, phases)
  phasedDat=t(apply(dat3col, 1, phaseCorr2)) ### output is a two column matrix: the phased real and the phased imaginary of freq data
  return(sum(phasedDat[,2]))
}
