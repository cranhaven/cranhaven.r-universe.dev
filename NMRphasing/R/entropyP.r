#' A function rather aimed at developers
#' @noRd
#'
entropyP = function(phasePara, specDat) {

  n=dim(specDat)[1]
  phases=phasePara[1]+phasePara[2]*c(1:n)/n

  dat3col=cbind(specDat, phases)
  phasedDat=t(apply(dat3col, 1, phaseCorr2))
  absA=abs(phasedDat[,1])
  entro=absA*log(absA)
  negs=which(phasedDat[,1]<0)
  negs=phasedDat[negs,1]
  pp=sum(negs**2)
  return(-sum(entro)+pp)

}
