
#' SPC_AAM
#' @description A single linear model with minimization on absolute area.
#' @details  This function is to process phase error correction through a single linear model with minimization on absolute area, followed by
#'    polynomial baseline correction if necessary
#' @param specdat A complex number vector of observed frequency domain data
#' @param withBC A logical parameter that enables/disables baseline correction after baseline correction
#' @return A numeric vector of phase corrected absorption spectrum
#' @concept phase correction
#' @author Aixiang Jiang
#' @references
#' de Brouwer, H. (2009). Evaluation of algorithms for automated phase correction of NMR spectra. J Magn Reson, 201, 230-238.
#'
#' Dzakula, Z. (2000). Phase angle measurement from peak areas (PAMPAS). J Magn Reson, 146, 20-32.
#'
#' Liland KH, Almøy T, Mevik B (2010), Optimal Choice of Baseline
#' Correction for Multivariate Calibration of Spectra, Applied Spectroscopy 64, pp. 1007-1016.
#'
#' @import baseline
#'
#' @examples
#' data("fdat")
#' spc_aam_phased1 <- SPC_AAM(fdat$frequency_domain)

#' @export


SPC_AAM = function (specdat, withBC = TRUE){

  hdat=cbind(Re(specdat), Im(specdat))

  pspec=hdat[,1]**2+hdat[,2]**2
  maxi=which.max(pspec)
  ph0Initial = -atan2(hdat[maxi,2],hdat[maxi,1])
  ph1Initial=0.005

  #### get optimized parameters of ph0 and ph1

  optimRes=stats::optim(par=c(ph0Initial,ph1Initial),fn=absArea, specDat=hdat)
  bestPh=optimRes$par

  nn=dim(hdat)[1]
  angles=bestPh[1]+bestPh[2]*c(1:nn)/nn

  dat3col=cbind(hdat, angles)
  phasedDat=t(apply(dat3col, 1, phaseCorr2))

  phasedAll = phasedDat[,1]

  if(withBC == TRUE){
    tryBL=myBaseline(phasedAll,bsDf=5, BL_method="modpolyfit")
    phasedAll = as.numeric(tryBL)
  }

  return(phasedAll)

}


