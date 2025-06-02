#' MPC_DANM
#' @description Multiple linear models that minimize the difference between absolute area and net area.
#' @details This function processes phase error correction through multiple linear models that minimize the difference between absolute area and net area, followed by polynomial baseline correction when necessary.
#' @param specdat A complex number vector of observed frequency domain data
#' @param withBC A logical parameter that enables/disables baseline correction after baseline correction.
#' @return A numeric vector of phase corrected absorption spectrum
#' @concept phase correction
#' @author Aixiang Jiang
#' @references
#'
#' Liland KH, Almøy T, Mevik B (2010), Optimal Choice of Baseline
#' Correction for Multivariate Calibration of Spectra, Applied Spectroscopy 64, pp. 1007-1016.
#' @examples
#' data("fdat")
#' mpc_danm_phased1 <- MPC_DANM(fdat$frequency_domain)

#' @export


MPC_DANM=function(specdat, withBC = TRUE){

  cplxDat=specdat
  pp=(Re(cplxDat))**2+(Im(cplxDat))**2
  mm=sqrt(pp)

  peakIndex=peakSearch(mm)
  k=length(peakIndex)

  peak1=peakIndex[-k]
  peak2=peakIndex[-1]
  peaks=cbind(peak1,peak2)

  #### find valley index
  valleyIndex=apply(peaks,1,FUN=function(x){
    mdat=mm[x[1]:x[2]]
    which.min(mdat)+x[1]-1
  })

  nn=length(specdat)
  valleyIndex=c(1,valleyIndex, nn)

  vv=length(valleyIndex)
  valleyL=valleyIndex[1:(vv-1)]
  valleyL[2:(vv-1)]=valleyL[2:(vv-1)]+1
  valleyR=valleyIndex[2:vv]
  valleys=cbind(valleyL,valleyR)

  phasedComb=apply(valleys,1,FUN=function(x){
    res=DANM(cplxDat[x[1]:x[2]])
  })

  phasedAll=unlist(phasedComb)

  if(withBC == TRUE){
    tryBL=myBaseline(phasedAll,bsDf=5, BL_method="modpolyfit")
    phasedAll = as.numeric(tryBL)
  }

  return(phasedAll)

}














