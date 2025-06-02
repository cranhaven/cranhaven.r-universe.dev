#' A function rather aimed at developers
#' @noRd

phaseCorr2=function(datpoint){
  ### datpoint contain 3 elements:real, imaginary, and phase error correction angle, i.e. the adding angle
  creal=datpoint[1]*cos(datpoint[3])-datpoint[2]*sin(datpoint[3])
  cimag=datpoint[1]*sin(datpoint[3])+datpoint[2]*cos(datpoint[3])
  return(c(creal,cimag))
}
