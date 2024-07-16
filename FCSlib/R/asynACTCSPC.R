#' @title Asynchronous Autocorrelation of Time-Correlated Single-Photon Counting
#' 
#' @description Calculates the auto-correlation of the Macrotime data, returning a correlation function.
#' @aliases asynACTCSPC
#' @usage asynACTCSPC(macro, n = 5, B = 10)
#' @param macro A numeric vector containig a Macrotime Data.
#' @param n numeric parameter that represents the number of layers of the cascade.
#' @param B numeric parameter that represents the number of values in every layer of the cascade.
#' @details This function creates list of tau's with a length of n*B, this list is used to perform the correlation of the data.
#' 
#' @export
#' @return A numeric vector G containing either the autocorrelation for the input vector macro, with a length of n*B.
#' @references wahl, M., Gregor, I., Patting, M. & Enderlein, J. Fast calculation of fluorescence correlation data with
#' asynchronous time-correlated single-photon counting. Opt. Express 11, 3583–3591 (2003).
#' @author Raúl Pinto Cámara, José Damián Martínez Reyes.
#' 
#' @seealso \code{\link{readFileSPC}}
#' 
#' @examples
#' \donttest{
#' spcData <- readFileSPC("atto532_atto655_m1.spc")
#' asynCorrData <- asynACTCSPC(macro = spcData$MacroTime)
#' }

asynACTCSPC <- function(macro, n = 5, B = 10){
  tauList<-NULL
  for(j in 1:(n*B)){
    if(j==1){
      tauList<-c(tauList,1)
    }else{
      cascval<-tauList[length(tauList)] + 2**(floor((j-1)/B))
      tauList<-c(tauList,cascval)
    }
  }
  CorrTotal <- NULL
  len <- length(macro)
  for(tau in tauList){
    m1 <- m2 <- 0
    i <- j <- 1
    macro2 <- macro + tau
    corrAct <- 0
    while(m1 <= len  && m2 <= len){
      m1 <- m1 + 1
      while(macro[m1] < macro2[j] && m1 <= len){
        m1 <- m1 + 1
      }
      if(m1 > len){
        break
      }
      i <- m1
      if(macro[m1] == macro2[j]){
        corrAct <- corrAct + 1
      }
      m2 <- m2 + 1
      while(macro2[m2] < macro[i] && m2 <= len){
        m2 <- m2 + 1
      }
      if(m2 > len){
        break
      }
      j <- m2
      if(macro2[m2] == macro[i]){
        corrAct <- corrAct + 1
      }
    }
    CorrTotal <- c(CorrTotal, corrAct)
  }
  return(CorrTotal)
}