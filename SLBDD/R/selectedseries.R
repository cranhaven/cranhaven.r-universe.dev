#' Identified the Series with the Given Order
#'
#' To be used after the command "SummaryModel". The input "M" must be an output from "SummaryModel".
#' Selected time series of a given order \eqn{(p,d,q)}.
#'
#' @param M Matrix that is an output from "SummaryModel" command, that is, M1, M2 or M3.
#' @param order Specification of the non-seasonal part of the ARIMA model:
#' the three integer components \eqn{(p,d,q)} are \eqn{p} the AR order, \eqn{d} the degree of differencing,
#' and \eqn{q} the MA order. Default value is c(1, 0, 1).
#'
#' @return The number of series with the given order and the names of the resulting series.
#'
#' @export
#'
#' @examples
#' data(TaiwanAirBox032017)
#' outputSummaryModel <- SummaryModel(TaiwanAirBox032017[,1:3])
#' SelectedSeries(outputSummaryModel$M1, order = c(2,0,0))
#'
"SelectedSeries" <- function(M, order = c(1,0,1)){

  if(is.vector(M)){ M <- t(as.matrix(M))}

  if(is.null(M)){
    message("No series of the given difference d","\n")
  }else{

    k <- nrow(M)
    idx <- c(1:k)[M[,1]==order[1]]
    icnt <- length(idx)
    if(icnt==0){
      message("No series of the given order","\n")
    }else{
      M1 <- M[idx,]
      if(icnt==1){M1 <- matrix(c(M1),1,3); rownames(M1) <- rownames(M)[idx]}
      jdx <- c(1:nrow(M1))[M1[,2]==order[2]]
      jcnt <- length(jdx)
      if(jcnt==0){
        message("No series of the given order","\n")
      }else{
        M2 <- M1[jdx,]
        if(jcnt==1){M2 <- matrix(c(M2),1,3); rownames(M2) <- rownames(M1)[jdx]}
        kdx <- c(1:nrow(M2))[M2[,3]==order[3]]
        kcnt <- length(kdx)
        if(kcnt==0){
          message("No series of the given order","\n")
        }else{
          M3 <- M2[kdx,]
          if(kcnt==1){M3 <- matrix(c(M3),1,3); rownames(M3) <- rownames(M2)[kdx]}
          ser <- rownames(M3)
          nseries <- nrow(M3)
          message("There are ",nseries," of order: ",paste(order, collapse = " "),"\n")
          message("Names of the series: ","\n")
          return(ser)
        }
      }
    }
  }
}
