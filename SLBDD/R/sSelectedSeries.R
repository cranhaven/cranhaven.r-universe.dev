#' Selected Seasonal Time Series
#'
#' To be used after the command "sSummaryModel". The input "M" must be output from "sSummaryModel".
#' Selected seasonal time series of a given order \eqn{(p,d,q)*(P, D, Q)}.
#'
#' @param M Matrix that is an output from "sSummaryModel" command, that is, M1, M2, M3, M4, M5, or M6.
#' @param order order of ARIMA model \eqn{(p,d,q)*(P, D, Q)}. Default values is (0, 1, 1, 0, 1, 1).
#'
#' @return A list with the series names and count.
#'
#' @export
#'
#' @examples
#' data(TaiwanAirBox032017)
#' outputSummaryModel <- sSummaryModel(TaiwanAirBox032017[,1:3])
#' sSelectedSeries(outputSummaryModel$M1)
#'
"sSelectedSeries" <- function(M, order = c(0,1,1,0,1,1)){

  Series <- NULL
  icnt <- 0

  if(!is.null(M)){
    if(!is.matrix(M)) M <- as.matrix(M)
    nseries <- nrow(M)
    Order <- M
    if(nseries <= 0){
      message("No series of the selected order","\n")
    }else{
      k <- nrow(M)
      xname <- rownames(Order)
      icnt <- 0
      Series <- NULL
      for (i in 1:k){
        if((Order[i,1]==order[1]) && (Order[i,2]==order[2]) && (Order[i,3]==order[3]) &&
           (Order[i,4]==order[4]) && (Order[i,5]==order[5]) && (Order[i,6]==order[6])){
          icnt <- icnt+1
          Series <- c(Series,xname[i])
        }
      }

      if(is.null(Series)){
        message("No series of the seleted order exist!","\n")
      }else{
        message("Order specified (p,d,q,P,D,Q): ",paste(order, collapse = " "),"\n")
        message("Names of selected series: ",paste(Series, collapse = " "),"\n")
      }
    }
  }else{
    message("No series of the seleted order exist!","\n")
  }

  sSelectedSeries <- list(Names=Series,count=icnt)
}
