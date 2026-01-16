#' Summary Outliers
#'
#' Use the command "tso" of the R package "tsoutliers" to identify outliers
#' for each individual time series.
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param type A character vector indicating the type of outlier to be considered by the
#' detection procedure. See 'types' in tso function.
#' @param tsmethod The framework for time series modeling. Default is "arima". See 'tsmethod' in tso function.
#' @param args.tsmethod An optional list containing arguments to be passed to the function invoking the
#' method selected in tsmethod. See 'args.tsmethod' in tso function. Default value is c(5,0,0).
#'
#' @return A list containing:
#' \itemize{
#' \item Otable - Summary of various types of outliers detected.
#' \item x.cleaned - Outlier-adjusted data.
#' \item xadja - T-dimensional vector containing the number of time series that have outlier
#' at a given time point.
#' }
#'
#' @importFrom tsoutliers tso
#'
#' @examples
#' data(TaiwanAirBox032017)
#' output <- SummaryOutliers(TaiwanAirBox032017[1:50,1:3])
#' @export
"SummaryOutliers" <- function(x, type = c("LS","AO","TC"), tsmethod = "arima",
                              args.tsmethod = list(order=c(5,0,0))){

  x <- as.matrix(x)
  k <- ncol(x)
  nT <- nrow(x)
  noutliers <- NULL
  nseries <- rep(0,nT)
  xadj <- NULL
  Otable <- matrix(0,k,length(type)+1)
  colnames(Otable) <- c("Series",type)
  ##
  for (i in 1:k){
    xt <- as.ts(x[,i])
    mm <- tsoutliers::tso(xt,tsmethod=tsmethod,args.tsmethod=args.tsmethod)
    if(is.null(mm$outliers)){
      noutliers <- c(noutliers,0)
      xadj <- cbind(xadj, xt)
    }else{
      noutliers <- c(noutliers,nrow(mm$outliers))
      idx <- mm$times
      nseries[idx] <- nseries[idx]+1
      xadj <- cbind(xadj,mm$yadj)
      kk <- nrow(mm$outliers)
      Otable[i,1] <- i
      for (j in 1:length(type)){
        jdx <- c(1:kk)[mm$outliers$type==type[j]]
        Otable[i,(j+1)] = length(jdx)
      }
    }

  }

  message("Summary of Detected Outliers: ","\n")

  return(list(Otable=Otable, nseries=nseries, xadj=xadj))
}
