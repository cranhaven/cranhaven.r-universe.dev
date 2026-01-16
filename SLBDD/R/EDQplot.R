#' Plot the Observed Time Series and Selected EDQs (Empirical Dynamic Quantiles)
#'
#' Plot the observed time series and selected empirical dynamic quantiles (EDQs) computed as in Peña, Tsay and Zamar (2019).
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param prob Probability, the quantile series of which is to be computed.
#' Default values are 0.05, 0.5, 0.95.
#' @param h Number of time series used in the algorithm. Default value is 30.
#' @param loc Locations of the EDQ. If loc is not null, then prob is not used.
#' @param color Colors for plotting the EDQ. Default is "yellow", "red", and "green".
#'
#' @return The observed time series plot with the selected EDQs.
#'
#' @export
#'
#' @examples
#' data(TaiwanAirBox032017)
#' edqplot(TaiwanAirBox032017[1:100,1:25])
#'
#' @references Peña, D. Tsay, R. and Zamar, R. (2019). Empirical Dynamic Quantiles for
#' Visualization of High-Dimensional Time Series, \emph{Technometrics}, 61:4, 429-444.
#'
"edqplot" <- function(x, prob = c(0.05,0.5,0.95), h = 30, loc = NULL,
                      color = c("yellow","red","green")){
  if(!is.matrix(x))x <- as.matrix(x)
  np <- length(prob)
  if(is.null(loc)){
    idx <- NULL
    for (i in 1:np){
      ii <- edqts(x,p=prob[i],h=h)
      idx <- c(idx,ii)
    }
  }else{idx <- loc}
  ts.plot(x, main="EDQ(0.05,0.5,0.95)")
  nc <- length(color)
  for (i in 1:np){
    ii <- idx[i]
    if(i <= nc) {
      j <- i}else{ j <- i%%nc
      if(j==0)j <- nc
      }
    lines(x[,ii],col=color[j],lwd=2)
  }
}
