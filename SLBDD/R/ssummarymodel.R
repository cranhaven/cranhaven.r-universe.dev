#' Collects All Models Specified by "sarimaSpec"
#'
#' Models specified by "sarimaSpec".
#'
#' @param x T by k data matrix: T data points in rows with each row being data at a given time point,
#' and k time series in columns.
#' @param maxorder Maximum order of ARIMA model \eqn{(p,d,q)} where \eqn{p} is the AR order, \eqn{d} the degree of differencing,
#' and \eqn{q} the MA order. Default value is (3,1,2).
#' @param period Seasonal period. The default is 12.
#' @param criterion Information criterion used for model selection. Either AIC or BIC.
#' Default is "bic".
#' @param method Estimation method. See the arima command in R. Possible values are "CSS-ML", "ML", and "CSS".
#' Default is "CSS".
#'
#' @return A list containing:
#' \itemize{
#'   \item Order - Order of ARIMA model \eqn{(p, d, q, P, D, Q)} of each series. A matrix of (ncol(x),6). The six columns are "p","d","q", "P", "D", "Q".
#'   \item Mean - A logical vector indicating whether each series needs a constant (or mean).
#'   \item M1 - Contains orders the stationary series.
#'   \item M2 - Contains orders of series with (d=1) and (D=0).
#'   \item M3 - Contains orders of  series with (d=2) and (D=0).
#'   \item M4 - Contains orders of series with (d=0) and (D=1).
#'   \item M5 - Contains orders of series with (d=1) and (D=1).
#'   \item M6 - Contains orders of series with (d=2) and (D=1).
#' }
#'
#' @export
#'
#' @examples
#' data(TaiwanAirBox032017)
#' summary <- sSummaryModel(TaiwanAirBox032017[,1:3])
#'
"sSummaryModel" <- function(x, maxorder = c(3,1,2), period = 12, criterion = "bic", method = "CSS"){
  if(!is.matrix(x)) x <- as.matrix(x)
  Order <- NULL; Mean <- NULL
  for (i in 1:ncol(x)){

    m1 <- sarimaSpec(x[,i],maxorder=maxorder,output=FALSE,criterion=criterion,method=method)
    Order <- rbind(Order,m1$order)
    Mean <- c(Mean,m1$include.mean)
  }

  nclass <- rep(0,6)
  k <- ncol(x)
  colnames(Order) <- c("p","d","q","P","D","Q")
  rownames(Order) <- paste("x",1:k,sep="")

  M1 <- M2 <- M3 <- M4 <- M5 <- M6 <- NULL
  ##
  istat <- c(1:k)[sapply(1:k, function(x) Order[x,2]==0 && Order[x,5]==0)]
  nsta <- length(istat)
  if(nsta > 0){
    message("Number of stationary series: ",nsta,"\n")
    nclass[1] <- nsta
    order <- Order[istat,]
    M1 <- as.matrix(order)
    colnames(M1) <- colnames(Order)
    rownames(M1) <- rownames(Order)[istat]
  }else{
    message("All series are non-stationary","\n")
  }

  idiff1 <- c(1:k)[sapply(1:k, function(x) Order[x,2]==1 && Order[x,5]==0)]
  ndiff1 <- length(idiff1)
  if(ndiff1 > 0){
    order <- Order[idiff1,]
    message("Number of (d=1) and (D=0) series: ",ndiff1,"\n")
    M2 <- as.matrix(order)
    colnames(M2) <- colnames(Order)
    rownames(M2) <- rownames(Order)[idiff1]
    nclass[2] <- ndiff1
  }
  #
  idiff2 <- c(1:k)[sapply(1:k, function(x) Order[x,2]==2 && Order[x,5]==0)]
  ndiff2 <- length(idiff2)
  if(ndiff2 > 0){
    order <- Order[idiff2,]
    message("Number of (d=2) and (D=0) series: ",ndiff2,"\n")
    M3 <- as.matrix(order)
    colnames(M3) <- colnames(Order)
    rownames(M3) <- rownames(Order)[idiff2]
    nclass[3] <- ndiff2
  }

  idiff3 <- c(1:k)[sapply(1:k, function(x) Order[x,2]==0 && Order[x,5]==1)]
  ndiff3 <- length(idiff3)
  if(ndiff3 > 0){
    order <- Order[idiff3,]
    order <- as.matrix(order)
    colnames(order) <- c("p","d","q","P","D","Q")
    rownames(order) <- rownames(idiff3)
    message("Number of (d=0) and (D=1) series: ",ndiff3,"\n")
    M4 <- order
    nclass[4] <- ndiff3
  }

  idiff4 <- c(1:k)[sapply(1:k, function(x) Order[x,2]==1 && Order[x,5]==1)]
  ndiff4 <- length(idiff4)
  if(ndiff4 > 0){
    order <- Order[idiff4,]
    order <- as.matrix(order)
    colnames(order) <- c("p","d","q","P","D","Q")
    rownames(order) <- rownames(idiff4)
    message("Number of (d=1) and (D=1) series: ",ndiff4,"\n")
    M5 <- order
    nclass[5] <- ndiff4
  }

  idiff5 <- c(1:k)[sapply(1:k, function(x) Order[x,2]==2 && Order[x,5]==1)]
  ndiff5 <- length(idiff5)
  if(ndiff5 > 0){
    order <- Order[idiff5,]
    order <- as.matrix(order)
    colnames(order) <- c("p","d","q","P","D","Q")
    rownames(order) <- rownames(idiff5)
    message("Number of (d=2) and (D=1) sries: ", ndiff5,"\n")
    M6 <- order
    nclass[6] <- ndiff5
  }

  return(list(order=Order,Mean=Mean,M1=M1,M2=M2,M3=M3,M4=M4,M5=M5,M6=M6,nclass=nclass,data=x))
}
