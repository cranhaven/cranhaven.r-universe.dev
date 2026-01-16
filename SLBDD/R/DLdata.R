#' Create an input data matrix for a Deep learning program that uses time series data.
#'
#' R command to setup the training and forecasting data for deep learning.
#'
#' @param x T by k data matrix: T data points in rows and k time series in columns.
#' @param forerate Fraction of sample size to form the forecasting (or testing) sample.
#' @param locY Locator for the dependent variable.
#' @param lag Number of lags to be used to form predictors.
#'
#' @return A list containing:
#' \itemize{
#' \item Xtrain - Standardized predictors matrix.
#' \item Ytrain - Dependent variable in training sample.
#' \item Xtest - Predictor in testing sample, standardized according to X_train.
#' \item Ytest - Dependent variable in the testing sample.
#' \item nfore - Number of forecasts.
#' }
#'
#' @export
#'
#' @examples
#' x <- matrix(rnorm(7000), nrow=700, ncol=100)
#' m1 <- DLdata(x, forerate=c(200/nrow(x)), lag=6, locY=6)
"DLdata" <- function(x, forerate = 0.2, locY = 1, lag = 1){
  da <- x
  if(!is.matrix(da))da <- as.matrix(da)
  k <- ncol(da)
  nT <- nrow(da)
  name <- colnames(da)
  if(is.null(name))name <- paste("V",1:k,sep="")
  nfore <- floor(forerate*nT)
  nobe <- nT-lag
  ist <- lag+1
  if(nfore < nobe){
    ntrain <- nobe-nfore
  }else{nfore=0
  ntrain=nobe
  cat("Not enough data for testing","\n")
  }
  Xtrain = Ytrain = Xtest = Ytest <- NULL
  if(nfore > 0){
    Y <- da[ist:nT,locY]
    X <- NULL
    na <- NULL
    nb <- paste(name,"L",sep="")
    for (i in 1:lag){
      X <- cbind(X,da[(ist-i):(nT-i),])
      na <- c(na,paste(nb,1:k,sep=""))
    }
    colnames(X) <- na
    Xtrain <- X[1:ntrain,]
    Ytrain <- Y[1:ntrain]
    Xtest <- X[(ntrain+1):nobe,]
    Ytest <- Y[(ntrain+1):nobe]

    Xtrain <- scale(Xtrain,center=TRUE,scale=TRUE)
    col_means_train <- attr(Xtrain,"scaled:center")
    col_sds_train <- attr(Xtrain,"scaled:scale")
    Xtest <- scale(Xtest,center=col_means_train,scale=col_sds_train)
  }
  list(Xtrain=Xtrain,Ytrain=Ytrain,Xtest=Xtest,Ytest=Ytest,nfore=nfore)
}
