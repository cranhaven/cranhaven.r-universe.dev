#' Setup the Input and Output for a Recurrent Neural Network
#'
#' R command to setup the input and output for a Recurrent Neural Network.
#' It is used in
#' the Wiley book \emph{Statistical Learning with Big Dependent Data}
#' by Daniel Peña and Ruey S. Tsay (2021).
#'
#'
#' @param z Input in integer values.
#' @param h Number of lags used as input.
#' @param nfore Data points in the testing subsample.
#'
#' @return A list containing:
#' \itemize{
#' \item Xfit - Predictor in training sample (binary).
#' \item Yfit - Dependent variable in the training sample (binary).
#' \item yp - Dependent variable in testing sample.
#' \item Xp - Predictor in the testing sample (binary).
#' \item X - Predictor in the training sample.
#' \item yfit - Dependent variable in the training sample.
#' \item newX - Predictor in the testing sample.
#' }
#'
#' @importFrom rnn int2bin
#'
#' @examples
#' output <- rnnStream(rnorm(100), h=5, nfore=20)
#' @export
"rnnStream" <- function(z, h=25, nfore=200){

  if(!is.integer(z))z=as.integer(z)
  ist <- h+1
  nT <- length(z)
  y <- z[ist:nT]
  X <- NULL
  for (i in 1:h){
    zz <- z[(ist-i):(nT-i)]
    X <- cbind(X,zz)
  }

  nobe <- length(y)
  orig <- nobe-nfore
  yp<- y[(orig+1):nobe]
  yfit <- y[1:orig]
  s <- NULL
  for (i in 1:h){
    x1 <- rnn::int2bin(X[1:orig,i],length=8)
    s <- c(s,c(x1))
  }
  Xfit <- array(s,dim=c(orig,8,h))
  Yfit <- rnn::int2bin(yfit,length=8)
  Yfit <- array(Yfit,dim=c(orig,8,1))

  s <- NULL
  for (i in 1:h){
    x2 <- rnn::int2bin(X[(orig+1):nobe,i],length=8)
    s <- c(s,c(x2))
  }
  Xp <- array(s,dim=c(nfore,8,h))
  rnnStream <- list(Xfit=Xfit,Yfit=Yfit,yp=yp,Xp=Xp,X=X[1:orig,],
                    yfit=yfit,newX=X[(orig+1):nobe,])
}
