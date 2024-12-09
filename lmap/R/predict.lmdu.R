#'  The function predict.lmdu makes predictions for a test/validation set
#'  based on a fitted lrmdu model (lmdu with X)
#'
#' @param object An \code{lmdu} object
#' @param newX An N by P matrix with predictor variables for a test/validation set
#' @param newY An N by R matrix with response variables  for a test/validation set
#' @param \dots additional arguments to be passed.
#'
#' @return This function returns an object of the class \code{lpca} with components:
#' \item{Yhat}{Predicted values for the test set}
#' \item{devr}{Estimated prediction deviance for separate responses}
#' \item{devtot}{Estimated prediction deviance for all responses}
#' \item{Brier.r}{Estimated Brier score for separate responses}
#' \item{Brier}{Estimated Brier score for all responses}
#'
#' @examples
#' \dontrun{
#' data(dataExample_lpca)
#' Y = as.matrix(dataExample_lmdu[-c(1:20) , 1:8])
#' X = as.matrix(dataExample_lmdu[-c(1:20) , 9:13])
#' newY = as.matrix(dataExample_lmdu[1:20 , 1:8])
#' newX = as.matrix(dataExample_lmdu[1:20 , 9:13])
#' # supervised
#' output = lmdu(Y = Y, X = X, S = 2)
#' preds = predict(output, newX = newX, newY = newY)
#' }
#'
#' @importFrom stats plogis
#'
#' @export

predict.lmdu = function(object, newX, newY = NULL,...){


  if(is.null(object$X)) stop("The predict function is only implemented for supervised analysis")

  X = scale(newX, center = object$mx, scale = object$sdx)
  U = X %*% object$B
  V = object$V
  D = sqrt(outer(diag(U %*% t(U)), rep(1, nrow(V))) + outer(rep(1, nrow(U)), diag(V %*% t(V))) - 2 * U %*% t(V))
  theta = outer(rep(1, nrow(X)), object$m) - D
  Yhat = plogis(theta)

  if(!is.null(newY)){
    Q = 2 * newY - 1
    devr <- -2 * colSums(log(plogis(Q * theta)))
    devtot <- sum(devr)
    Brier.r = colMeans((newY - Yhat)^2)
    Brier = mean(Brier.r)
  }
  else{
    devr = NULL
    devtot  = NULL
    Brier.r = NULL
    Brier = NULL
  }
  # make output object
  output = list(
    Yhat = Yhat,
    devr = devr,
    devtot = devtot,
    Brier.r = Brier.r,
    Brier = Brier
  )
  class(output) = "predlmdu"
  return(output)
}
