#'  The function predict.mru makes predictions for a test/validation set
#'  based on a fitted mru model
#'
#' @param object An \code{lmdu} object
#' @param newX An N by P matrix with predictor variables for a test/validation set
#' @param newG An N by R matrix with response variables  for a test/validation set
#' @param \dots additional arguments to be passed.
#'
#' @return This function returns an object of the class \code{p.mru} with components:
#' \item{Yhat}{Predicted values for the test set}
#' \item{dev}{Estimated prediction deviance}
#'
#' @examples
#' \dontrun{
#' data(dataExample_lpca)
#' Y = as.matrix(dataExample_mru[-c(1:20) , 1:8])
#' X = as.matrix(dataExample_mru[-c(1:20) , 9:13])
#' newY = as.matrix(dataExample_mru[1:20 , 1:8])
#' newX = as.matrix(dataExample_mru[1:20 , 9:13])
#' # supervised
#' output = mru(Y = Y, X = X, S = 2)
#' preds = predict(output, newX = newX, newY = newY)
#' }
#'
#'
#' @export

predict.mru = function(object, newX, newG = NULL,...){


  X = scale(newX, center = object$mx, scale = object$sdx)
  U = X %*% object$B
  V = object$V
  D = sqrt(outer(diag(U %*% t(U)), rep(1, nrow(V))) + outer(rep(1, nrow(U)), diag(V %*% t(V))) - 2 * U %*% t(V))
  theta = - D
  Yhat = exp(theta)/rowSums(exp(theta))

  if(is.null(newG)){
    dev = NULL
  }
  else{
#    G = nnet::class.ind(newY)
    dev = -2 * sum(newG * log(Yhat))
  }
  output = list(
    Yhat = Yhat,
    dev = dev
  )
  class(output) = "p.mru"
  return(output)
}
