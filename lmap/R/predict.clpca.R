#'  The function predict.clpca makes predictions for a test/validation set
#'  based on a fitted clrrr model (clpca with X)
#'
#' @param object An \code{clpca} object
#' @param newX An N by P matrix with predictor variables for a test/validation set
#' @param newY An N by R matrix with response variables  for a test/validation set
#' @param \dots additional arguments to be passed.
#'
#' @return This function returns an object of the class \code{predclpca} with components:
#' \item{Yhat}{Predicted values for the test set}
#' \item{devr}{Estimated prediction deviance for separate responses}
#' \item{devtot}{Estimated prediction deviance for all responses}
#'
#' @examples
#' \dontrun{
#' data(dataExample_clpca)
#' Y = as.matrix(dataExample_clpca[ , 1:8])
#' X = as.matrix(dataExample_clpca[ , 9:13])
#' newY = as.matrix(dataExample_clpca[1:20 , 1:8])
#' newX = as.matrix(dataExample_clpca[1:20 , 9:13])
#' # supervised
#' output = clpca(Y = Y, X = X, S = 2)
#' preds = predict(output, newX = newX, newY = newY)
#' }
#'
#' @importFrom stats plogis
#' @importFrom nnet class.ind
#'
#' @export

predict.clpca = function(object, newX, newY = NULL,...){


  if(is.null(object$X)) stop("The predict function is only implemented for supervised analysis")

  X = scale(newX, center = object$mx, scale = object$sdx)
  theta = X %*% object$B %*% t(object$V)
  R = ncol(theta)
  Yhat <- vector(mode = "list", length = R)

  for(r in 1:R){
    zeta = c(-Inf, object$m[[r]], Inf)
    Cr = length(zeta)
    P = plogis(outer(rep(1,nrow(X)), zeta) - outer(theta[, r], rep(1, Cr)))
    Yhat[[r]] = t(apply(P, 1, diff))
  }

  if(!is.null(newY)){
    devr = rep(NA, R)
    for(r in 1:R){
      G = class.ind(newY[ , r])
      YY = Yhat[[r]]
      devr[r] = -2 * sum( log(YY[G == 1] ) )
    }
    devtot <- sum(devr)
  }
  else{
    devr = NULL
    devtot  = NULL
  }
  # make output object
  output = list(
    Yhat = Yhat,
    devr = devr,
    devtot = devtot
  )
  class(output) = "predclpca"
  return(output)
}
