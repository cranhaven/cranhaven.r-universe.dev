#' Mean of Multivariate Response
#'
#' This model is fed to \code{\link{conformal.multidim.full}},
#' \code{\link{conformal.multidim.split}}, and \code{\link{conformal.multidim.msplit}}.
#' It outputs a training function and a prediction function.
#' @return A list with the training function and the prediction function.
#' @details
#' The training function takes as input:
#'
#' x The feature matrix  (dim n x p)
#' y The response matrix (dim n x q)
#'
#' The predict function, instead, takes as input:
#'
#' out The output of a previous call to train.fun
#' newx The new features to evaluate (i.e. an n0 x p matrix)
#'
#' @export mean_multi

mean_multi = function() {

  # Training function
  train.fun = function(x,y,out=NULL) {

    m=colMeans(y)

    return(list(m=m))

    }

  # Prediction function
  predict.fun = function(out,newx) {

    temp=out$m
    n0=dim(newx)[1]
    len=length(temp)

    sol= t(matrix(rep(temp,n0),nrow=len))
    return(sol)
  }



  return(list(train.fun=train.fun, predict.fun=predict.fun))
}
