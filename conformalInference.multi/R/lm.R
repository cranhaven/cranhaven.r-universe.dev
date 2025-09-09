#' Linear Modeling of Multivariate Response
#'
#' This model is fed to \code{\link{conformal.multidim.full}},
#' \code{\link{conformal.multidim.split}}, and \code{\link{conformal.multidim.msplit}}.
#' It outputs a training function and a prediction function.
#'
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
#' Here I defined an lm model for every dimension of the responses (q).
#' @importFrom stats lm
#' @export lm_multi


lm_multi = function() {

  # Training function
  train.fun = function(x,y,out=NULL) {

    q=dim(y)[2]
    p=dim(x)[2]

    df1=data.frame(cbind(x,y[,1]))

    coeff=vapply(1:q, function(i) lm(formula = y[,i] ~  x)$coefficients,numeric(p+1))
    # dim (p+1) x q

    return(list(coeff=coeff))

  }

  # Prediction function
  predict.fun = function(out,newx) {

    c=out$coeff
    n0=dim(newx)[1]
    newxx=cbind(rep(1,n0),newx)
    sol=newxx%*%c


    return(sol)
  }



  return(list(train.fun=train.fun, predict.fun=predict.fun))
}



