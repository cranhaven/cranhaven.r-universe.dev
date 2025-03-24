#' Regress set of variables on its parents
#'
#' Uses the generalised additive model \link{gam} to non-linearly and non-parametrically regress set of variables X on a set of variables S and returns residuals of X.
#'
#' @param X numeric matrix, set of variables to be regressed. Each column represents separate variable
#' @param S numeric matrix, set of variables we will regress on. Each column represents separate variable
#' @details If the number of variables in S is \eqn{\leq 5}{<= 5} we use \link{frml.full.smooth} as formula for \link{gam} to regress X on S, otherwise we use \link{frml.additive.smooth}.
#' @importFrom mgcv gam
#' @import methods
#' @export
#' @return regrXonS() returns the residuals of X regressed on S.
#' @author Petras Verbyla (\email{petras.verbyla@mrc-bsu.cam.ac.uk})
#' @seealso \link{kernelCItest}
#' @examples
#' set.seed(10)
#' library(energy)
#' z <- 10*runif(300)
#' w <- 10*runif(300)
#' x <- sin(z) + runif(300)
#' y <- cos(z) + runif(300)
#' data <- cbind(x,y,z,w)
#'
#' hsic.gamma(x,y)
#' hsic.perm(x,y)
#' dcov.test(x,y)
#'
#' resid <- regrXonS(cbind(x,y),cbind(z,w))
#'
#' hsic.gamma(resid[,1],resid[,2])
#' hsic.perm(resid[,1],resid[,2])
#' dcov.test(resid[,1],resid[,2])

regrXonS <- function(    X, # set of variables to be regressed
                         S  # set of variables on which we regress
                         ){

  #library(mgcv)

  X <- as.matrix(X)
  S <- as.matrix(S)

  n1 <- ncol(X)
  n2 <- ncol(S)

  if (n2 > 2){formulaXS <- frml.additive.smooth}
  else {formulaXS <- frml.full.smooth}

  data <- data.frame(cbind(X, S))

  colnames(data) <- paste("x",1:(n1+n2),sep="")

  resX <- matrix(0,nrow=nrow(X),ncol=ncol(X))

  for (i in 1:n1){
    formx <- formulaXS(i,(n1+1):(n1+n2))
    resX[,i] <- gam(formx,data=data)$residuals
  }

  return(resX)

}


