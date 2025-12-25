#' @aliases SMME_predict SMME.predict
#' @title Make Prediction From a SMME Object
#'
#' @description  Given new covariate data this function computes the linear predictors
#' based on the estimated model coefficients in an object produced by the function
#' \code{softmaximin}. Note that the data can be supplied in three different
#' formats: i) for general models as a \eqn{n' \times p} matrix (\eqn{p} is the
#' number of model coefficients and \eqn{n'} is the number of new data points),
#' ii) for array models with custom design as a list of one, two or three Kronecker component
#' matrices each of size \eqn{n_i' \times p_i, i = 1, 2, 3}
#' (\eqn{n_i'} is the number of new marginal data points in the \eqn{i}th dimension),
#' iii) for wavelet based models a string indicating the wavelet used to produce
#' the model object.
#'
#'
#' @param object An object of class SMME, produced with \code{softmaximin} with
#' \eqn{m_\zeta} fitted models for each value of \code{zeta}.
#' @param x An object that should be like the input to the \code{softmaximin} call
#' that produced \code{object}. For general  models a matrix with column
#' dimension equal to that of  the original input.For array models with custom
#' design a list like the one supplied to \code{softmaximin} to produce \code{object}
#' and for a wavelet design the name of the wavelet used to produce \code{object}.
#' @param ... ignored.
#' @return A list of length \code{length(zeta)}. If \code{x} is a \eqn{n' \times p}
#' matrix each list item is a \eqn{n'\times m_\zeta} matrix containing the linear
#' predictors computed for each \code{lambda}. If \code{x} is a string or a list of
#' tensor component matrices and \code{fit$dim = d}, each list item is a \eqn{d + 1}
#' array  containing predictions computed for each \code{lambda}.
#'
#' @examples
#' ##size of example
#' n1 <- 65; n2 <- 26; n3 <- 13; p1 <- 13; p2 <- 5; p3 <- 4
#'
#' ##marginal design matrices (Kronecker components)
#' X1 <- matrix(rnorm(n1 * p1, 0, 0.5), n1, p1)
#' X2 <- matrix(rnorm(n2 * p2, 0, 0.5), n2, p2)
#' X3 <- matrix(rnorm(n3 * p3, 0, 0.5), n3, p3)
#' X <- list(X1, X2, X3)
#'
#' component <- rbinom(p1 * p2 * p3, 1, 0.1)
#' Beta1 <- array(rnorm(p1 * p2 * p3, 0, 0.1) + component, c(p1 , p2, p3))
#' Beta2 <- array(rnorm(p1 * p2 * p3, 0, 0.1) + component, c(p1 , p2, p3))
#' mu1 <- RH(X3, RH(X2, RH(X1, Beta1)))
#' mu2 <- RH(X3, RH(X2, RH(X1, Beta2)))
#' Y1 <- array(rnorm(n1 * n2 * n3, mu1), dim = c(n1, n2, n3))
#' Y2 <- array(rnorm(n1 * n2 * n3, mu2), dim = c(n1, n2, n3))
#'
#' Y <- array(NA, c(dim(Y1), 2))
#' Y[,,, 1] <- Y1; Y[,,, 2] <- Y2;
#'
#' fit <- softmaximin(X, Y, zeta = c(1, 10), penalty = "lasso", alg = "npg")
#'
#' ##new data in tensor component form
#' X1 <- matrix(rnorm(2 * p1), nrow = 2)
#' X2 <- matrix(rnorm(3 * p2), nrow = 3)
#' X3 <- matrix(rnorm(4 * p3), nrow = 4)
#' Yhat <- predict(fit, x = list(X1, X2, X3))
#'
#' @author Adam Lund
#' @method predict SMME
#' @export
predict.SMME <- function(object, x, ...){
out <- vector("list", length(object$zeta))
names(out) <- object$zeta
if(is.character(x) || is.list(x)){ #array model
if(is.list(x)){##custom design (non wavelets)
if(length(x) != object$dim){stop("length of x must be equal to dimension of model!")}
if(object$dim == 1){
x[[2]] <- matrix(1, 1, 1)
x[[3]] <- matrix(1, 1, 1)
}else if(object$dim == 2){x[[3]] <- matrix(1, 1, 1)}
  px <- nx <- rep(NA, length(x))
  for( i in 1:length(x)){
    nx[i] = dim(x[[i]])[1]
    px[i] = dim(x[[i]])[2]
  }
if(sum(px[px > 1] != object$dimcoef[object$dimcoef > 1]) > 0){
stop(paste("column dimensions of new data (", paste(px, collapse = ",") ,") is not equal those of fit (", paste(object$dimcoef, collapse = ",") ,")", sep = ""))
}
}else{#wavelet
nx<-px <- c(object$dimcoef, rep(1, 3 - length(object$dimcoef)))
}
for(z in 1:length(object$zeta)){
nlambda <- length(object$lambda[[z]])
res <- array(NA, c(nx, nlambda))
for(i in 1:nlambda){
if(!(is.character(x))){
res[,,, i] <- RH(x[[3]], RH(x[[2]], RH(x[[1]], array(object$coef[[z]][, i], dim = px))))
}else{
res[,,, i] <- iwt(array(object$coef[[z]][, i], dim = px), wf = x)
}
}
out[[z]] <- drop(res)}
}else if(is.matrix(x)){#non array
px <- dim(x)[2]
if(px != object$dimcoef){
stop(paste("column dimension of new data (", px ,") is not equal that of fit (", object$dimcoef ,")", sep = ""))
}
for(z in 1:length(object$zeta)){
  nlambda <- length(object$lambda[[z]])
  res <- matrix(NA , dim(x)[1], nlambda)
  for(i in 1:nlambda){res[, i] <- x %*% object$coef[[z]][ , i]}
  out[[z]] <- res}
}
#}else{stop(paste("dimension of new data inconsistent with existing data"))}
return(out)
}
