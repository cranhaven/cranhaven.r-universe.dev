#' @aliases FRESHD_predict FRESHD.predict
#' @title Make Prediction From a FRESHD Object
#'
#' @description  Given covariate data this function computes the linear predictors
#' based on the estimated model coefficients in an object produced by the function
#' \code{maximin} or \code{magging}. Note that the data can be supplied in two different
#' formats:
#' i) for wavelet based models as a string indicating the wavelet used to produce
#' the model object.
#' ii) for  models with custom design as a list of one, two or three Kronecker component
#' matrices each of size \eqn{n_i' \times p_i, i = 1, 2, 3}. Note \code{x} will
#' typically be the original design (covariate data) that was used to produce \code{object}
#' using \code{maximin} or \code{magging} so \eqn{n_i'} is the number of
#' marginal data points in the \eqn{i}th dimension i.e. \eqn{n_i' = n_i}.
#'
#' @param object An object of class FRESHD, produced with \code{maximin} or \code{magging}.
#' @param x An object that should be like the input to the call
#' that produced \code{object}. For models with custom design a list like the one
#' supplied to produce \code{object} and for a wavelet design
#' the name of the wavelet used to produce \code{object}.
#' @param ... ignored.
#'
#' @return If \code{x} is a string indicating a wavelet an array of the same size
#' as the input data used to produce \code{object}. Otherwise an array of size
#' \eqn{n'_1 \times \cdots \times n'_d}, with \eqn{d\in \{1,2,3\}}.
#'
#' @examples
#' ##size of example
#' set.seed(42)
#' G = 50; N1 = 2^10; p = 101; J = 3; amp = 20; sigma2 = 10
#' y <- matrix(0, N1, G)
#' z <- seq(0, 2, length.out = N1)
#' sig <- cos(10 * pi * z) + 1.5 * sin(5 * pi * z)
#' for (i in 1:G){
#' freqs <- sample(1:100, size = J, replace = TRUE)
#' y[, i] <- sig * 2 + rnorm(N1, sd = sqrt(sigma2))
#' for (j in 1:J){
#' y[, i] <- y[, i] + amp * sin(freqs[j] * pi * z + runif(1, -pi, pi))
#' }
#' }
#' system.time(fitmm <- maximin(y, "la8", alg = "aradmm", kappa = 0.95))
#' mmy <- predict(fitmm, "la8")
#' plot(mmy[, 2], type = "l")
#' lines(sig, col = "red")
#'
#' @author Adam Lund
#' @method predict FRESHD
#' @export
predict.FRESHD <- function(object, x, ...){
    if(is.list(x) || is.matrix(x)){##custom design (non wavelets)
     
      #if(length(x) != object$dim){stop("length of x must be equal to dimension of model!")}

      if(object$dim == 1){
        if(is.matrix(x)){
          x <- list(x, matrix(1, 1, 1), matrix(1, 1, 1))
        }else{
          x <- list(x[[1]], matrix(1, 1, 1), matrix(1, 1, 1))
        }
      }else if(object$dim == 2){
        x <- list(x[[1]], x[[2]], matrix(1, 1, 1))
      }
      
      px <- nx <- rep(NA, length(x))
      for( i in 1:length(x)){
        nx[i] = dim(x[[i]])[1]
        px[i] = dim(x[[i]])[2]
      }
      if(sum(px[px > 1] != object$dimcoef[object$dimcoef > 1]) > 0){
        stop(paste("column dimensions of new data (", paste(px, collapse = ",") ,") is not equal those of fit (", paste(object$dimcoef, collapse = ",") ,")", sep = ""))
      }
    }else{#wavelet
      nx <- px <- c(object$dimcoef, rep(1, 3 - length(object$dimcoef)))
    }
      nlambda <- length(object$lambda)
      res <- array(NA, c(nx, nlambda))
      for(i in 1:nlambda){
        if(!(is.character(x))){
          res[,,, i] <- RH(x[[3]], RH(x[[2]], RH(x[[1]], array(object$coef[, i], dim = px))))
        }else{
          res[,,, i] <- iwt(array(object$coef[, i], dim = px), wf = x)
        }
      }
  #}else{stop(paste("dimension of new data inconsistent with existing data"))}
  return(drop(res))
}
