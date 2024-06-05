#' Extract Model Coefficients from a `nc.hdsvm` Object
#'
#' Retrieves the coefficients at specified values of `lambda` from a fitted `nc.hdsvm()` model.
#'
#' This function extracts coefficients for specified `lambda` values from a `nc.hdsvm()` object.
#' If `s`, the vector of `lambda` values, contains values not originally used in the model fitting,
#' the `coef` function employs linear interpolation between the closest `lambda` values from the 
#' original sequence to estimate coefficients at the new `lambda` values.
#'
#' @importFrom methods rbind2
#' @param object Fitted `nc.hdsvm()` object.
#' @param s Values of the penalty parameter `lambda` for which coefficients are requested.
#'   Defaults to the entire sequence used during the model fit.
#' @param type Type of prediction required. Type `"coefficients"` computes the coefficients at the requested 
#'   values for `s`. Type `"nonzero"` returns a list of the indices of the nonzero coefficients for each 
#'   value of \code{s}.
#' @param ... Not used.
#' @seealso \code{\link{nc.hdsvm}}, \code{\link{predict.nc.hdsvm}}
#'
#' @return Returns a matrix or vector of coefficients corresponding to the specified `lambda` values.
#'
#' @method coef nc.hdsvm
#' @export
#' @examples
#' set.seed(315)
#' n <- 100
#' p <- 400
#' x1 <- matrix(rnorm(n / 2 * p, -0.25, 0.1), n / 2)
#' x2 <- matrix(rnorm(n / 2 * p, 0.25, 0.1), n / 2)
#' x <- rbind(x1, x2)
#' beta <- 0.1 * rnorm(p)
#' prob <- plogis(c(x %*% beta))
#' y <- 2 * rbinom(n, 1, prob) - 1
#' lam2 <- 0.01
#' lambda <- 10^(seq(1,-4, length.out = 30)) 
#' nc.fit <- nc.hdsvm(x = x, y = y, lambda = lambda, lam2 = lam2, pen = "scad")
#' nc.coefs <- coef(nc.fit, s = nc.fit$lambda[3:5])

coef.nc.hdsvm <- function(object, s=NULL, 
    type=c("coefficients", "nonzero"), ...) {
  type <- match.arg(type)
  b0 <- t(as.matrix(object$b0))
  rownames(b0) <- "(Intercept)"
  nbeta <- rbind2(b0, object$beta)
  if (!is.null(s)) {
    vnames <- dimnames(nbeta)[[1]]
    dimnames(nbeta) <- list(NULL, NULL)
    lambda <- object$nc.lambda
    lamlist <- lambda.interp(lambda, s)
    nbeta <- nbeta[,lamlist$left,drop=FALSE] %*% 
      Diagonal(x=lamlist$frac) +
      nbeta[,lamlist$right,drop=FALSE] %*% 
      Diagonal(x=1-lamlist$frac)
    dimnames(nbeta) <- list(vnames, paste(seq(along=s)))
  }
  if (type == "coefficients") 
    return(nbeta)
  if (type == "nonzero") 
    return(nonzero(nbeta[-1, , drop=FALSE], bystep=TRUE))
} 

#' Make Predictions from a `nc.hdsvm` Object
#'
#' Produces fitted values for new predictor data using a fitted `nc.hdsvm()` object.
#'
#' This function generates predictions at specified `lambda` values from a fitted `nc.hdsvm()` object.
#' It is essential to provide a new matrix of predictor values (`newx`) at which these predictions are to be made.
#'
#' @param object Fitted `nc.hdsvm()` object from which predictions are to be derived.
#' @param newx Matrix of new predictor values for which predictions are desired.
#'   This must be a matrix and is a required argument.
#' @param s Values of the penalty parameter `lambda` for which predictions are requested.
#'   Defaults to the entire sequence used during the model fit.
#' @param type Type of prediction required. Type `"class"` produces the predicted binary class labels and
#' type `"loss"` returns the fitted values. Default is \code{"class"}.
#' @param ... Not used.
#' @seealso \code{\link{nc.hdsvm}}, \code{\link{coef.nc.hdsvm}}
#'
#' @return Returns a vector or matrix of predicted values corresponding to the specified `lambda` values.
#'
#' @method predict nc.hdsvm
#' @export
#' @examples
#' set.seed(315)
#' n <- 100
#' p <- 400
#' x1 <- matrix(rnorm(n / 2 * p, -0.25, 0.1), n / 2)
#' x2 <- matrix(rnorm(n / 2 * p, 0.25, 0.1), n / 2)
#' x <- rbind(x1, x2)
#' beta <- 0.1 * rnorm(p)
#' prob <- plogis(c(x %*% beta))
#' y <- 2 * rbinom(n, 1, prob) - 1
#' lam2 <- 0.01
#' lambda <- 10^(seq(1,-4, length.out = 30)) 
#' nc.fit <- nc.hdsvm(x = x, y = y, lambda = lambda, lam2 = lam2, pen = "scad")
#' nc.preds <- predict(nc.fit, newx = tail(x), s = nc.fit$lambda[3:5])

predict.nc.hdsvm <- function(object, newx, s=NULL, type=c("class", "loss"),...) {
  type <- match.arg(type)
  b0 <- t(as.matrix(object$b0))
  rownames(b0) <- "(Intercept)"
  nbeta <- rbind2(b0, object$beta)
  if (!is.null(s)) {
    vnames <- dimnames(nbeta)[[1]]
    dimnames(nbeta) <- list(NULL, NULL)
    lambda <- object$nc.lambda
    lamlist <- lambda.interp(lambda, s)
    nbeta <- nbeta[ , lamlist$left, drop=FALSE] %*% 
            Diagonal(x=lamlist$frac) +
            nbeta[ , lamlist$right, drop=FALSE] %*% 
            Diagonal(x=1-lamlist$frac)
    dimnames(nbeta) <- list(vnames, paste(seq(along=s)))
  }
  nfit <- as.matrix(as.matrix(cbind2(1, newx)) %*% nbeta) 
  switch(type, loss=nfit, class=ifelse(nfit > 0, 1, -1))
}