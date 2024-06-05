#' Extract Model Coefficients from a `hdqr` Object
#'
#' Retrieves the coefficients at specified values of `lambda` from a fitted `hdqr()` model.
#'
#' This function extracts coefficients for specified `lambda` values from a `hdqr()` object.
#' If `s`, the vector of `lambda` values, contains values not originally used in the model fitting,
#' the `coef` function employs linear interpolation between the closest `lambda` values from the 
#' original sequence to estimate coefficients at the new `lambda` values.
#'
#' @importFrom methods rbind2
#' @importFrom stats coef predict
#' @param object Fitted `hdqr()` object.
#' @param s Values of the penalty parameter `lambda` for which coefficients are requested.
#'   Defaults to the entire sequence used during the model fit.
#' @param type Type of prediction required. Type `"coefficients"` computes the coefficients at the requested 
#'   values for `s`. Type `"nonzero"` returns a list of the indices of the nonzero coefficients for each 
#'   value of \code{s}.
#' @param ... Not used.
#' @seealso \code{\link{hdqr}}, \code{\link{predict.hdqr}}
#'
#' @return Returns a matrix or vector of coefficients corresponding to the specified `lambda` values.
#'
#' @method coef hdqr
#' @export
#' @examples
#' set.seed(315)
#' n <- 100
#' p <- 400
#' x <- matrix(data = rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
#' beta_star <- c(c(2, 1.5, 0.8, 1, 1.75, 0.75, 0.3), rep(0, (p - 7)))
#' eps <- rnorm(n, mean = 0, sd = 1)
#' y <- x %*% beta_star + eps
#' tau <- 0.5
#' lam2 <- 0.01
#' fit <- hdqr(x = x, y = y, tau = tau, lam2 = lam2)
#' coefs <- coef(fit, s = fit$lambda[3:5])

coef.hdqr <- function(object, s = NULL,
                             type = c("coefficients", "nonzero"), ...) {
    type <- match.arg(type)
    b0 <- t(as.matrix(object$b0))
    rownames(b0) <- "(Intercept)"
    nbeta <- rbind2(b0, object$beta)
    if (!is.null(s)) {
      vnames <- dimnames(nbeta)[[1]]
      dimnames(nbeta) <- list(NULL, NULL)
      lambda <- object$lambda
      lamlist <- lambda.interp(lambda, s)
      nbeta <- nbeta[,lamlist$left, drop=FALSE]%*%Diagonal(x=lamlist$frac) +
               nbeta[,lamlist$right,drop=FALSE]%*%Diagonal(x=1-lamlist$frac)
      dimnames(nbeta) <- list(vnames, paste(seq(along = s)))
    }
    if (type == "coefficients") return(nbeta)
    ## return locations of nonzero coefficients for each lambda
    if (type == "nonzero")
      return(nonzero(nbeta[-1, , drop = FALSE], bystep = TRUE))
}

#' Make Predictions from a `hdqr` Object
#'
#' Produces fitted values for new predictor data using a fitted `hdqr()` object.
#'
#' This function generates predictions at specified `lambda` values from a fitted `hdqr()` object.
#' It is essential to provide a new matrix of predictor values (`newx`) at which these predictions are to be made.
#'
#' @param object Fitted `hdqr()` object from which predictions are to be derived.
#' @param newx Matrix of new predictor values for which predictions are desired.
#'   This must be a matrix and is a required argument.
#' @param s Values of the penalty parameter `lambda` for which predictions are requested.
#'   Defaults to the entire sequence used during the model fit.
#' @param ... Not used.
#' @seealso \code{\link{hdqr}}, \code{\link{coef.hdqr}}
#'
#' @return Returns a vector or matrix of predicted values corresponding to the specified `lambda` values.
#'
#' @method predict hdqr
#' @export
#' @examples
#' set.seed(315)
#' n <- 100
#' p <- 400
#' x <- matrix(data = rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
#' beta_star <- c(c(2, 1.5, 0.8, 1, 1.75, 0.75, 0.3), rep(0, (p - 7)))
#' eps <- rnorm(n, mean = 0, sd = 1)
#' y <- x %*% beta_star + eps
#' tau <- 0.5
#' lam2 <- 0.01
#' fit <- hdqr(x = x, y = y, tau = tau, lam2 = lam2)
#' preds <- predict(fit, newx = tail(x), s = fit$lambda[3:5])

predict.hdqr <- function(object, newx, s=NULL, ...) {
    b0 <- t(as.matrix(object$b0))
    rownames(b0) <- "(Intercept)"
    nbeta <- rbind2(b0, object$beta)
    if (!is.null(s)) {
      vnames <- dimnames(nbeta)[[1]]
      dimnames(nbeta) <- list(NULL, NULL)
      lambda <- object$lambda
      lamlist <- lambda.interp(lambda, s)
      nbeta <- nbeta[,lamlist$left, drop=FALSE]%*%Diagonal(x=lamlist$frac) +
               nbeta[,lamlist$right,drop=FALSE]%*%Diagonal(x=1-lamlist$frac)
      dimnames(nbeta) <- list(vnames, paste(seq(along = s)))
    }
    nfit <- as.matrix(as.matrix(cbind2(1, newx)) %*% nbeta)
    nfit
} 

