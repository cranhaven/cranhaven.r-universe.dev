#' Extract Coefficients from a `cv.nc.hdsvm` Object
#'
#' Retrieves coefficients at specified values of `lambda` from a fitted `cv.nc.hdsvm()` model.
#' Utilizes the stored `"nchdsvm.fit"` object and the optimal `lambda` values determined during
#' the cross-validation process.
#'
#' @param object A fitted `cv.nc.hdsvm()` object from which coefficients are to be extracted.
#' @param s Specifies the `lambda` values at which coefficients are requested.
#'   The default is `s = "lambda.1se"`, representing the largest `lambda` such that the cross-validation
#'   error estimate is within one standard error of the minimum. Alternatively, `s = "lambda.min"`
#'   corresponds to the `lambda` yielding the minimum cross-validation error. If `s` is numeric, these
#'   values are directly used as the `lambda` values for coefficient extraction.
#' @param ... Not used.
#'
#' @return Returns a vector or matrix of coefficients corresponding to the specified `lambda` values.
#' @seealso \code{\link{cv.nc.hdsvm}}, \code{\link{predict.cv.nc.hdsvm}}
#' @method coef cv.nc.hdsvm
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
#' \donttest{cv.nc.fit <- cv.nc.hdsvm(x = x, y = y, lambda = lambda, lam2 = lam2, pen = "scad")}
#' \donttest{coef(cv.nc.fit, s = c(0.02, 0.03))}

coef.cv.nc.hdsvm <- function(object, s = c("lambda.1se", "lambda.min"),...) {
  if (is.numeric(s)) {
    lambda <- s
  } else if (is.character(s)) {
    s <- match.arg(s)
    lambda <- object[[s]]
  } else stop("Invalid form for s")
  coef(object$nchdsvm.fit, s = lambda, ...)
}

#' Make Predictions from a `cv.nc.hdsvm` Object
#'
#' Generates predictions using a fitted `cv.nc.hdsvm()` object. This function utilizes the
#' stored `nchdsvm.fit` object and an optimal value of `lambda` determined during the
#' cross-validation process.
#'
#' @param newx Matrix of new predictor values for which predictions are desired.
#'   This must be a matrix and is a required argument.
#' @param object A fitted `cv.nc.hdsvm()` object from which predictions are to be made.
#' @param s Specifies the value(s) of the penalty parameter `lambda` at which predictions
#'   are desired. The default is `s = "lambda.1se"`, representing the largest value of `lambda`
#'   such that the cross-validation error estimate is within one standard error of the minimum.
#'   Alternatively, `s = "lambda.min"` can be used, corresponding to the minimum of the
#'   cross-validation error estimate. If `s` is numeric, these are taken as the actual values
#'   of `lambda` to use for predictions.
#' @param type Type of prediction required. Type `"class"` produces the predicted binary class labels and
#' type `"loss"` returns the fitted values. Default is \code{"class"}.
#' @param ... Not used.
#' @return Returns a matrix or vector of predicted values corresponding to the specified
#'   `lambda` values.
#' @seealso \code{\link{cv.nc.hdsvm}}, \code{\link{predict.cv.nc.hdsvm}}
#' @method predict cv.nc.hdsvm
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
#' \donttest{cv.nc.fit <- cv.nc.hdsvm(x = x, y = y, lambda = lambda, lam2 = lam2, pen = "scad")}
#' \donttest{predict(cv.nc.fit, newx = x[50:60, ], s = "lambda.min")}

predict.cv.nc.hdsvm <- function(object, newx,s = c("lambda.1se", "lambda.min"),
  type = c("class", "loss"), ...) {

  if (is.numeric(s)) {
    lambda <- s
  } else if (is.character(s)) {
    s <- match.arg(s)
    lambda <- object[[s]]
  } else stop("Invalid form for s")
    predict(object$nchdsvm.fit, newx, s = lambda, type, ...)
}