#' Extract Coefficients from a `cv.hdsvm` Object
#'
#' Retrieves coefficients from a cross-validated `hdsvm()` model, using the
#' stored `"hdsvm.fit"` object and the optimal `lambda` value determined during
#' cross-validation.
#'
#' @param object A fitted `cv.hdsvm()` object from which coefficients are to be extracted.
#' @param s Specifies the value(s) of the penalty parameter `lambda` for which coefficients are desired.
#'   The default is `s = "lambda.1se"`, which corresponds to the largest value of `lambda` such that the
#'   cross-validation error estimate is within one standard error of the minimum. Alternatively,
#'   `s = "lambda.min"` can be used, corresponding to the minimum of the cross-validation error estimate.
#'   If `s` is numeric, these are taken as the actual values of `lambda` to use.
#' @param ... Not used.
#'
#' @return Returns the coefficients at the specified `lambda` values.
#' @seealso \code{\link{cv.hdsvm}}, \code{\link{predict.cv.hdsvm}}
#' @method coef cv.hdsvm
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
#' cv.fit <- cv.hdsvm(x, y, lam2 = 0.01)
#' coef(cv.fit, s = c(0.02, 0.03))

coef.cv.hdsvm <- function(object, s = c("lambda.1se", "lambda.min"),...) {
  if (is.numeric(s)) {
    lambda <- s
  } else if (is.character(s)) {
    s <- match.arg(s)
    lambda <- object[[s]]
  } else stop("Invalid form for s")
  coef(object$hdsvm.fit, s = lambda, ...)
}

#' Make Predictions from a `cv.hdsvm` Object
#'
#' Generates predictions using a fitted `cv.hdsvm()` object. This function utilizes the
#' stored `hdsvm.fit` object and an optimal value of `lambda` determined during the
#' cross-validation process.
#'
#' @param newx Matrix of new predictor values for which predictions are desired.
#'   This must be a matrix and is a required argument.
#' @param object A fitted `cv.hdsvm()` object from which predictions are to be made.
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
#' @seealso \code{\link{cv.hdsvm}}, \code{\link{coef.cv.hdsvm}}
#' @method predict cv.hdsvm
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
#' cv.fit <- cv.hdsvm(x, y, lam2 = 0.01)
#' predict(cv.fit, newx = x[50:60, ], s = "lambda.min")
predict.cv.hdsvm <- function(object, newx,s = c("lambda.1se", "lambda.min"),
  type = c("class", "loss"), ...) {
  if (is.numeric(s)) {
    lambda <- s
  } else if (is.character(s)) {
    s <- match.arg(s)
    lambda <- object[[s]]
  } else stop("Invalid form for s")
    predict(object$hdsvm.fit, newx, s = lambda, type, ...)
}