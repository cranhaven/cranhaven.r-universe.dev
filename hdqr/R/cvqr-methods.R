#' Extract Coefficients from a `cv.hdqr` Object
#'
#' Retrieves coefficients from a cross-validated `hdqr()` model, using the
#' stored `"hdqr.fit"` object and the optimal `lambda` value determined during
#' cross-validation.
#'
#' @param object A fitted `cv.hdqr()` object from which coefficients are to be extracted.
#' @param s Specifies the value(s) of the penalty parameter `lambda` for which coefficients are desired.
#'   The default is `s = "lambda.1se"`, which corresponds to the largest value of `lambda` such that the
#'   cross-validation error estimate is within one standard error of the minimum. Alternatively,
#'   `s = "lambda.min"` can be used, corresponding to the minimum of the cross-validation error estimate.
#'   If `s` is numeric, these are taken as the actual values of `lambda` to use.
#' @param ... Not used.
#'
#' @return Returns the coefficients at the specified `lambda` values.
#' @seealso \code{\link{cv.hdqr}}, \code{\link{predict.cv.hdqr}}
#' @method coef cv.hdqr
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
#' cv.fit <- cv.hdqr(x = x, y = y, tau = tau, lam2 = lam2)
#' coef(cv.fit, s = c(0.02, 0.03))

coef.cv.hdqr <- function(object,
                            s = c("lambda.1se", "lambda.min"),
                            ...) {
  if (is.numeric(s)) {
    lambda <- s
  } else if (is.character(s)) {
    s <- match.arg(s)
    lambda <- object[[s]]
  } else stop("Invalid form for s")
  coef(object$hdqr.fit, s = lambda, ...)
}

#' Make Predictions from a `cv.hdqr` Object
#'
#' Generates predictions using a fitted `cv.hdqr()` object. This function utilizes the
#' stored `hdqr.fit` object and an optimal value of `lambda` determined during the
#' cross-validation process.
#'
#' @param newx Matrix of new predictor values for which predictions are desired.
#'   This must be a matrix and is a required argument.
#' @param object A fitted `cv.hdqr()` object from which predictions are to be made.
#' @param s Specifies the value(s) of the penalty parameter `lambda` at which predictions
#'   are desired. The default is `s = "lambda.1se"`, representing the largest value of `lambda`
#'   such that the cross-validation error estimate is within one standard error of the minimum.
#'   Alternatively, `s = "lambda.min"` can be used, corresponding to the minimum of the
#'   cross-validation error estimate. If `s` is numeric, these are taken as the actual values
#'   of `lambda` to use for predictions.
#' @param ... Not used.
#' @return Returns a matrix or vector of predicted values corresponding to the specified
#'   `lambda` values.
#' @seealso \code{\link{cv.hdqr}}, \code{\link{coef.cv.hdqr}}
#' @method predict cv.hdqr
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
#' cv.fit <- cv.hdqr(x = x, y = y, tau = tau, lam2 = lam2)
#' predict(cv.fit, newx = x[50:60, ], s = "lambda.min")

predict.cv.hdqr <- function(object, newx,
                               s = c("lambda.1se", "lambda.min"),
                               ...) {
  if (is.numeric(s)) {
    lambda <- s
  } else if (is.character(s)) {
    s <- match.arg(s)
    lambda <- object[[s]]
  } else stop("Invalid form for s")
    predict(object$hdqr.fit, newx, s = lambda, ...)
}