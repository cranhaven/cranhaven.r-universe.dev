#' A prediction function for a new data set for prediction purpose
#'
#' This function allows you to obtain the predicted values of a CMARS model.
#' @param object A cmaRs object which is obtained by prediction.
#' @param new The data for which the fitted values will be constructed.
#' @param ... Additional parameters affecting the predictions.
#' @importFrom stats fitted
#' @importFrom stats model.matrix
#' @importFrom stats coefficients
#' @return y The predicted values.
#' @export
#' @examples
#' \dontrun{
#' # Without \code{MOSEK}, the example code is not executable.
#' # For installation of Mosek, plese see the documentation of 'Rmosek'.
#' data("trees", package = "earth")
#' model.prediction <- cmaRs(Volume ~ ., degree = 5, nk = 20, data = trees)
#' predict.cmaRs(model.prediction, data = trees)
#' }
predict.cmaRs <- function(object, new = NULL, ...) {
  if (is.null(new)) {
    y <- stats::fitted(object)
  } else {
    if (!is.null(object$formula)) {
      ## model has been fitted using formula interface
      x <- stats::model.matrix(object$formula, new)
      x <- x[, -1]
      mm <- c()
      for (i in 1:(object$number.of.BF))
      {
        mm <- cbind(mm, eval(parse(text = object$bf.cmars[i]), envir = new))
      }
      mm <- cbind(1, mm)
      x <- as.matrix(mm)
    } else {
      x <- as.data.frame(stats::model.matrix(object$formula, new))
    }
    y <- as.vector(x %*% stats::coefficients(object))
  }
  y
}
