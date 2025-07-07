#' Get lambda.min and lambda.1se values
#'
#' Get lambda.min and lambda.1se values and indices.
#'
#' @param lambda The values of lambda used in the fits.
#' @param cvm The mean cross-validated error: a vector of length
#' `length(lambda)`.
#' @param cvsd Estimate of standard error of `cvm`.
#' @param type.measure Loss function used for CV.
#'
#' @return A list with the following elements:
#' \item{lambda.min}{Value of `lambda` that gives minimum `cvm`.}
#' \item{lambda.1se}{Largest value of `lambda` such that the error is within
#' 1 standard error of the minimum.}
#' \item{index}{A one-column matrix with the indices of `lambda.min` and
#' `lambda.1se` in the sequence of coefficients, fits etc.}
getOptLambda <- function (lambda, cvm, cvsd, type.measure) {
    if (match(type.measure, c("auc", "C"), 0)) cvm <- -cvm

    # compute lambda.min
    cvmin <- min(cvm, na.rm = TRUE)
    idmin <- which(cvm <= cvmin)
    idmin <- min(idmin, na.rm = TRUE)
    lambda.min <- lambda[idmin]

    # compute lambda.1se
    semin <- (cvm + cvsd)[idmin]
    id1se <- which(cvm <= semin)
    id1se <- min(id1se, na.rm = TRUE)
    lambda.1se <- lambda[id1se]

    index <- matrix(c(idmin,id1se), ncol = 1,
                    dimnames = list(c("min", "1se"), "Lambda"))
    return(list(lambda.min = lambda.min, lambda.1se = lambda.1se,
                index = index))
  }
