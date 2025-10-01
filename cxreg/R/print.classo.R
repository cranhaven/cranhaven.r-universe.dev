#' print a classo object
#'
#' Print a summary of the classo path at each step along the path.
#' @details
#' The call that produced the object `x` is printed, followed by a
#' three-column matrix with columns `Df`, `%Dev` and `Lambda`.
#' The `Df` column is the number of nonzero coefficients (Df is a
#' reasonable name only for lasso fits). `%Dev` is the percent deviance
#' explained (relative to the null deviance).
#'
#' @aliases print.classo
#' @param x fitted classo object
#' @param digits significant digits in printout
#' @param \dots additional print arguments
#' @return The matrix above is silently returned
#' @seealso \code{classo}, \code{predict} and \code{coef} methods.
#' @keywords models regression
#'
#' @method print classo
#' @export
print.classo <- function (x, digits = max(3, getOption("digits") - 3), ...) {
    cat("\nCall: ", deparse(x$call), "\n\n")
    out <- data.frame(Df = x$df, `%Dev` = round(x$dev*100, 2),
                   Lambda = signif(x$lambda, digits),
                   check.names=FALSE,row.names=seq(along=x$df))
    class(out) <- c("anova",class(out))
    print(out)
}
