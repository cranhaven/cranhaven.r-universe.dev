#' Print a class `cvobj` object
#'
#' Print a summary of results of cross-validation for a class `cvobj` object.
#'
#' @param x A `"cvobj"` object.
#' @param digits Significant digits in printout.
#' @param ... Other print arguments.
#'
#' @return A summary is printed, and nothing is returned.
#'
#' @export
print.cvobj <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("Measure:", x$name,"\n\n")

  optlams <- c(x$lambda.min, x$lambda.1se)
  opt_idx <- match(optlams, x$lambda)
  mat <- with(x, cbind(optlams, opt_idx, cvm[opt_idx], cvsd[opt_idx]))
  dimnames(mat) <- list(c("min", "1se"),
                        c("Lambda", "Index", "Measure", "SE"))

  mat <- data.frame(mat, check.names = FALSE)
  class(mat) <- c("anova",class(mat))
  print(mat, digits = digits)
}
