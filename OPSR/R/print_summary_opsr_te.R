#' Print Method for Summary OPSR TE Objects
#'
#' @param x an object of class `"summary.opsr.te"`.
#' @param digits minimum number of significant digits to be used for most numbers (passed to [`stats::printCoefmat`]).
#' @param print.call if `TRUE`, prints the underlying call.
#' @param ... further arguments passed to or from other methods.
#'
#' @return Prints `x` in 'pretty' form and returns it invisibly.
#'
#' @method print summary.opsr.te
#' @export
print.summary.opsr.te <- function(x, digits = max(3L, getOption("digits") - 3L),
                                  print.call = FALSE, ...) {
  heading <- "Treatment Effects"

  cat(heading, "\n\n")
  if (print.call) {
    cat("Call:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
        "\n\n", sep = "")
  }
  cat("TE\n\n")
  print(x$te, digits, signif.legend = FALSE)

  cat("\n\nATE\n\n")
  print(x$ate, digits, signif.legned = TRUE)
  invisible(x)
}
