#' Print Method for Summary OPSR Objects
#'
#' @param x and object of class `"summary.opsr"`
#'
#' @param digits minimum number of significant digits to be used for most numbers (passed to [`stats::printCoefmat`]).
#' @param print.call if `TRUE`, prints the underlying [`opsr`] call.
#' @param ... further arguments passed to or from other methods.
#'
#' @return Prints summary in 'pretty' form and returns `x` invisibly.
#'
#' @method print summary.opsr
#'
#' @seealso [`stats::printCoefmat`], [`summary.opsr`]
#' @export
print.summary.opsr <- function(x, digits = max(3L, getOption("digits") - 3L),
                               print.call = TRUE, ...) {
  ## output formatting for wald tests
  r_ <- function(x) round(x, digits = digits)
  cat_wald <- function(wald, note) {
    cat("Wald chi2 (", note, "): ", r_(wald$chisq), " on ", wald$df, " DF, ",
        "p-value: < ", r_(wald$pval), "\n", sep = "")
  }
  if (print.call) {
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
        "\n\n", sep = "")
  }
  cat(x$maxim_type, ", ", x$iterations, " iterations\n", sep = "")
  cat("Return code ", x$return_code, ": ", x$message, "\n", sep = "")
  cat("Runtime: ", format(unclass(x$runtime), digits = 3), " ", attr(x$runtime, "units"), "\n", sep = "")
  cat("Number of regimes:", x$nReg, "\n")
  cat("Number of observations: "); cat(x$nObs[1], "("); cat(x$nObs[-1], sep = ", "); cat(")\n")
  cat("Estimated parameters:", x$nParams, "\n\n")
  cat("Log-Likelihood:", x$GOF$LLfinal, "\n")
  cat("AIC:", x$GOF$AIC, "\n")
  cat("BIC:", x$GOF$BIC, "\n")
  cat("Pseudo R-squared (EL):", r_(x$GOFcomponents$pseudoR2el), "\n")
  cat("Pseudo R-squared (MS):", r_(x$GOFcomponents$pseudoR2ms), "\n")
  cat("Multiple R-squared: "); cat(r_(x$GOFcomponents$R2[1]), "("); cat(r_(x$GOFcomponents$R2[-1]), sep = ", "); cat(")\n\n")
  cat("Estimates:\n")
  stats::printCoefmat(x$coef_table, digits = digits)
  cat("\n")
  cat_wald(x$wald$null, "null")
  cat_wald(x$wald$rho, "rho")

  invisible(x)
}
