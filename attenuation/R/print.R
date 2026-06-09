#' Print method for \code{ccaf}
#' @param x Object of class \code{ccaf}.
#' @param digits Passed to \code{signif}.
#' @param ... Ignored.
#' @return Invisible copy of \code{x}.
#' @export

print.ccaf = function(x, digits = 3, ...) {
  r = attr(x, "r")
  N = attr(x, "N")
  if (length(x) == 1) {
    cat("\np-value for corrected correlation coefficients\n\n")
  } else {
    if (attr(x, "type") == "Confidence curve") {
      cat("\nConfidence curve for corrected correlation coefficients\n\n")
    } else {
      cat("\np-values for corrected correlation coefficients\n\n")
    }
  }

  cat("Arguments\n")
  cat("    Sample correlations:", signif(r[1], digits),
                                  signif(r[2], digits),
                                  signif(r[3], digits))
  cat("\n    Sample sizes:", N[1], N[2], N[3])

  if (length(x) == 1) {
    cat("\n\nHypothesis test\n")
    cat("    rho =", attr(x, "rho"), "\n")
    cat("    p-value:", signif(x, digits))
    cat("    method:", attr(x, "method"))
  }

  cat("\n\n")
  invisible(x)
}