#' Print method for \code{optcheck} and \code{summary.optcheck} objects.
#'
#' @name print.optcheck
#' @aliases print.summary.optcheck print.optproj print.summary.optproj print.optrefit print.summary.optrefit
#' @param x Object of class \code{optcheck} or \code{summary.optcheck}, currently returned by \code{\link{optim_proj}}, \code{\link{optim_refit}}, or a summary of either of those calls.
#' @param digits Number of digits to display.
#' @param n Number of elements of solution vector to display (see Details).
#' @param ... Further arguments to be passed to or from other methods.
#' @return Invisibly \code{x} itself.
#' @details The \code{print} methods for \code{optcheck} and \code{summary.optcheck} objects both display three-column matrix, consisting of the potential solution (\code{xsol}), the absolute difference between it and the optimal solution (\code{xopt}) return by either \code{\link{optim_proj}} and \code{\link{optim_refit}}, and the relative difference (\code{R = (xopt - xsol)/|xsol|}).  Only the elemnts corresponding to the top-\code{n} relative differences are displayed.
#' @export
print.optcheck <- function(x, digits = max(3L, getOption("digits")-3L),
                           n = 5L, ...) {
  print(summary(x), digits = digits, n = n)
  invisible(x)
}

#' @rdname print.optcheck
#' @export
print.summary.optcheck <- function(x,
                                   digits = max(3L, getOption("digits")-3L),
                                   n = 5L, ...) {
  nx <- length(x$xsol)
  nmax <- min(nx, n)
  otype <- ifelse(x$maximize, "maximization", "minimization")
  ctype <- switch(class(x)[1],
                  summary.optproj = "\'optim_proj\'",
                  summary.optrefit = "\'optim_refit\'")
  cat("\n", ctype, " check on ", nx, "-variable ", otype, " problem.\n\n",
      "Top ", nmax, " relative errors in potential solution:\n\n",
      sep = "")
  res <- cbind(x$xsol, x$xdiff[,"abs"], x$xdiff[,"rel"])
  colnames(res) <- c("xsol", "D=xopt-xsol", "R=D/|xsol|")
  ord <- order(round(abs(res[,3]), 6), decreasing = TRUE)[1:nmax]
  print(signif(res[ord,], digits = digits))
  cat("\n")
  invisible(x)
}
