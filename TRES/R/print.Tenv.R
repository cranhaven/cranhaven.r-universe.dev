#' @method print Tenv
#' @export
#' @importFrom stats coef

print.Tenv <- function(x, ...){
  cat("Call:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  cat("Coefficients:\n")
  cf <- stats::coef(x)
  if (!is.null(cf)) {
    print(cf)
  }
  else cat("No coefficients\n")
  cat("\n")
  invisible(x)
}
