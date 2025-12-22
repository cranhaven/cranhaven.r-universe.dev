#' Personalised Synthetic Controls - print
#'
#' @param x an object of class 'psc'
#' @param ... not used
#' @return printing psc results
#' @export
print.psc <- function(x,...){

  cat("Call:\n", "CFM model + beta")
  cat("\n")
  cat("\n")
  cat("Coefficients:\n")

  coef(x)

}

