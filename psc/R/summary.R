#' Personalised Synthetic Controls - summary
#'
#' A generic function to provide a summary of a 'psc' object obtained from
#' pscfit.R
#'
#' @param object an object of class 'psc'
#' @param ... not used
#' @return A summary of a psc object obtained using pscSumm and a copy of the pscfit object
#' @examples
#' library(psc)
#' e4_data <-psc::e4_data
#' gemCFM <- psc::gemCFM
#' psc <- pscfit(gemCFM,e4_data)
#' summary(psc)
#' @export
summary.psc <- function(object,...){

  cat("Summary: \n \n")
  summ <-pscSumm(object$DC_clean)

  cat("\n")
  print(object)

}


