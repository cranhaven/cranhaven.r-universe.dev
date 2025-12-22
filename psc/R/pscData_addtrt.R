#' A function that includes a treatment indicator when multiple
#' treatment comparisons are required
#'
#' The purpose of this function is to organise a treatment indicator where
#' multiple treatment comparisons are being evaluated. This acts as a
#' sub-function to the pscData.R function.
#'
#' @param DC a data cohort to be 'cleaned'
#' @param trt a treatment indicator
#' @return a dataset which is checked and compatible with the CFM
pscData_addtrt <- function(DC,trt){
  DC <- data.frame(cbind(DC,trt))
  trt.nm <- names(DC)[ncol(DC)]
  ret <- list(DC,trt.nm)
}





