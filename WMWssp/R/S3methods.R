####################################################################################################################################
### Filename:    S3methods.R
### Description: S3 methods for class 'WMWssp' returned from functions from file samplesize.R
###
###
###
###
####################################################################################################################################

print.WMWssp <- function(x, ...){

  cat("Wilcoxon-Mann-Whitney Sample Size Calculation\n \n")
  cat("Total sample size needed: ")
  cat(x$N, paste("(n_1 = ", ceiling(x$N*x$t), ", n_2 = ", ceiling(x$N*(1-x$t)), ")", sep = ""))
  cat("\n")

  call <- x$call
  temp <- strsplit(as.character(x$call), "::")
  if(length(temp[[1]]) > 1) {
    call <- temp[[1]][2]
  }

  if(as.character(call) %in% c("WMWssp_minimize", "WMWssp_maximize") ) {
    cat("Optimal allocation rate to the first group: ")
    cat(x$t)
    cat("\n")
  } else {
    cat("Allocation rate to the first group: ")
    cat(x$t)
    cat("\n")
  }

  if(x$simulation >= 0){
    cat("Simulated Power: ")
    cat(x$simulation)
    cat("\n")
  }

  if(as.character(call) %in% c("WMWssp_maximize")){
    cat("Maximal Power: ")
    cat(x$power)
    cat("\n")
  }

  cat("\nUse 'summary' for more details.")

}


summary.WMWssp <- function(object, ...){
  cat("Wilcoxon-Mann-Whitney Sample Size Calculation\n \n")
  cat("Summary\n")
  cat("Call: ")
  cat(as.character(object$call))
  cat("\n")
  cat("Type-I error (two-sided): ")
  cat(object$alpha)
  cat("\n")
  if(as.character(object$call) != "WMWssp_maximize"){
    cat("Power: ")
    cat(object$power)
    cat("\n")
  }
  if(as.character(object$call) == "WMWssp_maximize"){
    cat("Sample size: ")
    cat(object$N)
    cat("\n")
  }
  cat("\n")

  print(object$result)
}
