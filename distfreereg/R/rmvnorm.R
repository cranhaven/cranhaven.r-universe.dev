rmvnorm <- function(n, reps, mean = rep(0, nrow(SqrtSigma)), SqrtSigma = NULL){
  stopifnot(is.numeric(SqrtSigma))
  if(is.matrix(SqrtSigma)){
    output <- t(matrix(rnorm(reps * ncol(SqrtSigma)), nrow = reps, byrow = TRUE) %*% SqrtSigma)
  } else {
    # if(isTRUE(length(SqrtSigma) == 1)){
    #   SqrtSigma <- rep(SqrtSigma, n)
    # }
    stopifnot(length(SqrtSigma) %in% c(1, n))
    output <- matrix(rnorm(reps * n, sd = SqrtSigma), ncol = reps)
  }
  return(drop(output))
}
