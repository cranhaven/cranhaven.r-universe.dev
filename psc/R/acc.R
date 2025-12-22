#' acc
#'
#' Function to accept (or not) a proposed solution used as part of the MCMC procedure
#'
#' @param old a numeric value
#' @param new a numeric value
#' @return returns the an evaluation of old/new > U where U is a draw from the uniform distribution
#' @import stats
#' @details A function for the evaluation of two likelihoods as part of the MCMC procedure
acc <-  function(old,new){
  ret <- FALSE
  r <- runif(1)
  e <-exp(old-new)
  try(if(e>r) ret <- TRUE)
  ret
}

