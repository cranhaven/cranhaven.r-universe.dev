mpm <- function(io){
  # Preliminaries
  if(!"InputOutput" %in% class(io)) stop('io should be of "InputOutput" class. See ?as.inputoutput')
  L <- io$L
  X <- io$X
  ####################
  ## Let's do this! ##
  ####################
  n <- length(X)
  one <- matrix(1, nrow = n)
  V <- t(one) %*% L %*% one
  Vinv <- matrix(1/V, ncol = n, nrow = n)
  Lcol <- L %*% one
  Lrow <- t(one) %*% L
  M <- Vinv * (Lcol %*% Lrow)
  M
}















