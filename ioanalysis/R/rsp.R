rsp <- function(io){
  if(!"InputOutput" %in% class(io)) stop('io should be of "InputOutput" class. See ?as.inputoutput')
  A <- io$A
  X <- io$X
  M <- import.total(io)
  E <- export.total(io)
  ##############################
  ## Calculating calculations ##
  ##############################
  n <- length(X)
  p <- (X - E) / (X - E + M)
  phat <- matrix(p, ncol = n, nrow = n) # A faster computation than diag(p)
  Anew <- phat * A
  Anew
}