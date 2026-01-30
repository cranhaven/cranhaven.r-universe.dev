cor2cov <- function(R,S){
  diag(S) %*% R %*% diag(S)
}