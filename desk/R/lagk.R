lagk = function(u, lag = 1, delete = TRUE){
  n = length(u)
  out = matrix(NA, n, lag)
  for (i in 1:lag){
    out[(i+1):n, i] = u[1:(n-i)]
  }
  out = cbind(u, out)
  dimnames(out) = list(1:n, paste0("lag", 0:lag))
  if (delete) out = out[(lag+1):n,]
  return(out)
}
