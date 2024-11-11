not.converged <- function(cur, old, tol){
  if(any(is.infinite(old))) return(TRUE)
  stopifnot(all(is.finite(cur)))
  mismatch.new <- any(cur > 1e-13 & old <= 1e-13)
  mismatch.old <- any(cur <= 1e-13 & old > 1e-13)
  same <- ifelse(cur > 1e-13 & old > 1e-13,1,0)
  diffconv <- any(ifelse(same, abs( (cur - old)/old) > tol, FALSE))
  convcheck <- (mismatch.new || mismatch.old) ||
    (diffconv)
  
  return(convcheck)
}
