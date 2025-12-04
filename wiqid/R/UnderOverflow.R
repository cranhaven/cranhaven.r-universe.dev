
# Utilities to handle calculations on log(probabilities) avoiding under/overflow
#   and NOT exported.

# See http://www.mikemeredith.net/blog/2017/UnderOverflow.htm
# and http://www.mikemeredith.net/blog/2017/UnderOverflow_2.htm


# logSumExp: sum probabilities without over/underflow
# log_p is a vector with log(p); return value is log(sum(p)), scalar
# Note the check for all-zero vectors of p, which would otherwise produce NaN
logSumExp <- function(log_p) {
  if(max(log_p) == -Inf)
    return(-Inf)
  p_max <- which.max(log_p)
  log1p(sum(exp(log_p[-p_max] - max(log_p)))) + max(log_p)
}
# .......................................................................

# log1minusExp: get 1 - p without over/underflow
# log_p is a vector with log(p); return value is vector with log(1 - p)
log1minusExp <- function(log_p)
  ifelse(log_p > log(0.5), log(-expm1(log_p)), log1p(-exp(log_p)))
# .........................................................................

# logAddExp: add together 2 vectors of probabilities without under/overflow
# logp1 and logp2 are vectors with log(p1) and log(p2); returns log(p1 + p2)
# avoids calculations if either p1 or p2 or both are zero.
logAddExp <- function(logp1, logp2) {
  bigger <- pmax(logp1, logp2)
  smaller <- pmin(logp1, logp2)
  fix <- smaller > -Inf
  bigger[fix] <- log1p(exp(smaller[fix] - bigger[fix])) + bigger[fix]
  return(bigger)
}
# .........................................................................

# logMatMultexp: matrix multiplication using matrices of logs
# logA and logB are matrices with log(A) and log(B); returns log(A %*% B)
logMatMultExp <- function(logA, logB) {
  if(ncol(logA) != nrow(logB))
    stop("non-conformable matrices")
  logX <- matrix(NA_real_, nrow(logA), ncol(logB))
  for(i in 1:nrow(logX))
    for(j in 1:ncol(logX))
      logX[i,j] <- logSumExp(logA[i, ] + logB[, j])
  return(logX)
}
# .........................................................................

