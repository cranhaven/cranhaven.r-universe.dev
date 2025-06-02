offset_convergence <- function(r, xind, dgz)
{
  d <- ncol(xind)
  Vmat <- xind * dgz
  Vmat[abs(Vmat) < .Machine$double.eps] <- 0 # Numerical instability otherwise
  Q <- qr.Q(qr(Vmat), complete = TRUE)  
  off <- sqrt(mean((t(Q[,1:d]) %*% r)^2) / mean((t(Q[,-(1:d)]) %*% r)^2))
  return(off)
}

