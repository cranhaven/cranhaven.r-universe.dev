insideIndex <- function (obspts, pts, tri, tricoef=NULL)
{
  #  insideIndex returns the index of the triangle containing a point
  # (X,Y) if such a triangle exists, and NA otherwise.
  #  TRICOEF may have already been calculated for efficiency,
  #  but if the function is called with three arguments, it is calculated.
  
  #  Last modified 21 December 2020 by Jim Ramsay.
  
  eps   <- 2.2204e-016
  small <- eps
  
  ntri   <- dim(tri)[1]
  nobs   <- dim(obspts)[1]
  indtri <- matrix(1:ntri,ncol=1)
  
  #  compute coefficients for computing barycentric coordinates if needed
  
  if (is.null(tricoef))
  {
    tricoef <- matrix(0, nrow=ntri, ncol=4)
    tricoef[,1] <- pts[tri[,1],1] - pts[tri[,3],1]
    tricoef[,2] <- pts[tri[,2],1] - pts[tri[,3],1]
    tricoef[,3] <- pts[tri[,1],2] - pts[tri[,3],2]
    tricoef[,4] <- pts[tri[,2],2] - pts[tri[,3],2]
    detT <- matrix((tricoef[,1]*tricoef[,4] - 
                    tricoef[,2]*tricoef[,3]),ncol=1)
    tricoef <- tricoef/(detT %*% matrix(1,nrow=1,ncol=4))
  }
  
  #  compute barycentric coordinates
  
  onet <- matrix(1,ntri,1)
  ind  <- matrix(0,nobs,1)
  for (i in 1:nobs) {
    Xi   <- obspts[i,1]*onet - pts[tri[,3],1]
    Yi   <- obspts[i,2]*onet - pts[tri[,3],2]
    lam1 <- ( tricoef[,4]*Xi - tricoef[,2]*Yi)
    lam2 <- (-tricoef[,3]*Xi + tricoef[,1]*Yi)
    lam3 <- 1 - lam1 - lam2
    testi  <- (-small <= lam1 & lam1 <= 1+small) & 
              (-small <= lam2 & lam2 <= 1+small) & 
              (-small <= lam3 & lam3 <= 1+small)
    indi <- (1:ntri)[testi]
    if (!is.null(indi)) {
      ind[i] <- indi[1]
    } else {
      ind[i] <- NA
    }
  }
  return(ind)           
}
