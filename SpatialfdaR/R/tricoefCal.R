tricoefCal <- function (pts, tri) {
  #  TRICOEFCAL compute the coefficient matrix TRICOEF
  #  required to test of a point is inside a triangle
  
  #  Last modified 21 December 2020 by Jim Ramsay.
  
  ntri <- dim(tri)[[1]]
  
  #  compute coefficients for computing barycentric coordinates if
  #  needed
  
  tricoef <- matrix(0, nrow=ntri, ncol=4)
  tricoef[,1] <- pts[tri[,1],1]-pts[tri[,3],1]
  tricoef[,2] <- pts[tri[,2],1]-pts[tri[,3],1]
  tricoef[,3] <- pts[tri[,1],2]-pts[tri[,3],2]
  tricoef[,4] <- pts[tri[,2],2]-pts[tri[,3],2]
  detT <- matrix((tricoef[,1]*tricoef[,4] - tricoef[,2]*tricoef[,3]),ncol=1)
  tricoef <- tricoef/(detT %*% matrix(1,nrow=1,ncol=4))
  
  return(tricoef)
}
