
#===================================================================#
#  A function that computes integral of M-splines to give I-splines #
#===================================================================#

ispline <- function(x, knots1, m) {
  
  # x is a vector of values to compute the I-spline at
  # i ith I-spline, k= knots vector, m= order
  # m <- order 4
  # k <-knots (the knots used in computing M-splines of order 5 should be used
  # in computing the I-splines)
  dk <- knots1[2]-knots1[1]
  k  <- c(knots1[1]-dk*(4:1), knots1, knots1[length(knots1)]+dk*(1:4))
  
  msplinedesign5 <- dmsplinedesign (x, knots1, 5, deriv=0) # A matrix of order 5 to be used in computing the I-splines
  d <- length(k)-m-1 # number of columuns for the I-sipline matrices
  resu <- matrix(0, length(x), length(k)-m-1)
  for (j in 1:length(x))
    for (i in 1:(length(k)-m-1))
      if (x[j]>k[i+m+1]){
        resu[j,i] <- 1
      } else if (x[j] < k[i+1]) {
        resu[j,i]<- 0
      } else if (x[j]> k[i+1] && x[j]<=k[i+1+1]) {
        resu[j,i] <- (k[i+m+1+1]-k[i+1])*(msplinedesign5[j,i+1])/(m+1)
      } else if (x[j]>k[i+1+1] && x[j]<=k[i+2+1]) {
        resu[j,i] <- (k[i+m+1+1]-k[i+1])*(msplinedesign5[j,i+1])/(m+1) + (k[i+1+m+1+1]-k[i+1+1])*(msplinedesign5[j,i+1+1])/(m+1)
      } else if (x[j]>k[i+2+1] && x[j] <= k[i+3+1]){
        resu[j,i] <- (k[i+m+1+1]-k[i+1])*(msplinedesign5[j,i+1])/(m+1) + (k[i+1+m+1+1]-k[i+1+1])*(msplinedesign5[j,i+1+1])/(m+1) + (k[i+2+m+1+1]-k[i+2+1])*(msplinedesign5[j,i+2+1])/(m+1)
      } else
        #if (x>=k[i+3] && x < k[i+4])
      {
        resu[j,i] <- (k[i+m+1+1]-k[i+1])*(msplinedesign5[j,i+1])/(m+1) + (k[i+1+m+1+1]-k[i+1+1])*(msplinedesign5[j,i+1+1])/(m+1) + (k[i+2+m+1+1]-k[i+2+1])*(msplinedesign5[j,i+2+1])/(m+1) + (k[i+3+m+1+1]-k[i+3+1])*(msplinedesign5[j,i+3+1])/(m+1)
        #} else if (x>=k[i+4] && x < k[i+5]) {
        #resu <- (k[i+m+1]-k[i])*(mspline(x, knots, i, m+1))/(m+1) + (k[i+1+m+1]-k[i+1])*(mspline(x, knots, i+1, m+1))/(m+1) + (k[i+2+m+1]-k[i+2])*(mspline(x, knots, i+2, m+1))/(m+1) + (k[i+3+m+1]-k[i+3])*(mspline(x, knots, i+3, m+1))/(m+1) + (k[i+4+m+1]-k[i+3])*(mspline(x, knots, i+4, m+1))/(m+1)
        #} else {
        #resu <- (k[i+m+1]-k[i])*(mspline(x, knots, i, m+1))/(m+1) + (k[i+1+m+1]-k[i+1])*(mspline(x, knots, i+1, m+1))/(m+1) + (k[i+2+m+1]-k[i+2])*(mspline(x, knots, i+2, m+1))/(m+1) + (k[i+3+m+1]-k[i+3])*(mspline(x, knots, i+3, m+1))/(m+1) + (k[i+4+m+1]-k[i+4])*(mspline(x, knots, i+4, m+1))/(m+1) + (k[i+5+m+1]-k[i+4])*(mspline(x, knots, i+5, m+1))/(m+1)
      }
  return(resu[,1:(d-1)])
}
