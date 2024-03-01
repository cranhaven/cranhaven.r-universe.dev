
#===================================#
#   A function to Compute Design    #
# Matrix for M-splines of order k   #
#===================================#

msplinedesign <- function(x, k, m=4) { # x = event time k=knots, m=order
  
  bspline<- splineDesign(k, x, ord=m, outer.ok = FALSE) # Design matrix of B-spline basis functions
  
  for (i in 1:(length(k)-m)){
    bspline[,i] <- bspline[,i]*m/(k[i+m]-k[i])
  }
  return(bspline)
  
}