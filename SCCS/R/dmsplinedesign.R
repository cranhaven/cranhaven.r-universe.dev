
############################################
# M-spline functions and their Derivatives #
############################################

dmsplinedesign <- function(x, knots1, m, deriv) { # x = event time knots=knots, m=order, deriv = the order of derivative should be less than the order of the M-spline
  # 
  dk <- knots1[2]-knots1[1]
  knots <- c(knots1[1]-dk*((m-1):1), knots1, knots1[length(knots1)]+dk*(1:(m-1)))
  
  dbspline<- splineDesign(knots, x, ord=m, derivs= rep(deriv, length(x)), outer.ok = TRUE) # Design matrix of deriv'th derivative of B-spline basis functions
  
  for (i in 1:(length(knots)-m)){
    dbspline[,i] <- dbspline[,i]*(m/(knots[i+m]-knots[i]))
  }
  return(dbspline)
  
}
