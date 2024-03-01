# A function that evaluates integral of an I-splines (first, second or third integral)

integrateIspline <- function(x, knots1, m, int) {
  
  if (int==1) {
    integratedIspline <- ispline1(x, knots1, m)
    return(integratedIspline)
  }
  
  if (int==2) {
    
    integratedIspline <- ispline2(x, knots1, m)
    return(integratedIspline)
  }
  
  integratedIspline <- ispline3(x, knots1, m)
  return(integratedIspline)
  
}

