getInitialParameters = function(X) {
  
  initialParameters = abs(rnorm(ncol(X), 0, 0.1))
  
  return(initialParameters)
  
}