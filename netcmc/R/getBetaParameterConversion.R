getBetaParameterConversion = function(X, betaSamples) {
  
  numberOfColumns = ncol(X)
  
  if(any(colnames(X) == "(Intercept)") && numberOfColumns > 1) {
    
    for(i in 2:numberOfColumns) {
      betaSamples[, 1] = betaSamples[, 1] - betaSamples[, i] * (mean(X[, i]) / sd(X[, i]))
    }
    
    for(i in 2:numberOfColumns) {
      betaSamples[, i] = betaSamples[, i] / sd(X[, i])
    }
    
  } 
  else {
    
    for(i in 1:numberOfColumns) {
      betaSamples[, i] = betaSamples[, i] / sd(X[, i])
    } 
  
  }
  
  return(betaSamples = betaSamples)
  
}