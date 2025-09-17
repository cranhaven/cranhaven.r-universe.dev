getStandardizedCovariates = function(formula, data) {
  
  matchCall = getFormulaInfo(formula, data)$matchCall
  matchCallAttributes = getFormulaInfo(formula, data)$matchCallAttributes
  
  y = model.response(matchCall, "numeric")
  X = model.matrix(matchCallAttributes, matchCall)
  
  standardizedX = X
  if(any(colnames(X) == "(Intercept)")) {
    standardizedX[, -which(colnames(standardizedX) == "(Intercept)")] = apply(standardizedX[, -which(colnames(standardizedX) == "(Intercept)"), drop = F] , 2, function(x) (x - mean(x)) / sd(x))
  } 
  else {
    standardizedX = apply(standardizedX , 2, function(x) (x - mean(x)) / sd(x))
  }
 
  return(list(y = y, X = X, standardizedX = standardizedX))
  
}