smooth_eval <-
function(param, formula, data, delta, maxit, tol, residuals, pred, response, ns, p, h = NULL, lambda = NULL, target = c("lambda", "h", "both")){

  target <- match.arg(target)
  
  if(target == "both"){
    h <- param[1]
    lambda <- param[-1]
  }
  
  if(target == "lambda"){
    lambda <- param
  }
  
  if(target == "h"){
    h <- param
  }
 
  regi <- regIt(formula = formula, data = data, delta = delta, h = h, maxit = maxit, tol = tol, residuals = residuals, pred = pred, response = response, lambda = lambda, ns = ns, p = p)
  if(length(regi$reg)==1 && is.na(regi$reg)) return(10^10)
  
  plscore <- pl(bw0 = h, bwreg = regi$reg, response = response, dat = data, delta = delta, lambda = lambda, ns = ns)
  
  # aic
  aic <- ifelse(plscore == -10^10, 10^10, - 2 * plscore + 2 * sum(influence(regi$reg)))
  
  
  return(aic)
}
