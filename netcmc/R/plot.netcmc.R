plot.netcmc = function(x, ...) {
  
  covariateNames =  colnames(x$samples)
  
  oldPar = par(no.readonly = TRUE)
  on.exit(par(oldPar))
  
  par(mfrow = c(1,3))
  
  for(i in 1:ncol(x$samples)){
    
    buffer = 0.1 * (max(x$samples[, i]) - min(x$samples[, i]))
    maxValue = x$samples[, i] + buffer
    minValue = x$samples[, i] - buffer
    
    plot(x = 1:length(x$samples[, i]), y = x$samples[, i], type = "l", xlab = "Iteration", ylab = covariateNames[i], ylim = range(minValue, maxValue))
    
    plot(density(x$samples[, i]), main = paste(covariateNames[i]), xlim = range(minValue, maxValue))
    
    acf(x$samples[, i], main = paste(covariateNames[i]))
    
  } 
  
}
