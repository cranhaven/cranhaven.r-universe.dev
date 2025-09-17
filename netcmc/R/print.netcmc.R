print.netcmc = function(x, ...){
  
  # firstly print out the call
  cat("\nCall:\n\n", paste(deparse(x$call), sep="\n", collapse="\n"), "\n")
  
  # print out coeffiecient estimation section
  numberOfTableRows = ncol(x$samples)
  columnNamesInCoefficentTable = c("Mean", "Variance", "2.5%", "Median", "97.5%", "ESS", "Accept. %")
  coefficentNames = colnames(x$samples)
  coefficentTableCells = matrix(NA, numberOfTableRows, length(columnNamesInCoefficentTable), dimnames = list(coefficentNames, columnNamesInCoefficentTable))
  
  mean = apply(x$samples, 2, mean)
  # see Congdon 2010 "Applied Bayesian Hierarchical Methods"
  variance = apply(x$samples, 2, function(x) sum((x - mean(x))^2 / length(x)))
  q2.5 = apply(x$samples, 2, quantile, probs = 0.025)
  median = apply(x$samples, 2, median)
  q97.5 = apply(x$samples, 2, quantile, probs = 0.975)
  ess = effectiveSize(x$samples)
  acceptanceRates = x$acceptanceRates
  
  coefficentTableCells[, 1] = round(mean, 3)
  coefficentTableCells[, 2] = round(variance, 3)
  coefficentTableCells[, 3] = round(q2.5, 3)
  coefficentTableCells[, 4] = round(median, 3)
  coefficentTableCells[, 5] = round(q97.5, 3)
  coefficentTableCells[, 6] = round(ess)
  coefficentTableCells[, 7] = 100 * round(acceptanceRates, 4)
  
  cat("\nMCMC Coefficients:\n\n" )
  print(coefficentTableCells)
  
  # print out diagnostics section
  columnNamesInDiagnosticTable = c("Geweke Z")
  diagnosticTableCells = matrix(NA, numberOfTableRows, length(columnNamesInDiagnosticTable), dimnames = list(coefficentNames, columnNamesInDiagnosticTable))
  gewekeDiagnostic = geweke.diag(x$samples)[[1]]
  diagnosticTableCells[, 1] = round(gewekeDiagnostic, 3)
  
  cat("\nMCMC Diagnostics:\n\n" )
  print(diagnosticTableCells)
  
  cat("\nMCMC Model Information:\n\n" )
  cat("Number of observations:", paste(length(x$y)), "\n")
  
  if(is.null(x$spatialRandomEffectsSamples)){
    
  } else {
    cat("Number of spatial/group random effects:", paste(ncol(x$spatialRandomEffectsSamples)), "\n")
  }
  
  if(is.null(x$uRandomEffectsSamples)){
    
  } else {
    cat("Number of network random effects:", paste(ncol(x$uRandomEffectsSamples)), "\n\n")
  }

  if(is.null(x$DIC)){
    
  } else {
    cat("DIC:", paste(round(x$DIC, 3)))
    cat(",\tD.bar:", paste(round(x$DBar,3)), "\n")
    cat("pd:", paste(round(x$pd,3)))
    cat(",\tlog likelihood:", paste(round(x$posteriorLogLikelihood,3)), "\n")
  }
  cat("\nMCMC Information:\n\n" )
  
  numberOfSamples = nrow(x$samples)
  timeTaken = round(x$timeTaken[[1]], 2)
  cat("Number of samples:", paste(numberOfSamples))
  burnin = x$burnin
  cat(",\tBurn-in:", paste(burnin), "\n")
  thin = x$thin
  cat("Thin:", paste(thin))
  cat(",\t Time Taken:", paste(timeTaken), "Secs \n")
}