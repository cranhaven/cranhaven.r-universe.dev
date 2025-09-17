summary.netcmc = function(object, ...){
  
  # firstly print out the call
  cat("\nCall:\n\n", paste(deparse(object$call), sep="\n", collapse="\n"), "\n")
  
  # print out coeffiecient estimation section
  numberOfTableRows = ncol(object$samples)
  columnNamesInCoefficentTable = c("Mean", "Variance", "2.5%", "Median", "97.5%", "ESS", "Accept. %")
  coefficentNames = colnames(object$samples)
  coefficentTableCells = matrix(NA, numberOfTableRows, length(columnNamesInCoefficentTable), dimnames = list(coefficentNames, columnNamesInCoefficentTable))
  
  mean = apply(object$samples, 2, mean)
  # see Congdon 2010 "Applied Bayesian Hierarchical Methods"
  variance = apply(object$samples, 2, function(x) sum((x - mean(x))^2 / length(x)))
  q2.5 = apply(object$samples, 2, quantile, probs = 0.025)
  median = apply(object$samples, 2, median)
  q97.5 = apply(object$samples, 2, quantile, probs = 0.975)
  ess = effectiveSize(object$samples)
  acceptanceRates = object$acceptanceRates
  
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
  gewekeDiagnostic = geweke.diag(object$samples)[[1]]
  diagnosticTableCells[, 1] = round(gewekeDiagnostic, 3)
  
  cat("\nMCMC Diagnostics:\n\n" )
  print(diagnosticTableCells)
  
  cat("\nMCMC Model Information:\n\n" )
  cat("Number of observations:", paste(length(object$y)), "\n")
  
  if(is.null(object$spatialRandomEffectsSamples)){
    
  } else {
    cat("Number of spatial/group random effects:", paste(ncol(object$spatialRandomEffectsSamples)), "\n")
  }
  
  if(is.null(object$uRandomEffectsSamples)){
    
  } else {
    cat("Number of network random effects:", paste(ncol(object$uRandomEffectsSamples)), "\n\n")
  }

  if(is.null(object$DIC)){
    
  } else {
    cat("DIC:", paste(round(object$DIC, 3)))
    cat(",\tD.bar:", paste(round(object$DBar,3)), "\n")
    cat("pd:", paste(round(object$pd,3)))
    cat(",\tlog likelihood:", paste(round(object$posteriorLogLikelihood,3)), "\n")
  }
  cat("\nMCMC Information:\n\n" )
  
  numberOfSamples = nrow(object$samples)
  timeTaken = round(object$timeTaken[[1]], 2)
  cat("Number of samples:", paste(numberOfSamples))
  burnin = object$burnin
  cat(",\tBurn-in:", paste(burnin), "\n")
  thin = object$thin
  cat("Thin:", paste(thin))
  cat(",\t Time Taken:", paste(timeTaken), "Secs \n")
}