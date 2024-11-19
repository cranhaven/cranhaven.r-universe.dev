univariateScore <- function(target, dataset, test, wei = NULL, targetID = -1, ncores = 1) {
  #how many tests
  nTests <- dim(dataset)[2];
  univariateModels <- NULL;
  univariateModels$pvalue <- numeric(nTests) 
  univariateModels$stat <- numeric(nTests)
  poia <- Rfast::check_data(dataset)
  if ( sum(poia) > 0 )   dataset[, poia] <- rnorm( dim(dataset)[1] )  
  
  #for way to initialize the univariateModel
  for (i in 1:nTests) {
    #arguments order for any CI test are fixed
    if ( i != targetID ) {
      test_results <- test(target, dataset, xIndex = i, csIndex = 0, wei = wei)
      univariateModels$pvalue[[ i ]] <- test_results$pvalue;
      univariateModels$stat[[ i ]] <- test_results$stat;
    } else {
      univariateModels$pvalue[[ i ]] <= log(1);
      univariateModels$stat[[ i ]] <- 0;
    }
  }
  
  if ( sum(poia>0) > 0 ) {
    univariateModels$stat[poia] <- 0
    univariateModels$pvalue[poia] <- log(1)
  }
  
  univariateModels
}
