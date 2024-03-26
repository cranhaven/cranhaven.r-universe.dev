#' Simple Bayesian modeling for combining Gaussian dates
#'
#' @author Anne Philippe, Marie-Anne Vibet
#'
#' @references
#'
#' @examples
#'
#' @export


combinationWithOutliers_Gauss <- function(M, s, refYear=NULL, outliersIndivVariance, outliersBernouilliProba, 
                                   studyPeriodMin, studyPeriodMax, 
                                   numberChains = 2, numberAdapt = 10000, numberUpdate = 10000, 
                                   variable.names = c('theta'), numberSample = 50000, thin = 10){
  
  # Checking inputs
  if(length(M)!=length(s)) stop("Vector of measurements and vector of errors should have the same length")
  if( !is.null(refYear) ){
    if(length(M)!=length(refYear)) stop("Vector of measurements and vector of refYear should have the same length")
  }
  if(length(M)!=length(outliersIndivVariance)) stop("Vector of measurements and vector of outliersIndivVariance should have the same length")
  if(length(M)!=length(outliersBernouilliProba)) stop("Vector of measurements and vector of outliersBernouilliProba should have the same length")
  
  # Conversion in BC/AD format
  if (!is.null(refYear)){
    M = refYear - M
  }
  
  # Bayesian model
  model.file <- system.file(package="ArchaeoChron", "model", "combinationWithOutliers_Gauss.txt")
  
  jags <- jags.model(file = model.file, data = list('Nbobs' = length(M),'M' = M,'s'=s, 'sigma.delta' = outliersIndivVariance,'p'= outliersBernouilliProba,'ta'=studyPeriodMin,'tb'=studyPeriodMax),
                     n.chains = numberChains, n.adapt = numberAdapt)
  print('Update period')
  update(object = jags, n.iter = numberUpdate)
  print('Acquire period')
  MCMCSample = coda.samples(model = jags, variable.names = variable.names, n.iter = numberSample, thin =thin)
  
  return(MCMCSample)
}


