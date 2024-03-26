#' Bayesian modeling for combining Gaussian dates.
#'
#' @author Anne Philippe, Marie-Anne Vibet
#'
#' @references
#'
#' @examples
#'
#' @export


eventModel_Gauss <- function(M, s, refYear=NULL, studyPeriodMin, studyPeriodMax, 
                             numberChains = 2, numberAdapt = 10000, numberUpdate = 10000, 
                             variable.names = c('theta'), numberSample = 50000, thin = 10){
  
  # Checking inputs
  if(length(M)!=length(s)) stop("Vector of measurements and vector of errors should have the same length")
  if( !is.null(refYear) ){
    if(length(M)!=length(refYear)) stop("Vector of measurements and vector of refYear should have the same length")
  }
  
  # Data
  N=length(M)
  s02 <- 1/mean(1/s^2)
  
  # Conversion in BC/AD format
  if (!is.null(refYear)){
    M = refYear - M
  }
  
  # Bayesian model
  model.file <- system.file(package="ArchaeoChron", "model", "eventModel_Gauss.txt")

  jags <- jags.model(file =model.file, data = list('N' = N,'M' = M,'s'=s,'s02'=s02,'ta'=studyPeriodMin,'tb'=studyPeriodMax),
                     n.chains = numberChains, n.adapt = numberAdapt)
  print('Update period')
  update(object = jags, n.iter = numberUpdate)
  print('Acquire period')
  MCMCSample = coda.samples(model = jags, variable.names = variable.names, n.iter = numberSample, thin =thin)
  
  return(MCMCSample)
}


