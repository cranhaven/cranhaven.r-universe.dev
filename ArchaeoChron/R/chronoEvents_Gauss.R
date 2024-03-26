#' Bayesian chronologies of Gaussian dates using the Event Model.
#'
#' @author Anne Philippe, Marie-Anne Vibet
#'
#' @references
#'
#' @examples
#'
#' @export


chronoEvents_Gauss <- function(M, s, refYear=NULL, measurementsPerEvent, studyPeriodMin, studyPeriodMax, 
                             numberChains = 2, numberAdapt = 10000, numberUpdate = 10000, 
                             variable.names = c('theta'), numberSample = 50000, thin = 10){
  
  # Checking inputs
  if(length(M)!=length(s)) stop("Vector of measurements and vector of errors should have the same length")
  if(sum(measurementsPerEvent)!=length(M)) stop("All the measurements should be affected to an event")
  
  if( !is.null(refYear) ){
    if(length(M)!=length(refYear)) stop("Vector of measurements and vector of refYear should have the same length")
  }
  
  # Data
  NbEvents = length(measurementsPerEvent)
  pos =  1 + c(0 , cumsum(measurementsPerEvent) )
  s02 = 1:NbEvents 
  for (i in  1:NbEvents){
    s02[i]= 1/mean( 1/ s[pos[i]: (pos[i+1] -1)]^2 ) 
    }
  
  # Conversion in BC/AD format
  if (!is.null(refYear)){
    M = refYear - M
  }
  
  # Bayesian model
  model.file <- system.file(package="ArchaeoChron", "model", "chronoEvents_Gauss.txt")    

  print('Modeling done with JAGS following')  
  jags <- jags.model(file = model.file, data = list('NbEvents' = NbEvents,'M' = M,'s'=s, 'pos' = pos, 's02'=s02,'ta'=studyPeriodMin,'tb'=studyPeriodMax),
                     n.chains = numberChains, n.adapt = numberAdapt)
  print('Update period')
  update(object = jags, n.iter = numberUpdate)
  print('Acquire period')
  MCMCSample = coda.samples(model = jags, variable.names = variable.names, n.iter = numberSample, thin =thin)
  
  return(MCMCSample)
  
}


