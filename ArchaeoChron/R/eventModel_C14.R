#' Bayesian modeling for combining radiocarbon dates
#'
#' @author Anne Philippe, Marie-Anne Vibet
#'
#' @references
#'
#' @examples
#'
#' @export


eventModel_C14 <- function(M, s, calibCurve='intcal13', studyPeriodMin, studyPeriodMax, 
                                    numberChains = 2, numberAdapt = 10000, numberUpdate = 10000, variable.names = c('theta'), numberSample = 50000, thin = 10){
 
   # Checking inputs
  if(length(M)!=length(s)) stop("Vector of measurements and vector of errors should have the same length")

  # Data
  N=length(M)
  
  # Estimate S02 used in the shrinkage distribution
    ## Individual calibration using BchronCalibrate 
  ages = Bchron::BchronCalibrate(ages=M,ageSds=s,calCurves=rep(calibCurve, N))
  s2hat = 1:N
    ## Estimation of the deviation of each date
  for (j in 1:N){
    step = ages[[j]]$ageGrid[2] - ages[[j]]$ageGrid[1] # time between 2 consecutive time units
    s2hat[j] = sum(step * ages[[j]]$ageGrid^2 * ages[[j]]$densities) - ( sum(step * ages[[j]]$ageGrid * ages[[j]]$densities)^2 ) 
  }
    ## Estimation of s02 for the shrinkage distribution 
  s02 = 1/mean( 1/ s2hat ) 
  
  # Calibration curve
    pathToCalCurves <- system.file(package="ArchaeoChron", "data")
    calCurveFile = paste(pathToCalCurves,'/',calibCurve,'.RData',sep='')
    if(!file.exists(calCurveFile)) stop(paste('Calibration curve file',calibCurve,'not found'))
    x = load(calCurveFile)
    calCurve = get(x)
    Xca = 1950 - calCurve[,1] # conversion in BC/AD format
    Gca = calCurve[,2]
    SDca = calCurve[,3]

  # Bayesian model
  model.file <- system.file(package="ArchaeoChron", "model", "eventModel_C14.txt")
  print('Bayesian modeling using JAGS')
  jags <- jags.model(file =model.file, data = list('M' = M,'N' = N,'s'=s,'s02'=s02,'ta'=studyPeriodMin,'tb'=studyPeriodMax, 'Xca'=Xca,'Gca'= Gca,'SDca'=SDca),
                     n.chains = numberChains, n.adapt = numberAdapt)
  print('Update period')
  update(object = jags, n.iter = numberUpdate)
  print('Acquire period')
  MCMCSample = coda.samples(model = jags, variable.names = variable.names, n.iter = numberSample, thin =thin)
  
  return(MCMCSample)
}


