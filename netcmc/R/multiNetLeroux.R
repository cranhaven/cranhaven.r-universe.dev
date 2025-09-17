multiNetLeroux = function(formula, 
                        data,
                        trials = NULL,
                        family = "gaussian",
                        squareSpatialNeighbourhoodMatrix,
                        spatialAssignment,
                        W, 
                        numberOfSamples = 10, 
                        burnin = 0, 
                        thin = 1,
                        seed = 1, 
                        trueBeta = NULL,
                        trueSpatialRandomEffects = NULL,
                        trueURandomEffects = NULL, 
                        trueSpatialTauSquared = NULL, 
                        trueSpatialRho = NULL, 
                        trueVarianceCovarianceU = NULL,
                        trueSigmaSquaredE = NULL,
                        covarianceBetaPrior = 10^5,
                        a1 = 0.001, 
                        b1 = 0.001, 
                        xi = NULL, 
                        omega = NULL, 
                        a3 = 0.001, 
                        b3 = 0.001, 
                        centerSpatialRandomEffects = TRUE, 
                        centerURandomEffects = TRUE) {
  
  multiNetLerouxCall = match.call() 
  
  if(!family %in% c("gaussian", "binomial", "poisson")){
    stop("The family specified is not gaussian, binomial or poisson!")
  } 
  
  standardizedCovariates = getStandardizedCovariates(formula, data)
  y = as.vector(standardizedCovariates$y)
  standardizedX = standardizedCovariates$standardizedX
  numberOfResponses = length(y) / nrow(standardizedX)
  
  if(is.null(xi)) {
    xi = numberOfResponses + 1
  } 
  if(is.null(omega)) {
    omega = diag(rep(1, numberOfResponses))
  } 
  
  if(family == "gaussian"){
    
    output = multivariateGaussianNetworkLerouxMH(formula = formula, 
                                               data = data, 
                                               squareSpatialNeighbourhoodMatrix = squareSpatialNeighbourhoodMatrix, 
                                               spatialAssignment = spatialAssignment, 
                                               W = W, 
                                               numberOfSamples = numberOfSamples, 
                                               burnin = burnin, 
                                               thin = thin, 
                                               seed = seed, 
                                               trueBeta = trueBeta,
                                               trueSpatialRandomEffects = trueSpatialRandomEffects,
                                               trueURandomEffects = trueURandomEffects, 
                                               trueSpatialTauSquared = trueSpatialTauSquared, 
                                               trueSpatialRho = trueSpatialRho, 
                                               trueVarianceCovarianceU = trueVarianceCovarianceU,
                                               trueSigmaSquaredE = trueSigmaSquaredE,
                                               covarianceBetaPrior = covarianceBetaPrior,
                                               a1 = a1, 
                                               b1 = b1, 
                                               xi = xi, 
                                               omega = omega,  
                                               a3 = a3, 
                                               b3 = b3, 
                                               centerSpatialRandomEffects = centerSpatialRandomEffects, 
                                               centerURandomEffects = centerURandomEffects)
    
    results = list(call = multiNetLerouxCall,
                   y = output$y,
                   X = output$X,
                   standardizedX = output$standardizedX,
                   squareSpatialNeighbourhoodMatrix = output$squareSpatialNeighbourhoodMatrix ,
                   spatialAssignment = output$spatialAssignment ,
                   W = output$W,
                   samples = cbind(output$betaSamples, output$spatialTauSquaredSamples, 
                                   output$spatialRhoSamples, output$varianceCovarianceUSamples,
                                   output$sigmaSquaredESamples),
                   betaSamples = output$betaSamples,
                   spatialTauSquaredSamples = output$spatialTauSquaredSamples,
                   spatialRhoSamples = output$spatialRhoSamples,
                   varianceCovarianceUSamples = output$varianceCovarianceUSamples,
                   sigmaSquaredESamples = output$sigmaSquaredESamples,
                   spatialRandomEffectsSamples = output$spatialRandomEffectsSamples,
                   uRandomEffectsSamples = output$uRandomEffectsSamples,
                   acceptanceRates = c(output$betaAcceptanceRate, output$spatialTauSquaredAcceptanceRate,
                                       output$spatialRhoAcceptanceRate, output$varianceCovarianceUAcceptanceRate,
                                       output$sigmaSquaredEAcceptanceRate),
                   spatialRandomEffectsAcceptanceRate = output$spatialRandomEffectsAcceptanceRate,
                   uRandomEffectsAcceptanceRate = output$uRandomEffectsAcceptanceRate,
                   timeTaken = output$timeTaken,
                   burnin = output$burnin,
                   thin = output$thin,
                   DBar = output$DBar,
                   posteriorDeviance = output$posteriorDeviance,
                   posteriorLogLikelihood = output$posteriorLogLikelihood,
                   pd = output$pd,
                   DIC = output$DIC)
    
  } else if(family == "poisson") {
    
    output =  multivariatePoissonNetworkLeroux(formula = formula, 
                                               data = data, 
                                               squareSpatialNeighbourhoodMatrix = squareSpatialNeighbourhoodMatrix, 
                                               spatialAssignment = spatialAssignment, 
                                               W = W, 
                                               numberOfSamples = numberOfSamples, 
                                               burnin = burnin, 
                                               thin = thin,
                                               seed = seed, 
                                               trueBeta = trueBeta,
                                               trueSpatialRandomEffects = trueSpatialRandomEffects,
                                               trueURandomEffects = trueURandomEffects, 
                                               trueSpatialTauSquared = trueSpatialTauSquared, 
                                               trueSpatialRho = trueSpatialRho, 
                                               trueVarianceCovarianceU = trueVarianceCovarianceU,
                                               covarianceBetaPrior = covarianceBetaPrior,
                                               a1 = a1, 
                                               b1 = b1, 
                                               xi = xi, 
                                               omega = omega,  
                                               centerSpatialRandomEffects = centerSpatialRandomEffects, 
                                               centerURandomEffects = centerURandomEffects)
    
    results = list(call = multiNetLerouxCall,
                   y = output$y,
                   X = output$X,
                   standardizedX = output$standardizedX,
                   squareSpatialNeighbourhoodMatrix = output$squareSpatialNeighbourhoodMatrix ,
                   spatialAssignment = output$spatialAssignment ,
                   W = output$W,
                   samples = cbind(output$betaSamples, output$spatialTauSquaredSamples, 
                                   output$spatialRhoSamples, output$varianceCovarianceUSamples),
                   betaSamples = output$betaSamples,
                   spatialTauSquaredSamples = output$spatialTauSquaredSamples,
                   spatialRhoSamples = output$spatialRhoSamples,
                   varianceCovarianceUSamples = output$varianceCovarianceUSamples,
                   spatialRandomEffectsSamples = output$spatialRandomEffectsSamples,
                   uRandomEffectsSamples = output$uRandomEffectsSamples,
                   acceptanceRates = c(output$betaAcceptanceRate, output$spatialTauSquaredAcceptanceRate,
                                       output$spatialRhoAcceptanceRate, output$varianceCovarianceUAcceptanceRate),
                   spatialRandomEffectsAcceptanceRate = output$spatialRandomEffectsAcceptanceRate,
                   uRandomEffectsAcceptanceRate = output$uRandomEffectsAcceptanceRate,
                   timeTaken = output$timeTaken,
                   burnin = output$burnin,
                   thin = output$thin,
                   DBar = output$DBar,
                   posteriorDeviance = output$posteriorDeviance,
                   posteriorLogLikelihood = output$posteriorLogLikelihood,
                   pd = output$pd,
                   DIC = output$DIC)
    
  } else {
    
    output = multivariateBinomialNetworkLeroux(formula = formula,
                                                data = data,
                                                trials = trials,
                                                squareSpatialNeighbourhoodMatrix = squareSpatialNeighbourhoodMatrix,
                                                spatialAssignment = spatialAssignment,
                                                W = W, 
                                                numberOfSamples = numberOfSamples, 
                                                burnin = burnin, 
                                                thin = thin,
                                                seed = seed, 
                                                trueBeta = trueBeta,
                                                trueSpatialRandomEffects = trueSpatialRandomEffects,
                                                trueURandomEffects = trueURandomEffects, 
                                                trueSpatialTauSquared = trueSpatialTauSquared, 
                                                trueSpatialRho = trueSpatialRho, 
                                                trueVarianceCovarianceU = trueVarianceCovarianceU,
                                                covarianceBetaPrior = covarianceBetaPrior,
                                                a1 = a1, 
                                                b1 = b1, 
                                                xi = xi, 
                                                omega = omega, 
                                                centerSpatialRandomEffects = centerSpatialRandomEffects, 
                                                centerURandomEffects = centerURandomEffects)
    
    results = list(call = multiNetLerouxCall,
                   y = output$y,
                   X = output$X,
                   standardizedX = output$standardizedX,
                   squareSpatialNeighbourhoodMatrix = output$squareSpatialNeighbourhoodMatrix ,
                   spatialAssignment = output$spatialAssignment ,
                   W = output$W,
                   samples = cbind(output$betaSamples, output$spatialTauSquaredSamples, 
                                   output$spatialRhoSamples, output$varianceCovarianceUSamples),
                   betaSamples = output$betaSamples,
                   spatialTauSquaredSamples = output$spatialTauSquaredSamples,
                   spatialRhoSamples = output$spatialRhoSamples,
                   varianceCovarianceUSamples = output$varianceCovarianceUSamples,
                   spatialRandomEffectsSamples = output$spatialRandomEffectsSamples,
                   uRandomEffectsSamples = output$uRandomEffectsSamples,
                   acceptanceRates = c(output$betaAcceptanceRate, output$spatialTauSquaredAcceptanceRate,
                                       output$spatialRhoAcceptanceRate, output$varianceCovarianceUAcceptanceRate),
                   spatialRandomEffectsAcceptanceRate = output$spatialRandomEffectsAcceptanceRate,
                   uRandomEffectsAcceptanceRate = output$uRandomEffectsAcceptanceRate,
                   timeTaken = output$timeTaken,
                   burnin = output$burnin,
                   thin = output$thin,
                   DBar = output$DBar,
                   posteriorDeviance = output$posteriorDeviance,
                   posteriorLogLikelihood = output$posteriorLogLikelihood,
                   pd = output$pd,
                   DIC = output$DIC)
    
  }
  
  class(results) = "netcmc"
  
  return(results)
  
} 