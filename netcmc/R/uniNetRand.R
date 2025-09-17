uniNetRand = function(formula, 
                      data, 
                      trials = NULL,
                      family = "gaussian", 
                      groupAssignment,
                      W, 
                      numberOfSamples = 10, 
                      burnin = 0, 
                      thin = 1,
                      seed = 1, 
                      trueBeta = NULL,
                      trueGroupRandomEffects = NULL,
                      trueURandomEffects = NULL,
                      trueTauSquared = NULL,
                      trueSigmaSquaredU = NULL,
                      trueSigmaSquaredE = NULL,
                      covarianceBetaPrior = 10^5,
                      a1 = 0.001,
                      b1 = 0.001,
                      a2 = 0.001,
                      b2 = 0.001,
                      a3 = 0.001, 
                      b3 = 0.001,
                      centerGroupRandomEffects = TRUE, 
                      centerURandomEffects = TRUE) {
  
  UniNetRandCall = match.call() 
  
  spatialAssignment = groupAssignment
  trueSpatialRandomEffects = trueGroupRandomEffects
  trueSpatialTauSquared = trueTauSquared
  centerSpatialRandomEffects = centerGroupRandomEffects
  
  if(!family %in% c("gaussian", "binomial", "poisson")){
    stop("The family specified is not gaussian, binomial or poisson!")
  } 
  
  squareSpatialNeighbourhoodMatrix = matrix(0, nrow = ncol(spatialAssignment), ncol = ncol(spatialAssignment))
  trueSpatialRho = 0
  
  if(family == "gaussian"){
    
    output = univariateGaussianNetworkLerouxMH(formula = formula, 
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
                                             trueSigmaSquaredU = trueSigmaSquaredU,
                                             trueSigmaSquaredE = trueSigmaSquaredE,
                                             covarianceBetaPrior = covarianceBetaPrior,
                                             a1 = a1, 
                                             b1 = b1, 
                                             a2 = a2, 
                                             b2 = b2, 
                                             a3 = a3, 
                                             b3 = a3, 
                                             centerSpatialRandomEffects = centerGroupRandomEffects, 
                                             centerURandomEffects = centerURandomEffects)
    
    results = list(call = UniNetRandCall,
                   y = output$y,
                   X = output$X,
                   standardizedX = output$standardizedX,
                   squareSpatialNeighbourhoodMatrix = output$squareSpatialNeighbourhoodMatrix ,
                   spatialAssignment = output$spatialAssignment ,
                   W = output$W,
                   samples = cbind(output$betaSamples, "tauSquared" = output$spatialTauSquaredSamples, 
                                   "sigmaSquaredU" = output$sigmaSquaredUSamples,
                                   "sigmaSquaredE" = output$sigmaSquaredESamples),
                   betaSamples = output$betaSamples,
                   tauSquaredSamples = output$spatialTauSquaredSamples,
                   sigmaSquaredUSamples = output$sigmaSquaredUSamples,
                   sigmaSquaredESamples = output$sigmaSquaredESamples,
                   groupRandomEffectsSamples = output$spatialRandomEffectsSamples,
                   uRandomEffectsSamples = output$uRandomEffectsSamples,
                   acceptanceRates = c(output$betaAcceptanceRate, output$spatialTauSquaredAcceptanceRate,
                                       output$sigmaSquaredUAcceptanceRate,
                                       output$sigmaSquaredEAcceptanceRate),
                   groupRandomEffectsAcceptanceRate = output$spatialRandomEffectsAcceptanceRate,
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
    
    output = univariatePoissonNetworkLeroux(formula = formula, 
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
                                              trueSigmaSquaredU = trueSigmaSquaredU,
                                              covarianceBetaPrior = covarianceBetaPrior,
                                              a1 = a1, 
                                              b1 = b1, 
                                              a2 = a2, 
                                              b2 = b2, 
                                              centerSpatialRandomEffects = centerGroupRandomEffects, 
                                              centerURandomEffects = centerURandomEffects)
    
    results = list(call = UniNetRandCall,
                   y = output$y,
                   X = output$X,
                   standardizedX = output$standardizedX,
                   squareSpatialNeighbourhoodMatrix = output$squareSpatialNeighbourhoodMatrix ,
                   spatialAssignment = output$spatialAssignment ,
                   W = output$W,
                   samples = cbind(output$betaSamples, "tauSquared" = output$spatialTauSquaredSamples, 
                                   "sigmaSquaredU" = output$sigmaSquaredUSamples),
                   betaSamples = output$betaSamples,
                   tauSquaredSamples = output$spatialTauSquaredSamples,
                   sigmaSquaredUSamples = output$sigmaSquaredUSamples,
                   groupRandomEffectsSamples = output$spatialRandomEffectsSamples,
                   uRandomEffectsSamples = output$uRandomEffectsSamples,
                   acceptanceRates = c(output$betaAcceptanceRate, output$spatialTauSquaredAcceptanceRate,
                                       output$sigmaSquaredUAcceptanceRate),
                   groupRandomEffectsAcceptanceRate = output$spatialRandomEffectsAcceptanceRate,
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
    
    output = univariateBinomialNetworkLeroux(formula = formula, 
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
                                               trueSigmaSquaredU = trueSigmaSquaredU,
                                               covarianceBetaPrior = covarianceBetaPrior,
                                               a1 = a1, 
                                               b1 = b1, 
                                               a2 = a2, 
                                               b2 = b2, 
                                               centerSpatialRandomEffects = centerGroupRandomEffects, 
                                               centerURandomEffects = centerURandomEffects)
    
    results = list(call = UniNetRandCall,
                   y = output$y,
                   X = output$X,
                   standardizedX = output$standardizedX,
                   spatialAssignment = output$spatialAssignment ,
                   W = output$W,
                   samples = cbind(output$betaSamples, "tauSquared" = output$spatialTauSquaredSamples,
                                   "sigmaSquaredU" = output$sigmaSquaredUSamples),
                   betaSamples = output$betaSamples,
                   tauSquaredSamples = output$spatialTauSquaredSamples,
                   sigmaSquaredUSamples = output$sigmaSquaredUSamples,
                   groupRandomEffectsSamples = output$spatialRandomEffectsSamples,
                   uRandomEffectsSamples = output$uRandomEffectsSamples,
                   acceptanceRates = c(output$betaAcceptanceRate, output$spatialTauSquaredAcceptanceRate,
                                       output$sigmaSquaredUAcceptanceRate),
                   groupRandomEffectsAcceptanceRate = output$spatialRandomEffectsAcceptanceRate,
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