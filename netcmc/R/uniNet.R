uniNet = function(formula, 
                  data, 
                  trials = NULL,
                  family = "gaussian", 
                  W, 
                  numberOfSamples = 10, 
                  burnin = 0, 
                  thin = 1,
                  seed = 1, 
                  trueBeta = NULL,
                  trueURandomEffects = NULL, 
                  trueSigmaSquaredU = NULL,
                  trueSigmaSquaredE = NULL,
                  covarianceBetaPrior = 10^5,
                  a2 = 0.001, 
                  b2 = 0.001, 
                  a3 = 0.001, 
                  b3 = 0.001,
                  centerURandomEffects = TRUE) {
  
  UniNetCall = match.call() 
  
  if(!family %in% c("gaussian", "binomial", "poisson")){
    stop("The family specified is not gaussian, binomial or poisson!")
  } 
  
  squareSpatialNeighbourhoodMatrix = matrix(0, nrow = 2, ncol = 2)
  spatialAssignment = matrix(0, nrow = nrow(data), ncol = 2)
  trueSpatialRandomEffects = rep(0, 2)
  trueSpatialTauSquared = 0
  trueSpatialRho = 0
  a1 = 0.001
  b1 = 0.001
  
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
                                              centerSpatialRandomEffects = TRUE, 
                                              centerURandomEffects = centerURandomEffects)
    
    results = list(call = UniNetCall,
                   y = output$y,
                   X = output$X,
                   standardizedX = output$standardizedX,
                   W = output$W,
                   samples = cbind(output$betaSamples, "sigmaSquaredU" = output$sigmaSquaredUSamples,
                                   "sigmaSquaredE" = output$sigmaSquaredESamples),
                   betaSamples = output$betaSamples,
                   sigmaSquaredUSamples = output$sigmaSquaredUSamples,
                   sigmaSquaredESamples = output$sigmaSquaredESamples,
                   uRandomEffectsSamples = output$uRandomEffectsSamples,
                   acceptanceRates = c(output$betaAcceptanceRate, output$sigmaSquaredUAcceptanceRate,
                                       output$sigmaSquaredEAcceptanceRate),
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
                                              centerSpatialRandomEffects = TRUE, 
                                              centerURandomEffects = centerURandomEffects)
    
    results = list(call = UniNetCall,
                   y = output$y,
                   X = output$X,
                   standardizedX = output$standardizedX,
                   W = output$W,
                   samples = cbind(output$betaSamples, "sigmaSquaredU" = output$sigmaSquaredUSamples),
                   betaSamples = output$betaSamples,
                   sigmaSquaredUSamples = output$sigmaSquaredUSamples,
                   uRandomEffectsSamples = output$uRandomEffectsSamples,
                   acceptanceRates = c(output$betaAcceptanceRate, output$sigmaSquaredUAcceptanceRate),
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
                                               centerSpatialRandomEffects = TRUE, 
                                               centerURandomEffects = centerURandomEffects)
    
    results = list(call = UniNetCall,
                   y = output$y,
                   X = output$X,
                   standardizedX = output$standardizedX,
                   W = output$W,
                   samples = cbind(output$betaSamples, "sigmaSquaredU" = output$sigmaSquaredUSamples),
                   betaSamples = output$betaSamples,
                   sigmaSquaredUSamples = output$sigmaSquaredUSamples,
                   uRandomEffectsSamples = output$uRandomEffectsSamples,
                   acceptanceRates = c(output$betaAcceptanceRate, output$sigmaSquaredUAcceptanceRate),
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