multiNetRand = function(formula, 
                        data, 
                        trials = NULL,
                        family = "gaussian",
                        V,
                        W, 
                        numberOfSamples = 10, 
                        burnin = 0, 
                        thin = 1,
                        seed = 1, 
                        trueBeta = NULL,
                        trueVRandomEffects = NULL,
                        trueURandomEffects = NULL, 
                        trueVarianceCovarianceV = NULL, 
                        trueVarianceCovarianceU = NULL,
                        trueSigmaSquaredE = NULL,
                        covarianceBetaPrior = 10^5,
                        xiV = NULL, 
                        omegaV = NULL, 
                        xi = NULL, 
                        omega = NULL, 
                        a3 = 0.001, 
                        b3 = 0.001, 
                        centerVRandomEffects = TRUE, 
                        centerURandomEffects = TRUE) {
  
  multiNetRandCall = match.call() 
  
  if(!family %in% c("gaussian", "binomial", "poisson")){
    stop("The family specified is not gaussian, binomial or poisson!")
  } 
  
  standardizedCovariates = getStandardizedCovariates(formula, data)
  y = as.vector(standardizedCovariates$y)
  standardizedX = standardizedCovariates$standardizedX
  numberOfResponses = length(y) / nrow(standardizedX)
  
  if(is.null(xiV)) {
    xiV = numberOfResponses + 1
  } 
  if(is.null(omegaV)) {
    omegaV = diag(rep(1, numberOfResponses))
  } 
  if(is.null(xi)) {
    xi = numberOfResponses + 1
  } 
  if(is.null(omega)) {
    omega = diag(rep(1, numberOfResponses))
  } 
  
  if(family == "gaussian"){
    
    output = multivariateGaussianNetworkRand(formula = formula, 
                                              data = data, 
                                              V = V,
                                              W = W, 
                                              numberOfSamples = numberOfSamples, 
                                              burnin = burnin, 
                                              thin = thin,
                                              seed = seed, 
                                              trueBeta = trueBeta,
                                              trueVRandomEffects = trueVRandomEffects,
                                              trueURandomEffects = trueURandomEffects, 
                                              trueVarianceCovarianceV = trueVarianceCovarianceV, 
                                              trueVarianceCovarianceU = trueVarianceCovarianceU,
                                              trueSigmaSquaredE = trueSigmaSquaredE,
                                              covarianceBetaPrior = covarianceBetaPrior,
                                              xiV = xiV, 
                                              omegaV = omegaV,
                                              xi = xi, 
                                              omega = omega, 
                                              a3 = a3, 
                                              b3 = b3, 
                                              centerVRandomEffects = centerVRandomEffects, 
                                              centerURandomEffects = centerURandomEffects)
    
    results = list(call = multiNetRandCall,
                   y = output$y,
                   X = output$X,
                   V = output$V,
                   W = output$W,
                   samples = output$samples,
                   betaSamples = output$betaSamples,
                   varianceCovarianceVSamples = output$varianceCovarianceVSamples,
                   varianceCovarianceUSamples = output$varianceCovarianceUSamples,
                   sigmaSquaredESamples = output$sigmaSquaredESamples,
                   vRandomEffectsSamples = output$vRandomEffectsSamples,
                   uRandomEffectsSamples = output$uRandomEffectsSamples,
                   acceptanceRates = output$acceptanceRates,
                   vRandomEffectsAcceptanceRate = output$vRandomEffectsAcceptanceRate,
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
    
    output = multivariatePoissonNetworkRand(formula = formula, 
                                             data = data, 
                                             V = V,
                                             W = W, 
                                             numberOfSamples = numberOfSamples, 
                                             burnin = burnin, 
                                             thin = thin,
                                             seed = seed, 
                                             trueBeta = trueBeta,
                                             trueVRandomEffects = trueVRandomEffects,
                                             trueURandomEffects = trueURandomEffects, 
                                             trueVarianceCovarianceV = trueVarianceCovarianceV, 
                                             trueVarianceCovarianceU = trueVarianceCovarianceU,
                                             covarianceBetaPrior = covarianceBetaPrior,
                                             xiV = xiV, 
                                             omegaV = omegaV,
                                             xi = xi, 
                                             omega = omega, 
                                             centerVRandomEffects = centerVRandomEffects, 
                                             centerURandomEffects = centerURandomEffects)
    
    results = list(call = multiNetRandCall,
                   y = output$y,
                   X = output$X,
                   V = output$V,
                   W = output$W,
                   samples = output$samples,
                   betaSamples = output$betaSamples,
                   varianceCovarianceVSamples = output$varianceCovarianceVSamples,
                   varianceCovarianceUSamples = output$varianceCovarianceUSamples,
                   vRandomEffectsSamples = output$vRandomEffectsSamples,
                   uRandomEffectsSamples = output$uRandomEffectsSamples,
                   acceptanceRates = output$acceptanceRates,
                   vRandomEffectsAcceptanceRate = output$vRandomEffectsAcceptanceRate,
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
    
    output = multivariateBinomialNetworkRand(formula = formula, 
                                            data = data, 
                                            trials = trials,
                                            V = V,
                                            W = W, 
                                            numberOfSamples = numberOfSamples, 
                                            burnin = burnin, 
                                            thin = thin,
                                            seed = seed, 
                                            trueBeta = trueBeta,
                                            trueVRandomEffects = trueVRandomEffects,
                                            trueURandomEffects = trueURandomEffects, 
                                            trueVarianceCovarianceV = trueVarianceCovarianceV, 
                                            trueVarianceCovarianceU = trueVarianceCovarianceU,
                                            covarianceBetaPrior = covarianceBetaPrior,
                                            xiV = xiV, 
                                            omegaV = omegaV,
                                            xi = xi, 
                                            omega = omega, 
                                            centerVRandomEffects = centerVRandomEffects, 
                                            centerURandomEffects = centerURandomEffects)
    
    results = list(call = multiNetRandCall,
                   y = output$y,
                   X = output$X,
                   V = output$V,
                   W = output$W,
                   samples = output$samples,
                   betaSamples = output$betaSamples,
                   varianceCovarianceVSamples = output$varianceCovarianceVSamples,
                   varianceCovarianceUSamples = output$varianceCovarianceUSamples,
                   vRandomEffectsSamples = output$vRandomEffectsSamples,
                   uRandomEffectsSamples = output$uRandomEffectsSamples,
                   acceptanceRates = output$acceptanceRates,
                   vRandomEffectsAcceptanceRate = output$vRandomEffectsAcceptanceRate,
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