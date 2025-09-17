multivariateGaussianNetworkRand = function(formula, 
                                            data, 
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
                                            xiV, 
                                            omegaV,
                                            xi, 
                                            omega, 
                                            a3 = 0.001, 
                                            b3 = 0.001, 
                                            centerVRandomEffects = TRUE, 
                                            centerURandomEffects = TRUE) {
  
  startTime = proc.time()
  set.seed(seed)
  call = match.call()
  
  # get the appropriate data.
  standardizedCovariates = getStandardizedCovariates(formula, data)
  y = as.vector(standardizedCovariates$y)
  X = standardizedCovariates$X
  standardizedX = standardizedCovariates$standardizedX
  numberOfResponses = length(y) / nrow(standardizedX)
  
  # perform checks.
  checkModelMCMCInputParameters(numberOfSamples, burnin, thin)
  
  initialBetaParameters = rep(getInitialParameters(X), numberOfResponses)
  initialVRandomEffects = rep(getInitialParameters(V), numberOfResponses)
  initialURandomEffects = rep(getInitialParameters(W), numberOfResponses)
  initialVarianceCovarianceV = diag(rep(1, numberOfResponses))
  initialVarianceCovarianceU = diag(rep(1, numberOfResponses))
  initialSigmaSquaredE = rep(1, numberOfResponses)
  
  currentNumberOfIterations = 1
  
  VInTripletForm = getTripletForm(V)
  WInTripletForm = getTripletForm(W)
  
  if(is.null(trueBeta)) {
    betaFixed = FALSE
    trueBetaValues = initialBetaParameters
  } else {
    betaFixed = TRUE
    trueBetaValues = trueBeta
  }
  
  if(is.null(trueVRandomEffects)) {
    vRandomEffectsFixed = FALSE
    trueVRandomEffectsValues = initialVRandomEffects
  } else {
    vRandomEffectsFixed = TRUE
    trueVRandomEffectsValues = trueVRandomEffects
  }
  
  if(is.null(trueURandomEffects)) {
    uRandomEffectsFixed = FALSE
    trueURandomEffectsValues = initialURandomEffects
  } else {
    uRandomEffectsFixed = TRUE
    trueURandomEffectsValues = trueURandomEffects
  }
  
  if(is.null(trueVarianceCovarianceV)) {
    varianceCovarianceVFixed = FALSE
    trueVarianceCovarianceVValues = initialVarianceCovarianceV
  } else {
    varianceCovarianceVFixed = TRUE
    trueVarianceCovarianceVValues = trueVarianceCovarianceV
  }
  
  if(is.null(trueVarianceCovarianceU)) {
    varianceCovarianceUFixed = FALSE
    trueVarianceCovarianceUValues = initialVarianceCovarianceU
  } else {
    varianceCovarianceUFixed = TRUE
    trueVarianceCovarianceUValues = trueVarianceCovarianceU
  }
  
  if(is.null(trueSigmaSquaredE)) {
    sigmaSquaredEFixed = FALSE
    trueSigmaSquaredEValues = initialSigmaSquaredE
  } else {
    sigmaSquaredEFixed = TRUE
    trueSigmaSquaredEValues = trueSigmaSquaredE
  }
  
  output = multivariateGaussianNetworkRandAllUpdate(standardizedX = standardizedX,
                                                    y = y,
                                                    numberOfResponses = numberOfResponses,
                                                    V = V,
                                                    W = W,
                                                    VInTripletForm = VInTripletForm,
                                                    WInTripletForm = WInTripletForm,
                                                    beta = initialBetaParameters,
                                                    vRandomEffects = initialVRandomEffects,
                                                    uRandomEffects = initialURandomEffects,
                                                    varianceCovarianceV = initialVarianceCovarianceV,
                                                    varianceCovarianceU = initialVarianceCovarianceU,
                                                    sigmaSquaredE = initialSigmaSquaredE,
                                                    covarianceBetaPrior = covarianceBetaPrior,
                                                    xiV = xiV,
                                                    omegaV = omegaV,
                                                    xi = xi,
                                                    omega = omega,
                                                    a3 = a3,
                                                    b3 = b3,
                                                    currentNumberOfIterations = currentNumberOfIterations,
                                                    numberOfSamples = numberOfSamples,
                                                    burnin = burnin,
                                                    thin = thin,
                                                    betaFixed = betaFixed,
                                                    vRandomEffectsFixed = vRandomEffectsFixed,
                                                    uRandomEffectsFixed = uRandomEffectsFixed,
                                                    varianceCovarianceVFixed = varianceCovarianceVFixed,
                                                    varianceCovarianceUFixed = varianceCovarianceUFixed,
                                                    sigmaSquaredEFixed = sigmaSquaredEFixed,
                                                    trueBetaValues = trueBetaValues,
                                                    trueVRandomEffectsValues = trueVRandomEffectsValues,
                                                    trueURandomEffectsValues = trueURandomEffectsValues,
                                                    trueVarianceCovarianceVValues = trueVarianceCovarianceVValues,
                                                    trueVarianceCovarianceUValues = trueVarianceCovarianceUValues,
                                                    trueSigmaSquaredEValues = trueSigmaSquaredEValues,
                                                    centerVRandomEffects = centerVRandomEffects,
                                                    centerURandomEffects = centerURandomEffects)
  
  if(ncol(X) == 1 && colnames(X) == "(Intercept)"){
    betaSamples = output[[1]]
  } else {
    unconvertedBetaSamples = output[[1]]
    betaSamples = c()
    for(i in 1:numberOfResponses){
      betaSamples = cbind(betaSamples, getBetaParameterConversion(X, unconvertedBetaSamples[, (((i - 1) * ncol(X)) + 1):(i * ncol(X))]))
    }
  }
  
  vRandomEffectsSamples = output[[2]]
  uRandomEffectsSamples = output[[3]]
  varianceCovarianceVSamples = output[[4]]
  varianceCovarianceUSamples = output[[5]]
  sigmaSquaredESamples = output[[6]]
  
  betaAcceptanceRate = rep(1, ncol(betaSamples))
  vRandomEffectsAcceptanceRate = rep(1, ncol(vRandomEffectsSamples))
  uRandomEffectsAcceptanceRate = rep(1, ncol(uRandomEffectsSamples))
  varianceCovarianceVAcceptanceRate = rep(1, numberOfResponses  + (numberOfResponses * numberOfResponses - numberOfResponses) / 2)
  varianceCovarianceUAcceptanceRate = rep(1, numberOfResponses  + (numberOfResponses * numberOfResponses - numberOfResponses) / 2)
  sigmaSquaredEAcceptanceRate = rep(1, numberOfResponses)
  
  acceptanceRates = c(betaAcceptanceRate, varianceCovarianceVAcceptanceRate, 
                      varianceCovarianceUAcceptanceRate, sigmaSquaredEAcceptanceRate)
  
  betaColumnNames = c()
  for(i in 1:numberOfResponses){
    betaColumnNames = c(betaColumnNames, paste(colnames(X), i))
  }
  
  vRandomEffectsColumnNames = c()
  for(i in 1:numberOfResponses){
    vRandomEffectsColumnNames = c(vRandomEffectsColumnNames, paste("v", seq(1, ncol(V)), i))
  }
  
  uRandomEffectsColumnNames = c()
  for(i in 1:numberOfResponses){
    uRandomEffectsColumnNames = c(uRandomEffectsColumnNames, paste("u", seq(1, ncol(W)), i))
  }
  
  # store samples and transform beta to original scale.
  colnames(betaSamples) = betaColumnNames

  colnames(sigmaSquaredESamples) = paste("sigmaSquaredE", seq(1, numberOfResponses)) 
  colnames(vRandomEffectsSamples) = vRandomEffectsColumnNames
  colnames(uRandomEffectsSamples) = uRandomEffectsColumnNames
  colnames(varianceCovarianceVSamples) = paste("sigmaV", seq(1, numberOfResponses  + (numberOfResponses * numberOfResponses - numberOfResponses) / 2)) 
  colnames(varianceCovarianceUSamples) = paste("sigmaU", seq(1, numberOfResponses  + (numberOfResponses * numberOfResponses - numberOfResponses) / 2)) 
  samples = cbind(betaSamples, varianceCovarianceVSamples, varianceCovarianceUSamples, sigmaSquaredESamples)
  
  DBar = output[[9]]
  posteriorDeviance = output[[10]]
  posteriorLogLikelihood = -0.5 * posteriorDeviance
  pd = output[[11]]
  DIC = output[[12]]
  
  endTime = proc.time()
  timeTaken = endTime - startTime
  
  results = list(call = call,
                 y = y,
                 X = X,
                 V = V,
                 W = W,
                 standardizedX = standardizedX,
                 samples = samples,
                 betaSamples = betaSamples,
                 varianceCovarianceVSamples= varianceCovarianceVSamples,
                 varianceCovarianceUSamples= varianceCovarianceUSamples,
                 sigmaSquaredESamples = sigmaSquaredESamples,
                 vRandomEffectsSamples = vRandomEffectsSamples,
                 uRandomEffectsSamples = uRandomEffectsSamples,
                 acceptanceRates = acceptanceRates,
                 betaAcceptanceRate = betaAcceptanceRate,
                 varianceCovarianceVAcceptanceRate = varianceCovarianceVAcceptanceRate,
                 varianceCovarianceUAcceptanceRate = varianceCovarianceUAcceptanceRate,
                 sigmaSquaredEAcceptanceRate = sigmaSquaredEAcceptanceRate,
                 vRandomEffectsAcceptanceRate = vRandomEffectsAcceptanceRate,
                 uRandomEffectsAcceptanceRate = uRandomEffectsAcceptanceRate,
                 timeTaken = timeTaken,
                 burnin = burnin,
                 thin = thin,
                 DBar = DBar,
                 posteriorDeviance = posteriorDeviance,
                 posteriorLogLikelihood = posteriorLogLikelihood,
                 pd = pd,
                 DIC = DIC)
  
  class(results) = "netcmc"
  
  return(results)
  
} 