multivariatePoissonNetworkRand = function(formula, 
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
                                            covarianceBetaPrior = 10^5,
                                            xiV, 
                                            omegaV, 
                                            xi, 
                                            omega, 
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
  
  numberOfFixedEffects = ncol(X)
  numberOfColumnsInX = ncol(X)
  maxBetaBlockSize = numberOfFixedEffects
  if (numberOfColumnsInX %% maxBetaBlockSize == 0 ) {
    numberOfBetaBlocks = numberOfColumnsInX / maxBetaBlockSize
  } else {
    numberOfBetaBlocks = floor(numberOfColumnsInX / maxBetaBlockSize) + 1
  }
  betaTuningParameter = rep(1, numberOfFixedEffects * numberOfResponses)
  betaAcceptanceRate = rep(0, numberOfFixedEffects * numberOfResponses)
  numberOfAcceptedBetaDraws = rep(0, numberOfFixedEffects * numberOfResponses)
  numberOfAllAcceptedBetaDraws = rep(0, numberOfFixedEffects * numberOfResponses)
  
  vRandomEffectsTuningParameters = rep(1, ncol(V) * numberOfResponses)
  vRandomEffectsAcceptanceRate = rep(0, ncol(V) * numberOfResponses)
  numberOfAcceptedVREDraws = rep(0, ncol(V) * numberOfResponses)
  numberOfAllAcceptedVREDraws = rep(0, ncol(V) * numberOfResponses)
  
  uRandomEffectsTuningParameters = rep(1, ncol(W) * numberOfResponses)
  uRandomEffectsAcceptanceRate = rep(0, ncol(W) * numberOfResponses)
  numberOfAcceptedUREDraws = rep(0, ncol(W) * numberOfResponses)
  numberOfAllAcceptedUREDraws = rep(0, ncol(W) * numberOfResponses)
  
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
  
  output = multivariatePoissonNetworkRandAllUpdate(standardizedX = standardizedX,
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
                                                     covarianceBetaPrior = covarianceBetaPrior,
                                                     numberOfBetaBlocks = numberOfBetaBlocks,
                                                     maxBetaBlockSize = maxBetaBlockSize,
                                                     betaTuningParameter = betaTuningParameter,
                                                     betaAcceptanceRate = betaAcceptanceRate,
                                                     numberOfAcceptedBetaDraws = numberOfAcceptedBetaDraws,
                                                     numberOfAllAcceptedBetaDraws = numberOfAllAcceptedBetaDraws,
                                                     vRandomEffectsTuningParameters = vRandomEffectsTuningParameters,
                                                     vRandomEffectsAcceptanceRate = vRandomEffectsAcceptanceRate,
                                                     numberOfAcceptedVREDraws = numberOfAcceptedVREDraws,
                                                     numberOfAllAcceptedVREDraws = numberOfAllAcceptedVREDraws,
                                                     uRandomEffectsTuningParameters = uRandomEffectsTuningParameters,
                                                     uRandomEffectsAcceptanceRate = uRandomEffectsAcceptanceRate,
                                                     numberOfAcceptedUREDraws = numberOfAcceptedUREDraws,
                                                     numberOfAllAcceptedUREDraws = numberOfAllAcceptedUREDraws,
                                                     xiV = xiV,
                                                     omegaV = omegaV,
                                                     xi = xi,
                                                     omega = omega,
                                                     currentNumberOfIterations = currentNumberOfIterations,
                                                     numberOfSamples = numberOfSamples,
                                                     burnin = burnin,
                                                     thin = thin,
                                                     betaFixed = betaFixed,
                                                     vRandomEffectsFixed = vRandomEffectsFixed,
                                                     uRandomEffectsFixed = uRandomEffectsFixed,
                                                     varianceCovarianceVFixed = varianceCovarianceVFixed,
                                                     varianceCovarianceUFixed = varianceCovarianceUFixed,
                                                     trueBetaValues = trueBetaValues,
                                                     trueVRandomEffectsValues = trueVRandomEffectsValues,
                                                     trueURandomEffectsValues = trueURandomEffectsValues,
                                                     trueVarianceCovarianceVValues = trueVarianceCovarianceVValues,
                                                     trueVarianceCovarianceUValues = trueVarianceCovarianceUValues,
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
  
  betaAcceptanceRate = output[[6]]
  vRandomEffectsAcceptanceRate = output[[7]]
  uRandomEffectsAcceptanceRate = output[[8]]
  varianceCovarianceVAcceptanceRate = rep(1, numberOfResponses  + (numberOfResponses * numberOfResponses - numberOfResponses) / 2)
  varianceCovarianceUAcceptanceRate = rep(1, numberOfResponses  + (numberOfResponses * numberOfResponses - numberOfResponses) / 2)
  
  acceptanceRates = c(betaAcceptanceRate, varianceCovarianceVAcceptanceRate, varianceCovarianceUAcceptanceRate)
  
  
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
  colnames(vRandomEffectsSamples) = vRandomEffectsColumnNames
  colnames(uRandomEffectsSamples) = uRandomEffectsColumnNames
  colnames(varianceCovarianceVSamples) = paste("sigmaV", seq(1, numberOfResponses  + (numberOfResponses * numberOfResponses - numberOfResponses) / 2)) 
  colnames(varianceCovarianceUSamples) = paste("sigmaU", seq(1, numberOfResponses  + (numberOfResponses * numberOfResponses - numberOfResponses) / 2)) 
  samples = cbind(betaSamples, varianceCovarianceVSamples, varianceCovarianceUSamples)
  
  DBar = output[[14]]
  posteriorDeviance = output[[15]]
  posteriorLogLikelihood = -0.5 * posteriorDeviance
  pd = output[[16]]
  DIC = output[[17]]
  
  endTime = proc.time()
  timeTaken = endTime - startTime
  
  results = list(call = call,
                 y = y,
                 X = X,
                 v = V,
                 W = W,
                 standardizedX = standardizedX,
                 samples = samples,
                 betaSamples = betaSamples,
                 varianceCovarianceVSamples= varianceCovarianceVSamples ,
                 varianceCovarianceUSamples= varianceCovarianceUSamples ,
                 vRandomEffectsSamples = vRandomEffectsSamples,
                 uRandomEffectsSamples = uRandomEffectsSamples,
                 acceptanceRates = acceptanceRates,
                 betaAcceptanceRate = betaAcceptanceRate,
                 varianceCovarianceVAcceptanceRate = varianceCovarianceVAcceptanceRate,
                 varianceCovarianceUAcceptanceRate = varianceCovarianceUAcceptanceRate,
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