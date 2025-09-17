multivariatePoissonNetworkLeroux = function(formula, 
                                              data, 
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
                                              covarianceBetaPrior = 10^5,
                                              a1 = 0.001, 
                                              b1 = 0.001, 
                                              xi, 
                                              omega, 
                                              centerSpatialRandomEffects = TRUE, 
                                              centerURandomEffects = TRUE) {
  
  startTime = proc.time()
  set.seed(seed)
  call = match.call()
  
  if(ncol(squareSpatialNeighbourhoodMatrix) != nrow(squareSpatialNeighbourhoodMatrix)){
    stop("squareSpatialNeighbourhoodMatrix is not square")
  } else {
    numberOfSpatialAreas = ncol(squareSpatialNeighbourhoodMatrix)
  }
  
  # get the appropriate data.
  standardizedCovariates = getStandardizedCovariates(formula, data)
  y = as.vector(standardizedCovariates$y)
  X = standardizedCovariates$X
  standardizedX = standardizedCovariates$standardizedX
  numberOfResponses = length(y) / nrow(standardizedX)
  
  # perform checks.
  checkModelMCMCInputParameters(numberOfSamples, burnin, thin)
  
  initialBetaParameters = rep(getInitialParameters(X), numberOfResponses)
  initialSpatialRandomEffects = rep(getInitialParameters(spatialAssignment) , numberOfResponses)
  initialURandomEffects = rep(getInitialParameters(W), numberOfResponses)
  initialSpatialTauSquared = rep(1, numberOfResponses)
  initialSpatialRho = rep(0.5, numberOfResponses)
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
  
  spatialRandomEffectsTuningParameters = rep(1, ncol(spatialAssignment) * numberOfResponses)
  spatialRandomEffectsAcceptanceRate = rep(0, ncol(spatialAssignment) * numberOfResponses)
  numberOfAcceptedSpatialRandomEffectsDraws = rep(0, ncol(spatialAssignment) * numberOfResponses)
  numberOfAllAcceptedSpatialRandomEffectsDraws = rep(0, ncol(spatialAssignment) * numberOfResponses)
  
  uRandomEffectsTuningParameters = rep(1, ncol(W) * numberOfResponses)
  uRandomEffectsAcceptanceRate = rep(0, ncol(W) * numberOfResponses)
  numberOfAcceptedUREDraws = rep(0, ncol(W) * numberOfResponses)
  numberOfAllAcceptedUREDraws = rep(0, ncol(W) * numberOfResponses)
  
  spatialRhoTuningParameters = rep(1, numberOfResponses)
  spatialRhoAcceptanceRate = rep(0, numberOfResponses)
  numberOfAcceptedSpatialRhoDraws = rep(0, numberOfResponses)
  numberOfAllAcceptedSpatialRhoDraws = rep(0, numberOfResponses)
  
  currentNumberOfIterations = 1
  
  squareSpatialNeighbourhoodMatrixInTripletForm = getTripletForm(squareSpatialNeighbourhoodMatrix)
  spatialAssignmentMatrixInTripletForm = getTripletForm(spatialAssignment)
  WInTripletForm = getTripletForm(W)
  
  if(is.null(trueBeta)) {
    betaFixed = FALSE
    trueBetaValues = initialBetaParameters
  } else {
    betaFixed = TRUE
    trueBetaValues = trueBeta
  }
  
  if(is.null(trueSpatialRandomEffects)) {
    spatialRandomEffectsFixed = FALSE
    trueSpatialRandomEffectsValues = initialSpatialRandomEffects
  } else {
    spatialRandomEffectsFixed = TRUE
    trueSpatialRandomEffectsValues = trueSpatialRandomEffects
  }
  
  if(is.null(trueURandomEffects)) {
    uRandomEffectsFixed = FALSE
    trueURandomEffectsValues = initialURandomEffects
  } else {
    uRandomEffectsFixed = TRUE
    trueURandomEffectsValues = trueURandomEffects
  }
  
  if(is.null(trueSpatialTauSquared)) {
    spatialTauSquaredFixed = FALSE
    trueSpatialTauSquaredValues = initialSpatialTauSquared
  } else {
    spatialTauSquaredFixed = TRUE
    trueSpatialTauSquaredValues = trueSpatialTauSquared
  }
  
  if(is.null(trueSpatialRho)) {
    spatialRhoFixed = FALSE
    trueSpatialRhoValues = initialSpatialRho
  } else {
    spatialRhoFixed = TRUE
    trueSpatialRhoValues = trueSpatialRho
  }
  
  if(is.null(trueVarianceCovarianceU)) {
    varianceCovarianceUFixed = FALSE
    trueVarianceCovarianceUValues = initialVarianceCovarianceU
  } else {
    varianceCovarianceUFixed = TRUE
    trueVarianceCovarianceUValues = trueVarianceCovarianceU
  }
  
  output = multivariatePoissonNetworkLerouxAllUpdate(standardizedX = standardizedX,
                                                       y = y,
                                                       numberOfResponses = numberOfResponses,
                                                       squareSpatialNeighbourhoodMatrix = squareSpatialNeighbourhoodMatrix,
                                                       spatialAssignment = spatialAssignment,
                                                       W = W,
                                                       numberOfSpatialAreas = numberOfSpatialAreas,
                                                       squareSpatialNeighbourhoodMatrixInTripletForm = squareSpatialNeighbourhoodMatrixInTripletForm,
                                                       spatialAssignmentMatrixInTripletForm = spatialAssignmentMatrixInTripletForm,
                                                       WInTripletForm = WInTripletForm,
                                                       beta = initialBetaParameters,
                                                       spatialRandomEffects = initialSpatialRandomEffects,
                                                       uRandomEffects = initialURandomEffects,
                                                       spatialTauSquared = initialSpatialTauSquared,
                                                       spatialRho = initialSpatialRho,
                                                       varianceCovarianceU = initialVarianceCovarianceU,
                                                       covarianceBetaPrior = covarianceBetaPrior,
                                                       numberOfBetaBlocks = numberOfBetaBlocks,
                                                       maxBetaBlockSize = maxBetaBlockSize,
                                                       betaTuningParameter = betaTuningParameter,
                                                       betaAcceptanceRate = betaAcceptanceRate,
                                                       numberOfAcceptedBetaDraws = numberOfAcceptedBetaDraws,
                                                       numberOfAllAcceptedBetaDraws = numberOfAllAcceptedBetaDraws,
                                                       spatialRandomEffectsTuningParameters = spatialRandomEffectsTuningParameters,
                                                       spatialRandomEffectsAcceptanceRate = spatialRandomEffectsAcceptanceRate,
                                                       numberOfAcceptedSpatialRandomEffectsDraws = numberOfAcceptedSpatialRandomEffectsDraws,
                                                       numberOfAllAcceptedSpatialRandomEffectsDraws = numberOfAllAcceptedSpatialRandomEffectsDraws,
                                                       uRandomEffectsTuningParameters = uRandomEffectsTuningParameters,
                                                       uRandomEffectsAcceptanceRate = uRandomEffectsAcceptanceRate,
                                                       numberOfAcceptedUREDraws = numberOfAcceptedUREDraws,
                                                       numberOfAllAcceptedUREDraws = numberOfAllAcceptedUREDraws,
                                                       spatialRhoTuningParameters = spatialRhoTuningParameters,
                                                       spatialRhoAcceptanceRate = spatialRhoAcceptanceRate,
                                                       numberOfAcceptedSpatialRhoDraws = numberOfAcceptedSpatialRhoDraws,
                                                       numberOfAllAcceptedSpatialRhoDraws = numberOfAllAcceptedSpatialRhoDraws,
                                                       a1 = a1,
                                                       b1 = b1,
                                                       xi = xi,
                                                       omega = omega,
                                                       currentNumberOfIterations = currentNumberOfIterations,
                                                       numberOfSamples = numberOfSamples,
                                                       burnin = burnin,
                                                       thin = thin,
                                                       betaFixed = betaFixed,
                                                       spatialRandomEffectsFixed = spatialRandomEffectsFixed,
                                                       uRandomEffectsFixed = uRandomEffectsFixed,
                                                       spatialTauSquaredFixed = spatialTauSquaredFixed,
                                                       spatialRhoFixed = spatialRhoFixed,
                                                       varianceCovarianceUFixed = varianceCovarianceUFixed,
                                                       trueBetaValues = trueBetaValues,
                                                       trueSpatialRandomEffectsValues = trueSpatialRandomEffectsValues,
                                                       trueURandomEffectsValues = trueURandomEffectsValues,
                                                       trueSpatialTauSquaredValues = trueSpatialTauSquaredValues,
                                                       trueSpatialRhoValues = trueSpatialRhoValues,
                                                       trueVarianceCovarianceUValues = trueVarianceCovarianceUValues,
                                                       centerSpatialRandomEffects = centerSpatialRandomEffects,
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
  
  spatialRandomEffectsSamples = output[[2]]
  uRandomEffectsSamples = output[[3]]
  spatialTauSquaredSamples = output[[4]]
  spatialRhoSamples = output[[5]]
  varianceCovarianceUSamples = output[[6]]
  
  betaAcceptanceRate = output[[7]]
  spatialRandomEffectsAcceptanceRate = output[[8]]
  uRandomEffectsAcceptanceRate = output[[9]]
  spatialRhoAcceptanceRate = output[[10]]
  spatialTauSquaredAcceptanceRate = rep(1, numberOfResponses)
  varianceCovarianceUAcceptanceRate = rep(1, numberOfResponses  + (numberOfResponses * numberOfResponses - numberOfResponses) / 2)
  
  acceptanceRates = c(betaAcceptanceRate, spatialTauSquaredAcceptanceRate, 
                      spatialRhoAcceptanceRate, varianceCovarianceUAcceptanceRate)
  
  
  betaColumnNames = c()
  for(i in 1:numberOfResponses){
    betaColumnNames = c(betaColumnNames, paste(colnames(X), i))
  }
  
  spatialRandomEffectsColumnNames = c()
  for(i in 1:numberOfResponses){
    spatialRandomEffectsColumnNames = c(spatialRandomEffectsColumnNames, paste("phi", seq(1, ncol(spatialAssignment)), i))
  }
  
  uRandomEffectsColumnNames = c()
  for(i in 1:numberOfResponses){
    uRandomEffectsColumnNames = c(uRandomEffectsColumnNames, paste("u", seq(1, ncol(W)), i))
  }
  
  # store samples and transform beta to original scale.
  colnames(betaSamples) = betaColumnNames
  colnames(spatialRandomEffectsSamples) = spatialRandomEffectsColumnNames
  colnames(spatialTauSquaredSamples) = paste("tauSquared", seq(1, numberOfResponses)) 
  colnames(spatialRhoSamples) = paste("rho", seq(1, numberOfResponses)) 
  colnames(uRandomEffectsSamples) = uRandomEffectsColumnNames
  colnames(varianceCovarianceUSamples) = paste("sigmaU", seq(1, numberOfResponses  + (numberOfResponses * numberOfResponses - numberOfResponses) / 2)) 
  samples = cbind(betaSamples, spatialTauSquaredSamples, spatialRhoSamples, varianceCovarianceUSamples)
  
  DBar = output[[17]]
  posteriorDeviance = output[[18]]
  posteriorLogLikelihood = -0.5 * posteriorDeviance
  pd = output[[19]]
  DIC = output[[20]]
  
  endTime = proc.time()
  timeTaken = endTime - startTime
  
  results = list(call = call,
                 y = y,
                 X = X,
                 squareSpatialNeighbourhoodMatrix = squareSpatialNeighbourhoodMatrix ,
                 spatialAssignment = spatialAssignment ,
                 W = W,
                 standardizedX = standardizedX,
                 samples = samples,
                 betaSamples = betaSamples,
                 spatialTauSquaredSamples = spatialTauSquaredSamples,
                 spatialRhoSamples = spatialRhoSamples,
                 varianceCovarianceUSamples= varianceCovarianceUSamples ,
                 spatialRandomEffectsSamples = spatialRandomEffectsSamples,
                 uRandomEffectsSamples = uRandomEffectsSamples,
                 acceptanceRates = acceptanceRates,
                 betaAcceptanceRate = betaAcceptanceRate,
                 spatialTauSquaredAcceptanceRate = spatialTauSquaredAcceptanceRate,
                 spatialRhoAcceptanceRate = spatialRhoAcceptanceRate,
                 varianceCovarianceUAcceptanceRate = varianceCovarianceUAcceptanceRate,
                 spatialRandomEffectsAcceptanceRate = spatialRandomEffectsAcceptanceRate,
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