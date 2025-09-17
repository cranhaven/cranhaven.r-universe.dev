univariateGaussianNetworkLerouxMH = function(formula, 
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
                                           trueSigmaSquaredU = NULL,
                                           trueSigmaSquaredE = NULL,
                                           covarianceBetaPrior = 10^5,
                                           a1 = 0.001, 
                                           b1 = 0.001, 
                                           a2 = 0.001, 
                                           b2 = 0.001, 
                                           a3 = 0.001, 
                                           b3 = 0.001, 
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
  y = standardizedCovariates$y
  X = standardizedCovariates$X
  standardizedX = standardizedCovariates$standardizedX
  
  # perform checks.
  checkModelMCMCInputParameters(numberOfSamples, burnin, thin)
  
  initialBetaParameters = getInitialParameters(X)
  initialSpatialRandomEffects = getInitialParameters(spatialAssignment) 
  initialURandomEffects = getInitialParameters(W)
  
  initialSpatialTauSquared = 1
  initialSpatialRho = 0.5
  initialSigmaSquaredU = 1
  initialSigmaSquaredE = 1
  
  numberOfFixedEffects = ncol(X)
  numberOfColumnsInX = ncol(X)
  
  spatialRandomEffectsTuningParameters = rep(1, ncol(spatialAssignment))
  spatialRandomEffectsAcceptanceRate = rep(0, ncol(spatialAssignment))
  numberOfAcceptedSpatialRandomEffectsDraws = rep(0, ncol(spatialAssignment))
  numberOfAllAcceptedSpatialRandomEffectsDraws = rep(0, ncol(spatialAssignment))
  
  spatialRhoTuningParameters = 1
  spatialRhoAcceptanceRate = 0
  numberOfAcceptedSpatialRhoDraws = 0
  numberOfAllAcceptedSpatialRhoDraws = 0
  
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
  
  if(is.null(trueSigmaSquaredU)) {
    sigmaSquaredUFixed = FALSE
    trueSigmaSquaredUValues = initialSigmaSquaredU
  } else {
    sigmaSquaredUFixed = TRUE
    trueSigmaSquaredUValues = trueSigmaSquaredU
  }
  
  if(is.null(trueSigmaSquaredE)) {
    sigmaSquaredEFixed = FALSE
    trueSigmaSquaredEValues = initialSigmaSquaredE
  } else {
    sigmaSquaredEFixed = TRUE
    trueSigmaSquaredEValues = trueSigmaSquaredE
  }
  
  output = univariateGaussianNetworkLerouxAllMHUpdate(standardizedX = standardizedX,
                                                    y = y,
                                                    squareSpatialNeighbourhoodMatrix = squareSpatialNeighbourhoodMatrix,
                                                    spatialAssignment = spatialAssignment,
                                                    W = W,
                                                    squareSpatialNeighbourhoodMatrixInTripletForm = squareSpatialNeighbourhoodMatrixInTripletForm,
                                                    spatialAssignmentMatrixInTripletForm = spatialAssignmentMatrixInTripletForm,
                                                    WInTripletForm = WInTripletForm,
                                                    beta = initialBetaParameters,
                                                    spatialRandomEffects = initialSpatialRandomEffects,
                                                    uRandomEffects = initialURandomEffects,
                                                    spatialTauSquared = initialSpatialTauSquared,
                                                    spatialRho = initialSpatialRho,
                                                    sigmaSquaredU = initialSigmaSquaredU,
                                                    sigmaSquaredE = initialSigmaSquaredE,
                                                    covarianceBetaPrior = covarianceBetaPrior,
                                                    spatialRandomEffectsTuningParameters = spatialRandomEffectsTuningParameters,
                                                    spatialRandomEffectsAcceptanceRate = spatialRandomEffectsAcceptanceRate,
                                                    numberOfAcceptedSpatialRandomEffectsDraws = numberOfAcceptedSpatialRandomEffectsDraws,
                                                    numberOfAllAcceptedSpatialRandomEffectsDraws = numberOfAllAcceptedSpatialRandomEffectsDraws,
                                                    spatialRhoTuningParameters = spatialRhoTuningParameters,
                                                    spatialRhoAcceptanceRate = spatialRhoAcceptanceRate,
                                                    numberOfAcceptedSpatialRhoDraws = numberOfAcceptedSpatialRhoDraws,
                                                    numberOfAllAcceptedSpatialRhoDraws = numberOfAllAcceptedSpatialRhoDraws,
                                                    a1 = a1,
                                                    b1 = b1,
                                                    a2 = a2,
                                                    b2 = b2,
                                                    a3 = a3,
                                                    b3 = b3,
                                                    currentNumberOfIterations = currentNumberOfIterations,
                                                    numberOfSamples = numberOfSamples,
                                                    burnin = burnin,
                                                    thin = thin,
                                                    betaFixed = betaFixed,
                                                    spatialRandomEffectsFixed = spatialRandomEffectsFixed,
                                                    uRandomEffectsFixed = uRandomEffectsFixed,
                                                    spatialTauSquaredFixed = spatialTauSquaredFixed,
                                                    spatialRhoFixed = spatialRhoFixed,
                                                    sigmaSquaredUFixed = sigmaSquaredUFixed,
                                                    sigmaSquaredEFixed = sigmaSquaredEFixed,
                                                    trueBetaValues = trueBetaValues,
                                                    trueSpatialRandomEffectsValues = trueSpatialRandomEffectsValues,
                                                    trueURandomEffectsValues = trueURandomEffectsValues,
                                                    trueSpatialTauSquaredValues = trueSpatialTauSquaredValues,
                                                    trueSpatialRhoValues = trueSpatialRhoValues,
                                                    trueSigmaSquaredUValues = trueSigmaSquaredUValues,
                                                    trueSigmaSquaredEValues = trueSigmaSquaredEValues,
                                                    centerSpatialRandomEffects = centerSpatialRandomEffects,
                                                    centerURandomEffects = centerURandomEffects)

  if(ncol(X) == 1 && colnames(X) == "(Intercept)"){
    betaSamples = output[[1]]
  } else {
    unconvertedBetaSamples = output[[1]]
    betaSamples =  getBetaParameterConversion(X, unconvertedBetaSamples)
  }
  spatialRandomEffectsSamples = output[[2]]
  uRandomEffectsSamples = output[[3]]
  spatialTauSquaredSamples = output[[4]]
  spatialRhoSamples = output[[5]]
  sigmaSquaredUSamples = output[[6]]
  sigmaSquaredESamples = output[[7]]
  
  spatialRandomEffectsAcceptanceRate = output[[8]]
  spatialRhoAcceptanceRate = output[[9]]
  
  betaColumnNames = c()
  betaColumnNames = paste(colnames(X))
  
  spatialRandomEffectsColumnNames = c()
  spatialRandomEffectsColumnNames = paste("phi", seq(1, ncol(spatialAssignment)))
  
  uRandomEffectsColumnNames = c()
  uRandomEffectsColumnNames = paste("u", seq(1, ncol(W)))
  
  # store samples and transform beta to original scale.
  colnames(betaSamples) = betaColumnNames 
  colnames(spatialRandomEffectsSamples) = spatialRandomEffectsColumnNames
  colnames(uRandomEffectsSamples) = uRandomEffectsColumnNames
  samples = cbind(betaSamples, spatialTauSquaredSamples, spatialRhoSamples, sigmaSquaredUSamples, sigmaSquaredESamples)
  
  DBar = output[[14]]
  posteriorDeviance = output[[15]]
  posteriorLogLikelihood = -0.5 * posteriorDeviance
  pd = output[[16]]
  DIC = output[[17]]
  
  # store the acceptance information.
  betaAcceptanceRate = rep(1, ncol(X))
  spatialTauSquaredAcceptanceRate = 1
  sigmaSquaredUAcceptanceRate = 1
  sigmaSquaredEAcceptanceRate = 1
  
  acceptanceRates = c(betaAcceptanceRate, spatialTauSquaredAcceptanceRate, 
                      spatialRhoAcceptanceRate, sigmaSquaredUAcceptanceRate,
                      sigmaSquaredEAcceptanceRate)
  
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
                 sigmaSquaredUSamples= sigmaSquaredUSamples,
                 sigmaSquaredESamples= sigmaSquaredESamples,
                 spatialRandomEffectsSamples = spatialRandomEffectsSamples,
                 uRandomEffectsSamples = uRandomEffectsSamples,
                 acceptanceRates = acceptanceRates,
                 betaAcceptanceRate = betaAcceptanceRate,
                 spatialTauSquaredAcceptanceRate = spatialTauSquaredAcceptanceRate,
                 spatialRhoAcceptanceRate = spatialRhoAcceptanceRate,
                 sigmaSquaredUAcceptanceRate = sigmaSquaredUAcceptanceRate,
                 sigmaSquaredEAcceptanceRate = sigmaSquaredEAcceptanceRate,
                 spatialRandomEffectsAcceptanceRate = spatialRandomEffectsAcceptanceRate,
                 timeTaken = timeTaken,
                 burnin = burnin,
                 thin = thin,
                 DBar = DBar,
                 posteriorDeviance = posteriorDeviance,
                 posteriorLogLikelihood = posteriorLogLikelihood,
                 pd = pd,
                 DIC = DIC)
  
  return(results)
  
} 