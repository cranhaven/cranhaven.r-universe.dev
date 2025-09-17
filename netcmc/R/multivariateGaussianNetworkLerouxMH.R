multivariateGaussianNetworkLerouxMH = function(formula, 
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
                                              trueSigmaSquaredE = NULL,
                                              covarianceBetaPrior = 10^5,
                                              a1 = 0.001, 
                                              b1 = 0.001, 
                                              xi, 
                                              omega, 
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
  initialSigmaSquaredE = rep(1, numberOfResponses)
  
  spatialRandomEffectsTuningParameters = rep(1, ncol(spatialAssignment) * numberOfResponses)
  spatialRandomEffectsAcceptanceRate = rep(0, ncol(spatialAssignment) * numberOfResponses)
  numberOfAcceptedSpatialRandomEffectsDraws = rep(0, ncol(spatialAssignment) * numberOfResponses)
  numberOfAllAcceptedSpatialRandomEffectsDraws = rep(0, ncol(spatialAssignment) * numberOfResponses)
  
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
  
  if(is.null(trueSigmaSquaredE)) {
    sigmaSquaredEFixed = FALSE
    trueSigmaSquaredEValues = initialSigmaSquaredE
  } else {
    sigmaSquaredEFixed = TRUE
    trueSigmaSquaredEValues = trueSigmaSquaredE
  }
  
  output = multivariateGaussianNetworkLerouxAllMHUpdate(standardizedX = standardizedX,
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
                                                      xi = xi,
                                                      omega = omega,
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
                                                      varianceCovarianceUFixed = varianceCovarianceUFixed,
                                                      sigmaSquaredEFixed = sigmaSquaredEFixed,
                                                      trueBetaValues = trueBetaValues,
                                                      trueSpatialRandomEffectsValues = trueSpatialRandomEffectsValues,
                                                      trueURandomEffectsValues = trueURandomEffectsValues,
                                                      trueSpatialTauSquaredValues = trueSpatialTauSquaredValues,
                                                      trueSpatialRhoValues = trueSpatialRhoValues,
                                                      trueVarianceCovarianceUValues = trueVarianceCovarianceUValues,
                                                      trueSigmaSquaredEValues = trueSigmaSquaredEValues,
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
  sigmaSquaredESamples = output[[7]]
  
  betaAcceptanceRate = rep(1, ncol(betaSamples))
  spatialRandomEffectsAcceptanceRate = output[[8]]
  uRandomEffectsAcceptanceRate = rep(1, ncol(uRandomEffectsSamples))
  spatialRhoAcceptanceRate = output[[9]]
  spatialTauSquaredAcceptanceRate = rep(1, numberOfResponses)
  varianceCovarianceUAcceptanceRate = rep(1, numberOfResponses  + (numberOfResponses * numberOfResponses - numberOfResponses) / 2)
  sigmaSquaredEAcceptanceRate = rep(1, numberOfResponses)
  
  acceptanceRates = c(betaAcceptanceRate, spatialTauSquaredAcceptanceRate, 
                      spatialRhoAcceptanceRate, varianceCovarianceUAcceptanceRate, sigmaSquaredEAcceptanceRate)
  
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
  colnames(sigmaSquaredESamples) = paste("sigmaSquaredE", seq(1, numberOfResponses)) 
  colnames(uRandomEffectsSamples) = uRandomEffectsColumnNames
  colnames(varianceCovarianceUSamples) = paste("sigmaU", seq(1, numberOfResponses  + (numberOfResponses * numberOfResponses - numberOfResponses) / 2)) 
  samples = cbind(betaSamples, spatialTauSquaredSamples, spatialRhoSamples, varianceCovarianceUSamples, sigmaSquaredESamples)
  
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
                 squareSpatialNeighbourhoodMatrix = squareSpatialNeighbourhoodMatrix ,
                 spatialAssignment = spatialAssignment ,
                 W = W,
                 standardizedX = standardizedX,
                 samples = samples,
                 betaSamples = betaSamples,
                 spatialTauSquaredSamples = spatialTauSquaredSamples,
                 spatialRhoSamples = spatialRhoSamples,
                 varianceCovarianceUSamples= varianceCovarianceUSamples,
                 sigmaSquaredESamples = sigmaSquaredESamples,
                 spatialRandomEffectsSamples = spatialRandomEffectsSamples,
                 uRandomEffectsSamples = uRandomEffectsSamples,
                 acceptanceRates = acceptanceRates,
                 betaAcceptanceRate = betaAcceptanceRate,
                 spatialTauSquaredAcceptanceRate = spatialTauSquaredAcceptanceRate,
                 spatialRhoAcceptanceRate = spatialRhoAcceptanceRate,
                 varianceCovarianceUAcceptanceRate = varianceCovarianceUAcceptanceRate,
                 sigmaSquaredEAcceptanceRate = sigmaSquaredEAcceptanceRate,
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