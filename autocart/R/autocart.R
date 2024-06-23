#' Create the object used for the controlling of the splits in the autocart model
#'
#' @param minsplit The minimum observations in a node before a split is attempted
#' @param minbucket The minimum number of observations in a terminal node.
#' @param maxdepth Set the maximum depth in the final tree. A root node is counted as a height of 0.
#' @param maxobsMtxCalc Optional maximum number of observations in a node where computationally intensive matrix calculations like autocorrelation and compactness are performed.
#' @param distpower The power of inverse distance to use when calculating spatial weights matrix.
#' @param islonglat Are the coordinates longitude and latitude coordinates? If TRUE, then use great circle distance calculations
#' @param givePredAsFactor In the returned autocart model, should the prediction vector also be returned as a factor?
#' @param retainCoords After creating the autocart model, should the coordinates for each of the predictions be kept in the returned model?
#' @param useGearyC Should autocart use Geary's C instead of Moran's I in the splitting function?
#' @param runParallel Logical value indicating whether autocart should run using parallel processing.
#' @param spatialWeightsType What type of spatial weighting should be used when calculating spatial autocorrelation? Options are "default" or "gaussian".
#' @param customSpatialWeights Use this parameter to pass in an optional spatial weights matrix for use in autocorrelation calculations. Must have nrow and ncol equal to rows in training dataframe.
#' @param spatialBandwidthProportion What percentage of the maximum pairwise distances should be considered the maximum distance for spatial influence? Cannot be simultaneously set with \code{spatialBandwidth}
#' @param spatialBandwidth What is the maximum distance where spatial influence can be assumed? Cannot be simultaneously set with \code{spatialBandwidthProportion}.
#' @param asForest A logical indicating if the tree should be created as a forest component with random subsetting of predictors at each node. Set this to true if you are using this tree inside an ensemble.
#' @return An object passed in to the \code{autocart} function that controls the splitting.
#'
#' @examples
#' # Load some data for an autocartControl example
#' snow <- na.omit(read.csv(system.file("extdata", "ut2017_snow.csv", package = "autocart")))
#' y <- snow$yr50[1:40]
#' X <- data.frame(snow$ELEVATION, snow$MCMT, snow$PPTWT)[1:40, ]
#' locations <- as.matrix(cbind(snow$LONGITUDE, snow$LATITUDE))[1:40, ]
#'
#' # Create a control object that disallows the tree from having a depth more
#' # than 10 and give spatial weights only to observations that are a third of the
#' # distance of the longest distance between any two points in the dataset.
#' snow_control <- autocartControl(maxdepth = 10, spatialBandwidthProportion = 0.33)
#'
#' # Pass the created control object to an autocart model
#' snow_model <- autocart(y, X, locations, 0.30, 0, snow_control)
#' @export
autocartControl <- function(minsplit = 20, minbucket = round(minsplit/3), maxdepth = 30,
                            maxobsMtxCalc = NULL, distpower = 1, islonglat = TRUE,
                            givePredAsFactor = TRUE, retainCoords = TRUE, useGearyC = FALSE,
                            runParallel = TRUE, spatialWeightsType = "default",
                            customSpatialWeights = NULL,
                            spatialBandwidthProportion = 1,
                            spatialBandwidth = NULL,
                            asForest = FALSE) {

  # Check the TYPES on the user input
  if (!is.numeric(minsplit)) {
    stop("\"minsplit\" parameter must be a numeric or integer.")
  }
  if (!is.numeric(minbucket)) {
    stop("\"minbucket\" parameter must be a numeric or integer.")
  }
  if (!is.numeric(distpower)) {
    stop("\"distpower\" parameter must be a numeric or integer.")
  }
  if (!is.logical(islonglat)) {
    stop("\"islonglat\" parameter must be logical.")
  }
  if (!is.logical(givePredAsFactor)) {
    stop("\"givePredAsFactor\" parameter must be logical.")
  }
  if (!is.logical(retainCoords)) {
    stop("\"retainCoords\" parameter must be logical.")
  }
  if (!is.logical(useGearyC)) {
    stop("\"useGearyC\" parameter must be logical.")
  }
  if (!is.logical(runParallel)) {
    stop("\"runParallel\" must be logical.")
  }
  if (!is.logical(asForest)) {
    stop("\"asForest\" must be a logical value.")
  }
  if (!is.character(spatialWeightsType)) {
    stop("\"spatialWeightsType\" must be a valid character type.")
  }
  if (!is.null(customSpatialWeights) & !is.numeric(customSpatialWeights)) {
    stop("\"customSpatialWeights\" must be a numeric type.")
  }
  if (!is.null(spatialBandwidthProportion) & !is.numeric(spatialBandwidthProportion)) {
    stop("\"spatialBandwidthProportion\" must be numeric.")
  }
  if (!is.null(spatialBandwidth) & !is.numeric(spatialBandwidth)) {
    stop("\"spatialBandwidth\" must be numeric.")
  }
  if (!is.null(maxobsMtxCalc) & !is.numeric(maxobsMtxCalc)) {
    stop("\"maxobsMtxCalc\" argument must be a numeric number.")
  }

  # This is the allowable set of weighting types
  validSpatialWeightings <- c("default", "gaussian")

  if (!(spatialWeightsType %in% validSpatialWeightings)) {
    stop("spatial weighting type not recognized.")
  }

  # Weights has to be a matrix
  if (!is.null(customSpatialWeights) & !is.matrix(customSpatialWeights)) {
    stop("\"customSpatialWeights\" must be a matrix.")
  }

  # The user must only supply one of spatialBandwidthProportion and spatialBandwidth
  if (!missing(spatialBandwidthProportion) & !missing(spatialBandwidth)) {
    stop("User must only supply one of \"spatialBandwidthProportion\" and \"spatialBandwidth\"")
  }

  # If the user supplies both a weighting scheme or their own weights matrix, that's a problem.
  if (!missing(spatialWeightsType) & !missing(customSpatialWeights)) {
    warning("Ambiguous weighting scheme: \"spatialWeightsType\" will be overriden with the supplied custom spatial weights.")
  }

  # If they only supply spatialBandwidth, then we want to set the proportion to NULL so that the autocart
  # function knows to use the user-overridden spatialBandwidth parameter
  if (!missing(spatialBandwidth) & missing(spatialBandwidthProportion)) {
    spatialBandwidthProportion <- NULL
  }

  # If they only supply customSpatialWeights, then we want to set the weighting type to NULL so that autocart
  # knows to not calculate its own weight matrix
  if (!missing(customSpatialWeights)) {
    spatialWeightsType <- "custom"
  }

  # if the user specifies only minbucket, then the splitting function will have
  # issues without an appropriately set minsplit
  if (missing(minsplit) && !missing(minbucket)) {
    minsplit <- minbucket * 3
  }

  if (!(is.null(spatialBandwidth))) {
    if (spatialBandwidth < 0) {
      stop("\"spatialBandwidth\" can not be negative.")
    }
  }
  if (!is.null(spatialBandwidthProportion)) {
    if ((spatialBandwidthProportion < 0 | spatialBandwidthProportion > 1)) {
      stop("\"spatialBandwidthProportion\" must be between 0 and 1.")
    }
  }

  minsplit = as.integer(minsplit)
  minbucket = as.integer(minbucket)
  maxdepth = as.integer(maxdepth)
  distpower = as.integer(distpower)
  maxobsMtxCalc = as.integer(maxobsMtxCalc)
  islonglat = as.logical(islonglat)
  givePredAsFactor = as.logical(givePredAsFactor)
  retainCoords = as.logical(retainCoords)
  useGearyC = as.logical(useGearyC)
  spatialWeightsType = as.character(spatialWeightsType)
  spatialBandwidth = as.numeric(spatialBandwidth)
  spatialBandwidthProportion = as.numeric(spatialBandwidthProportion)
  asForest = as.logical(asForest)

  control <- list(
    minsplit = minsplit,
    minbucket = minbucket,
    maxdepth = maxdepth,
    distpower = distpower,
    maxobsMtxCalc = maxobsMtxCalc,
    islonglat = islonglat,
    givePredAsFactor = givePredAsFactor,
    retainCoords = retainCoords,
    useGearyC = useGearyC,
    saddlepointApproximation = FALSE,
    runParallel = runParallel,
    spatialWeightsType = spatialWeightsType,
    customSpatialWeights = customSpatialWeights,
    spatialBandwidthProportion = spatialBandwidthProportion,
    spatialBandwidth = spatialBandwidth,
    asForest = asForest
  )

  # Set the name for the control object
  class(control) <- append(class(control), "autocartControl")
  control
}

#' Relative mean absolute error
#' @param pred The vector of predictions
#' @param obs The actual observed values
#' @param na.rm Should NA values be taken out of the vectors?
#' @return The relative mean average error of the two vectors.
#'
#' @examples
#' # Create two vectors, add some noise, and evaluate the RMAE.
#' firstVec <- 1:10
#' secondVec <- 1:10 + rnorm(10)
#' rmae(firstVec, secondVec)
#' @export
rmae <- function(pred, obs, na.rm = TRUE) {
  if (length(pred) != length(obs)) {
    stop("Predicted and observed vectors must be same length.")
  }
  mean(abs((pred-obs)/mean(obs))*100, na.rm = na.rm)
}

#' Find the best alpha, beta, and bandwidth values with k-fold cross-validation
#'
#' @param response The vector of response values to test on.
#' @param data The data frame of predictor variables.
#' @param locations The n by 2 matrix of coordinate information for the known observations
#' @param k The number of folds to create in k-fold cross-validation for tuning
#' @param control An optional control function to send to the autocart creation function
#' @param customGroups Here, you may supply custom groups for cross-validation. This parameter must be supplied as a factor and labeled from 1:numLevels.
#' @param alphaVals Override the alpha values that are expanded in the grid in this function.
#' @param betaVals Override the beta values that are expanded in the grid in this function.
#' @param bandwidthVals Override the bandwidth values that are expanded in the grid in this function.
#' @param outputProgress Print the result of the cross-validations as you are going. This is useful when the cross-validation will be very long and you do not wish to wait.
#' @param useSpatialNodes Use an interpolative process at the terminal nodes of the autocart tree for the prediction process
#' @param spatialNodesMethod The type of interpolation to use at the terminal nodes
#' @param spatialNodesDistPower The power parameter to use in inverse distance weighting at terminal nodes
#' @param spatialNodesDistPowerRange The ranged power parameter p1, p2 to use for a varying power parameter
#' @param spatialNodesModelByResidual Do the interpolative process on the residuals of the prediction formed by response average at terminal nodes
#' @return A list of the labeled optimal parameters that were chosen for the best predictive accuracy on cross-validation.
#'
#' @examples
#' # Load some data for an autotune example
#' # (Note that a low sample size is used here for quick example computation.
#' #  In a practical application this function can be quite computationally
#' #  demanding due to the grid-search nature of the function.)
#' snow <- na.omit(read.csv(system.file("extdata", "ut2017_snow.csv", package = "autocart")))
#' y <- snow$yr50[1:35]
#' X <- data.frame(snow$ELEVATION, snow$MCMT, snow$PPTWT)[1:35, ]
#' locations <- as.matrix(cbind(snow$LONGITUDE, snow$LATITUDE))[1:35, ]
#'
#' # Find optimal parameters via cross-validation. We'll search through the
#' # following alpha/beta/bandwidth values:
#' alphaVec <- c(0.0, 0.5)
#' betaVec <- c(0.0, 0.2)
#' bandwidthVec <- c(1.0)
#'
#' # We'll find the optimal values with 3-fold cross validation:
#' # (Due to the large number of cross-validations and trainings that occur,
#' # this can take a few minutes.)
#' myTune <- autotune(y, X, locations, k = 3, alphaVals = alphaVec,
#'                    betaVals = betaVec, bandwidthVals = bandwidthVec)
#' # Inspect the results
#' myTune
#'
#' @export
autotune <- function(response, data, locations, k = 8, control = NULL, customGroups = NULL,
                     alphaVals = NULL, betaVals = NULL, bandwidthVals = NULL,
                     outputProgress = FALSE, useSpatialNodes = FALSE,
                     spatialNodesMethod = "idw", spatialNodesDistPower = 2,
                     spatialNodesDistPowerRange = c(0, 2), spatialNodesModelByResidual = FALSE) {

  # Use a default control if nothing is supplied
  if (missing(control)) {
    control <- autocartControl()
  }

  # Error checking
  # -----------------
  if (!is.numeric(response)) {
    stop("response vector must be numeric.")
  }
  if (!is.data.frame(data)) {
    stop("\"data\" must be a data frame.")
  }
  if (!is.matrix(locations)) {
    stop("\"locations\" must be a matrix.")
  }
  if (!inherits(control, "autocartControl")) {
    stop("\"control\" must be obtained from the autocartControl function.")
  }
  if (!is.numeric(k)) {
    stop("\"k\" must be numeric.")
  }
  if (missing(k) & missing(customGroups)) {
    stop("\"k\", the number of folds for cross-validation must be supplied")
  }
  if (!missing(customGroups) & length(customGroups) != length(response)) {
    stop("Custom groups for cross-validation is not the same length as the response.")
  }
  if (!missing(customGroups) & !is.factor(customGroups)) {
    stop("Custom groups must be supplied as a factor.")
  }
  if (length(response) != nrow(data)) {
    stop("Response vector must be the same length as the rows in the data.")
  }
  if (length(response) != nrow(locations)) {
    stop("Response vector must be the same length as the rows in the locations.")
  }

  # Warnings
  # ----------
  if (!missing(k) & !missing(customGroups)) {
    warning("Both \"k\" and \"customGroups\" supplied to autotune, this is ambiguous. Will cross-validate with customGroups.")
  }
  if (!useSpatialNodes & (!missing(spatialNodesMethod) | !missing(spatialNodesDistPower) | !missing(spatialNodesDistPowerRange) | !missing(spatialNodesModelByResidual))) {
    warning("Spatial nodes custom parameters are supplied when \"useSpatialNodes\" parameter has been specified to be FALSE.")
  }

  # If the range of alpha/beta/bandwidth is not supplied, then we create them here
  defaultStep <- 0.20
  if (missing(alphaVals)) {
    alphaVals <- seq(0, 1.0, defaultStep)
  }
  if (missing(betaVals)) {
    betaVals <- seq(0, 1.0, defaultStep)
  }
  if (missing(bandwidthVals)) {
    bandwidthVals <- seq(defaultStep, 1.0, defaultStep)
  }

  # Create the grid of all alpha, beta, and bandwidth proportion values
  testingGrid <- expand.grid(alpha = alphaVals, beta = betaVals, bandwidth = bandwidthVals)

  # Take out all the rows where alpha+beta is greater than 1
  testingGrid <- testingGrid[(testingGrid$alpha + testingGrid$beta) <= 1.0, ]

  # Take out any rows where the bandwidth is zero, as that will cause problems in autocart
  testingGrid <- testingGrid[testingGrid$bandwidth != 0, ]

  # Create the groups of cross-validation
  xvs <- rep(NA, length = length(response))
  if (!missing(customGroups)) {
    xvs <- customGroups
  } else {
    xvs <- rep(1:k, length = length(response))
    xvs <- sample(xvs)
    xvs <- as.factor(xvs)
  }

  # With all the configurations in testingGrid, return the error rate.
  minimumRMAE <- 100000
  rowWithBestRMAE <- -1
  testingRMAE <- vector(mode = "numeric", length = nrow(testingGrid))
  for (testingRow in 1:nrow(testingGrid)) {
    myAlpha <- testingGrid$alpha[testingRow]
    myBeta <- testingGrid$beta[testingRow]
    myBandwidth <- testingGrid$bandwidth[testingRow]

    control$spatialBandwidthProportion <- myBandwidth

    # Cross-validate
    predVector <- rep(NA, length = length(response))
    for (fold in 1:length(levels(xvs))) {
      train_data <- data[xvs != fold, ]
      train_response <- response[xvs != fold]
      train_locations <- locations[xvs != fold, ]

      test_data <- data[xvs == fold, ]
      test_locations <- locations[xvs == fold, ]

      trainedModel <- autocart(train_response, train_data, train_locations, myAlpha, myBeta, control)

      # Make the prediction depending on the type of prediction process
      if (useSpatialNodes) {
        if (missing(spatialNodesDistPowerRange)) {
          predVector[xvs == fold] <- spatialNodes(trainedModel, test_data, test_locations,
                                                  spatialNodesMethod, spatialNodesDistPower,
                                                  modelByResidual = spatialNodesModelByResidual, decideByGC = FALSE)
        } else {
          predVector[xvs == fold] <- spatialNodes(trainedModel, test_data, test_locations,
                                                  spatialNodesMethod, distpowerRange = spatialNodesDistPowerRange,
                                                  modelByResidual = spatialNodesModelByResidual, decideByGC = FALSE)
        }
      } else {
        predVector[xvs == fold] <- predictAutocart(trainedModel, test_data)
      }
    }

    # Get the result and possibly output to console
    thisRMAE <- rmae(predVector, response)
    testingRMAE[testingRow] <- rmae(predVector, response)

    if (outputProgress) {
      print(paste("CV ", testingRow, "/", nrow(testingGrid), sep=""))
      if (thisRMAE < minimumRMAE) {
        print(paste("New min RMAE:", thisRMAE))
      }
    }
    if (thisRMAE < minimumRMAE) {
      minimumRMAE <- thisRMAE
      rowWithBestRMAE <- testingRow
    }
  }

  # Output as a list the parameters that gave the best RMAE
  list(alpha = testingGrid$alpha[rowWithBestRMAE],
       beta = testingGrid$beta[rowWithBestRMAE],
       bandwidth = testingGrid$bandwidth[rowWithBestRMAE],
       bestRMAE = minimumRMAE)
}
