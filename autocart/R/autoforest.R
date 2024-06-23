#' Create a forest of autocart trees..
#'
#' @param response The response vector that goes along with the dataframe of predictors.
#' @param data The dataframe of predictors.
#' @param locations A matrix of the locations of the dataframe of predictors.
#' @param alpha The percentage of weighting on spatial autocorrelation in the splitting function.
#' @param beta The percentage of weighting on spatial compactness in the splitting function.
#' @param control A control object from the \code{autocartControl} function that will be used for each tree in the forest.
#' @param numtrees The number of autocart trees to create in the forest.
#' @param mtry The number of variables to subset at each node of the splitting in the trees. By default, this will be 1/3 of the features.
#' @return An object of type "autoforest", which is a list of the autocart trees.
#'
#' @examples
#' # Load some data for an autoforest example
#' snow <- na.omit(read.csv(system.file("extdata", "ut2017_snow.csv", package = "autocart")))
#' y <- snow$yr50[1:40]
#' X <- data.frame(snow$ELEVATION, snow$MCMT, snow$PPTWT, snow$HUC)[1:40, ]
#' locations <- as.matrix(cbind(snow$LONGITUDE, snow$LATITUDE))[1:40, ]
#'
#' # Create a control object for the autoforest tree
#' snow_control <- autocartControl(spatialBandwidthProportion = 1.0)
#'
#' # Create an autoforest model with 5 trees
#' snow_model <- autoforest(y, X, locations, 0.30, 0, snow_control, numtrees = 5)
#'
#' @export
autoforest <- function(response, data, locations, alpha, beta, control, numtrees, mtry = NULL) {

  # Error check
  if (!is.numeric(response)) {
    stop("Response vector must be a numeric type.")
  }
  if (!is.data.frame(data)) {
    stop("\"data\" argument must be of type data.frame.")
  }
  if (!is.matrix(locations)) {
    stop("\"locations\" argument must be a matrix.")
  }
  if (!is.numeric(alpha)) {
    stop("Alpha argument is not numeric.")
  }
  if (!is.numeric(beta)) {
    stop("Beta argument is not numeric.")
  }
  if (!inherits(control, "autocartControl")) {
    stop("\"control\" argument must be of type \"autocartControl\", obtained from the autocartControl() function.")
  }
  if (!is.numeric(numtrees)) {
    stop("\"numtrees\" argument is not numeric.")
  }
  if (length(response) != nrow(data)) {
    stop("Response vector must have the same length as the number of rows in \"data\"")
  }
  if (nrow(data) != nrow(locations)) {
    stop("\"data\" and \"locations\" must have the same number of rows.")
  }
  if (ncol(locations) != 2) {
    stop("\"locations\" matrix must have exactly two columns.")
  }
  if (length(alpha) != 1) {
    stop("\"alpha\" must be of length 1.")
  }
  if (length(beta) != 1) {
    stop("\"beta\" must be of length 1.")
  }
  if (length(numtrees) != 1) {
    stop("\"numtrees\" must be of length 1.")
  }


  numtrees <- as.integer(numtrees)
  n <- length(response)
  nFeatures <- ncol(data)
  mTry <- ceiling(nFeatures / 3)

  if (!missing(mtry)) {
    mTry <- mtry
  }
  if (mTry < 1 | mTry > nFeatures) {
    stop("\"mtry\" is not a valid number. Ensure it is at least one and no less than the number of features.")
  }

  allTrees <- vector("list", length = numtrees)

  for (treeIndex in 1:numtrees) {

    # Bootstrapped sample of data -
    # Sample 2/3 of the data to prevent infinite spatial weights.
    indices <- 1:n
    indices <- sample(indices, size = as.integer((2/3)* n))

    thisResponse <- response[indices]
    thisData <- data[indices, ]
    thisLocations <- locations[indices, ]

    # Randomly choose a number of features - default 1/3 of the total number predictors
    # NOTE: This is now done at the node level inside the CPP code.
    #allFeatures <- 1:nFeatures
    #nodeFeatures <- sample(allFeatures, mTry, replace = FALSE)
    # Select only the data that is needed
    #thisData <- thisData[ , nodeFeatures]

    # Split as a forest
    control$asForest <- TRUE
    control$asForestMTry <- mTry

    tree <- autocart(thisResponse, thisData, thisLocations, alpha, beta, control)
    allTrees[[treeIndex]] <- tree
  }

  class(allTrees) <- append(class(allTrees), "autoforest")
  allTrees
}

#' Make a prediction using an autoforest model returned from the \code{autoforest} function.
#'
#' @param autoforestModel An S3 object of type "autoforest" returned from the \code{autoforest} function.
#' @param newdata The dataframe of predictors for use in prediction.
#' @param newdataCoords the matrix of locations for all the information in newdata. Required argument if you set "useSpatialNodes" to TRUE.
#' @param useSpatialNodes If TRUE, instead of running all the observations through the autocart tree, use the \code{spatialNodes} function to make predictions.
#' @param method If using the spatial nodes type of prediction, then the type of interpolation to use. The options are "idw" and "tps".
#' @param distpower If using "idw" for the method, the power on distance. For example, setting this to 2 would mean inverse squared distance squared weighting.
#' @param distpowerRange If using "idw" for the interpolation method, the range of distance powers to use on inverse distance weighting matched to terminal node Moran I measurements.
#' @param modelByResidual When using interpolation, make a prediction using the region of interest's average and then interpolate the residual.
#' @param decideByGC Use Geary's C in deciding to induce a local spatial process rather than Moran's I.
#' @return A vector of predictions that correspond to the rows in \code{newdata}.
#'
#' @examples
#' # Load some data for an autoforest example
#' snow <- na.omit(read.csv(system.file("extdata", "ut2017_snow.csv", package = "autocart")))
#' y <- snow$yr50[1:40]
#' X <- data.frame(snow$ELEVATION, snow$MCMT, snow$PPTWT, snow$HUC)[1:40, ]
#' locations <- as.matrix(cbind(snow$LONGITUDE, snow$LATITUDE))[1:40, ]
#'
#' # Create a control object for the autoforest tree
#' snow_control <- autocartControl(spatialBandwidthProportion = 1.0)
#'
#' # Create an autoforest model with 5 trees (low number chosen for computation time)
#' snow_model <- autoforest(y, X, locations, 0.30, 0, snow_control, numtrees = 5)
#'
#' # Predict for a subset of the data
#' new_X <- X[1:10, ]
#' new_loc <- locations[1:10, ]
#' predicted_values <- predictAutoforest(snow_model, new_X, new_loc, TRUE)
#' @export
predictAutoforest <- function(autoforestModel, newdata, newdataCoords = NULL, useSpatialNodes = FALSE,
                              method = "idw", distpower = 2, distpowerRange = c(0, 2),
                              modelByResidual = TRUE, decideByGC = FALSE) {

  # Error check
  if (!inherits(autoforestModel, "autoforest")) {
    stop("\"autoforestModel\" parameter must be of type \"autoforest\", returned from the autoforest() function.")
  }
  if (useSpatialNodes & missing(newdataCoords)) {
    stop("If using spatialNodes in predictAutoforest, newdataCoords must be provided.")
  }
  if (!is.data.frame(newdata)) {
    stop("newdata must be a dataframe.")
  }
  if (!is.matrix(newdataCoords)) {
    stop("newdataCoords must be a matrix.")
  }
  if (nrow(newdata) != nrow(newdataCoords)) {
    stop("Number of rows in newdata and newdataCoords must be the same.")
  }
  if (ncol(newdataCoords) != 2) {
    stop("newdataCoords must have exactly two columns.")
  }
  if (!is.numeric(distpowerRange)) {
    stop("\"distpowerRange\" must be a numeric vector.")
  }
  if (length(distpowerRange) != 2) {
    stop("\"distpowerRange\" must have exactly two elements.")
  }
  if (distpowerRange[2] <= distpowerRange[1]) {
    stop("distpowerRange's second element must be greater than its first element.")
  }
  if (!missing(distpowerRange) & !missing(distpower)) {
    stop("Both distpowerRange and distpower are provided in spatialNodes, this is ambiguous.")
  }

  # Warnings
  if (!useSpatialNodes & (!missing(method) | !missing(distpower) | !missing(decideByGC) | !missing(distpowerRange) | !missing(modelByResidual))) {
    warning("Spatial nodes parameters \"method\", \"distpower\" \"distpowerRange\", \"modelByResidual\", and \"decideByGC\" are being ignored as useSpatialNodes is FALSE.")
  }


  # Induce a spatial process at the terminal nodes if desired
  if (!useSpatialNodes) {
    predictionList <- lapply(autoforestModel, predictAutocart, newdata)
  } else {
    if (!missing(distpowerRange)) {
      predictionList <- lapply(autoforestModel, spatialNodes, newdata = newdata,
                               newdataCoords = newdataCoords, method = method,
                               distpowerRange = distpowerRange,
                               modelByResidual = modelByResidual, decideByGC = decideByGC)
    } else {
      predictionList <- lapply(autoforestModel, spatialNodes, newdata = newdata,
                               newdataCoords = newdataCoords, method = method,
                               distpower = distpower,
                               modelByResidual = modelByResidual, decideByGC = decideByGC)
    }
  }


  numTrees <- length(predictionList)

  # Error check and makes sure that all elements of predictionList are
  # same length
  sameLength <- length(predictionList[[1]])
  for (i in 2:numTrees) {
    if (length(predictionList[[i]]) != sameLength) {
      stop("Error in predict.autoforest: Not all elements of predictionList are same length.")
    }
    sameLength <- length(predictionList[[i]])
  }

  returnVector <- vector(mode="numeric", length=length(predictionList[[1]]))
  for (i in 1:numTrees) {
    returnVector <- returnVector + predictionList[[i]]
  }
  returnVector / numTrees
}
