#' Using an autocart model, use the terminal nodes to form a spatial process that uses inverse
#' distance weighting to interpolate. The prediction for the new data that is passed in is formed
#' by making a prediction to assign it to a group. Next, the residual for the new prediction is
#' formed by inverse distance weighting the residual for the other points that are a part of that geometry.
#'
#' @param autocartModel an autocart model returned from the \code{autocart} function.
#' @param newdata a dataframe that contains the same predictors that were used to form the tree.
#' @param newdataCoords a matrix of coordinates for all the predictors contained in \code{newdata}
#' @param method The type of interpolation to use. Options are "idw" for inverse distance weighting and "tps" for thin-plate splines.
#' @param distpower the power to use if you would like to use something other than straight inverse distance, such as inverse distance squared.
#' @param distpowerRange A range of distpower to use. This is an adaptive inverse distance weighting method that linearly matches measures of spatial autocorrelation measured by Moran I to the range mentioned in distpower.
#' @param modelByResidual If true, then predict using the average of the "spatial node", and then model the residual using a spatial process. If false, fit a spatial process directly.
#' @param decideByGC When determining if a spatial process should be ran at a terminal node, should we use the Geary C statistic instead of Moran I?
#' @return a prediction for the observations that are represented by \code{newdata} and \code{newdataCoords}
#'
#' @examples
#' # Load some data for a spatial nodes example
#' snow <- na.omit(read.csv(system.file("extdata", "ut2017_snow.csv", package = "autocart")))
#' y <- snow$yr50[1:40]
#' X <- data.frame(snow$ELEVATION, snow$MCMT, snow$PPTWT, snow$HUC)[1:40, ]
#' locations <- as.matrix(cbind(snow$LONGITUDE, snow$LATITUDE))[1:40, ]
#'
#' # Create an autocart model
#' snow_model <- autocart(y, X, locations, 0.30, 0)
#'
#' # Predit with the spatial node effect
#' new_X <- X[1:10, ]
#' new_loc <- locations[1:10, ]
#' spatial_node_predictions <- spatialNodes(snow_model, new_X, new_loc, distpowerRange = c(0, 2))
#'
#' @import fields
#' @import mgcv
#' @import stats
#' @export
spatialNodes <- function(autocartModel, newdata, newdataCoords, method = "idw", distpower = 2,
                         distpowerRange = c(0, 2), modelByResidual = TRUE, decideByGC = FALSE) {

  # Check user input
  if (!inherits(autocartModel, "autocart")) {
    stop("\"autocartModel\" parameter is not an autocart model.")
  }
  if (!is.data.frame(newdata)) {
    stop("newdata parameter must be a dataframe.")
  }
  if (!is.matrix(newdataCoords)) {
    stop("newdataCoords parameter must be a matrix.")
  }
  if (ncol(newdataCoords) != 2) {
    stop("newdataCoords must have only two columns.")
  }
  if (nrow(newdata) != nrow(newdataCoords)) {
    stop("The number of rows in newdata and newdataCoords are not the same.")
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

  # Use whether the autocartModel used long/lat to determine if this should use long/lat
  # (Great circle distance or euclidean distance?)
  islonglat <- autocartModel$splitparams$islonglat

  colnames(newdataCoords) <- c("X", "Y")

  # Check to make sure that the method type is allowable
  allowableMethods <- c("idw", "tps", "krg")
  if (!(method %in% allowableMethods)) {
    stop("\"method\" parameter is not a valid method.")
  }
  if (method != "idw" & !missing(distpower)) {
    warning("\"distpower\" parameter ignored, as using thin-plate splines instead of inverse distance weighting.")
  }
  if (method != "idw" & !missing(distpowerRange)) {
    warning("\"distpowerRange\" parameter ignored, as not using inverse distance weighting method.")
  }

  # Thin plate splines on the sphere requires a certain number of degrees of freedom, so
  # we need to throw an error if the number of minimum observations in each node is not at least as great
  # as the required degrees of freedom.
  tpsSphereDf <- 5
  if ((autocartModel$splitparams$minbucket <= tpsSphereDf) & islonglat & method == "tps") {
    stop("When using long/lat version of thin-plate splines, \"minbucket\" in the trained model must be greater than the degrees of freedom in the smoothing function, which defaults to 50.")
  }

  # If we are missing the decideByGC parameter, then we will use the splitting parameter that was used
  # in the creation of the autocart model.
  if (missing(decideByGC)) {
    decideByGC <- autocartModel$splitparams$useGearyC
  }

  allCoords <- autocartModel$coords
  predFactor <- as.factor(allCoords$pred)
  numLevels <- length(levels(predFactor))

  # Extract data necessary to find Moran's I at each terminal node.
  splitFrame <- autocartModel$splitframe
  allTerminalNodes <- splitFrame[splitFrame$isterminal, ]

  # Separate out all the terminal nodes into their own individual collection of points
  leafGeometryList <- vector("list", numLevels)
  for (level in 1:numLevels) {
    leafGeometryList[[level]] <- allCoords[predFactor == levels(predFactor)[level], ]
  }

  # Find minimum and maximum of Moran's I in all terminal nodes
  minimumDp <- distpowerRange[1]
  maximumDp <- distpowerRange[2]
  minimumMi <- 1.0
  maximumMi <- -1.0
  for (terminalNode in 1:nrow(allTerminalNodes)) {
    myMi <- allTerminalNodes[terminalNode, "mi"]
    if (!is.nan(myMi)) {
      if (myMi > allTerminalNodes[terminalNode, "expectedMi"]) {
        if (myMi < minimumMi) {
          minimumMi <- myMi
        }
        if (myMi > maximumMi) {
          maximumMi <- myMi
        }
      }
    }
  }

  # Find out which spatial process each new prediction belongs to
  whichLayer <- predictAutocart(autocartModel, newdata)
  if (modelByResidual) {
    returnPredictions <- whichLayer
  } else {
    returnPredictions <- rep(0, length(whichLayer))
  }

  # For each row in the new data we wish to predict, find out which spatial process it is a part of
  # then inverse distance weight each of the observations in that spatial process
  for (row in 1:length(whichLayer)) {
    thisGeometry <- leafGeometryList[[which(levels(predFactor) == whichLayer[row])]]
    thisGeometryCoordinates <- as.matrix(cbind(thisGeometry$x, thisGeometry$y))

    # Only use a spatial effect if a spatial effect exists in this node. If no spatial effect exists, just predict
    # using the average of this node.
    thisTerminalNode <- allTerminalNodes[allTerminalNodes$prediction == whichLayer[row], ]

    # Evaluate if a spatial process should exist at this terminal node, depending on whether
    # we want to use Geary's C or Moran's I.
    spatialProcessExists <- FALSE
    if (decideByGC) {
      spatialProcessExists <- thisTerminalNode$gc < thisTerminalNode$expectedGc
    } else {
      if (is.nan(thisTerminalNode$mi)) {
        spatialProcessExists <- FALSE
      } else {
        spatialProcessExists <- thisTerminalNode$mi > thisTerminalNode$expectedMi
      }
    }

    if (spatialProcessExists) {

      if (method == "idw") {
        # Get a distance matrix from this point to all other points in the geometry
        if (islonglat) {
          distToAllGeomPoints <- fields::rdist.earth(t(as.matrix(newdataCoords[row, ])), thisGeometryCoordinates)
        } else {
          distToAllGeomPoints <- fields::rdist(t(as.matrix(newdataCoords[row, ])), thisGeometryCoordinates)
        }

        # Assign distpower according to what mi is
        # (assuming that distpowerRange is supplied instead of distpower)
        if (!missing(distpowerRange)) {
          myMi <- thisTerminalNode$mi
          distpower <- ((myMi - minimumMi) / (maximumMi - minimumMi)) * (maximumDp - minimumDp) + minimumDp
        }

        # Standard inverse distance weighting procedures
        invDistMatrix <- 1 / (distToAllGeomPoints ^ distpower)
        weights <- as.vector(invDistMatrix)

        if (modelByResidual) {
          predictVector <- thisGeometry$actual - thisGeometry$pred
        } else {
          predictVector <- thisGeometry$actual
        }

        # If an infinite value exists, then we have the exact same location as something that was
        # used to train the model, in which case we should borrow that observation's value for prediction.
        if (any(is.infinite(weights))) {
          whereInfinite <- which(is.infinite(weights))
          if (length(whereInfinite > 1)) {
            whereInfinite <- whereInfinite[1]
          }
          returnPredictions[row] <- returnPredictions[row] + predictVector[whereInfinite]

        } else {
          sumWeights <- sum(weights)

          # Using the weights we found, weight the actual observation value by its weight to obtain a prediction
          predictedValue <- sum(weights * predictVector) / sumWeights

          returnPredictions[row] <- returnPredictions[row] + predictedValue
        }
      } else if (method == "tps") {
        # Thin-plate spline interpolation using the fields package

        if (modelByResidual) {
          predictVector <- thisGeometry$actual - thisGeometry$pred
        } else {
          predictVector <- thisGeometry$actual
        }

        #fit <- fields::Tps(thisGeometryCoordinates, predictVector)
        #returnPredictions[row] <- returnPredictions[row] + predict.Krig(fit, t(as.matrix(newdataCoords[row, ])))

        if (islonglat) {
          # For TPS there exists another type for spherical data.
          myData <- as.data.frame(cbind(X = thisGeometryCoordinates[, 1],
                                        Y = thisGeometryCoordinates[, 2], resp = predictVector))
          fit <- mgcv::gam(resp ~ s(X, Y, bs = "sos", k=autocartModel$splitparams$minbucket-1), data = myData)
          returnPredictions[row] <- returnPredictions[row] + predict(fit, data.frame(X = newdataCoords[row, 1],
                                                                                     Y = newdataCoords[row, 2]))
        } else {
          # Use regular TPS
          fit <- fields::Tps(thisGeometryCoordinates, predictVector)
          returnPredictions[row] <- returnPredictions[row] + predict(fit, t(as.matrix(newdataCoords[row, ])))
        }
      }
    }
  }

  return(returnPredictions)
}
