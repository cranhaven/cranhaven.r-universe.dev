
#############################################
# Default preProcessing function
#
# Input: intamap object
#        lgFUN - function for grouping data locally
#        cid - country ID - or other regional grouping factor
#
# Output: intamap object with the following added
#         localBias - data frame with local biases
#         regionalBias - data frame with biases between countries
#         Modifications of observations
#         Elevations added (still not properly implemented
#         Duplicated data observations deleted
#         Projections are conformed and an interpolation projection
#                     set if they do not conform, or dont have a projection
#
###########################################


preProcess.default = function(object,...) {
  params = object$params
  observations = object$observations
#  FUN = try(match.fun(cleanData),silent=TRUE)
#  if (!inherits(FUN,"try-error")) {
#    print("Found function cleanData for cleaning data")
#    observations = FUN(observations,...) # practically unimplemented
#  } else if (sum(duplicated(coordinates(observations))) > 1) {
#  FUN = try(match.fun(findElevation),silent=TRUE)
#  if (!inherits(FUN,"try-error")) {
#    print("Found function findElevation for adding elevations to data frame")
#    observations$elev = FUN(observations,...) # practically unimplemented
#  }
  
  object$observations = observations
#  if (!is.na(params$removeBias[[1]]) && require(intamapInteractive)) 
#		object = biasCorr(object,...)
  object
}

estimateParameters.default = function(object, ...) {
	stop(paste("there is no parameter estimation method or default method for objects of class",class(object)))
}

spatialPredict.default = function(object, ...) {
	stop(paste("there is no prediction method or default method for objects of class",class(object)))
}

postProcess.default = function(object, ...) {

# Tranform output to requested target projection
    if ("targetCRS" %in% names(object)) {
      targetCRS = object$targetCRS
      if (proj4string(object$predictions) != targetCRS) {
          object$predictions = as(st_transform(as(object$predictions, "sf"), crs = targetCRS), "Spatial")
    }
  }
# find out what to output
  object$outputTable = getOutputTable(object)

	# write to data base

	return(object)
}


