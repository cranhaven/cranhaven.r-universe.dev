#####################################
#
# getIntamapParams - function for setting intamap parameters
# Input - parameters to be set
# Output - parameter list containing:
#
# formulaString = formula string for parameter estimation and 
#                 prediction functions
# doAnisotropy = defining whether anisitropy should be calculated
# removeBias = definition which biases to remove
# addBias = Biases that can be added again (if possible, e.g. regional biases 
#           might be added, localBias cannot be added
# biasRemovalMethod = Which method to use for bias removal, 
#                    "UK" (universal kriging using whole dataset) 
#                     or "LM" (local methods)
# doCluster = if the prediction region should be divided into a group of clusters
# maxCluster = The maximum number of clusters if clusters will be used
# numberOfClusters = If a fixed number of clusters is to be used (still 
#                    to be decided exactly how to use this
# nmax = maximum number of neighbours to use for kriging
# predictType = List of different prediction types (all TRUE/FALSE)
#     threshCor = Prediction corrected for shrinkage in kriging predictions
#                 MOK Modified ordinary kriging predictor
#                 IWQSEL IWQSEL-predictor
#     block = Predictions for block 
#     blockFat = Estimated fraction above threshold (if block kriging)
# thresh = Threshold for Fraction above threshold (blockFat) and exceedance probability (exc)
# isEmergency = parameter to disable certain functions, e.g. bias correction
# confProj = if Projections should be conformed, setting intCRS as 
#            interpolation projection
# processType = gaussian, nonGaussian, logNormal
########################################




getIntamapParams = function(oldPar, newPar,...){

  params = getIntamapDefaultParams()
  if (!missing(oldPar) && !is.null(oldPar)) params = modifyList(params, oldPar)  
  if (!missing(newPar) && !is.null(newPar)) params = modifyList(params, newPar) 
  dots = list(...)
  if (!is.null(dots)) params = modifyList(params, dots)
  class(params) = "IntamapParams"
  return(params)
}



getIntamapDefaultParams = function(doAnisotropy = TRUE, 
  testMean = FALSE, removeBias = NA,  addBias = NA, biasRemovalMethod = "LM", 
  nmax = 50, nmin = 0, omax = 0, beta = NULL, maxdist = Inf, ngrid = 100, nsim = 100, sMin = 4, block=numeric(0),  
  processType="gaussian",
  confProj = FALSE, debug.level = 0, nclus = 1, usergdal = FALSE, ... ) {
return(list(doAnisotropy = doAnisotropy, testMean = testMean, removeBias = removeBias, addBias = addBias,
  biasRemovalMethod = biasRemovalMethod, 
  nmax = nmax, nmin = nmin, omax = omax, beta = beta, maxdist = maxdist, ngrid = ngrid, nsim = nsim, sMin = 4, block = block, processType = processType,
  confProj = confProj, debug.level = debug.level, nclus = nclus, usergdal = FALSE, ... ))
}




createIntamapObject = function(observations, obsChar, formulaString, predictionLocations=100,
  targetCRS,boundaries,boundaryLines,intCRS, params=list(),boundFile,lineFile,class="idw",
  outputWhat, blockWhat = "none",...) {
  
  if (!inherits(observations, "Spatial")) observations = as(observations, "Spatial")
  if (inherits(predictionLocations, "sf") | inherits(predictionLocations, "stars")) predictionLocations = as(predictionLocations, "Spatial")
  object = list()
  dots = list(...)
  
  rgdalMessage = FALSE
  rgdalMessage2 = FALSE
  rgdalMessage3 = FALSE
  usergdal = getIntamapParams()$usergdal
  if ("usergdal" %in% names(params)) usergdal = params$usergdal else if ("usergdal" %in% names(dots)) usergdal = dots$usergdal
  if ("targetCRS" %in% names(params) && missing(targetCRS)) {
    targetCRS = params$targetCRS
    params = params[-which(names(params) == "targetCRS")]
#    if (usergdal) {
#      if (requireNamespace("rgdal", quietly = TRUE)) targetCRS = rgdal::CRSargs(CRS(targetCRS)) else rgdalMessage = TRUE
#    } else rgdalMessage3 = TRUE
    rgdalMessage2 = TRUE
  }
  if ("intCRS" %in% names(params) && missing(intCRS)) {
    intCRS = params$intCRS
    params = params[-which(names(params) == "intCRS")]
#    if (usergdal) {
#      if (requireNamespace("rgdal", quietly = TRUE)) intCRS = rgdal::CRSargs(CRS(intCRS)) else rgdalMessage = TRUE
#    } else rgdalMessage3 = TRUE
    rgdalMessage2 = TRUE
  }
  if (!is.na(proj4string(observations))) {
 #   if (usergdal) {
#      if (requireNamespace("rgdal", quietly = TRUE)) {
#        observations@proj4string = CRS(proj4string(observations)) 
#      } else  rgdalMessage = TRUE
#    } else rgdalMessage3 = TRUE
   rgdalMessage2 = TRUE
  }
  if (!missing(predictionLocations) && !is.na(proj4string(predictionLocations))) {
#    if (usergdal) {
#      if (requireNamespace("rgdal", quietly = TRUE)) 
#           predictionLocations@proj4string = CRS(proj4string(predictionLocations)) else rgdalMessage = TRUE
#    } else rgdalMessage3 = TRUE
    rgdalMessage2 = TRUE
  }
  
 # if (rgdalMessage) print("rgdal is not installed, standardization of projections is not possible")
#  if (rgdalMessage3) print("standardization of projections is not possible when usergdal = FALSE")
  if (rgdalMessage2) print("rgdal has been retired. 
                           As a result of this, some of the checks on projections in the 
                           intamap package have  disappeared")
  
  if (!missing(observations) && !extends(class(observations),"Spatial")) 
  	stop("observations not object of class Spatial*")
  if (missing(observations)) 
  	stop("Observations not submitted, cannot perform interpolation without data")
  if (!missing(observations)) 
  	object$observations = observations 
  if (!missing(obsChar) && !all(is.na(obsChar)))
    object$obsChar = obsChar
  
  if (missing(formulaString) || is.null(formulaString)) {
    if ("value" %in% names(observations)) {
      formulaString = "value~1"
    } else formulaString = paste(names(observations)[1],"~1")
    print(paste("createIntamapObject: formulaString is missing, using: ",formulaString))
  }
  if (!inherits(formulaString,"formula")) 
  	formulaString = as.formula(formulaString)
  object$formulaString = formulaString
  if (!is.numeric(predictionLocations)) {
    if (extends(class(predictionLocations),"Spatial")) {
      object$predictionLocations = predictionLocations
    } else stop("predictionLocations not spatial object or number of samples")    
  } else {
    if (!missing(boundaries)) {
      warning("createIntamapObject: No prediction locations submitted - sampling from boundaries")
      object$predictionLocations = spsample(boundaries,predictionLocations,"regular", offset = c(0.5,0.5)) 
    } else if (!missing(observations)) {
      warning("createIntamapObject: No prediction locations submitted - sampling from bbox of observations")
      object$predictionLocations = spsample(observations,predictionLocations,"regular", offset = c(0.5,0.5)) 
    }
  }
  if (!missing(boundaries)) {
    object$boundaries = boundaries
    if (!is.na(proj4string(boundaries))) object$boundCRS = proj4string(boundaries)
  }
  if (!missing(targetCRS) && 
    !(is.na(proj4string(observations)) && is.na(proj4string(predictionLocations)))) object$targetCRS = targetCRS
  if (!missing(intCRS) && 
    !(is.na(proj4string(observations)) && is.na(proj4string(predictionLocations)))) object$intCRS = intCRS
  if (!missing(observations) && "regCode" %in% names(observations)) 
              object$regCode = unique(observations$regCode)

  if (missing(params)) object$params = getIntamapParams() else 
    object$params = getIntamapParams(params) 
  if (!missing(boundaries)) {
    objectboundaries = boundaries
  } else if (!missing(boundFile)) {
	  object$boundaries = st_read(".", boundFile)
  }
  if (!missing(boundaryLines)) {
    object$boundaryLines = boundaryLines
  } else if (!missing(lineFile)) {
    object$lineFile = lineFile
    load(lineFile)
    object[[boundaryLines]] = lineFile
  }
  if (length(names(dots))>0)
  	object = modifyList(object,dots)
  if (missing(outputWhat)) {
    if (class == "idw") {
      outputWhat = list(mean = TRUE)
    } else outputWhat = list(mean = TRUE, variance = TRUE)
  }
  object$outputWhat = outputWhat
  object$blockWhat = blockWhat
  if (object$params$confProj) object = conformProjections(object)

  class(object) = class
  
  if (sum(names(params) %in% c("methodParameters", "variogramModel", "copulaParams")) > 1) 
    stop("you can only give one out of methodParameters, variogramModel and copulaParams")
  
  if ("methodParameters" %in% names(params)) {
    methodParameters = params$methodParameters
    if (length(grep("assign",methodParameters)) >0) stop("Illegal attempt to call function assign through methodParameters")
    if (length(grep("call",methodParameters)) >0) stop("Illegal attempt to call function call through methodParameters")
    if (length(grep("cat",methodParameters)) >0) stop("Illegal attempt to call function cat through methodParameters")
    if (length(grep("count",methodParameters)) >0) stop("Illegal attempt to call function count* through methodParameters")
    if (length(grep("download",methodParameters)) >0) stop("Illegal attempt to call function download through methodParameters")
    if (length(grep("env",methodParameters)) >0) stop("methodParameters contain illegal string env")
    if (length(grep("eval",methodParameters)) >0) stop("Illegal attempt to call function eval through methodParameters")
    if (length(grep("file",methodParameters)) >0) stop("Illegal attempt to call function file* through methodParameters")
    if (length(grep("format",methodParameters)) >0) stop("Illegal attempt to call function format* through methodParameters")
    if (length(grep("gettext",methodParameters)) >0) stop("Illegal attempt to call function gettext through methodParameters")
    if (length(grep("options",methodParameters)) >0) stop("Illegal attempt to call function options through methodParameters")
    if (length(grep("parse",methodParameters)) >0) stop("Illegal attempt to call function parse through methodParameters")
    if (length(grep("paste",methodParameters)) >0) stop("Illegal attempt to call function paste through methodParameters")
    if (length(grep("print",methodParameters)) >0) stop("Illegal attempt to call function print through methodParameters")
    if (length(grep(" q",methodParameters)) >0) stop("methodParameters contain illegal string q")
    if (length(grep("read",methodParameters)) >0) stop("Illegal attempt to call function read* through methodParameters")
    if (length(grep(" rm",methodParameters)) >0) stop("Illegal attempt to call function rm through methodParameters")
    if (length(grep("scan",methodParameters)) >0) stop("Illegal attempt to call function scan through methodParameters")
    if (length(grep("sink",methodParameters)) >0) stop("Illegal attempt to call function sink through methodParameters")
    if (length(grep("shell",methodParameters)) >0) stop("Illegal attempt to call function shell through methodParameters")                                                                           
    if (length(grep("sprintf",methodParameters)) >0) stop("Illegal attempt to call function sprintf through methodParameters")
    if (length(grep("system",methodParameters)) >0) stop("Illegal attempt to call function system through methodParameters")
    if (length(grep("url",methodParameters)) >0) stop("methodParameters contain illegal string url")
    if (length(grep("write",methodParameters)) >0) stop("Illegal attempt to call function write through methodParameters")
   
    params = params[-which(names(params) == "methodParameters")]
    eval(parse(text = methodParameters))
  }
  if ("variogramModel" %in% names(params)) {
    object$variogramModel = params$variogramModel
    if (class == "idw" | class == "automatic") class(object) = "automap"
  }
  if ("copulaParams" %in% names(params)) {
    object$copulaParams = params$copulaParams
    class(object) = "copula"
  }
  if (!is.null(object$params$set.seed)) set.seed(object$params$set.seed)
  return(object)
}




#########################################################
# ConformProjection
#
# Input: Intamap object
#
# Output: Intamap object with projections of observations and
#         predictionLocations equal to
#         (a) targetCRS - if not longlat
#         (b) observationCRS - if not longlat
#         (c) predictionCRS - if not longlat
#         (d) intCRS = "epsg:3035" if both above are longlat
#
############################################################

conformProjections = function(object) {
  projObs = is.projected(object$observations)
  projPred = is.projected(object$predictionLocations)
  if (is.na(projObs) & !is.na(projPred)) {
    proj4string(object$observations) = proj4string(object$predictionLocations)
    warning("proj4string was not set for observations, assumed to be equal to proj4string for predictionLocations")
  } else if (!is.na(projObs) & is.na(projPred)) {
    proj4string(object$predictionLocations) = proj4string(object$observations)
    warning("proj4string was not set for predictionLocations, assumed to be equal to proj4string for observations")
  } else if (all(is.na(projObs),is.na(projPred))) {
    return(object) 
  } else if (!all(!is.na(projObs),!is.na(projPred))) {
# This should never occur with the modifications above
    stop(paste("observations or predictionLocations is neither projected or LongLat, whereas the other one is. \n",  
       "Observation projection is:",projObs,"\n", 
       "Prediction projection is:",projPred,"\n" ))
  }
  observations = object$observations
  predictionLocations = object$predictionLocations
  obsCRS = proj4string(observations)
  predCRS = proj4string(predictionLocations)
    sfobs = as(observations, "sf")
    sfpred = as(predictionLocations, "sf")
    obsCRS = st_crs(sfobs)
    predCRS = st_crs(sfpred)
    if ("intCRS"%in% names(object)) {
      intCRS = object$intCRS
    } else if (obsCRS == predCRS && !length(grep("longlat",obsCRS)) >0) {
      intCRS = obsCRS
    } else {
      if ("targetCRS" %in% names(object) && !length(grep("longlat", st_crs(object$targetCRS))) > 0) {
        targetCRS = object$targetCRS
        intCRS = targetCRS
      } else {
        targetCRS = predCRS
        if (!length(grep("longlat",obsCRS)) > 0) {
          intCRS = obsCRS
        } else {
          if (!length(grep("longlat",predCRS)) >0) {
            intCRS = predCRS
          } else {                
            #          intCRS = "epsg:3035"
            stop("Interpolation in longlat not possible, a projection is needed.")          
          } 
        }
      }
    }
    if (obsCRS != st_crs(intCRS))
      object$observations = as(st_transform(sfobs, crs = st_crs(intCRS)), "Spatial")
    if (predCRS != st_crs(intCRS))  
      object$predictionLocations = as(st_transform(sfpred, crs = st_crs(intCRS)), "Spatial")
    if (!is.null(object$boundaries)) {
      boundaries = object$boundaries
      boundCRS = st_crs(as(object$boundaries, "sf"))
      if (boundCRS != st_crs(intCRS)) 
        object$boundaries = as(st_transform(as(boundaries, "sf"), crs = st_crs(intCRS)), "Spatial")
    }
    if (!"intCRS" %in% names(object)) object$intCRS = intCRS
    
  
	return(object)
}











