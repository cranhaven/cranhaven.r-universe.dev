#' Read in Y matrix for SCALPEL.
#'
#' This step allows the user to read in Y, the matrix of raw or processed video data, to use with several plotting functions.
#'
#' @param scalpelOutput An object returned by one of the SCALPEL functions:
#' \code{\link{scalpel}}, \code{\link{scalpelStep0}}, \code{\link{scalpelStep1}}, \code{\link{scalpelStep2}}, or \code{\link{scalpelStep3}}.
#' @param videoType Specify whether to read in the processed data from Step 0 (default; \code{videoType="processed"}) or raw data (\code{videoType="raw"}).
#' @param part The part of the video to read in, if it is split across multiple files. The default is NULL, which means that all parts will be read in and combined.
#'
#' @return An object of class \code{scalpelY} that can be provided as the \code{Y} argument in \code{\link{plotFrame}},
#' \code{\link{plotVideoVariance}}, \code{\link{plotBrightest}}, \code{\link{plotThresholdedFrame}}, and \code{\link{plotCandidateFrame}}.
#' If would like to call these functions many times, this avoids reading the video into memory repeatedly.
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "scalpel" function
#' #read in the raw data
#' rawY = getY(scalpelOutput = scalpelOutput, videoType = "raw")
#' #read in the processed data from Step 0
#' processedY = getY(scalpelOutput = scalpelOutput, videoType = "processed")
#' }
#' @export
getY = function(scalpelOutput, videoType="processed", part = NULL) {

  #check function arguments
  if (!(class(scalpelOutput) %in% c("scalpel", "scalpelStep0", "scalpelStep1", "scalpelStep2", "scalpelStep3"))) stop("The class of 'scalpelOutput' must be 'scalpel', 'scalpelStep0', 'scalpelStep1', 'scalpelStep2', or 'scalpelStep3'")
  if (videoType!="raw" & videoType!="processed") stop("Specify 'raw' or 'processed' for 'videoType'")
  if (!is.null(part)) if (length(part)>1) stop("Specify a single number for 'part'")

  output = vector("list")
  class(output) = "scalpelY"
  output$videoType = videoType
  output$part = part #NULL means all parts
  if (videoType=="raw") {
    output$allParts = scalpelOutput$partsRaw
    output$nFrames = scalpelOutput$nFramesRaw
  } else {
    output$allParts = scalpelOutput$partsDeltaf
    output$nFrames = scalpelOutput$nFramesDeltaf
  }

  if (is.null(part)) {
    output$currentFrames = 1:sum(output$nFrames)
  } else if (part==1) {
    output$currentFrames = 1:output$nFrames[1]
  } else {
    output$currentFrames = (sum(output$nFrames[1:(part-1)])+1):sum(output$nFrames[1:part])
  }
  #parts of video to read in
  if (!is.null(part)) {
    parts = part
    if (videoType=="raw") if (!(part %in% scalpelOutput$partsRaw)) stop("Specify an element of 'scalpelOutput$partsRaw' for 'part'")
    if (videoType=="processed") if (!(part %in% scalpelOutput$partsDeltaf)) stop("Specify an element of 'scalpelOutput$partsDeltaf' for 'part'")
  }  else {
    if (videoType=="raw") parts = scalpelOutput$partsRaw else parts = scalpelOutput$partsDeltaf
  }

  if (videoType=="raw") partialFileName = paste0(scalpelOutput$rawDataFolder,"Y_") else partialFileName = paste0(scalpelOutput$outputFolder,"Step0Data/Ydeltaf_part")
  #read in first part
  if (scalpelOutput$fileType=="matlab" & videoType=="raw") {
    Y = R.matlab::readMat(paste0(partialFileName,parts[1],".mat"))
    Y = with(Y, get(names(Y)))
  } else if (scalpelOutput$fileType=="text" & videoType=="raw") {
    Y = utils::read.table(paste0(partialFileName,parts[1],".txt"))
  } else if (scalpelOutput$fileType=="zippedText" & videoType=="raw") {
    Y = utils::read.table(paste0(partialFileName,parts[1],".txt.gz"))
  } else {
    Y = readRDS(paste0(partialFileName,parts[1],".rds"))
  }

  #read in remaining parts (if they are any)
  if (length(parts)>1) {
    for (currentPart in parts[2:length(parts)]) {
      if (scalpelOutput$fileType=="matlab" & videoType=="raw") {
        Ypart = R.matlab::readMat(paste0(partialFileName,currentPart,".mat"))
        Y = cbind(Y, with(Ypart, get(names(Ypart))))
      } else if (scalpelOutput$fileType=="text" & videoType=="raw") {
        Ypart = utils::read.table(paste0(partialFileName,currentPart,".txt"))
        Y = cbind(Y, Ypart)
      } else if (scalpelOutput$fileType=="zippedText" & videoType=="raw") {
        Ypart = utils::read.table(paste0(partialFileName,currentPart,".txt.gz"))
        Y = cbind(Y, Ypart)
      } else {
        Ypart = readRDS(paste0(partialFileName,currentPart,".rds"))
        Y = cbind(Y, Ypart)
      }
    }
  }
  output$Y = Y
  rm(Y)

  return(output)
}

#' Read in results from Step 0 of SCALPEL.
#'
#' This step allows the user to retrieve the object of class \code{scalpelStep0} for results from a previous session.
#'
#' @param outputFolder The existing directory where the results that the user wishes to use are saved.
#'
#' @return An object of class \code{scalpelStep0}, which can be used to run SCALPEL Step 1 using \code{\link{scalpelStep1}}
#' or can be used with the plotting functions \code{\link{plotFrame}}, \code{\link{plotThresholdedFrame}}, and \code{\link{plotVideoVariance}}.
#'
#' @seealso \code{\link{scalpelStep0}}, \code{\link{scalpel}}
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "scalpel" function
#' #folder where results were saved
#' outputFolder = "scalpelResults"
#'
#' #read previous results in
#' out = getScalpelStep0(outputFolder = outputFolder)
#' }
#' @export
getScalpelStep0 = function(outputFolder) {

  #check function arguments
  if (!dir.exists(outputFolder)) stop("Specify an existing folder with results for 'outputFolder'")

  output = vector("list")
  class(output) = "scalpelStep0"
  output$rawDataFolder = readRDS(paste0(outputFolder,"Step0Data/rawDataFolder.rds"))
  output$outputFolder = outputFolder
  output$videoHeight = readRDS(paste0(outputFolder,"Step0Data/videoHeight.rds"))
  output$partsDeltaf = readRDS(paste0(outputFolder,"Step0Data/partsDeltaf.rds"))
  output$partsRaw = readRDS(paste0(outputFolder,"Step0Data/partsRaw.rds"))
  output$nFramesRaw = readRDS(paste0(outputFolder,"Step0Data/nFramesRaw.rds"))
  output$nFramesDeltaf = readRDS(paste0(outputFolder,"Step0Data/nFramesDeltaf.rds"))
  output$minRaw = readRDS(paste0(outputFolder,"Step0Data/minRaw.rds"))
  output$maxRaw = readRDS(paste0(outputFolder,"Step0Data/maxRaw.rds"))
  output$minDeltaf = readRDS(paste0(outputFolder,"Step0Data/minDeltaf.rds"))
  output$maxDeltaf = readRDS(paste0(outputFolder,"Step0Data/maxDeltaf.rds"))
  output$fileType = readRDS(paste0(outputFolder,"Step0Data/fileType.rds"))
  output$lowThreshold = readRDS(paste0(outputFolder,"Step0Data/lowThreshold.rds"))
  output$highThreshold = readRDS(paste0(outputFolder,"Step0Data/highThreshold.rds"))

  return(output)
}

#' Read in results from Step 1 of SCALPEL.
#'
#' This step allows the user to retrieve the object of class \code{scalpelStep1} for results from a previous session.
#'
#' @param outputFolder The existing directory where the results that the user wishes to use are saved.
#' @param version The 5-digit folder ID for the results that the user wishes to load. If NULL, automatically chooses
#' the only version in outputFolder and if more than one version exists, returns an error.
#'
#' @return An object of class \code{scalpelStep1}, which can be used to run SCALPEL Step 2 using \code{\link{scalpelStep2}}
#' or can be used with the plotting function \code{\link{plotCandidateFrame}}.
#'
#' @seealso \code{\link{scalpelStep1}}, \code{\link{scalpel}}
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "scalpel" function
#' #folder where results were saved
#' outputFolder = "scalpelResults"
#'
#' #read previous results in
#' out = getScalpelStep1(outputFolder = outputFolder)
#' #note: if Step 1 has been run more than once, will need to specify 'version'
#' }
#'
#' @export
getScalpelStep1 = function(outputFolder, version=NULL) {

  if (is.null(version)) version = getStep1Version(outputFolder = outputFolder)

  #check function arguments
  step1folder = paste0(outputFolder, "Step1_", version,"/Step1Data/")
  if (!dir.exists(step1folder)) stop("The 'version' provided does not refer to an existing folder")

  output = getScalpelStep0(outputFolder = outputFolder)
  class(output) = "scalpelStep1"
  output$thresholdVec = readRDS(paste0(step1folder, "thresholdVec.rds"))
  output$Azero = readRDS(paste0(step1folder, "Azero.rds"))
  output$AzeroFrames = readRDS(paste0(step1folder, "AzeroFrames.rds"))
  output$AzeroThreshold = readRDS(paste0(step1folder, "AzeroThreshold.rds"))
  output$pixelsUse = readRDS(paste0(step1folder, "pixelsUse.rds"))
  output$version = version

  return(output)
}

#' Read in results from Step 2 of SCALPEL.
#'
#' This step allows the user to retrieve the object of class \code{scalpelStep2} for results from a previous session.
#'
#' @param outputFolder The existing directory where the results that the user wishes to use are saved.
#' @param version The 5-digit folder ID for the results that the user wishes to load. If NULL, automatically chooses
#' the only version in outputFolder and if more than one version exists, returns an error.
#' @param cutoff A value in [0,1] indicating the dendrogram cutpoint used. The default value is 0.18.
#' @param omega A value in [0,1] indicating the dissimilarity metric weight used for clustering. The default value is 0.2.
#'
#' @return An object of class \code{scalpelStep2}, which can be used to run SCALPEL Step 3 using \code{\link{scalpelStep3}}
#' or can be used with the plotting functions \code{\link{plotCluster}} and \code{\link{plotSpatial}}.
#'
#' @seealso \code{\link{scalpelStep2}}, \code{\link{scalpel}}
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "scalpel" function
#' #folder where results were saved
#' outputFolder = "scalpelResults"
#'
#' #read previous results in
#' #simplest example with default parameters:
#' out = getScalpelStep2(outputFolder = outputFolder)
#' #note: if Step 1 has been run more than once, will need to specify 'version'
#'
#' #example with optional parameters:
#' #need to enter if non-default options were used
#' out = getScalpelStep2(outputFolder = outputFolder, omega = 0.2, cutoff = 0.18)
#' }
#' @export
getScalpelStep2 = function(outputFolder, version=NULL, cutoff=0.18, omega=0.2) {

  if (is.null(version)) version = getStep1Version(outputFolder = outputFolder)

  #check function arguments
  if (cutoff<0 | cutoff>1) stop("Specify 'cutoff' in [0,1]")
  if (omega<0 | omega>1) stop("Specify 'omega' in [0,1]")
  step2folder = paste0(outputFolder,"Step1_",version,"/Step2_omega_",omega,"_cutoff_",cutoff,"/")
  if (!dir.exists(step2folder)) stop(paste0("The specified output folder (as determined by the function arguments) does not exist: ", step2folder))

  output = getScalpelStep1(outputFolder = outputFolder, version = version)
  class(output) = "scalpelStep2"
  output$treeList = readRDS(paste0(step2folder, "Step2Data/treeList.rds"))
  output$A = readRDS(paste0(step2folder, "Step2Data/A.rds"))
  output$repComps = readRDS(paste0(step2folder, "Step2Data/repComps.rds"))
  output$clusterID = readRDS(paste0(step2folder, "Step2Data/clusterID.rds"))
  output$overlapSetID = readRDS(paste0(step2folder, "Step2Data/overlapSetID.rds"))
  output$cutoff = cutoff
  output$omega = omega

  return(output)
}

#' Read in results from Step 3 of SCALPEL.
#'
#' This step allows the user to retrieve the object of class \code{scalpelStep3} for results from a previous session.
#'
#' @param outputFolder The existing directory where the results that the user wishes to use are saved.
#' @param version The 5-digit folder ID for the results that the user wishes to load. If NULL, automatically chooses
#' the only version in outputFolder and if more than one version exists, returns an error.
#' @param cutoff A value in [0,1] indicating the dendrogram cutpoint used. The default value is 0.18.
#' @param omega A value in [0,1] indicating the dissimilarity metric weight used for clustering. The default value is 0.2.
#' @param lambdaMethod How lambda was chosen: either \code{"trainval"} (default), \code{"distn"}, or \code{"user"}.
#' @param minClusterSize The minimum number of preliminary dictionary elements that a cluster must have contained to have been included
#' in the sparse group lasso. The default value is 1.
#' @param alpha The value of alpha used to fit the sparse group lasso. The default value is 0.9.
#' @param lambda The value of lambda used to fit the sparse group lasso. If NULL, automatically chooses
#' the only lambda in directory and if more than one lambda exists, returns an error.
#' @param removeBorder A logical scalar indicating whether the dictionary elements that contained pixels in the 10-pixel
#' border of the video were removed prior to fitting the sparse group lasso. The default value is \code{FALSE}.
#' @param excludeReps A vector giving the indices of which dictionary elements were excluded.
#' The default value is \code{NULL} meaning no dictionary elements were manually excluded.
#'
#' @return An object of class \code{scalpelStep3}, which can be used with the plotting functions
#' \code{\link{plotResults}}, \code{\link{plotResultsAllLambda}}, \code{\link{plotSpatial}}, \code{\link{plotTemporal}}, and \code{\link{plotBrightest}}.
#'
#' @seealso \code{\link{scalpelStep3}}, \code{\link{scalpel}}
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "scalpel" function
#' #folder where results were saved
#' outputFolder = "scalpelResults"
#'
#' #read previous results in
#' #simplest example with default parameters:
#' out = getScalpelStep3(outputFolder = outputFolder)
#' #note: if Step 1 has been run more than once, will need to specify 'version'
#'
#' #example with optional parameters:
#' #need to enter if non-default options were used
#' out = getScalpelStep3(outputFolder = outputFolder, omega = 0.2, cutoff = 0.18,
#'                      alpha = 0.9, minClusterSize = 1)
#' }
#' @export
getScalpelStep3 = function(outputFolder, version=NULL, cutoff=0.18, omega=0.2, lambdaMethod="trainval", minClusterSize=1, alpha=0.9, lambda=NULL, removeBorder=FALSE, excludeReps=NULL) {

  if (is.null(version)) version = getStep1Version(outputFolder = outputFolder)

  mainFolder = paste0(outputFolder, "Step1_", version, "/Step2_omega_", omega, "_cutoff_", cutoff, "/")

  #excludeReps is "discarded"
  if (length(excludeReps)==1) if (excludeReps=="discarded") {
    keepFilename = paste0(mainFolder,"Step2Data/keep.rds")
    if (!file.exists(keepFilename)) stop(paste0("When 'excludeReps' is set to 'discarded', the file ", keepFilename, " must exist"))
    keep = readRDS(keepFilename)
    excludeReps = which(keep=="no")
  }

  if (is.null(lambda)) lambda = getStep3Lambda(mainFolder = mainFolder, lambdaMethod = lambdaMethod,
                                               minClusterSize = minClusterSize, alpha = alpha,
                                               removeBorder = removeBorder, excludeReps = excludeReps)

  #check function arguments
  if (alpha<0 | alpha>1) stop("The value of 'alpha' must be in the interval [0,1]")
  if (!is.logical(removeBorder)) stop("Specify TRUE or FALSE for 'removeBorder'")
  if (lambdaMethod!="distn" & lambdaMethod!="trainval" & lambdaMethod!="user") stop("Specify 'distn', 'trainval', or 'user' for 'lambdaMethod', or specify 'lambda'")
  if (cutoff<0 | cutoff>1) stop("Specify 'cutoff' in [0,1]")
  if (omega<0 | omega>1) stop("Specify 'omega' in [0,1]")

  if (!is.null(excludeReps)) fileAppend = paste0("_excluded_",paste0(sort(excludeReps), collapse = "_")) else fileAppend = ""
  step3folder = paste0(outputFolder,"Step1_",version,"/Step2_omega_",omega,"_cutoff_",cutoff,
                       "/Step3_lambdaMethod_",lambdaMethod,"_lambda_",round(lambda,4),"_minClusterSize_",minClusterSize,
                       "_alpha_",alpha,"_removeBorder_",(removeBorder),fileAppend,"/")
  if (!dir.exists(step3folder)) stop(paste0("The following output folder (as determined by the function arguments) does not exist: ", step3folder))

  output = getScalpelStep2(outputFolder = outputFolder, version = version, omega = omega, cutoff = cutoff)
  class(output) = "scalpelStep3"
  output$Afilter = readRDS(paste0(step3folder,"Step3Data/Afilter.rds"))
  output$Zhat = readRDS(paste0(step3folder,"Step3Data/Zhat.rds"))
  output$lambda = readRDS(paste0(step3folder,"Step3Data/lambda.rds"))
  output$minClusterSize = minClusterSize
  output$alpha = alpha
  output$lambdaMethod = lambdaMethod
  output$removeBorder = removeBorder
  output$lambdaSeq = readRDS(paste0(step3folder,"Step3Data/lambdaSeq.rds"))
  output$ZhatList = readRDS(paste0(step3folder,"Step3Data/ZhatList.rds"))
  output$excludeReps = readRDS(paste0(step3folder,"Step3Data/excludeReps.rds"))
  output$clustersUse = readRDS(paste0(step3folder,"Step3Data/clustersUse.rds"))

  return(output)
}

#' Read in results from SCALPEL.
#'
#' This step allows the user to retrieve the object of class \code{scalpel} for results from a previous session.
#'
#' @param outputFolder The existing directory where the results that the user wishes to use are saved.
#' @param version The 5-digit folder ID for the results that the user wishes to load. If NULL, automatically chooses
#' the only version in outputFolder and if more than one version exists, returns an error.
#' @param cutoff A value in [0,1] indicating the dendrogram cutpoint used. The default value is 0.18.
#' @param omega A value in [0,1] indicating the dissimilarity metric weight used for clustering. The default value is 0.2.
#' @param lambdaMethod How lambda was chosen: either \code{"trainval"} (default), \code{"distn"}, or \code{"user"}.
#' @param minClusterSize The minimum number of preliminary dictionary elements that a cluster must have contained to have been included
#' in the sparse group lasso. The default value is 1.
#' @param lambda The value of lambda used to fit the sparse group lasso. If NULL, automatically chooses
#' the only lambda in directory and if more than one lambda exists, returns an error.
#' @param alpha The value of alpha used to fit the sparse group lasso. The default value is 0.9.
#' @param removeBorder A logical scalar indicating whether the dictionary elements that contained pixels in the 10-pixel
#' border of the video were removed prior to fitting the sparse group lasso. The default value is \code{FALSE}.
#' @param excludeReps A vector giving the indices of which dictionary elements were excluded.
#' The default value is \code{NULL} meaning no dictionary elements were manually excluded.
#'
#' @return An object of class \code{scalpel}, which can be used to rerun SCALPEL Steps 1-3 with new parameters using \code{\link{scalpelStep1}}, \code{\link{scalpelStep2}}, and \code{\link{scalpelStep3}}
#' or can be used with any of the plotting functions: \code{\link{plotFrame}}, \code{\link{plotThresholdedFrame}}, \code{\link{plotVideoVariance}}, \code{\link{plotCandidateFrame}},
#' \code{\link{plotCluster}}, \code{\link{plotResults}}, \code{\link{plotResultsAllLambda}}, \code{\link{plotSpatial}},
#' \code{\link{plotTemporal}}, and \code{\link{plotBrightest}}.
#'
#' @seealso \code{\link{scalpel}}
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "scalpel" function
#' #folder where results were saved
#' outputFolder = "scalpelResults"
#'
#' #read previous results in
#' #simplest example with default parameters:
#' out = getScalpel(outputFolder = outputFolder)
#' #note: if Step 1 has been run more than once, will need to specify 'version'
#'
#' #example with optional parameters:
#' #need to enter if non-default options were used
#' out = getScalpel(outputFolder = outputFolder, omega = 0.2, cutoff = 0.18,
#'                 alpha = 0.9, minClusterSize = 1)
#' }
#' @export
getScalpel = function(outputFolder, version=NULL, cutoff=0.18, omega=0.2, lambdaMethod="trainval", lambda=NULL, minClusterSize=1, alpha=0.9, removeBorder=FALSE, excludeReps=NULL) {

  if (is.null(version)) version = getStep1Version(outputFolder = outputFolder)
  #bug fixed 11/8/17: line below had 'omega = 0.2', instead of 'omega = omega'
  output = getScalpelStep3(outputFolder = outputFolder, version = version, omega = omega, cutoff = cutoff, lambdaMethod = lambdaMethod, lambda = lambda,
                                minClusterSize = minClusterSize, alpha = alpha, removeBorder = removeBorder, excludeReps=excludeReps)
  class(output) = "scalpel"

  return(output)
}

#' Summarize results from SCALPEL pipeline.
#'
#' Prints the parameters used and a summary of results for a specified step of SCALPEL.
#'
#' @param object An object returned by one of the SCALPEL functions:
#' \code{\link{scalpel}}, \code{\link{scalpelStep0}}, \code{\link{scalpelStep1}}, \code{\link{scalpelStep2}}, or \code{\link{scalpelStep3}}.
#' @param step The SCALPEL step (0, 1, 2, or 3) that you wish to summarize. This is only needed if summarizing an object of class \code{scalpel}.
#' @param ... Additional arguments to be passed, which are ignored in this function.
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the examples for the "scalpelStep0" and "scalpel" functions
#' summary(Step0Out)
#'  #summarize each step
#'  summary(scalpelOutput, step = 0)
#'  summary(scalpelOutput, step = 1)
#'  summary(scalpelOutput, step = 2)
#'  summary(scalpelOutput, step = 3)
#' }
#' @name summary
NULL

#' @rdname summary
#' @method summary scalpelStep0
#' @export
summary.scalpelStep0 = function(object, ...) {
  file = paste0(object$outputFolder, "Step0Summary.txt")
  writeLines(readLines(file))
}

#' @rdname summary
#' @method summary scalpelStep1
#' @export
summary.scalpelStep1 = function(object, ...) {
  file = paste0(object$outputFolder, "Step1_", object$version, "/Step1Summary.txt")
  writeLines(readLines(file))
}

#' @rdname summary
#' @method summary scalpelStep2
#' @export
summary.scalpelStep2 = function(object, ...) {
  step2folder = paste0(object$outputFolder,"Step1_",object$version,"/Step2_omega_",object$omega,"_cutoff_",object$cutoff,"/")
  file = paste0(step2folder, "Step2Summary.txt")
  writeLines(readLines(file))
}

#' @rdname summary
#' @method summary scalpelStep3
#' @export
summary.scalpelStep3 = function(object, ...) {
  if (!is.null(object$excludeReps)) fileAppend = paste0("_excluded_",paste0(sort(object$excludeReps), collapse = "_")) else fileAppend = ""
  step3folder = paste0(object$outputFolder,"Step1_",object$version,"/Step2_omega_",object$omega,"_cutoff_",object$cutoff,
                       "/Step3_lambdaMethod_",object$lambdaMethod,"_lambda_",round(object$lambda,4),"_minClusterSize_",object$minClusterSize,
                       "_alpha_",object$alpha,"_removeBorder_",(object$removeBorder),fileAppend,"/")
  file = paste0(step3folder, "Step3Summary.txt")
  writeLines(readLines(file))
}

#' @rdname summary
#' @method summary scalpel
#' @export
summary.scalpel = function(object, step, ...) {
  if (!(step %in% 0:3)) stop("Specify 0, 1, 2, or 3 for 'step'")
  if (step==0) {
    summary.scalpelStep0(object)
  } else if (step==1) {
    summary.scalpelStep1(object)
  } else if (step==2) {
    summary.scalpelStep2(object)
  } else if (step==3) {
    summary.scalpelStep3(object)
  }
}

#functions that are not exported

getStep1Version = function(outputFolder) {
  #make a list of folders in outputFolder
  allFolders = list.dirs(outputFolder, full.names = FALSE, recursive = FALSE)
  #check which contains "Step1_"
  folder = allFolders[grep("Step1_", allFolders)]
  if (length(folder)>1) {
    allVersions = gsub("^.*?_", "", folder)
    message("There are multiple folders with Step 1 results")
    stop(paste0(c("You must choose a version manually:",allVersions), collapse = " "))
  } else version = as.numeric(gsub("^.*?_","",folder))
  return(version)
}

getStep3Lambda = function(mainFolder, lambdaMethod, minClusterSize, alpha, removeBorder, excludeReps) {
  #make a list of folders in mainFolder
  allFolders = list.dirs(mainFolder, full.names = FALSE, recursive = FALSE)
  #check which contains "Step3_" and other parameters
  allFolders = allFolders[grep(paste0("Step3_lambdaMethod_",lambdaMethod), allFolders)]
  if (!is.null(excludeReps)) fileAppend = paste0("_excluded_",paste0(sort(excludeReps), collapse = "_")) else fileAppend = ""
  allFolders = allFolders[grep(paste0("_minClusterSize_",minClusterSize,"_alpha_",alpha,"_removeBorder_",(removeBorder),fileAppend), allFolders)]
  if (length(allFolders)>1) {
    allLambdas = gsub("_.*", "", gsub("^.*?lambda_","",allFolders))
    message("There are multiple possible lambda values with the given parameters")
    stop(paste0(c("You must choose lambda manually:",allLambdas), collapse = " "))
  } else lambda = as.numeric(gsub("_.*", "", gsub("^.*?lambda_","",allFolders)))
  return(lambda)
}

#function to prompt user to enter 'Y' or 'N' in response to 'prompt'
UIinputYesNo = function(prompt){

  x = NULL
  while(is.null(x)) {
    #Ask for user input
    x = readline(prompt = prompt)

    #Is it "Y" or "N"?
    if (!(x%in%c("Y","N"))) x = NULL

    #If not, tell user to try again
    if (is.null(x)) message("Please enter 'Y' or 'N'.")
  }
  x = ifelse(x=="Y", TRUE, FALSE)
  return(x)
}

#function to prompt user to enter one of the options ('options') in response to 'prompt'
UIinputOptions = function(prompt, options, optionsEnter){

  x = NULL
  while(is.null(x)) {
    #Ask for user input
    x = readline(prompt = prompt)

    #Is it in options?
    if (!(x %in% optionsEnter)) x = NULL

    #If not, tell user to try again
    if (is.null(x)) message("Please enter ", paste(optionsEnter[1:(length(optionsEnter)-1)], collapse = ", "), ", or ", optionsEnter[length(optionsEnter)])
  }
  x = options[match(x, optionsEnter)]
  return(x)
}

UIinputNumeric = function(prompt, min=NULL, max=NULL){

  x = NULL
  while(is.null(x)) {
    #Ask for user input
    x = as.numeric(readline(prompt = prompt))

    #Is it numeric? And between min and max (if applicable)?
    if (!is.numeric(x)) {
      x = NULL
      message("Please enter a number. \n")
    } else if (!is.null(min) & !is.null(max)) {
      if (x<min | x>max) {
        x = NULL
        message("Please enter a number between ", signif(min,3), " and ", signif(max,3), ".")
      }
    } else if (!is.null(min)) {
      if (x<min) {
        x = NULL
        message("Please enter a number greater than ", signif(min,3), ".")
      }
    } else if (!is.null(max)) {
      if (x>max) {
        x = NULL
        message("Please enter a number less than ", signif(max,3), ".")
      }
    }
  }
  return(x)
}

