#functions for reviewing A or Afilter

#' Manually classify the identified neurons from SCALPEL.
#'
#' We interactively review the set of identified neurons that result from either Step 2 or 3 of SCALPEL in order to manually classify them according
#' to whether they appear to be real neurons or not. To do this, the frame from which the dictionary element was derived is plotted.
#' The user can manually classify the neuron as real or not, or indicate that additional frames are needed to make the classification, in which
#' case the \code{\link{reviewNeuronsMoreFrames}} function can subsequently be used. A similar manual classification can be done non-interactively using \code{\link{reviewNeurons}}.
#'
#' @param scalpelOutput An object returned by one of the SCALPEL functions:
#' \code{\link{scalpel}}, \code{\link{scalpelStep2}}, or \code{\link{scalpelStep3}}.
#' @param neuronSet The set of neurons that should be reviewed:
#' use \code{"A"} for those resulting from \code{\link{scalpelStep2}} and saved as \code{scalpelOutput$A}, or use \code{"Afilter"} for those resulting from
#' \code{\link{scalpelStep3}} and saved as \code{scalpelOutput$Afilter}. This argument is ignored if the class of \code{scalpelOutput} is \code{scalpelStep2}.
#'
#' @return None
#'
#' @seealso For other functions useful in the classification process, see \code{\link{reviewNeuronsMoreFrames}},
#' \code{\link{reviewOverlappingNeurons}}, and \code{\link{updateNeuronsInteractive}}. Once classification is finished,
#' the argument \code{neuronsToOutline="kept"} can be used with \code{\link{plotBrightest}} and \code{\link{plotVideoVariance}},
#' and the argument \code{neuronsToDisplay="kept"} can be used with \code{\link{plotResults}}, \code{\link{plotResultsAllLambda}},
#'  \code{\link{plotTemporal}}, and \code{\link{plotSpatial}}. Finally, the argument \code{excludeReps="discarded"} allows
#'  the discarded dictionary elements to be excluded from the sparse group lasso model when running \code{\link{scalpelStep3}}.
#'
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "scalpel" function
#'
#' #we review the set of spatial components from Step 2,
#' #which are contained in scalpelOutput$A
#' reviewNeuronsInteractive(scalpelOutput = scalpelOutput, neuronSet = "A")
#' #enter "Y" for the first neuron and then "Q"
#' #entering "Q" allows us to finish manually classifying later using the same command
#' #this time there are fewer left to review
#' reviewNeuronsInteractive(scalpelOutput = scalpelOutput, neuronSet = "A")
#' #enter "N" for the first and "?" for the second this time
#' #note that once a neuron is classified as "N", it disappears from the plot
#' }
#' @export
reviewNeuronsInteractive = function(scalpelOutput, neuronSet) {

  #check function arguments
  if (class(scalpelOutput)!="scalpel" & class(scalpelOutput)!="scalpelStep2" & class(scalpelOutput)!="scalpelStep3") stop("The class of 'scalpelOutput' must be 'scalpel', 'scalpelStep2', or 'scalpelStep3'")
  if (class(scalpelOutput)=="scalpelStep2") neuronSet = "A"
  if (neuronSet!="A" & neuronSet!="Afilter") stop("Specify 'A' or 'Afilter' for 'neuronSet'")

  #set up place to save file and which A matrix to use
  if (neuronSet=="Afilter") {
    Amat = scalpelOutput$Afilter
    if (!is.null(scalpelOutput$excludeReps)) fileAppend = paste0("_excluded_",paste0(sort(scalpelOutput$excludeReps), collapse = "_")) else fileAppend = ""
    plotFolder = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,
                        "/Step3_lambdaMethod_",scalpelOutput$lambdaMethod,"_lambda_",round(scalpelOutput$lambda,4),"_minClusterSize_",scalpelOutput$minClusterSize,
                        "_alpha_",scalpelOutput$alpha,"_removeBorder_",(scalpelOutput$removeBorder),fileAppend,"/Step3Data/")
    keepFilename = paste0(plotFolder,"keep.rds")
    framesFolder = paste0(plotFolder, "FramesToReview/")
    plotFolder = paste0(plotFolder, "NeuronStatus/")
    neuronAzeroIndices = scalpelOutput$repComps[scalpelOutput$clustersUse]
    ROIname = 1:ncol(scalpelOutput$Afilter)
    numMembers = table(scalpelOutput$clusterID)[scalpelOutput$clustersUse]
  } else if (neuronSet=="A") {
    plotFolder = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,"/Step2Data/")
    keepFilename = paste0(plotFolder,"keep.rds")
    framesFolder = paste0(plotFolder, "FramesToReview/")
    plotFolder = paste0(plotFolder, "NeuronStatus/")
    Amat = scalpelOutput$A
    neuronAzeroIndices = scalpelOutput$repComps
    ROIname = 1:ncol(scalpelOutput$A)
    numMembers = table(scalpelOutput$clusterID)
  }
  keep = rep(NA, ncol(Amat))

  #check if keep file already exists
  if (file.exists(keepFilename)) {
    overwrite = UIinputYesNo(paste0("Neurons have been reviewed previously. Should we overwrite the results
                                    and start over? Enter 'Y' to start over, or 'N' to continue using the
                                    previously saved results. \n"))
    if (overwrite==TRUE) {
      file.remove(keepFilename)
      if (dir.exists(plotFolder)) unlink(plotFolder, recursive = TRUE)
      if (dir.exists(framesFolder)) unlink(framesFolder, recursive = TRUE)
    } else {
      keep = readRDS(keepFilename)
      if (dir.exists(plotFolder)) unlink(plotFolder, recursive = TRUE)
      if (dir.exists(framesFolder)) unlink(framesFolder, recursive = TRUE)
      if (sum(is.na(keep))==0) {
        message("All of the neurons were previously reviewed!")
        stopQuietly()
      }
    }
  }
  #prompt if neurons corresponding to clusters of a certain size should be automatically set to "yes"
  keepLarge = UIinputYesNo(paste0("Do you want to automatically classify neurons corresponding to large clusters to be kept? Enter 'Y' or 'N': \n"))
  if (keepLarge==TRUE) {
    minSizeToKeep = UIinputNumeric("Enter the minimum size of the neuron's cluster for which the neuron should be automatically kept: ")
    keep[which(numMembers>=minSizeToKeep)] = "yes"
  }
  #prompt if zeroed out neurons should be automatically set to "no"
  if (neuronSet=="Afilter") {
    #check if there are zeroed out neurons
    zeroedOut = which(apply(scalpelOutput$Zhat, 1, max)==0)
    if (length(zeroedOut)>0) {
      #ask if they should be discarded
      discardZeroed = UIinputYesNo(paste0("Should the ", length(zeroedOut), " neurons zeroed out in the sparse group lasso problem be discarded? Enter 'Y' to discard all, or 'N' to review each individually: \n"))
      #are some of these already classified?
      alreadyClassified = which(!is.na(keep[zeroedOut]))
      if (length(alreadyClassified)>0 & discardZeroed==TRUE) {
        reclassify = UIinputYesNo(paste0("There are ", length(alreadyClassified), " zeroed-out neurons that have already been classified. Enter 'Y' to discard all of these, or 'N' to keep their current classification: \n"))
        if (reclassify==TRUE) {
          keep[zeroedOut] = "no"
        } else {
          keep[zeroedOut[-alreadyClassified]] = "no"
        }
      } else {
        if (discardZeroed==TRUE) keep[zeroedOut] = "no"
      }
    }
  }
  #missing status indicator for keep
  keepNeedToFill = which(is.na(keep))
  #figure out the candidate frames for each neuron
  framesToPlot = scalpelOutput$AzeroFrames[neuronAzeroIndices[keepNeedToFill]]
  #figure out which parts each of the framesToPlot are in
  partByFrames = rep(NA, sum(scalpelOutput$nFramesDeltaf))
  cumFrames = cumsum(scalpelOutput$nFramesDeltaf)
  for (i in 1:length(cumFrames)) if (i>1) partByFrames[(cumFrames[i-1]+1):cumFrames[i]] = i else partByFrames[1:cumFrames[1]] = 1
  partsToLoad = sort(unique(partByFrames[framesToPlot]))

  if (length(partsToLoad)>0) message("We will review each ROI. When prompted, enter 'Y' to keep the ROI, 'N' to discard, '?' to consider later using more plots, or 'Q' to stop reviewing ROIs.")
  counter = 1

  #cycle through each part of the video and plot the candidate frames for neurons in that part
  for (part in partsToLoad) {
    #ROIs in this part
    toPlot = which(partByFrames[framesToPlot]==part)
    #load in data
    procY = getY(scalpelOutput = scalpelOutput, part = part)
    for (neuronIndex in toPlot) {
      ROInum = ROIname[keepNeedToFill][neuronIndex]
      frame = framesToPlot[neuronIndex]

      #plot graph showing candidate frame
      graphics::par(mfrow=c(1,2))
      plotFrame(scalpelOutput = scalpelOutput, frame = frame, Y = procY, shrinkLargest = TRUE)
      plotFrame(scalpelOutput = scalpelOutput, frame = frame, Y = procY, shrinkLargest = TRUE,
                title = paste0("ROI ",counter, " of ", length(framesToPlot), " to Review\n", numMembers[ROInum], ifelse(numMembers[ROInum]==1, " Member", " Members"), " in Cluster"))
      #plot other SCALPEL ROIs
      plotSpatial(A = Amat, neuronsToDisplay = which(keep!="no" | is.na(keep)), videoHeight = scalpelOutput$videoHeight, colVec = "dodgerblue", number = FALSE, border = TRUE, addToPlot = TRUE)
      #plot ROI of interest
      plotSpatial(A = Amat, neuronsToDisplay = ROInum, videoHeight = scalpelOutput$videoHeight, colVec = "orange", number = FALSE, border = TRUE, addToPlot = TRUE)

      #prompt for user input
      userInput = UIinputOptions("Enter 'Y', 'N', '?', or 'Q': \n", c("yes", "no", "unsure", "quit"), c("Y", "N", "?", "Q"))
      if (userInput!="quit") {
        keep[keepNeedToFill][neuronIndex] = userInput
      } else {
        message("There were ",sum(keep=="yes", na.rm = T), " ROIs kept, ",sum(keep=="no", na.rm = T) , " discarded, ",
                sum(keep=="unsure", na.rm = T)," to review further using 'reviewNeuronsMoreFrames()', and ",sum(is.na(keep))," not yet reviewed.")
        message("Continue reviewing ROIs at a later time by calling this function again.")
        graphics::par(mfrow=c(1,1))
        stopQuietly()
      }
      #save current version of keep
      saveRDS(keep, keepFilename)
      counter = counter + 1
    }
  }
  message("There were ",sum(keep=="yes"), " ROIs kept, ",sum(keep=="no") , " discarded, and ", sum(keep=="unsure")," to review further using 'reviewNeuronsMoreFrames()'.")
  graphics::par(mfrow=c(1,1))
}

stopQuietly <- function(...) {
  blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "));
  stop(simpleError(blankMsg));
}

getFrames = function(scalpelOutput, repNum, numFrames) {
  set.seed(100)
  frames = scalpelOutput$AzeroFrames[which(scalpelOutput$clusterID==repNum)]
  frames = unique(frames)
  if (length(frames)>numFrames) frames = frames[sample(1:length(frames), numFrames)]
  return(frames)
}

#' Save additional frames for manually classifying the identified neurons from SCALPEL.
#'
#' We use this function after running \code{\link{reviewNeurons}} or \code{\link{reviewNeuronsInteractive}} to plot additional frames for neurons whose classification was
#' unclear from the single frame plotted. The additional frames are saved, and the classification
#' for the neurons can then be updated using \code{\link{updateNeurons}} or \code{\link{updateNeuronsInteractive}}.
#'
#' @param scalpelOutput An object returned by one of the SCALPEL functions:
#' \code{\link{scalpel}}, \code{\link{scalpelStep2}}, or \code{\link{scalpelStep3}}.
#' @param neuronSet The set of neurons that should be reviewed:
#' use \code{"A"} for those resulting from \code{\link{scalpelStep2}} and saved as \code{scalpelOutput$A}, or use \code{"Afilter"} for those resulting from
#' \code{\link{scalpelStep3}} and saved as \code{scalpelOutput$Afilter}. This argument is ignored if the class of \code{scalpelOutput} is \code{scalpelStep2}.
#' @param numFrames The maximum number of frames that should be saved for each neuron being considered. Each neuron has a number of frames
#' equal to the number of members in that neuron's cluster that can be plotted. All frames will be saved when the total number of available frames for the neuron
#' is less than \code{numFrames}. The default value is 10.
#'
#' @return None
#'
#' @seealso \code{\link{reviewNeurons}}, \code{\link{updateNeurons}}, \code{\link{reviewNeuronsInteractive}}, \code{\link{updateNeuronsInteractive}}
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "reviewNeuronsInteractive" function
#'
#' #we save frames for the neurons previously classified
#' #as "?" using the "reviewNeuronsInteractive" function
#' reviewNeuronsMoreFrames(scalpelOutput = scalpelOutput, neuronSet = "A")
#' }
#' @export
reviewNeuronsMoreFrames = function(scalpelOutput, neuronSet, numFrames = 10) {

  #check function arguments
  if (class(scalpelOutput)!="scalpel" & class(scalpelOutput)!="scalpelStep2" & class(scalpelOutput)!="scalpelStep3") stop("The class of 'scalpelOutput' must be 'scalpel', 'scalpelStep2', or 'scalpelStep3'")
  if (class(scalpelOutput)=="scalpelStep2") neuronSet = "A"
  if (neuronSet!="A" & neuronSet!="Afilter") stop("Specify 'A' or 'Afilter' for 'neuronSet'")

  #check that reviewNeurons has been run previously
  if (neuronSet=="Afilter") {
    if (!is.null(scalpelOutput$excludeReps)) fileAppend = paste0("_excluded_",paste0(sort(scalpelOutput$excludeReps), collapse = "_")) else fileAppend = ""
    framesFolder = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,
                          "/Step3_lambdaMethod_",scalpelOutput$lambdaMethod,"_lambda_",round(scalpelOutput$lambda,4),"_minClusterSize_",scalpelOutput$minClusterSize,
                          "_alpha_",scalpelOutput$alpha,"_removeBorder_",(scalpelOutput$removeBorder),fileAppend,"/Step3Data/FramesToReview/")
    keepFilename = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,
                          "/Step3_lambdaMethod_",scalpelOutput$lambdaMethod,"_lambda_",round(scalpelOutput$lambda,4),"_minClusterSize_",scalpelOutput$minClusterSize,
                          "_alpha_",scalpelOutput$alpha,"_removeBorder_",(scalpelOutput$removeBorder),fileAppend,"/Step3Data/keep.rds")
  } else if (neuronSet=="A") {
    framesFolder = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,"/Step2Data/FramesToReview/")
    keepFilename = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,"/Step2Data/keep.rds")
  }
  if (!file.exists(keepFilename)) stop("Please initially review neurons using 'reviewNeurons()' or 'reviewNeuronsInteractive()' before using this function")
  #read in keep vector
  keep = readRDS(keepFilename)
  #check that some of the neurons need additional frames saved
  if (sum(keep=="unsure", na.rm = T)==0) stop("There are no ROIs requiring additional frames to be reviewed")
  #create the folder in which to save the frames
  if (!dir.exists(framesFolder)) dir.create(framesFolder)

  #set up which A matrix to use, other info
  if (neuronSet=="Afilter") {
    Amat = scalpelOutput$Afilter
    numMembers = table(scalpelOutput$clusterID)[scalpelOutput$clustersUse]
    set.seed(100)
    candidateList = sapply(scalpelOutput$clustersUse, getFrames, scalpelOutput=scalpelOutput, numFrames=numFrames, simplify=FALSE)
  } else if (neuronSet=="A") {
    Amat = scalpelOutput$A
    numMembers = table(scalpelOutput$clusterID)
    set.seed(100)
    candidateList = sapply(1:ncol(scalpelOutput$A), getFrames, scalpelOutput=scalpelOutput, numFrames=numFrames, simplify=FALSE)
  }

  #unsure status indicator for keep vector
  keepNeedToFill = which(keep=="unsure")
  message("We are saving additional plots for ", length(keepNeedToFill), ifelse(length(keepNeedToFill)==1, " ROI", " ROIs"), ".")

  #figure out which parts each of the frames are in
  partByFrames = rep(NA, sum(scalpelOutput$nFramesDeltaf))
  cumFrames = cumsum(scalpelOutput$nFramesDeltaf)
  for (i in 1:length(cumFrames)) if (i>1) partByFrames[(cumFrames[i-1]+1):cumFrames[i]] = i else partByFrames[1:cumFrames[1]] = 1
  partsToLoad = sort(unique(partByFrames[unlist(candidateList[keepNeedToFill])]))

  #cycle through each part of the video and plot the candidate frames for neurons in that part
  for (part in partsToLoad) {
    #load in data
    procY = getY(scalpelOutput = scalpelOutput, part = part)
    for (repNum in keepNeedToFill) {
      frameVec = candidateList[[repNum]]
      for (frame in frameVec) {
        #check if frame is in loaded part
        if (frame %in% procY$currentFrames) {
          if (!dir.exists(paste0(framesFolder, "ROI_", repNum, "/"))) dir.create(paste0(framesFolder, "ROI_", repNum, "/"))
          grDevices::jpeg(paste0(framesFolder, "ROI_", repNum, "/", "ROI_", repNum, "_Frame_",frame,".jpg"), width=900, quality=90)
          #plot graph showing candidate frame
          graphics::par(mfrow=c(1,2))
          plotFrame(scalpelOutput = scalpelOutput, frame = frame, Y = procY, shrinkLargest = TRUE)
          plotFrame(scalpelOutput = scalpelOutput, frame = frame, Y = procY, shrinkLargest = TRUE, title = paste0(numMembers[repNum], " Members in Cluster"))
          #plot other SCALPEL ROIs
          plotSpatial(A = Amat, neuronsToDisplay = which(keep!="no" | is.na(keep)), videoHeight = scalpelOutput$videoHeight, colVec = "dodgerblue", number = FALSE, border = TRUE, addToPlot = TRUE)
          #plot ROI of interest
          plotSpatial(A = Amat, neuronsToDisplay = repNum, videoHeight = scalpelOutput$videoHeight, colVec = "orange", number = FALSE, border = TRUE, addToPlot = TRUE)
          grDevices::dev.off()
        }
      }
    }
  }
  graphics::par(mfrow=c(1,1))
  message("There were ", sum(keep=="unsure", na.rm = T), " ROIs for which frames were saved in ",framesFolder,". To enter the status of these ROIs after reviewing the frames, use 'updateNeurons()' or 'updateNeuronsInteractive()'.")
}

#' Update the classifications of specified neurons from SCALPEL.
#'
#' This function allows the user to update the classifications of neurons, which
#' were reviewed previously using \code{\link{reviewNeuronsInteractive}}. Typically, this function
#' is used after running \code{\link{reviewNeuronsMoreFrames}}.
#'
#' @param scalpelOutput An object returned by one of the SCALPEL functions:
#' \code{\link{scalpel}}, \code{\link{scalpelStep2}}, or \code{\link{scalpelStep3}}.
#' @param neuronSet The set of neurons that should be reviewed:
#' use \code{"A"} for those resulting from \code{\link{scalpelStep2}} and saved as \code{scalpelOutput$A}, or use \code{"Afilter"} for those resulting from
#' \code{\link{scalpelStep3}} and saved as \code{scalpelOutput$Afilter}. This argument is ignored if the class of \code{scalpelOutput} is \code{scalpelStep2}.
#'
#' @return None
#'
#' @seealso \code{\link{reviewNeuronsInteractive}}, \code{\link{reviewNeuronsMoreFrames}}, \code{\link{reviewOverlappingNeurons}}
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "reviewNeuronsInteractive" function
#'
#' updateNeuronsInteractive(scalpelOutput = scalpelOutput, neuronSet = "A")
#' #you will be prompted for the changes you wish to make
#' }
#' @export
updateNeuronsInteractive = function(scalpelOutput, neuronSet) {

  #check function arguments
  if (class(scalpelOutput)!="scalpel" & class(scalpelOutput)!="scalpelStep2" & class(scalpelOutput)!="scalpelStep3") stop("The class of 'scalpelOutput' must be 'scalpel', 'scalpelStep2', or 'scalpelStep3'")
  if (class(scalpelOutput)=="scalpelStep2") neuronSet = "A"
  if (neuronSet!="A" & neuronSet!="Afilter") stop("Specify 'A' or 'Afilter' for 'neuronSet'")

  if (neuronSet=="Afilter") {
    if (!is.null(scalpelOutput$excludeReps)) fileAppend = paste0("_excluded_",paste0(sort(scalpelOutput$excludeReps), collapse = "_")) else fileAppend = ""
    keepFilename = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,
                          "/Step3_lambdaMethod_",scalpelOutput$lambdaMethod,"_lambda_",round(scalpelOutput$lambda,4),"_minClusterSize_",scalpelOutput$minClusterSize,
                          "_alpha_",scalpelOutput$alpha,"_removeBorder_",(scalpelOutput$removeBorder),fileAppend,"/Step3Data/keep.rds")
  } else if (neuronSet=="A") {
    keepFilename = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,"/Step2Data/keep.rds")
  }

  #check if keep file already exists
  if (file.exists(keepFilename)) {
    keep = readRDS(keepFilename)
  } else {
    stop("Please initially review neurons using 'reviewNeuronsInteractive()' before using this function")
  }

  message("There are ",sum(keep=="yes", na.rm = T), " kept ROIs, ",sum(keep=="no", na.rm = T) , " discarded ROIs, ", sum(keep=="unsure", na.rm = T)," ROIs needing further review, and ",sum(is.na(keep))," not yet reviewed.")
  seeAll = UIinputYesNo("Would you like to see the statuses of each ROI? Enter 'Y' or 'N': \n")
  if (seeAll==TRUE) {
    message("Kept ROIs: ", paste(which(keep=="yes"), collapse = ", "))
    message("Discarded ROIs: ", paste(which(keep=="no"), collapse = ", "))
    message("ROIs needing further review: ", paste(which(keep=="unsure"), collapse = ", "))
    message("ROIs not yet reviewed: ", paste(which(is.na(keep)), collapse = ", "))
  }
  message("We will update the classifications for specified ROIs. When prompted, enter 'Y' to keep the ROI, 'N' to discard, or '?' to consider later using more plots.")

  continue = TRUE
  while (continue==TRUE) {
    #which neuron should be updated?
    neuronIndex = UIinputOptions("Which neuron do you want to update? \n", 1:length(keep), 1:length(keep))

    #current status of neuron
    message("The current classification is '",keep[neuronIndex],"'. What should the new classification be?")

    #prompt for user input
    userInput = UIinputOptions("Enter 'Y', 'N', or '?': \n", c("yes", "no", "unsure"), c("Y", "N", "?"))
    keep[neuronIndex] = userInput

    #save current version of keep
    saveRDS(keep, keepFilename)

    #more neurons to update?
    continue = UIinputYesNo("Additional neurons to update? Enter 'Y' or 'N': \n")
  }
}

#' Save additional frames for overlapping neurons from SCALPEL.
#'
#' We use this function after running \code{\link{reviewNeurons}} or \code{\link{reviewNeuronsInteractive}} to plot additional frames for neurons that overlap with others.
#' These frames are saved, and the classification for the neurons can then be updated using \code{\link{updateNeurons}} or \code{\link{updateNeuronsInteractive}}.
#'
#' @param scalpelOutput An object returned by one of the SCALPEL functions:
#' \code{\link{scalpel}}, \code{\link{scalpelStep2}}, or \code{\link{scalpelStep3}}.
#' @param neuronSet The set of neurons that should be reviewed:
#' use \code{"A"} for those resulting from \code{\link{scalpelStep2}} and saved as \code{scalpelOutput$A}, or use \code{"Afilter"} for those resulting from
#' \code{\link{scalpelStep3}} and saved as \code{scalpelOutput$Afilter}. This argument is ignored if the class of \code{scalpelOutput} is \code{scalpelStep2}.
#' @param numFrames The maximum number of frames that should be saved for each neuron being considered. Each neuron has a number of frames
#' equal to the number of members in that neuron's cluster that can be plotted. All frames will be saved when the total number of available frames for the neuron
#' is less than \code{numFrames}. The default value is 10.
#'
#' @return None
#'
#' @seealso \code{\link{reviewNeurons}}, \code{\link{updateNeurons}}, \code{\link{reviewNeuronsInteractive}}, \code{\link{updateNeuronsInteractive}}
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "reviewNeuronsInteractive" function
#'
#' reviewOverlappingNeurons(scalpelOutput = scalpelOutput, neuronSet = "A")
#' }
#' @export
reviewOverlappingNeurons = function(scalpelOutput, neuronSet, numFrames = 10) {

  #check function arguments
  if (class(scalpelOutput)!="scalpel" & class(scalpelOutput)!="scalpelStep2" & class(scalpelOutput)!="scalpelStep3") stop("The class of 'scalpelOutput' must be 'scalpel', 'scalpelStep2', or 'scalpelStep3'")
  if (class(scalpelOutput)=="scalpelStep2") neuronSet = "A"
  if (neuronSet!="A" & neuronSet!="Afilter") stop("Specify 'A' or 'Afilter' for 'neuronSet'")

  #folder to save frames in
  if (neuronSet=="Afilter") {
    if (!is.null(scalpelOutput$excludeReps)) fileAppend = paste0("_excluded_",paste0(sort(scalpelOutput$excludeReps), collapse = "_")) else fileAppend = ""
    framesFolder = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,
                          "/Step3_lambdaMethod_",scalpelOutput$lambdaMethod,"_lambda_",round(scalpelOutput$lambda,4),"_minClusterSize_",scalpelOutput$minClusterSize,
                          "_alpha_",scalpelOutput$alpha,"_removeBorder_",(scalpelOutput$removeBorder),fileAppend,"/Step3Data/FramesToReview/")
    keepFilename = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,
                          "/Step3_lambdaMethod_",scalpelOutput$lambdaMethod,"_lambda_",round(scalpelOutput$lambda,4),"_minClusterSize_",scalpelOutput$minClusterSize,
                          "_alpha_",scalpelOutput$alpha,"_removeBorder_",(scalpelOutput$removeBorder),fileAppend,"/Step3Data/keep.rds")
    Amat = scalpelOutput$Afilter
    numMembers = table(scalpelOutput$clusterID)[scalpelOutput$clustersUse]
    set.seed(100)
    candidateList = sapply(scalpelOutput$clustersUse, getFrames, scalpelOutput=scalpelOutput, numFrames=numFrames, simplify=FALSE)
  } else if (neuronSet=="A") {
    framesFolder = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,"/Step2Data/FramesToReview/")
    keepFilename = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,"/Step2Data/keep.rds")
    Amat = scalpelOutput$A
    numMembers = table(scalpelOutput$clusterID)
    set.seed(100)
    candidateList = sapply(1:ncol(scalpelOutput$A), getFrames, scalpelOutput=scalpelOutput, numFrames=numFrames, simplify=FALSE)
  }
  #check if keep file already exists
  if (file.exists(keepFilename)) {
    keep = readRDS(keepFilename)
  } else {
    stop("Please initially review neurons using 'reviewNeurons()' or 'reviewNeuronsInteractive()' before using this function")
  }
  if (!dir.exists(framesFolder)) dir.create(framesFolder)

  #show a plot with all of the neurons
  plotSpatial(scalpelOutput = scalpelOutput, A = Amat)
  #overlapping set of neurons
  sets = findSets(Amat)
  #figure out which parts each of the frames are in
  partByFrames = rep(NA, sum(scalpelOutput$nFramesDeltaf))
  cumFrames = cumsum(scalpelOutput$nFramesDeltaf)
  for (i in 1:length(cumFrames)) if (i>1) partByFrames[(cumFrames[i-1]+1):cumFrames[i]] = i else partByFrames[1:cumFrames[1]] = 1

  continue = TRUE
  while (continue==TRUE) {
    #which neuron's overlapping set should be plotted?
    neuronIndex = UIinputOptions("Which neuron's overlapping set of neurons do you want to plot? \n", 1:ncol(Amat), 1:ncol(Amat))

    #get the overlapping set
    neuronSet = which(sets==sets[neuronIndex])
    #check that the neuron overlaps with other neurons
    if (length(neuronSet)==1) {
      message("The specified neuron does not overlap with any other neurons, and thus no files were saved.")
    } else {
      ROIfolder = paste0(framesFolder, "Set_For_ROI_", neuronIndex, "/")
      if (!dir.exists(ROIfolder)) dir.create(ROIfolder)
      #save the files
      partsToLoad = sort(unique(partByFrames[unlist(candidateList[neuronSet])]))
      #cycle through each part of the video and plot the candidate frames for neurons in that part
      for (part in partsToLoad) {
        #load in data
        procY = getY(scalpelOutput = scalpelOutput, part = part)
        for (repNum in neuronSet) {
          frameVec = candidateList[[repNum]]
          for (frame in frameVec) {
            #check if frame is in loaded part
            if (frame %in% procY$currentFrames) {
              grDevices::jpeg(paste0(ROIfolder, "ROI_", repNum, "_Frame_",frame,".jpg"), width=900, quality=90)
              #plot graph showing candidate frame
              graphics::par(mfrow=c(1,2))
              plotFrame(scalpelOutput = scalpelOutput, frame = frame, Y = procY, shrinkLargest = TRUE)
              plotFrame(scalpelOutput = scalpelOutput, frame = frame, Y = procY, shrinkLargest = TRUE, title = paste0(numMembers[repNum], " Members in Cluster"))
              #plot other SCALPEL ROIs
              plotSpatial(A = Amat, neuronsToDisplay = which(keep!="no" | is.na(keep)), videoHeight = scalpelOutput$videoHeight, colVec = "dodgerblue", number = FALSE, border = TRUE, addToPlot = TRUE)
              #plot ROI of interest
              plotSpatial(A = Amat, neuronsToDisplay = repNum, videoHeight = scalpelOutput$videoHeight, colVec = "orange", number = FALSE, border = TRUE, addToPlot = TRUE)
              grDevices::dev.off()
            }
          }
        }
      }
      #update the user
      message("There are ", length(neuronSet), " overlapping neurons in this set.")
      message("Frames for these neurons have been saved in ", ROIfolder)
    }

    #more neurons to plot?
    continue = UIinputYesNo("Additional neurons to plot? Enter 'Y' or 'N': \n")
  }
  graphics::par(mfrow=c(1,1))
}

#' Review and update the chosen threshold for image segmentation in Step 1 of SCALPEL.
#'
#' We plot random frames from the video processed in Step 0 of SCALPEL with shading to indicate the
#' smallest of the automatically chosen thresholds that will be used to perform image segmentation in
#' Step 1 of SCALPEL. The user is given the option to try out different thresholds and if desired, update
#' the threshold to use.
#'
#' @param step0Output An object of class \code{scalpel} or \code{scalpelStep0}, which result from running the
#' \code{\link{scalpel}} or \code{\link{scalpelStep0}} functions, respectively.
#'
#' @return An object identical to \code{step0Output}, except it may (depending on the user's decision) have \code{lowThreshold} updated.
#'
#' @seealso \code{\link{scalpelStep0}}
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "scalpel" function
#'
#' #update the smallest threshold used for image segmentation in Step 1
#' scalpelOutput = updateThreshold(step0Output = scalpelOutput)
#' }
#' @export
updateThreshold = function(step0Output) {

  #check function arguments
  if (class(step0Output)!="scalpel" & class(step0Output)!="scalpelStep0") stop("The class of 'step0Output' must be 'scalpel' or 'scalpelStep0'")

  #random frames to use
  frameVec = sample(1:sum(step0Output$nFramesDeltaf), 100)
  #create vector giving which part frame is in
  partByFrames = rep(NA, sum(step0Output$nFramesDeltaf))
  cumFrames = cumsum(step0Output$nFramesDeltaf)
  for (i in 1:length(cumFrames)) if (i>1) partByFrames[(cumFrames[i-1]+1):cumFrames[i]] = i else partByFrames[1:cumFrames[1]] = 1
  #which frame we're currently plotting
  counter = 1
  #load data
  procY = getY(scalpelOutput = step0Output, part = partByFrames[frameVec[counter]])

  continue = TRUE
  newThreshold = step0Output$lowThreshold
  while (continue==TRUE) {

    #load new part if necessary
    if (partByFrames[frameVec[counter]]!=procY$part) {
      procY = getY(scalpelOutput = step0Output, part = partByFrames[frameVec[counter]])
    }
    #plot the shaded thresholded frame for the lowest of the 3 thresholds
    graphics::par(mfrow=c(1,2))
    #frame with no shading
    plotFrame(scalpelOutput = step0Output, frame = frameVec[counter], Y = procY, shrinkLargest = TRUE)
    #frame with shading
    plotThresholdedFrame(scalpelOutput = step0Output, frame = frameVec[counter], Y = procY, shrinkLargest = TRUE, threshold = newThreshold, title = paste0("Threshold = ", signif(newThreshold,3)))

    #prompt for the new threshold value
    enterNewThreshold = UIinputYesNo("Would you like to try a different threshold? Enter 'Y' or 'N': \n")
    if (enterNewThreshold==TRUE) {
      newThreshold = UIinputNumeric("What threshold would you like to use? \n", min = step0Output$minDeltaf, max = step0Output$maxDeltaf)
    }
    #option to see another frame
    seeNewFrame = UIinputOptions("Would you like to see a different frame? Enter 'Y' to see a new frame, 'N' to see the same frame, or 'Q' to quit plotting: \n", c(TRUE, FALSE, "quit"), c("Y", "N", "Q"))
    if (seeNewFrame==TRUE) counter = counter + 1 else if (seeNewFrame=="quit") continue = FALSE
  }
  #prompt the user whether the updated threshold should be saved
  if (newThreshold!=step0Output$lowThreshold) {
    update = UIinputYesNo(paste0("Should we update the lowest threshold to ", newThreshold, "? Enter 'Y' or 'N': \n"))

    #if so, update step0Output and the saved version, saving a copy of the old version too
    if (update==TRUE) {
      if (!file.exists(paste0(step0Output$outputFolder,"Step0Data/originalLowThreshold.rds"))) saveRDS(step0Output$lowThreshold, file=paste0(step0Output$outputFolder,"Step0Data/originalLowThreshold.rds"))
      step0Output$lowThreshold = newThreshold
      saveRDS(newThreshold, file=paste0(step0Output$outputFolder,"Step0Data/lowThreshold.rds"))
    }
  }
  graphics::par(mfrow=c(1,1))
  return(step0Output)
}

#' Manually classify the identified neurons from SCALPEL.
#'
#' We save plots that will be used to review the set of identified neurons that result from either Step 2 or 3 of SCALPEL in order to manually classify them according
#' to whether they appear to be real neurons or not. To do this, the plot of the frame from which the dictionary element was derived is saved.
#' The user can then sort these saved plot into folders indicating whether the neuron is real or not, or indicate that additional frames are needed to make the classification, in which
#' case the \code{\link{reviewNeuronsMoreFrames}} function can subsequently be used. After finishing this sorting process, \code{\link{updateNeurons}} should be called.
#' A similar manual classification can be done interactively using \code{\link{reviewNeuronsInteractive}}.
#'
#' @param scalpelOutput An object returned by one of the SCALPEL functions:
#' \code{\link{scalpel}}, \code{\link{scalpelStep2}}, or \code{\link{scalpelStep3}}.
#' @param neuronSet The set of neurons that should be reviewed:
#' use \code{"A"} for those resulting from \code{\link{scalpelStep2}} and saved as \code{scalpelOutput$A}, or use \code{"Afilter"} for those resulting from
#' \code{\link{scalpelStep3}} and saved as \code{scalpelOutput$Afilter}. This argument is ignored if the class of \code{scalpelOutput} is \code{scalpelStep2}.
#' @param keepClusterSize Neurons corresponding to clusters with at least \code{keepClusterSize} members will be automatically classified as real neurons.
#' The default value is \code{NULL}, which means that none of the neurons will be automatically kept based on cluster size.
#' @param discardZeroedOut Logical value indicating whether neurons zeroed out in the sparse group lasso problem should automatically be discarded. This argument
#' is ignored when \code{neuronSet} is \code{"A"}, and has a default value of \code{FALSE}.
#'
#' @return None
#'
#' @details Plots are saved for each of the neurons under consideration in a certain folder. Also within that folder,
#' there will be folders called 'keep', 'discard', and 'unsure'. After running this function, the plots for each of the neurons
#' should be moved into the appropriate folder. After completing this sorting, call \code{\link{updateNeurons}} in order to
#' update the classification of the neurons. Any plots that are missing or that remain in the original folder will be classified as not having been sorted yet.
#'
#' @seealso After sorting the plots saved by this function, the user should call \code{\link{updateNeurons}}.
#' For other functions useful in the classification process, see \code{\link{reviewNeuronsMoreFrames}} and
#' \code{\link{reviewOverlappingNeurons}}. Once classification is finished,
#' the argument \code{neuronsToOutline="kept"} can be used with \code{\link{plotBrightest}} and \code{\link{plotVideoVariance}},
#' and the argument \code{neuronsToDisplay="kept"} can be used with \code{\link{plotResults}}, \code{\link{plotResultsAllLambda}},
#'  \code{\link{plotTemporal}}, and \code{\link{plotSpatial}}. Finally, the argument \code{excludeReps="discarded"} allows
#'  the discarded dictionary elements to be excluded from the sparse group lasso model when running \code{\link{scalpelStep3}}.
#'
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "scalpel" function
#'
#' #we review the set of spatial components from Step 3,
#' #which are contained in scalpelOutput$Afilter
#' reviewNeurons(scalpelOutput = scalpelOutput, neuronSet = "Afilter")
#' }
#' @export
reviewNeurons = function(scalpelOutput, neuronSet, keepClusterSize = NULL, discardZeroedOut = FALSE) {

  #check function arguments
  if (class(scalpelOutput)!="scalpel" & class(scalpelOutput)!="scalpelStep2" & class(scalpelOutput)!="scalpelStep3") stop("The class of 'scalpelOutput' must be 'scalpel', 'scalpelStep2', or 'scalpelStep3'")
  if (class(scalpelOutput)=="scalpelStep2") neuronSet = "A"
  if (neuronSet!="A" & neuronSet!="Afilter") stop("Specify 'A' or 'Afilter' for 'neuronSet'")

  #set up place to save file and which A matrix to use
  if (neuronSet=="Afilter") {
    Amat = scalpelOutput$Afilter
    if (!is.null(scalpelOutput$excludeReps)) fileAppend = paste0("_excluded_",paste0(sort(scalpelOutput$excludeReps), collapse = "_")) else fileAppend = ""
    plotFolder = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,
                        "/Step3_lambdaMethod_",scalpelOutput$lambdaMethod,"_lambda_",round(scalpelOutput$lambda,4),"_minClusterSize_",scalpelOutput$minClusterSize,
                        "_alpha_",scalpelOutput$alpha,"_removeBorder_",(scalpelOutput$removeBorder),fileAppend,"/Step3Data/")
    keepFilename = paste0(plotFolder,"keep.rds")
    framesFolder = paste0(plotFolder, "FramesToReview/")
    plotFolder = paste0(plotFolder, "NeuronStatus/")
    neuronAzeroIndices = scalpelOutput$repComps[scalpelOutput$clustersUse]
    ROIname = 1:ncol(scalpelOutput$Afilter)
    numMembers = table(scalpelOutput$clusterID)[scalpelOutput$clustersUse]
  } else if (neuronSet=="A") {
    plotFolder = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,"/Step2Data/")
    keepFilename = paste0(plotFolder,"keep.rds")
    framesFolder = paste0(plotFolder, "FramesToReview/")
    plotFolder = paste0(plotFolder, "NeuronStatus/")
    Amat = scalpelOutput$A
    neuronAzeroIndices = scalpelOutput$repComps
    ROIname = 1:ncol(scalpelOutput$A)
    numMembers = table(scalpelOutput$clusterID)
  }
  keep = rep(NA, ncol(Amat))

  #get folder structure set up
  if (dir.exists(plotFolder)) {
    rerun = UIinputYesNo(paste0("This function has been run previously. \nShould we overwrite the \nresults and start over? \nEnter 'Y' to start over, or 'N' to quit. \n"))
    if (rerun==FALSE) {
      stopQuietly() 
    } else {
      unlink(plotFolder, recursive = TRUE)
      if (dir.exists(framesFolder)) unlink(framesFolder, recursive = TRUE)
    }
  }
  dir.create(plotFolder)
  dir.create(paste0(plotFolder, "keep/"))
  dir.create(paste0(plotFolder, "discard/"))
  dir.create(paste0(plotFolder, "unsure/"))

  #check if keep file already exists
  if (file.exists(keepFilename)) {
    overwrite = UIinputYesNo(paste0("Neurons have been reviewed previously. Should we overwrite the results
                                    and start over? Enter 'Y' to start over, or 'N' to continue using the
                                    previously saved results. \n"))
    if (overwrite==TRUE) {
      file.remove(keepFilename)
      if (dir.exists(framesFolder)) unlink(framesFolder, recursive = TRUE)
    } else {
      keep = readRDS(keepFilename)
      if (dir.exists(framesFolder)) unlink(framesFolder, recursive = TRUE)
      if (sum(is.na(keep))==0) {
        message("All of the neurons were previously reviewed!")
        stopQuietly()
      }
    }
  }
  #automatically set to "yes" neurons corresponding to clusters of a certain size
  if (!is.null(keepClusterSize)) {
    keep[which(numMembers>=keepClusterSize)] = "yes"
  }
  #automatically set to "no" neurons zeroed out by the sparse group lasso problem
  if (neuronSet=="Afilter") {
    #check if there are zeroed out neurons
    zeroedOut = which(apply(scalpelOutput$Zhat, 1, max)==0)
    if (length(zeroedOut)>0 & discardZeroedOut==TRUE) keep[zeroedOut] = "no"
  }
  saveRDS(keep, keepFilename)

  #figure out the candidate frames for each neuron
  framesToPlot = scalpelOutput$AzeroFrames[neuronAzeroIndices]
  #figure out which parts each of the framesToPlot are in
  partByFrames = rep(NA, sum(scalpelOutput$nFramesDeltaf))
  cumFrames = cumsum(scalpelOutput$nFramesDeltaf)
  for (i in 1:length(cumFrames)) if (i>1) partByFrames[(cumFrames[i-1]+1):cumFrames[i]] = i else partByFrames[1:cumFrames[1]] = 1
  partsToLoad = sort(unique(partByFrames[framesToPlot]))

  #cycle through each part of the video and save the plots for the candidate frames for neurons in that part
  for (part in partsToLoad) {
    #ROIs in this part
    toPlot = which(partByFrames[framesToPlot]==part)
    #load in data
    procY = getY(scalpelOutput = scalpelOutput, part = part)
    for (neuronIndex in toPlot) {
      ROInum = ROIname[neuronIndex]
      frame = framesToPlot[neuronIndex]

      #plot graph showing candidate frame
      subFolder = ifelse(is.na(keep[neuronIndex]), "", ifelse(keep[neuronIndex]=="yes", "keep/", ifelse(keep[neuronIndex]=="no", "discard/", "unsure/")))
      grDevices::jpeg(paste0(plotFolder, subFolder, "/ROI_", ROInum, ".jpg"), width=900, quality=90)
      graphics::par(mfrow=c(1,2))
      plotFrame(scalpelOutput = scalpelOutput, frame = frame, Y = procY, shrinkLargest = TRUE)
      plotFrame(scalpelOutput = scalpelOutput, frame = frame, Y = procY, shrinkLargest = TRUE,
                title = paste0("ROI ", ROInum, "\n", numMembers[ROInum], ifelse(numMembers[ROInum]==1, " Member", " Members"), " in Cluster"))
      #plot other SCALPEL ROIs
      plotSpatial(A = Amat, neuronsToDisplay = which(keep!="no" | is.na(keep)), videoHeight = scalpelOutput$videoHeight, colVec = "dodgerblue", number = FALSE, border = TRUE, addToPlot = TRUE)
      #plot ROI of interest
      plotSpatial(A = Amat, neuronsToDisplay = ROInum, videoHeight = scalpelOutput$videoHeight, colVec = "orange", number = FALSE, border = TRUE, addToPlot = TRUE)
      grDevices::dev.off()
    }
  }
  message("The plots are saved in ", plotFolder, ". After sorting them into appropriate folders, call 'updateNeurons()'.")
}


#' Update the classifications of neurons from SCALPEL.
#'
#' This function allows the user to update the classifications of neurons, based on manual sorting of
#' plots saved as a result of running \code{\link{reviewNeurons}}.
#'
#' @param scalpelOutput An object returned by one of the SCALPEL functions:
#' \code{\link{scalpel}}, \code{\link{scalpelStep2}}, or \code{\link{scalpelStep3}}.
#' @param neuronSet The set of neurons that should be reviewed:
#' use \code{"A"} for those resulting from \code{\link{scalpelStep2}} and saved as \code{scalpelOutput$A}, or use \code{"Afilter"} for those resulting from
#' \code{\link{scalpelStep3}} and saved as \code{scalpelOutput$Afilter}. This argument is ignored if the class of \code{scalpelOutput} is \code{scalpelStep2}.
#'
#' @return None
#'
#' @seealso \code{\link{reviewNeurons}}, \code{\link{reviewNeuronsMoreFrames}}, \code{\link{reviewOverlappingNeurons}}
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "reviewNeurons" function
#'
#' updateNeurons(scalpelOutput = scalpelOutput, neuronSet = "Afilter")
#' }
#' @export
updateNeurons = function(scalpelOutput, neuronSet) {

  #check function arguments
  if (class(scalpelOutput)!="scalpel" & class(scalpelOutput)!="scalpelStep2" & class(scalpelOutput)!="scalpelStep3") stop("The class of 'scalpelOutput' must be 'scalpel', 'scalpelStep2', or 'scalpelStep3'")
  if (class(scalpelOutput)=="scalpelStep2") neuronSet = "A"
  if (neuronSet!="A" & neuronSet!="Afilter") stop("Specify 'A' or 'Afilter' for 'neuronSet'")

  if (neuronSet=="Afilter") {
    if (!is.null(scalpelOutput$excludeReps)) fileAppend = paste0("_excluded_",paste0(sort(scalpelOutput$excludeReps), collapse = "_")) else fileAppend = ""
    plotFolder = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,
                        "/Step3_lambdaMethod_",scalpelOutput$lambdaMethod,"_lambda_",round(scalpelOutput$lambda,4),"_minClusterSize_",scalpelOutput$minClusterSize,
                        "_alpha_",scalpelOutput$alpha,"_removeBorder_",(scalpelOutput$removeBorder),fileAppend,"/Step3Data/")
    keepFilename = paste0(plotFolder,"keep.rds")
    plotFolder = paste0(plotFolder, "NeuronStatus/")
  } else if (neuronSet=="A") {
    plotFolder = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,"/Step2Data/")
    keepFilename = paste0(plotFolder,"keep.rds")
    plotFolder = paste0(plotFolder, "NeuronStatus/")
  }

  #check if plotFolder exists
  if (!dir.exists(plotFolder) | !file.exists(keepFilename)) {
    stop("Please initially review neurons using 'reviewNeurons()' before using this function")
  }

  #update staus of neurons
  keep = readRDS(keepFilename)
  keep = rep(NA, length(keep))
  toKeep = as.numeric(gsub(".jpg", "", gsub("ROI_","",list.files(paste0(plotFolder, "keep/")))))
  toDiscard = as.numeric(gsub(".jpg", "", gsub("ROI_","",list.files(paste0(plotFolder, "discard/")))))
  toConsiderMore = as.numeric(gsub(".jpg", "", gsub("ROI_","",list.files(paste0(plotFolder, "unsure/")))))
  keep[toConsiderMore] = "unsure"
  keep[toDiscard] = "no"
  keep[toKeep] = "yes"

  #save current version of keep
  saveRDS(keep, keepFilename)
}

#' Read in the manual classifications of neurons from SCALPEL.
#'
#' This function allows the user to read in the manual classifications of neurons, based on the classifying done using
#' \code{\link{reviewNeurons}} or \code{\link{reviewNeuronsInteractive}}.
#'
#' @param scalpelOutput An object returned by one of the SCALPEL functions:
#' \code{\link{scalpel}}, \code{\link{scalpelStep2}}, or \code{\link{scalpelStep3}}.
#' @param neuronSet The set of neurons that should be reviewed:
#' use \code{"A"} for those resulting from \code{\link{scalpelStep2}} and saved as \code{scalpelOutput$A}, or use \code{"Afilter"} for those resulting from
#' \code{\link{scalpelStep3}} and saved as \code{scalpelOutput$Afilter}. This argument is ignored if the class of \code{scalpelOutput} is \code{scalpelStep2}.
#'
#' @return A vector of length equal to the number of columns in \code{scalpelOutput$A} if \code{neuronSet="A"} or \code{scalpelOutput$Afilter} if \code{neuronSet="Afilter"}.
#' The elements give the manual classifications of the neurons. The possible classifications are: \code{"yes"} if
#' a neuron is to be kept, \code{"no"} if a neuron is to be discarded, \code{"unsure"} if a neuron needs to be reviewed further, and
#' \code{NA} if a neuron has not yet been classified.
#'
#' @seealso \code{\link{reviewNeurons}}, \code{\link{reviewNeuronsInteractive}}
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "updateNeurons" function
#'
#' getNeuronStatus(scalpelOutput = scalpelOutput, neuronSet = "Afilter")
#' }
#' @export
getNeuronStatus = function(scalpelOutput, neuronSet) {
  #check function arguments
  if (class(scalpelOutput)!="scalpel" & class(scalpelOutput)!="scalpelStep2" & class(scalpelOutput)!="scalpelStep3") stop("The class of 'scalpelOutput' must be 'scalpel', 'scalpelStep2', or 'scalpelStep3'")
  if (class(scalpelOutput)=="scalpelStep2") neuronSet = "A"
  if (neuronSet!="A" & neuronSet!="Afilter") stop("Specify 'A' or 'Afilter' for 'neuronSet'")

  if (neuronSet=="Afilter") {
    if (!is.null(scalpelOutput$excludeReps)) fileAppend = paste0("_excluded_",paste0(sort(scalpelOutput$excludeReps), collapse = "_")) else fileAppend = ""
    keepFilename = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,
                          "/Step3_lambdaMethod_",scalpelOutput$lambdaMethod,"_lambda_",round(scalpelOutput$lambda,4),"_minClusterSize_",scalpelOutput$minClusterSize,
                          "_alpha_",scalpelOutput$alpha,"_removeBorder_",(scalpelOutput$removeBorder),fileAppend,"/Step3Data/","keep.rds")
  } else if (neuronSet=="A") {
    keepFilename = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,"/Step2Data/","keep.rds")
  }

  #check if keep vector exists
  if (!file.exists(keepFilename)) {
    stop("Please initially review neurons using 'reviewNeurons()' or 'reviewNeuronsInteractive()' before using this function")
  }

  #get keep vector
  keep = readRDS(keepFilename)
  return(keep)
}
