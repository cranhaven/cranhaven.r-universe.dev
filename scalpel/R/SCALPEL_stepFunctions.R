#main functions for performing each step of the pipeline
#along with function to call to run entire pipeline

###############################################################################################
## SCALPEL STEP 0: DATA PRE-PROCESSING
###############################################################################################

#' Perform Step 0 of SCALPEL.
#'
#' This step involves data pre-processing. We read in the raw data version of Y and perform standard pre-processing
#' techniques in order to smooth the data both temporally and spatially, remove the bleaching effect,
#' and calculate a standardized fluorescence.
#'
#' @param outputFolder The existing directory where the results should be saved.
#' @param rawDataFolder The directory where the raw data version of Y is saved. The data should be a
#' PxT matrix, where P is the total number of pixels per image frame and T
#' the number of frames of the video, for which the (i,j)th element contains the
#' fluorescence of the ith pixel in the jth frame. To create Y, you should
#' vectorize each 2-dimensional image frame by concatenating the columns of the image frame.  If the data is
#' saved in a single file, it should be named "Y_1.mat", "Y_1.rds", "Y_1.txt", or "Y_1.txt.gz" (depending on \code{fileType}),
#' and if the data is split over multiple files, they should be split into chunks of the columns
#' and named consecutively ("Y_1.mat", "Y_2.mat", etc.; "Y_1.rds", "Y_2.rds", etc.; "Y_1.txt", "Y_2.txt", etc.; or "Y_1.txt.gz", "Y_2.txt.gz", etc.).
#' @param videoHeight The height of the video (in pixels).
#' @param fileType Indicates whether raw data is an .rds (default value; \code{fileType="R"}), .mat (\code{fileType="matlab"}), .txt (\code{fileType="text"}), or
#' .txt.gz (\code{fileType="zippedText"}) file. Any text files should not have row or column names.
#' @param processSeparately Logical scalar giving whether the multiple raw data files should be
#' processed individually, versus all at once. Processing the files separately may be preferable for larger videos.
#' The default value is \code{TRUE}; this argument is ignored if the raw data is saved in a single file.
#'
#' @return An object of class \code{scalpelStep0}, which can be summarized using \code{\link{summary}}, used to run SCALPEL Step 1 using \code{\link{scalpelStep1}},
#' or can be used with the plotting functions \code{\link{plotFrame}}, \code{\link{plotThresholdedFrame}}, and \code{\link{plotVideoVariance}}.
#' \itemize{
#' \item{\code{minRaw, maxRaw, minDeltaf, maxDeltaf}: }{Minimum and maximum values for the raw and processed videos.}
#' \item{\code{partsRaw, partsDeltaf}: }{Vectors indicating the indices of the raw and processed data files, respectively.}
#' \item{\code{nFramesRaw, nFramesDeltaf}: }{The number of frames in each part of the raw and processed data.}
#' \item{\code{lowThreshold, highThreshold}: }{The default lowest and highest threshold values for image segmentation that may be used in Step 1.}
#' \item{Other elements: }{As specified by the user.}
#' }
#'
#' @details Several files containing data from this step and a summary of the step are saved in "outputFolder".
#'
#' @seealso The entire SCALPEL pipeline can be implemented using the \code{\link{scalpel}} function. The
#' other steps in the pipeline can be run using the \code{\link{scalpelStep1}}, \code{\link{scalpelStep2}}, \code{\link{scalpelStep3}} functions.
#' Results from this step can be summarized using \code{\link{summary}}, loaded at a later time using \code{\link{getScalpelStep0}}, and plotted using
#' \code{\link{plotFrame}}, \code{\link{plotThresholdedFrame}}, and \code{\link{plotVideoVariance}}.
#'
#' @import Matrix
#' @import gam
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #existing folder to save results (update this to an existing folder on your computer)
#' outputFolder = "scalpelResultsStepByStep"
#' #location on computer of raw data in R package to use
#' rawDataFolder = gsub("Y_1.rds", "", system.file("extdata", "Y_1.rds", package = "scalpel"))
#' #video height of raw data in R package
#' videoHeight = 30
#' #run Step 0 of SCALPEL
#' Step0Out = scalpelStep0(outputFolder = outputFolder,
#'                        rawDataFolder = rawDataFolder, videoHeight = videoHeight)
#' summary(Step0Out)
#' }
#' @export
scalpelStep0 = function(outputFolder, rawDataFolder, videoHeight, fileType="R", processSeparately=TRUE) {

  message("Beginning Step 0 of SCALPEL")

  #check function arguments
  #check that folders end in "/"
  if (substr(outputFolder, nchar(outputFolder), nchar(outputFolder))!="/") outputFolder = paste0(outputFolder, "/")
  if (substr(rawDataFolder, nchar(rawDataFolder), nchar(rawDataFolder))!="/") rawDataFolder = paste0(rawDataFolder, "/")
  if (!dir.exists(outputFolder)) stop("Specify an existing folder for 'outputFolder'")
  if (dir.exists(paste0(outputFolder, "Step0Data")) | file.exists(paste0(outputFolder, "Step0Summary.txt"))) {
    overwrite = UIinputYesNo(paste0("Results already exist in ",outputFolder,".\n Should we overwrite them? Enter 'Y' or 'N': \n"))
    if (overwrite==FALSE) {
      stop("Results not overwritten. Please rerun with a different 'outputFolder'.")
    } else {
      #delete existing summary file, Step0 data directory, and Step1 directories (if they exist)
      if (dir.exists(paste0(outputFolder, "Step0Data"))) unlink(paste0(outputFolder, "Step0Data"), recursive = TRUE)
      if (file.exists(paste0(outputFolder, "Step0Summary.txt"))) unlink(paste0(outputFolder, "Step0Summary.txt"), recursive = TRUE)
      allStep1Folders = list.dirs(outputFolder, full.names = FALSE, recursive = FALSE)
      allStep1Folders = allStep1Folders[grep("Step1_", allStep1Folders)]
      for (folder in allStep1Folders) unlink(paste0(outputFolder, folder), recursive = TRUE)
    }
  }
  if (!is.logical(processSeparately)) stop("Specify TRUE or FALSE for 'processSeparately'")
  if (fileType!="matlab" & fileType!="R" & fileType!="text" & fileType!="zippedText") stop("Specify 'R', 'matlab', 'text', or 'zippedText' for 'fileType'")

  #create subdirectories in outputFolder to save results
  dir.create(paste0(outputFolder,"Step0Data/"))
  fileTypeEnd = ifelse(fileType=="R", ".rds", ifelse(fileType=="matlab", ".mat", ifelse(fileType=="text", ".txt", ".txt.gz")))
  if (!file.exists(paste0(rawDataFolder, "Y_1", fileTypeEnd))) stop(paste0("Raw data should be saved as 'Y_1",fileTypeEnd,"', 'Y_2",fileTypeEnd,"', etc. in 'rawDataFolder'"))

  if (!dir.exists(paste0(outputFolder,"timings/"))) dir.create(paste0(outputFolder,"timings/"))
  utils::write.table(1,paste0(outputFolder,"timings/Step0_0_start.txt"))

  #figure out how many parts the raw data are split into
  numParts = 0
  while(file.exists(paste0(rawDataFolder, "Y_", numParts+1, fileTypeEnd))) {
    numParts = numParts + 1
  }
  message("Found ", numParts, " raw data files")

  partsRaw = 1:numParts
  if(processSeparately==FALSE) partsDeltaf = 1 else partsDeltaf = 1:numParts
  nFramesRaw = rep(NA, length(partsRaw))
  maxValBleach = rep(NA, length(partsDeltaf))
  minRaw <- maxRaw <- minDeltaf <- maxDeltaf <- NA
  thresholds = matrix(NA, nrow=max(partsDeltaf), ncol=2)

  for (part in partsDeltaf) {

    message("Performing processing on part ", part, " of ", max(partsDeltaf))
    message("Reading in raw data")
    #read in original Y matrix (no pre-processing) from Matlab file
    #should be a matrix with nrow=number of pixels, ncol=number of frames
    if (processSeparately==FALSE) {
      for (partRead in 1:numParts) {
        if (fileType=="matlab") {
          Ypart = R.matlab::readMat(paste0(rawDataFolder,"Y_",partRead,".mat"))
          if (length(names(Ypart))>1) stop(paste0(c("There are multiple variables saved in this Matlab file: ",names(Ypart)), collapse = " "))
          numRows = nrow(with(Ypart, get(names(Ypart))))
          if (numRows%%videoHeight!=0) {
            stop(paste0("There are ",numRows," rows in the matrix saved in 'Y_",
                        part,".mat' and the number of rows must be divisble by 'videoHeight', which is ",videoHeight))
          }
          if (partRead>1) Y = cbind(Y, with(Ypart, get(names(Ypart)))) else Y = with(Ypart, get(names(Ypart)))
          nFramesRaw[partRead] = ncol(with(Ypart, get(names(Ypart))))
        } else if (fileType=="R") {
          Ypart = readRDS(paste0(rawDataFolder,"Y_",partRead,".rds"))
          if (nrow(Ypart)%%videoHeight!=0) {
            stop(paste0("There are ",nrow(Ypart)," rows in the matrix saved in 'Y_",
                        part,".rds' and the number of rows must be divisble by 'videoHeight', which is ",videoHeight))
          }
          if (partRead>1) Y = cbind(Y, Ypart) else Y = Ypart
          nFramesRaw[partRead] = ncol(Ypart)
        } else {
          Ypart = utils::read.table(paste0(rawDataFolder,"Y_",partRead,fileTypeEnd))
          if (nrow(Ypart)%%videoHeight!=0) {
            stop(paste0("There are ",nrow(Ypart)," rows in the matrix saved in 'Y_",
                        part,fileTypeEnd,"' and the number of rows must be divisble by 'videoHeight', which is ",videoHeight))
          }
          if (partRead>1) Y = cbind(Y, Ypart) else Y = Ypart
          nFramesRaw[partRead] = ncol(Ypart)
        }
        rm(Ypart)
      }
      minRaw = min(Y); maxRaw = max(Y)
    } else {
      if (fileType=="matlab") {
        Y = R.matlab::readMat(paste0(rawDataFolder,"Y_",part,".mat"))
        if (length(names(Y))>1) stop(paste0(c("There are multiple variables saved in this Matlab file: ",names(Y)), collapse = " "))
        Y = with(Y, get(names(Y)))
        if (nrow(Y)%%videoHeight!=0) stop(paste0("The number of rows of the matrix saved in 'Y_",
                                                 part,".mat' should be divisble by 'videoHeight'"))
      } else if (fileType=="R") {
        Y = readRDS(paste0(rawDataFolder,"Y_",part,".rds"))
        if (nrow(Y)%%videoHeight!=0) stop(paste0("The number of rows of the matrix saved in 'Y_",
                                                 part,".rds' should be divisble by 'videoHeight'"))
      } else {
        Y = utils::read.table(paste0(rawDataFolder,"Y_",part,fileTypeEnd))
        if (nrow(Y)%%videoHeight!=0) stop(paste0("The number of rows of the matrix saved in 'Y_",
                                                 part,fileTypeEnd,"' should be divisble by 'videoHeight'"))
      }
      nFramesRaw[part] = ncol(Y)
      minRaw = min(c(minRaw, min(Y)), na.rm = T)
      maxRaw = max(c(maxRaw, max(Y)), na.rm = T)
    }

    #shift so that minimum value is 0
    message("Y has ",nrow(Y)," total pixels and ",ncol(Y)," frames")
    Y = Y - min(Y)
    utils::write.table(1,paste0(outputFolder,"timings/Step0_part",part,"_1_readdata.txt"))

    message("Performing temporal and spatial smoothing")
    #perform temporal and spatial smoothing
    prepped = imageFastSmoothHelper(m=videoHeight, n=nrow(Y)/videoHeight)
    for (frame in 1:ncol(Y)) {
      if (frame%%100==0) message("Frame ", frame, " of ", ncol(Y))
      Y[,frame] = as.vector(imageFastSmooth(matrix(Y[,frame], nrow=videoHeight), prepped))
    }
    prepped = imageFastSmoothHelper(1, ncol(Y))
    for (pixel in 1:nrow(Y)) {
      if (pixel%%1000==0) message("Pixel ", pixel, " of ", nrow(Y))
      Y[pixel,] = as.vector(imageFastSmooth(matrix(Y[pixel,],nrow=1), prepped))
    }
    rm(prepped)
    utils::write.table(1,paste0(outputFolder,"timings/Step0_part",part,"_2_smoothed.txt"))

    message("Adjusting for possible bleaching effect")
    #adjust for bleaching effect
    bleachVec = apply(Y, 2, stats::median)
    frames = 1:length(bleachVec)
    bleachModel = gam(bleachVec~s(frames,10))
    #nobleachY = t(t(Y) - bleachModel$fitted.values + max(bleachModel$fitted.values))
    nobleachY = t(t(Y) - bleachModel$fitted.values)
    maxValBleach[part] = max(bleachModel$fitted.values)
    rm(list=c('Y', 'bleachVec', 'frames', 'bleachModel'))
    utils::write.table(1,paste0(outputFolder,"timings/Step0_part",part,"_3_bleacheffect.txt"))
    #save nobleachY if there are multiple parts b/c we need to figure out the max value
    if (length(partsDeltaf)>1) {
      saveRDS(nobleachY, file=paste0(outputFolder,"Step0Data/Ynobleach_part",part,".rds"))
      rm(nobleachY)
    }
  }
  for (part in partsDeltaf) {
    if (length(partsDeltaf)>1) {
      nobleachY = readRDS(paste0(outputFolder,"Step0Data/Ynobleach_part",part,".rds"))
      unlink(paste0(outputFolder,"Step0Data/Ynobleach_part",part,".rds"))
    }
    nobleachY = nobleachY + max(maxValBleach)
    saveRDS(maxValBleach, paste0(outputFolder,"Step0Data/maxValBleach.rds"))
    message("Performing 'delta f over f' transformation")
    #perform our version of 'delta f over f' transformation
    adjFactor = stats::quantile(nobleachY, probs=0.1)
    deltafoverfY = t(apply(nobleachY, 1, function(vec, adjFactor)
      (vec - stats::median(vec))/(stats::median(vec) + adjFactor), adjFactor=adjFactor))
    rm(list=c('nobleachY','adjFactor'))
    utils::write.table(1,paste0(outputFolder,"timings/Step0_part",part,"_4_deltaf.txt"))

    #save processed data
    saveRDS(deltafoverfY, file=paste0(outputFolder,"Step0Data/Ydeltaf_part",part,".rds"))
    minDeltaf = min(c(minDeltaf, min(deltafoverfY)), na.rm = T)
    maxDeltaf = max(c(maxDeltaf, max(deltafoverfY)), na.rm = T)
    thresholds[part,] = c(-stats::quantile(c(deltafoverfY), probs=0.001), -min(deltafoverfY))
  }

  if (processSeparately==TRUE) nFramesDeltaf = nFramesRaw else nFramesDeltaf = sum(nFramesRaw)
  lowThreshold = max(thresholds[,1])
  highThreshold = max(thresholds[,2])

  #summary file of data
  if (max(partsRaw)==1) {
    #1 raw data file
    beginning = paste0("We found 1 raw data file, which had ", nFramesRaw, " frames. Thus 1 processed data file with ", nFramesDeltaf, " frames was produced.")
  } else if (processSeparately==TRUE) {
    #>1 raw data file, processed separately
    beginning = paste0("We found ", max(partsRaw), " raw data files, which were processed separately, and had ", paste0(nFramesRaw, collapse = ", "), " frames, respectively. ",
                       "Thus ", max(partsDeltaf), " processed data files with ", paste0(nFramesDeltaf, collapse = ", "), " frames, respectively, were produced.")
  } else {
    #>1 raw data file, processed together
    beginning = paste0("We found ", max(partsRaw), " raw data files, which were processed all at once, and had ", paste0(nFramesRaw, collapse = ", "), " frames, respectively. ",
                       "Thus ", max(partsDeltaf), " processed data file with ", nFramesDeltaf, " frames was produced.")
  }
  fileConn = file(paste0(outputFolder, "Step0Summary.txt"))
  writeLines(c("Step 0 of SCALPEL","","SUMMARY",
               paste0(beginning, " The video has a height of ", videoHeight, " and a width of ", nrow(deltafoverfY)/videoHeight,
                      ". The minimum and maximum values of the raw data were ", signif(minRaw, 3), " and ", signif(maxRaw, 3), ", respectively."),
               "", "ARGUMENTS USED", paste0("Raw data location: ", rawDataFolder), paste0("Raw data file type: ", fileType),
               paste0("Video height: ",videoHeight), paste0("Process raw data files separately? ",ifelse(processSeparately==TRUE,"Yes","No")),
               paste0("Output files location: ", outputFolder)), fileConn)
  close(fileConn)

  #save variables for later use
  saveRDS(rawDataFolder, file=paste0(outputFolder,"Step0Data/rawDataFolder.rds"))
  saveRDS(videoHeight, file=paste0(outputFolder,"Step0Data/videoHeight.rds"))
  saveRDS(partsRaw, file=paste0(outputFolder,"Step0Data/partsRaw.rds"))
  saveRDS(partsDeltaf, file=paste0(outputFolder,"Step0Data/partsDeltaf.rds"))
  saveRDS(nFramesRaw, file=paste0(outputFolder,"Step0Data/nFramesRaw.rds"))
  saveRDS(nFramesDeltaf, file=paste0(outputFolder,"Step0Data/nFramesDeltaf.rds"))
  saveRDS(minRaw, file=paste0(outputFolder,"Step0Data/minRaw.rds"))
  saveRDS(maxRaw, file=paste0(outputFolder,"Step0Data/maxRaw.rds"))
  saveRDS(minDeltaf, file=paste0(outputFolder,"Step0Data/minDeltaf.rds"))
  saveRDS(maxDeltaf, file=paste0(outputFolder,"Step0Data/maxDeltaf.rds"))
  saveRDS(fileType, file=paste0(outputFolder,"Step0Data/fileType.rds"))
  saveRDS(highThreshold, file=paste0(outputFolder,"Step0Data/highThreshold.rds"))
  saveRDS(lowThreshold, file=paste0(outputFolder,"Step0Data/lowThreshold.rds"))

  #return variables needed for Step 1
  output = getScalpelStep0(outputFolder=outputFolder)

  utils::write.table(1,paste0(outputFolder,"timings/Step0_5_end.txt"))
  message("Step 0 of SCALPEL finished!")

  return(output)
}

###############################################################################################
## SCALPEL STEP 1: CONSTRUCTION OF A SPATIAL COMPONENT DICTIONARY
## PERFORM IMAGE SEGMENTATION
###############################################################################################

#' Perform Step 1 of SCALPEL.
#'
#' This step involves constructing a spatial component dictionary. We apply a simple image segmentation
#' procedure to each frame of the video in order to derive a dictionary of preliminary dictionary elements.
#' Ideally, this dictionary is a superset of the true spatial components.
#'
#' @param step0Output An object of class \code{scalpel} or \code{scalpelStep0}, which result from running the
#' \code{\link{scalpel}} or \code{\link{scalpelStep0}} functions, respectively.
#' @param minSize,maxSize The minimum and maximum size, respectively, for a preliminary dictionary element with default values
#' of 25 and 500, respectively.
#' @param maxWidth,maxHeight The maximum width and height, respectively, for a preliminary dictionary element with default values of 30.
#' @param thresholdVec Optional advanced user argument: A vector with the desired thresholds to use for image segmentation. If not specified, the default is to
#' use the negative of the minimum of the processed Y data (i.e., \code{step0Output$highThreshold}), the negative of the 0.1\% quantile of the processed Y data
#' (i.e., \code{step0Output$lowThreshold}), and the mean of these. These automatically chosen thresholds can also be updated using \code{\link{updateThreshold}}.
#'
#' @return An object of class \code{scalpelStep1}, which can be summarized using \code{\link{summary}}, used to run SCALPEL Step 2 using \code{\link{scalpelStep2}},
#' or can be used with the plotting function \code{\link{plotCandidateFrame}}.
#' \itemize{
#' \item{\code{Azero}: }{A matrix containing the preliminary dictionary elements, where the ith column of \code{Azero}
#' is a vector of 1's and 0's, indicating whether each pixel is contained in the ith preliminary dictionary element.}
#' \item{\code{AzeroFrames}: }{A vector whose ith element gives the video frame from which the preliminary dictionary element
#' in the ith column of \code{Azero} was derived.}
#' \item{\code{AzeroThreshold}: }{A vector whose ith element gives the threshold used to obtain the preliminary dictionary element
#' in the ith column of \code{Azero}.}
#' \item{\code{pixelsUse}: }{A vector with the pixels (i.e., indices of the rows of \code{Azero}) that are contained in at
#' least one preliminary dictionary element.}
#' \item{\code{version}: }{A 5-digit unique identifier for the output folder name that is automatically generated in this step.}
#' \item{Other elements: }{As specified by the user or returned from a previous step.}
#' }
#'
#' @details Several files containing data from this step and a summary of the step are saved in
#' "outputFolder/Step1_version" where \code{version} is a 5-digit unique identifier that is automatically generated.
#'
#' @seealso The entire SCALPEL pipeline can be implemented using the \code{\link{scalpel}} function. The
#' other steps in the pipeline can be run using the \code{\link{scalpelStep0}}, \code{\link{scalpelStep2}}, \code{\link{scalpelStep3}} functions.
#' Results from this step can be summarized using \code{\link{summary}}, loaded at a later time using \code{\link{getScalpelStep1}}, and plotted
#' using \code{\link{plotCandidateFrame}}.
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "scalpelStep0" function
#'
#' #run Step 1 of SCALPEL
#' Step1Out = scalpelStep1(step0Output = Step0Out)
#' summary(Step1Out)
#' }
#' @import Matrix
#' @export
scalpelStep1 = function(step0Output, minSize=25, maxSize=500, maxWidth=30, maxHeight=30, thresholdVec=NULL) {

  message("Beginning Step 1 of SCALPEL")
  utils::write.table(1,paste0(step0Output$outputFolder,"timings/Step1_0_start.txt"))

  #check function arguments
  if (class(step0Output)!="scalpel" & class(step0Output)!="scalpelStep0") stop("The class of 'step0Output' must be 'scalpel' or 'scalpelStep0'")
  if (minSize<25) {
    message("Note 'minSize' is too small -- a minimum size of 25 pixels will be used")
    minSize = 25
  }
  if (minSize>=maxSize) stop("Must choose 'minSize' to be smaller than 'maxSize'")

  #generate 5-digit unique identifier for folder name
  folderCreated = FALSE
  while(!folderCreated) {
    version = as.numeric(paste(sample(1:9, 5, replace=TRUE), collapse=""))
    #create folder
    if (!dir.exists(paste0(step0Output$outputFolder,"Step1_",version,"/"))) {
      dir.create(paste0(step0Output$outputFolder,"Step1_",version,"/"))
      folderCreated = TRUE
    }
  }
  step1folder = paste0(step0Output$outputFolder,"Step1_",version,"/Step1Data/")
  dir.create(step1folder)

  totalFrames = 0
  for (part in step0Output$partsDeltaf) {
    message("Performing image segmentation on part ", part, " of ", max(step0Output$partsDeltaf))

    #load in delta f over f
    deltafoverfY = readRDS(paste0(step0Output$outputFolder,"Step0Data/Ydeltaf_part",part,".rds"))
    totalFrames = totalFrames + ncol(deltafoverfY)

    #calculate thresholdVec (vector of thresholds to use in image segmentation) if not provided
    #default is to use negative of minimum of delta f, negative of 0.1% quantile of delta f, and mean of these
    #if (is.null(thresholdVec)) thresholdVec = seq(-stats::quantile(c(deltafoverfY), probs=0.001), -min(deltafoverfY), len=3)
    if (is.null(thresholdVec)) thresholdVec = seq(step0Output$lowThreshold, step0Output$highThreshold, len=3)
    utils::write.table(1,paste0(step0Output$outputFolder,"timings/Step1_part",part,"_1_threshold.txt"))

    #store preliminary dictionary elements and frame from which component is derived
    AList = vector("list", length=length(thresholdVec))
    AframeList = vector("list", length=length(thresholdVec))

    #perform the image segmentation for each of the values in thresholdVec
    for (qindex in seq(thresholdVec)) {

      message("Performing segmentation for threshold ", qindex, " of ", length(thresholdVec))
      utils::write.table(1,paste0(step0Output$outputFolder,"timings/Step1_part",part,"_2_qindex_",qindex,".txt"))

      threshold = thresholdVec[qindex]
      #create binary image
      thresholdY = (deltafoverfY>=threshold)
      thresholdYSp = Matrix(thresholdY, sparse=TRUE)
      rm('thresholdY')

      #extract preliminary dictionary elements from each frame
      #based on finding connected components of size 25 pixels or greater
      AList[[qindex]] = Matrix(0, nrow=nrow(deltafoverfY), ncol=60000, sparse=TRUE)
      AframeList[[qindex]] = rep(NA, 60000)

      #only consider frames which have at least 25 pixels above the threshold
      brightFrames = which(colSums(thresholdYSp)>=25)
      nframes = length(brightFrames); counter = 1

      #column index for AList
      index = 1

      for (frame in brightFrames) {
        if (counter%%100==0) message("Segmenting frame ", counter, " of ", nframes, " bright frames")
        counter = counter + 1
        #get preliminary dictionary elements
        comps = getSpatialComponents(frameVec = thresholdYSp[,frame], videoHeight = step0Output$videoHeight,
                                     connectivity = 4, minSize = minSize, maxSize = maxSize,
                                     maxWidth = maxWidth, maxHeight = maxHeight)

        #add preliminary dictionary elements to AList
        if (ncol(comps)>0) {
          AList[[qindex]][,index:(index+ncol(comps)-1)] = comps
          AframeList[[qindex]][index:(index+ncol(comps)-1)] = frame + totalFrames - ncol(deltafoverfY)
          index = index + ncol(comps)
        }
      }
      rm('thresholdYSp')

      #### updated 11/29/17: account for possibly not finding any prelim elements
      #### BEGINNING OF UPDATES
      if (length(which(colSums(AList[[qindex]])>0))>0) { 
        filled = 1:max(which(colSums(AList[[qindex]])>0))
        AList[[qindex]] = AList[[qindex]][,filled]
        AframeList[[qindex]] = AframeList[[qindex]][filled]
      } else {
        AList[[qindex]] = NULL
        AframeList[[qindex]] = NULL
      }
    }
    utils::write.table(1,paste0(step0Output$outputFolder,"timings/Step1_part",part,"_3_finishsegmentation.txt"))
    rm('deltafoverfY')

    #combine components from different thresholds
    A = AList[[1]]
    Aframes = AframeList[[1]]
    if (!is.null(AList[[1]])) Athreshold = rep(thresholdVec[1], ncol(AList[[1]])) else Athreshold = c()

    if (length(AList)>1) {
      for (i in 2:length(AList)) {
        A = cbind(A, AList[[i]])
        Aframes = c(Aframes, AframeList[[i]])
        if (!is.null(AList[[i]])) Athreshold = c(Athreshold, rep(thresholdVec[i], ncol(AList[[i]])))
      }
    }
    if (is.null(A)) stop("No preliminary neurons found during Step 1. Different 'thresholdVec' needed.")
    #### END OF UPDATES
    
    saveRDS(A, paste0(step1folder,"Azero_part",part,".rds"))
    saveRDS(Aframes, paste0(step1folder,"AzeroFrames_part",part,".rds"))
    saveRDS(Athreshold, paste0(step1folder,"AzeroThreshold_part",part,".rds"))
    rm(list=c('AList','AframeList','A','Aframes','Athreshold'))
  }

  utils::write.table(1,paste0(step0Output$outputFolder,"timings/Step1_4_startcombine.txt"))

  #now combine preliminary dictionary elements from all parts in scalpelOutput$partsDeltaf
  # and figure out which pixels from the video are needed (pixelsUse)
  for (part in step0Output$partsDeltaf) {
    if (part==1) {
      A = readRDS(paste0(step1folder,"Azero_part",part,".rds"))
      Aframes = readRDS(paste0(step1folder,"AzeroFrames_part",part,".rds"))
      Athreshold = readRDS(paste0(step1folder,"AzeroThreshold_part",part,".rds"))
    } else {
      A = cbind(A, readRDS(paste0(step1folder,"Azero_part",part,".rds")))
      Aframes = c(Aframes, readRDS(paste0(step1folder,"AzeroFrames_part",part,".rds")))
      Athreshold = c(Athreshold, readRDS(paste0(step1folder,"AzeroThreshold_part",part,".rds")))
    }
  }
  #the pixels that are non-zero in at least one preliminary dictionary element
  pixelsUse = which(rowSums(A)!=0)
  utils::write.table(1,paste0(step0Output$outputFolder,"timings/Step1_5_endcombine.txt"))

  message("Saving data needed in Step 2 of SCALPEL")
  #create a matrix with the whole video
  #but only the pixels which are in at least one preliminary dictionary element
  zeroCut = min(thresholdVec)
  wholedeltafoverfY = matrix(0, nrow=length(pixelsUse), ncol=totalFrames)
  colIndex = 1
  for (part in step0Output$partsDeltaf) {
    deltafoverfY = readRDS(paste0(step0Output$outputFolder,"Step0Data/Ydeltaf_part",part,".rds"))
    deltafoverfY = deltafoverfY[pixelsUse,]
    deltafoverfY[deltafoverfY<=zeroCut] = 0
    wholedeltafoverfY[,colIndex:(colIndex+ncol(deltafoverfY)-1)] = deltafoverfY
    colIndex = colIndex + ncol(deltafoverfY)
    rm(deltafoverfY)
  }
  wholedeltafoverfY = Matrix(wholedeltafoverfY, sparse = TRUE)
  utils::write.table(1,paste0(step0Output$outputFolder,"timings/Step1_6_wholeY.txt"))

  utils::write.table(1,paste0(step0Output$outputFolder,"timings/Step1_7_thresholdY.txt"))

  #remove preliminary dictionary elements that have zero fluorescence using thresholded Y
  #i.e., those preliminary dictionary elements that don't have any pixels above the threshold for any frame
  totalFluor = rowSums(wholedeltafoverfY)
  Fluor = crossprod(A[pixelsUse,], matrix(totalFluor, ncol=1))
  remove = which(Fluor==0)
  if (length(remove)>0) {
    A = A[,-remove,drop=FALSE]
    Aframes = Aframes[-remove]
    Athreshold = Athreshold[-remove]
  }
  saveRDS(A, paste0(step1folder,"Azero.rds"))
  saveRDS(Athreshold, paste0(step1folder,"AzeroThreshold.rds"))
  saveRDS(Aframes, paste0(step1folder,"AzeroFrames.rds"))
  utils::write.table(1,paste0(step0Output$outputFolder,"timings/Step1_8_newcandidates.txt"))

  #update pixelsUse and corresponding matrices
  pixelsUseUpdated = which(rowSums(A[pixelsUse,])!=0)
  if (length(pixelsUseUpdated)!=length(pixelsUse)) {
    wholedeltafoverfY = wholedeltafoverfY[pixelsUseUpdated,]
  }
  saveRDS(wholedeltafoverfY, paste0(step1folder,"Ydeltaf_pixelsUse_thresholded.rds"))
  rm(wholedeltafoverfY)
  pixelsUse = pixelsUse[pixelsUseUpdated]
  saveRDS(pixelsUse, paste0(step1folder,"pixelsUse.rds"))

  #summary file of data
  fileConn = file(paste0(step0Output$outputFolder, "Step1_",version,"/Step1Summary.txt"))
  writeLines(c("Step 1 of SCALPEL","","SUMMARY",
               paste0("We performed image segmentation to produce a total of ", ncol(A), " preliminary dictionary elements, which came from ",
                      length(unique(Aframes)), " different frames of the video. There are ", length(pixelsUse),
                      " pixels contained in at least one preliminary dictionary element. The version (i.e., unique identifier) for this run of Step 1 is ", version, "."),
               "", "ARGUMENTS USED", paste0("Minimum size of preliminary dictionary element allowed (in pixels): ", minSize),
               paste0("Maximum size of preliminary dictionary element allowed (in pixels): ", maxSize),
               paste0("Maximum width of preliminary dictionary element allowed (in pixels): ", maxWidth),
               paste0("Maximum height of preliminary dictionary element allowed (in pixels): ", maxHeight),
               "Threshold values used for image segmentation: ", paste0(signif(thresholdVec, 3))), fileConn)
  close(fileConn)

  saveRDS(thresholdVec, paste0(step1folder,"thresholdVec.rds"))

  #return variables needed for Step 2
  output = getScalpelStep1(outputFolder=step0Output$outputFolder, version=version)

  utils::write.table(1,paste0(step0Output$outputFolder,"timings/Step1_9_end.txt"))
  message("Step 1 of SCALPEL finished!")
  return(output)
}

###############################################################################################
## SCALPEL STEP 2: REFINEMENT OF THE SPATIAL COMPONENT DICTIONARY
## PERFORM PROTOTYPE CLUSTERING WITH SPATIAL/TEMPORAL DISSIMILARITY
###############################################################################################

#' Perform Step 2 of SCALPEL.
#'
#' This step involves refinement of the spatial component dictionary from Step 1. We eliminate redundancy
#' in the spatial component dictionary by clustering together preliminary dictionary elements that represent
#' the same neuron, based on spatial and temporal information.
#'
#' @param step1Output An object of class \code{scalpel} or \code{scalpelStep1}, which result from running the
#' \code{\link{scalpel}} or \code{\link{scalpelStep1}} functions, respectively.
#' @param cutoff A value in [0,1] indicating where to cut the dendrogram that results from hierarchical clustering
#' of the preliminary dictionary elements. The default value is 0.18.
#' @param omega A value in [0,1] indicating how to weight spatial vs. temporal information in the dissimilarity metric
#' used for clustering. If \code{omega=1}, only spatial information is used. The default value is 0.2.
#' @param maxSizeToCluster Optional advanced user argument: The maximum number of preliminary dictionary elements to cluster at once. We attempt to cluster each
#' overlapping set of preliminary dictionary elements, but if one of these sets is very large (e.g., >10,000), memory issues may
#' result. Thus we perform a two-stage clustering in which we first cluster together random sets of size
#' approximately equaling \code{maxSizeToCluster} and then cluster together the representatives from the first stage.
#' Finally, we recalculate the representatives using all of the preliminary dictionary elements in the final clusters. The default value is 3000.
#' If \code{maxSizeToCluster} is set to \code{NULL}, single-stage clustering is done, regardless of the size of the overlapping sets.
#' Memory issues may result when using this option to force single-stage clustering if the size of
#' the largest overlapping set of preliminary dictionary elements is very large (e.g., >10,000).
#'
#' @return An object of class \code{scalpelStep2}, which can be summarized using \code{\link{summary}}, used to run SCALPEL Step 3 using \code{\link{scalpelStep3}},
#' or can be used with the plotting functions \code{\link{plotCluster}} and \code{\link{plotSpatial}}.
#' \itemize{
#' \item{\code{A}: }{A matrix containing the dictionary elements, where the ith column of \code{A}
#' is a vector of 1's and 0's, indicating whether each pixel is contained in the ith dictionary element.}
#' \item{\code{repComps}: }{A vector where the ith element indicates which preliminary dictionary element is the ith representive
#' component. That is, \code{A[,i]=step1Output$Azero[,repComps[i]]}.}
#' \item{\code{clusterID}: }{A vector whose ith element indicates which of the dictionary elements in \code{A} is the
#' representative for the ith preliminary dictionary element.}
#' \item{\code{overlapSetID}: }{A vector indicating which preliminary dictionary elements overlap, with the ith element
#' giving the group index for the ith preliminary dictionary element.}
#' \item{\code{treeList}: }{A list of length \code{max(overlapSetID)} with the ith element containing an object of class \code{protoclust} corresponding
#' to prototype clustering for the preliminary dictionary elements with \code{overlapSetID=i}. If two-stage clustering was done for a
#' particular set, then the element will be \code{NULL}.}
#' \item{Other elements: }{As specified by the user or returned from a previous step.}
#' }
#'
#' @details Several files containing data from this step and a summary of the step are saved in the folder
#' "outputFolder/Step1_version/Step2_omega_omega_cutoff_cutoff" where \code{version} is a 5-digit unique
#'  identifier that is automatically generated in Step 1 and \code{omega} and \code{cutoff} are the user-supplied parameters.
#'
#' @seealso The entire SCALPEL pipeline can be implemented using the \code{\link{scalpel}} function. The
#' other steps in the pipeline can be run using the \code{\link{scalpelStep0}}, \code{\link{scalpelStep1}}, \code{\link{scalpelStep3}} functions.
#' Results from this step can be summarized using \code{\link{summary}}, loaded at a later time using \code{\link{getScalpelStep2}}, and plotted using
#' \code{\link{plotCluster}} and \code{\link{plotSpatial}}.
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "scalpelStep1" function
#'
#' #run Step 2 of SCALPEL
#' Step2Out = scalpelStep2(step1Output = Step1Out)
#' summary(Step2Out)
#' }
#' @import Matrix
#' @export
scalpelStep2 = function(step1Output, cutoff=0.18, omega=0.2, maxSizeToCluster=3000) {

  message("Beginning Step 2 of SCALPEL")
  utils::write.table(1,paste0(step1Output$outputFolder,"timings/Step2_0_start.txt"))

  #check function arguments
  if (class(step1Output)!="scalpel" & class(step1Output)!="scalpelStep1") stop("The class of 'step1Output' must be 'scalpel' or 'scalpelStep1'")
  if (cutoff<0 | cutoff>1) stop("Specify 'cutoff' in [0,1]")
  if (omega<0 | omega>1) stop("Specify 'omega' in [0,1]")
  if (cutoff>omega) message("The clustering results may be inexact when 'cutoff'>'omega', as we only consider clustering overlapping sets of preliminary dictionary elements. When 'cutoff'>'omega', non-overlapping preliminary dictionary elements may be clustered together.")

  step1folder = paste0(step1Output$outputFolder,"Step1_",step1Output$version,"/Step1Data/")
  step2folder = paste0(step1Output$outputFolder,"Step1_",step1Output$version,"/Step2_omega_",omega,"_cutoff_",cutoff,"/")
  if (dir.exists(step2folder)) {
    overwrite = UIinputYesNo(paste0("Results already exist for omega=",omega," and cutoff=",cutoff,".\n
                                    Should we overwrite them? Enter 'Y' or 'N': \n"))
    if (overwrite==FALSE) {
      stop("Results not overwritten. Please rerun for a different combo of 'omega' and 'cutoff'.")
    } else {
      #delete existing data directory
      unlink(step2folder, recursive = TRUE)
    }
  }
  dir.create(step2folder)
  dir.create(paste0(step2folder,"Step2Data/"))

  #force one-stage clustering if maxSizeToCluster is NULL
  fullCluster = ifelse(is.null(maxSizeToCluster), TRUE, FALSE)

  #load thresholded version of Y
  YThreshold = readRDS(paste0(step1folder,"Ydeltaf_pixelsUse_thresholded.rds"))

  message("Determining sets of overlapping preliminary dictionary elements")
  #identify sets of overlapping preliminary dictionary elements, so we can perform prototype clustering on each
  #first find sets of connected preliminary dictionary elements for each chunk of 5000 components
  numChunks = ceiling(ncol(step1Output$Azero)/5000)
  set = c()
  for (i in 1:numChunks) {
    if (i==numChunks) cols = ((i-1)*5000+1):ncol(step1Output$Azero) else cols = ((i-1)*5000+1):(i*5000)
    newSet = findSets(step1Output$Azero[,cols])
    if (i==1) set = newSet else set = c(set, max(set) + newSet)
  }
  utils::write.table(1,paste0(step1Output$outputFolder,"timings/Step2_1_firstoverlap.txt"))

  #create new Azero matrix which adds together the components in the same cluster
  #then do clustering again on this matrix
  AzeroShrunk = matrix(NA, nrow=nrow(step1Output$Azero), ncol=max(set))
  for (col in 1:max(set)) {
    cols = which(set==col)
    AzeroShrunk[,col] = rowSums(step1Output$Azero[,cols,drop=FALSE])
  }
  AzeroShrunk = Matrix(AzeroShrunk, sparse=TRUE)
  setShrunk = findSets(AzeroShrunk)
  rm('AzeroShrunk')

  #final sets of overlapping preliminary dictionary elements
  finalSet = setShrunk[set]
  saveRDS(finalSet, paste0(step2folder, "Step2Data/overlapSetID.rds"))

  #matrix to store dictionary elements and vector for cluster IDs
  A = Matrix(0, nrow=nrow(step1Output$Azero), ncol=2000, sparse=TRUE)
  repComps = rep(NA, 2000)
  clusterID = rep(NA, ncol(step1Output$Azero))
  AIndex = 1
  treeList = vector("list", length=max(finalSet))

  utils::write.table(1,paste0(step1Output$outputFolder,"timings/Step2_2_ready.txt"))

  #do prototype clustering for each group of overlapping components
  for (setInd in 1:max(finalSet)) {
    message("Clustering overlapping set: ", setInd, " of ", max(finalSet))

    cols = which(finalSet==setInd)
    if (length(cols)>1) {
      #do prototype clustering but choose dictionary element that has
      #the smallest median dissimilarity from others
      clusterOut = clusterComponents(Azero=step1Output$Azero[which(rowSums(step1Output$Azero[,cols,drop=FALSE])!=0),cols],
                                     Y=YThreshold[which(rowSums(step1Output$Azero[step1Output$pixelsUse,cols,drop=FALSE])!=0),],
                                     videoHeight=step1Output$videoHeight, cutoff=cutoff, weight=omega,
                                     fullCluster=fullCluster, maxSizeToCluster=maxSizeToCluster)

      #save info
      clusterID[cols] = clusterOut$clusterID + max(c(clusterID, 0), na.rm=TRUE)
      A[,AIndex:(AIndex+length(clusterOut$repComps)-1)] = step1Output$Azero[,cols[clusterOut$repComps]]
      repComps[AIndex:(AIndex+length(clusterOut$repComps)-1)] = cols[clusterOut$repComps]
      AIndex = AIndex+length(clusterOut$repComps)
      if (!is.null(clusterOut$tree)) treeList[[setInd]] = clusterOut$tree
    } else {
      clusterID[cols] = 1 + max(c(clusterID, 0), na.rm=TRUE)

      A[,AIndex] = step1Output$Azero[,cols]
      repComps[AIndex] = cols
      AIndex = AIndex+1
    }
  }
  utils::write.table(1,paste0(step1Output$outputFolder,"timings/Step2_3_clustered.txt"))
  rm(YThreshold)

  repComps = repComps[1:max(which(colSums(A)>0))]
  A = A[,1:max(which(colSums(A)>0))]
  saveRDS(A, paste0(step2folder, "Step2Data/A.rds"))
  saveRDS(repComps, paste0(step2folder, "Step2Data/repComps.rds"))
  saveRDS(clusterID, paste0(step2folder, "Step2Data/clusterID.rds"))

  #summary file of data
  fileConn = file(paste0(step2folder, "Step2Summary.txt"))
  writeLines(c("Step 2 of SCALPEL","","SUMMARY",
               paste0("We performed clustering on the ", ncol(step1Output$Azero), " preliminary dictionary elements, which resulted in ",
                      ncol(A), " dictionary elements."),
               "", "ARGUMENTS USED",
               paste0("Cutoff for dendrogram: ", cutoff), paste0("Dissimilarity metric weight: ", omega),
               paste0("Maximum number of preliminary dictionary elements to cluster at once: ", ifelse(is.null(maxSizeToCluster), "N/A", maxSizeToCluster)),
               "", "ADVANCED USER INFO",
               paste0("There were ", max(finalSet), " overlapping sets of components with the largest set containing ",
                      max(table(finalSet)), " preliminary dictionary elements. The clustering shortcut for large sets of overlapping components ",
                      ifelse(fullCluster==TRUE, "was not", "was"), " used.",
                      ifelse(fullCluster==FALSE, paste0(" There were ", length(which(floor(table(finalSet)/maxSizeToCluster)>1)),
                                                        " overlapping sets for which the shortcut was used."),""))), fileConn)
  close(fileConn)
  rm(A)
  saveRDS(treeList, paste0(step2folder, "Step2Data/treeList.rds"))

  output = getScalpelStep2(outputFolder=step1Output$outputFolder, version=step1Output$version, cutoff=cutoff, omega=omega)
  utils::write.table(1,paste0(step1Output$outputFolder,"timings/Step2_4_end.txt"))

  message("Step 2 of SCALPEL finished!")
  return(output)
}

###############################################################################################
## SCALPEL STEP 3: SPATIAL COMPONENT SELECTION AND TEMPORAL COMPONENT ESTIMATION
## FIT SPARSE GROUP LASSO WITH NON-NEGATIVITY CONSTRAINT
###############################################################################################

#' Perform Step 3 of SCALPEL.
#'
#' This step involves spatial component selection and temporal component estimation. We estimate the temporal
#' components corresponding to the dictionary elements from Step 2 by solving a sparse group lasso problem
#' with a non-negativity constraint.
#'
#' @param step2Output An object of class \code{scalpel} or \code{scalpelStep2}, which result from running the
#' \code{\link{scalpel}} or \code{\link{scalpelStep2}} functions, respectively.
#' @param lambdaMethod A description of how lambda should be chosen: either \code{"trainval"} (default),
#' \code{"distn"}, or \code{"user"}. A value of \code{"trainval"} means lambda will be chosen using a training/validation
#' set approach. A value of \code{"distn"} means lambda will be chosen as the negative of the 0.1\% quantile
#' of elements of active pixels (i.e., those contained in at least one dictionary element) of Y.
#' Using \code{"distn"} is computationally faster than \code{"trainval"}. Alternatively with \code{"user"},
#' the value of lambda can be directly specified using \code{lambda}.
#' @param lambda The value of lambda to use when fitting the sparse group lasso. By default, the value is automatically
#' chosen using the approach specified by \code{lambdaMethod}. If a value is provided for \code{lambda}, then \code{lambdaMethod}
#' will be ignored.
#' @param minClusterSize The minimum number of preliminary dictionary elements that a cluster must contain in order to be included
#' in the sparse group lasso. The default value is 1 (i.e., all possible dictionary elements are included).
#' @param alpha The value of alpha to use when fitting the sparse group lasso. The default value is 0.9.
#' @param removeBorder A logical scalar indicating whether the dictionary elements containing pixels in the 10-pixel
#' border of the video should be removed prior to fitting the sparse group lasso. The default value is \code{FALSE}.
#' @param excludeReps A vector giving the indices of which dictionary elements to exclude, where the indices refer
#' to the columns of \code{step2Output$A}. The default value is \code{NULL} and no dictionary elements are excluded.
#' Users may also specify \code{"discarded"}, which will exclude all dictionary elements discarded
#' using a previous call to \code{\link{reviewNeurons}} or \code{\link{reviewNeuronsInteractive}}.
#'
#' @return An object of class \code{scalpelStep3}, which can be summarized using \code{\link{summary}} and used with the plotting functions
#' \code{\link{plotResults}}, \code{\link{plotResultsAllLambda}}, \code{\link{plotSpatial}}, \code{\link{plotTemporal}}, and \code{\link{plotBrightest}}.
#' \itemize{
#' \item{\code{Afilter}: }{A matrix containing the filtered dictionary elements, where the ith column of \code{Afilter}
#' is a vector of 1's and 0's, indicating whether each pixel is contained in the ith filtered dictionary element.
#' Note that \code{Afilter} is equivalent to \code{A} after removing the components excluded due to being on the border
#' (if \code{removeBorder=TRUE}) or having fewer preliminary dictionary elements in their cluster than \code{minClusterSize}.}
#' \item{\code{Zhat}: }{A matrix containing the estimated temporal components, where the ith row of \code{Zhat}
#' is the estimated calcium trace corresponding to the ith spatial component (i.e., the ith column of \code{Afilter}).}
#' \item{\code{lambda}: }{The value of lambda used in fitting the sparse group lasso.}
#' \item{\code{ZhatList}: }{A list of matrices containing the estimated temporal components for alternative values of \code{lambda}
#' specified in \code{lambdaSeq}. These can be plotted using \code{\link{plotResultsAllLambda}}}.
#' \item{\code{lambdaSeq}: }{A vector with length equaling the length of \code{ZhatList}, where the ith element indicates the value of lambda
#' corresponding to the temporal components in \code{ZhatList[[i]]}.}
#' \item{\code{clustersUse}: }{A vector with length equaling the number of columns of \code{Afilter}, where the ith element indicates which column of
#' \code{step2Output$A} the ith column of \code{Afilter} equals.}
#' \item{Other elements: }{As specified by the user or returned from a previous step.}
#' }
#'
#' @details To solve the sparse group lasso problem in this step, we minimize the following over \code{Z} with all non-negative elements:
#'
#' \code{0.5*sum((Y - AfilterTilde \%*\% Z)^2) + lambda*alpha*sum(Z)} \cr
#' \code{ + lambda*(1-alpha)*sum(sqrt(rowSums(Z^2)))}
#'
#' where \code{AfilterTilde} is a scaled version of \code{Afilter}.
#'
#' Several files containing data from this step and a summary of the step are saved in the folder
#' "outputFolder/Step1_version/Step2_omega_omega_cutoff_cutoff/Step3_lambdaMethod_lambdaMethod_
#' minClusterSize_minClusterSize_alpha_alpha_removeBorder_removeBorder" where \code{version} is a 5-digit unique
#'  identifier that is automatically generated in Step 1, \code{omega} and \code{cutoff} are the user-supplied parameters
#'  from Step 2, and \code{lambdaMethod}, \code{minClusterSize}, \code{alpha}, and \code{removeBorder} are
#'  the user-supplied parameters from this step. If dictionary elements were manually excluded using \code{excludeReps},
#'  this is appended to the folder name.
#'
#' @seealso The entire SCALPEL pipeline can be implemented using the \code{\link{scalpel}} function. The
#' other steps in the pipeline can be run using the \code{\link{scalpelStep0}}, \code{\link{scalpelStep1}}, \code{\link{scalpelStep2}} functions.
#' Results from this step can be summarized using \code{\link{summary}}, loaded at a later time using \code{\link{getScalpelStep3}}, and plotted using
#' \code{\link{plotSpatial}}, \code{\link{plotTemporal}}, \code{\link{plotResults}}, and \code{\link{plotBrightest}}.
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "scalpelStep2" function
#'
#' #run Step 3 of SCALPEL
#' Step3Out = scalpelStep3(step2Output = Step2Out)
#' summary(Step3Out)
#' }
#' @import Matrix
#' @export
scalpelStep3 = function(step2Output, lambdaMethod="trainval", lambda=NULL, minClusterSize=1, alpha=0.9, removeBorder=FALSE, excludeReps=NULL) {

  message("Beginning Step 3 of SCALPEL")
  utils::write.table(1,paste0(step2Output$outputFolder,"timings/Step3_0_start.txt"))

  if (!is.null(lambda)) lambdaMethod = "user"
  #check function arguments
  if (lambdaMethod=="user" & is.null(lambda)) stop("Must specify 'lambda' if 'lambdaMethod' is 'user'")
  if (class(step2Output)!="scalpel" & class(step2Output)!="scalpelStep2") stop("The class of 'step2Output' must be 'scalpel' or 'scalpelStep2'")
  if (alpha<0 | alpha>1) stop("The value of 'alpha' must be in the interval [0,1]")
  if (!is.logical(removeBorder)) stop("Specify TRUE or FALSE for 'removeBorder'")
  if (nrow(step2Output$A)%%step2Output$videoHeight!=0) stop(paste0("The number of rows of 'step2Output$A' should be divisble by 'step2Output$videoHeight'"))
  if (lambdaMethod!="distn" & lambdaMethod!="trainval" & lambdaMethod!="user") stop("Specify 'distn', 'trainval', or 'user' for 'lambdaMethod', or specify 'lambda'")
  if (max(step2Output$clusterID)!=ncol(step2Output$A)) stop("The number of columns of 'step2Output$A' should equal the maximum value of 'step2Output$clusterID'")
  if (!is.null(excludeReps)) {
    #excludeReps is "discarded"
    if (length(excludeReps)==1) if (excludeReps=="discarded") {
      keepFilename = paste0(step2Output$outputFolder,"Step1_",step2Output$version,"/Step2_omega_",step2Output$omega,"_cutoff_",step2Output$cutoff,"/Step2Data/keep.rds")
      if (!file.exists(keepFilename)) stop(paste0("When 'excludeReps' is set to 'discarded', the file ", keepFilename, " must exist"))
      keep = readRDS(keepFilename)
      excludeReps = which(keep=="no")
    }
    if (max(excludeReps)>ncol(step2Output$A)) stop(paste0("The elements of 'excludeReps' must all be integers from 1 to ",ncol(step2Output$A)))
  }
  step1folder = paste0(step2Output$outputFolder,"Step1_",step2Output$version,"/Step1Data/")

  totalPixels = nrow(step2Output$A)

  #remove SCALPEL neurons with pixels in a 10-pixel border around the edge
  excludeManually = rep(0, ncol(step2Output$A))
  excludeManually[excludeReps] = 1
  if (removeBorder==TRUE) {
    videoWidth = totalPixels/step2Output$videoHeight
    indicesRemove = unique(c(1:(10*step2Output$videoHeight), ((videoWidth-10)*step2Output$videoHeight+1):totalPixels, which(1:totalPixels %% step2Output$videoHeight<=10), which(1:totalPixels %% step2Output$videoHeight>=(step2Output$videoHeight-9))))
    onBoundary = (colSums(step2Output$A[indicesRemove,])!=0)
    clustersUse = which(table(step2Output$clusterID)>=minClusterSize & !onBoundary & !excludeManually)
  } else {
    #only use dictionary elements based on minClusterSize or more preliminary dictionary elements
    clustersUse = which(table(step2Output$clusterID)>=minClusterSize & !excludeManually)
  }
  numExcluded = ncol(step2Output$A) - length(clustersUse)
  if (length(clustersUse)==0) stop("All of the dictionary elements have been excluded, so we cannot fit the sparse group lasso model.")

  #load in delta f over f Y
  nFramesDeltaf = readRDS(paste0(step2Output$outputFolder,"Step0Data/nFramesDeltaf.rds"))
  partsDeltaf = readRDS(paste0(step2Output$outputFolder,"Step0Data/partsDeltaf.rds"))
  Ypixelsuse = matrix(0, nrow=length(step2Output$pixelsUse), ncol=sum(nFramesDeltaf))
  colIndex = 1
  for (part in partsDeltaf) {
    deltafoverfY = readRDS(paste0(step2Output$outputFolder,"Step0Data/Ydeltaf_part",part,".rds"))
    Ypixelsuse[,colIndex:(colIndex+ncol(deltafoverfY)-1)] = deltafoverfY[step2Output$pixelsUse,]
    colIndex = colIndex + ncol(deltafoverfY)
    rm(deltafoverfY)
  }
  Ysgl = Ypixelsuse[which(rowSums(step2Output$A[step2Output$pixelsUse,clustersUse,drop=FALSE])!=0),]
  rm(Ypixelsuse)

  #A to use in SGL problem
  finalPixelsUse = which(rowSums(step2Output$A[,clustersUse,drop=FALSE])!=0)
  Asgl = step2Output$A[finalPixelsUse,clustersUse,drop=FALSE]

  #find sets of overlapping neurons
  setVec = findSets(Asgl)
  utils::write.table(1,paste0(step2Output$outputFolder,"timings/Step3_1_prelim.txt"))

  if (lambdaMethod=="distn") {

    message("Calculating value of lambda to use")
    #lambda chosen as negative of 0.1% quantile of elements of active pixels of deltafoverfY
    #choose a single lambda
    #if Ysgl is large, just use 1000 rows of it
    set.seed(100)
    if (nrow(Ysgl)>1000) rows = sample(1:nrow(Ysgl),1000) else rows = 1:nrow(Ysgl)
    lambdaByDistn = -stats::quantile(as.matrix(Ysgl)[rows,], p=0.001)
    #the above value is lambda*alpha so divide by alpha
    lambdaToFit = lambdaByDistn/alpha

  } else if (lambdaMethod=="trainval") {

    message("Constructing training and validation sets")
    #construct training and validation sets
    seed = 300
    pixelsTrain = c()
    for (i in 1:max(setVec)) {
      pixelsTrain = c(pixelsTrain, samplePixels(A=Asgl, columns=which(setVec==i), seed=seed))
    }
    pixelsVal = (1:nrow(Asgl))[-pixelsTrain]

    #scale A before starting
    ATildesgl = t(t(Asgl) / colSums(Asgl))

    message("Fitting SGL on training set")
    sglOutTrain = sgl(Y=Ysgl[pixelsTrain,], A=Matrix(ATildesgl[pixelsTrain,], sparse=TRUE),
                      videoHeight=step2Output$videoHeight, totalPixels=NA, pixelsUse=NA, lambdaMinRatio = 0.001,
                      alpha = alpha, tolerance = 10e-8, l2Scale = "no")

    message("Calculating validation error")
    #calculate validation error
    valError = rep(NA, length(sglOutTrain$lambdaSeq))
    #calculate validation error using thresholded Y
    YsglThreshold = Ysgl
    YsglThreshold[YsglThreshold<(-min(YsglThreshold))] = 0
    YsglThreshold = Matrix(YsglThreshold, sparse = TRUE)

    for (i in seq(valError)) {
      valError[i] = mean((YsglThreshold[pixelsVal,] - ATildesgl[pixelsVal,] %*% sglOutTrain$ZList[[i]])^2)
    }

    #calculate maximum lambda whose validation error is within 5% of the minimum validation error
    lambdaSmallChange = sglOutTrain$lambdaSeq[min(which((valError-min(valError))/min(valError)<=0.05))]

    #refit on entire A using lambda = (selected lambda using training set)/(pct of pixels in training set)
    #this adjusts for the fact that we don't scale the sum of sq. errors by the number of pixels
    #feed in non-scaled A and use usual sq. l2 scaling
    lambdaToFit = lambdaSmallChange/(length(pixelsTrain)/(length(pixelsTrain)+length(pixelsVal)))
  } else {
    lambdaToFit = lambda
  }

  utils::write.table(1,paste0(step2Output$outputFolder,"timings/Step3_2_lambdaFound.txt"))

  message("Fitting SGL")
  lambdaSeq = c(exp(seq(log(lambdaToFit*10), log(lambdaToFit), len=5)), exp(seq(log(lambdaToFit), log(lambdaToFit*0.1), len=6))[2:6])
  sglOutOverall = sgl(Y=Ysgl, A=Matrix(Asgl, sparse=TRUE), lambdaSeq = lambdaSeq,
                      videoHeight=step2Output$videoHeight, totalPixels=totalPixels, pixelsUse=finalPixelsUse,
                      alpha = alpha, tolerance = 10e-8)

  utils::write.table(1,paste0(step2Output$outputFolder,"timings/Step3_3_sglfit.txt"))

  message("Saving results")
  #save A hat and Z hat
  estZ = sglOutOverall$ZList[[5]]
  fullAsgl = Matrix(0, nrow=totalPixels, ncol=ncol(Asgl), sparse=TRUE)
  fullAsgl[finalPixelsUse,] = Asgl

  if (!is.null(excludeReps)) fileAppend = paste0("_excluded_",paste0(sort(excludeReps), collapse = "_")) else fileAppend = ""
  step3folder = paste0(step2Output$outputFolder,"Step1_",step2Output$version,"/Step2_omega_",step2Output$omega,"_cutoff_",step2Output$cutoff,
                       "/Step3_lambdaMethod_",lambdaMethod,"_lambda_",round(lambdaToFit,4),"_minClusterSize_",minClusterSize,
                       "_alpha_",alpha,"_removeBorder_",(removeBorder),fileAppend,"/")
  if (dir.exists(step3folder)) {
    overwrite = UIinputYesNo(paste0("Results already exist for these Step 3 parameters.\n
                                    Should we overwrite them? Enter 'Y' or 'N': \n"))
    if (overwrite==FALSE) {
      stop("Results not overwritten. Please rerun for a different combo of parameters.")
    } else {
      #delete existing data directory
      unlink(step3folder, recursive = TRUE)
    }
  }
  dir.create(step3folder)
  dir.create(paste0(step3folder,"Step3Data/"))

  saveRDS(fullAsgl, file=paste0(step3folder,"Step3Data/Afilter.rds"))
  saveRDS(estZ, file=paste0(step3folder,"Step3Data/Zhat.rds"))
  saveRDS(lambdaToFit, file=paste0(step3folder,"Step3Data/lambda.rds"))
  saveRDS(lambdaSeq, file=paste0(step3folder,"Step3Data/lambdaSeq.rds"))
  saveRDS(sglOutOverall$ZList, file=paste0(step3folder,"Step3Data/ZhatList.rds"))
  saveRDS(clustersUse, file=paste0(step3folder,"Step3Data/clustersUse.rds"))
  saveRDS(excludeReps, file=paste0(step3folder,"Step3Data/excludeReps.rds"))

  #summary file of data
  fileConn = file(paste0(step3folder, "Step3Summary.txt"))
  writeLines(c("Step 3 of SCALPEL","","SUMMARY",
               paste0("We began with ", ncol(step2Output$A), " dictionary elements, and ",
                      ifelse(ncol(step2Output$A)==ncol(Asgl), "all", ncol(Asgl)), " of these were included in the sparse group lasso model. The sparse group lasso was fit with lambda=",
                      signif(lambdaToFit, 4), " and alpha=", signif(alpha, 4), ", and there were ", nrow(estZ[rowSums(estZ)!=0,,drop=FALSE]),
                      " neurons with non-zero temporal components (i.e., the neuron was estimated to be active in at least one frame)."),
               "", "ARGUMENTS USED", paste0("Method to choose lambda: ", ifelse(lambdaMethod=="trainval", "Training/validation set", ifelse(lambdaMethod=="distn", "Using distribution of Y", "User-supplied"))),
               paste0("User-supplied lambda value: ", ifelse(is.null(lambda), "N/A", lambda)), paste0("Value of alpha: ", alpha),
               paste0("Minimum cluster size for dictionary element to be included in the sparse group lasso: ", minClusterSize),
               paste0("Should spatial components on the border of the video be included in the sparse group lasso? ", ifelse(removeBorder==TRUE, "No", "Yes")),
               paste0("Indices of dictionary elements to exclude from the sparse group lasso: ", ifelse(is.null(excludeReps), "N/A", paste0(excludeReps, collapse=", ")))), fileConn)
  close(fileConn)

  output = getScalpelStep3(outputFolder=step2Output$outputFolder, version=step2Output$version, cutoff=step2Output$cutoff,
                           omega=step2Output$omega, lambdaMethod=lambdaMethod, minClusterSize=minClusterSize, alpha=alpha,
                           lambda=lambdaToFit, removeBorder=removeBorder, excludeReps=excludeReps)

  utils::write.table(1,paste0(step2Output$outputFolder,"timings/Step3_4_end.txt"))
  message("Step 3 of SCALPEL finished!")
  return(output)
}

###############################################################################################
## SCALPEL FUNCTION
## PERFORMS ALL STEPS WITH A SINGLE FUNCTION CALL
###############################################################################################

#' Perform entire SCALPEL pipeline.
#'
#' Segmentation, Clustering, and Lasso Penalties (SCALPEL) is a method for neuronal calcium imaging
#' data that identifies the locations of neurons, and estimates their calcium concentrations over time.
#' The pipeline involves several steps, each of which is described briefly in its corresponding
#' function. See \code{\link{scalpelStep0}}, \code{\link{scalpelStep1}}, \code{\link{scalpelStep2}},
#' \code{\link{scalpelStep3}} for more details.
#' Full details for the SCALPEL method are provided in Petersen, A., Simon, N., and Witten, D. (Forthcoming).
#' SCALPEL: Extracting Neurons from Calcium Imaging Data.
#'
#' @param outputFolder Step 0 parameter: The existing directory where the results should be saved.
#' @param rawDataFolder Step 0 parameter: The directory where the raw data version of Y is saved. The data should be a
#' PxT matrix, where P is the total number of pixels per image frame and T
#' the number of frames of the video, for which the (i,j)th element contains the
#' fluorescence of the ith pixel in the jth frame. To create Y, you should
#' vectorize each 2-dimensional image frame by concatenating the columns of the image frame.  If the data is
#' saved in a single file, it should be named "Y_1.mat", "Y_1.rds", "Y_1.txt", or "Y_1.txt.gz" (depending on \code{fileType}),
#' and if the data is split over multiple files, they should be split into chunks of the columns
#' and named consecutively ("Y_1.mat", "Y_2.mat", etc.; "Y_1.rds", "Y_2.rds", etc.; "Y_1.txt", "Y_2.txt", etc.; or "Y_1.txt.gz", "Y_2.txt.gz", etc.).
#' @param videoHeight Step 0 parameter: The height of the video (in pixels).
#' @param minClusterSize Step 3 parameter: The minimum number of preliminary dictionary elements that a cluster must contain in order to be included
#' in the sparse group lasso.
#' @param lambdaMethod Step 3 parameter: A description of how lambda should be chosen: either \code{"trainval"} (default),
#' \code{"distn"}, or \code{"user"}. A value of \code{"trainval"} means lambda will be chosen using a training/validation
#' set approach. A value of \code{"distn"} means lambda will be chosen as the negative of the 0.1\% quantile
#' of elements of active pixels (i.e., those contained in at least one dictionary element) of Y.
#' Using \code{"distn"} is computationally faster than \code{"trainval"}. Alternatively with \code{"user"},
#' the value of lambda can be directly specified using \code{lambda}.
#' @param lambda Step 3 parameter: The value of lambda to use when fitting the sparse group lasso. By default, the value is automatically
#' chosen using the approach specified by \code{lambdaMethod}. If a value is provided for \code{lambda}, then \code{lambdaMethod}
#' will be ignored.
#' @param cutoff Step 2 parameter: A value in [0,1] indicating where to cut the dendrogram that results from hierarchical clustering
#' of the preliminary dictionary elements. The default value is 0.18.
#' @param omega Step 2 parameter: A value in [0,1] indicating how to weight spatial vs. temporal information in the dissimilarity metric
#' used for clustering. If \code{omega=1}, only spatial information is used. The default value is 0.2.
#' @param fileType Step 0 parameter: Indicates whether raw data is an .rds (default value; \code{fileType="R"}), .mat (\code{fileType="matlab"}),
#' .txt (\code{fileType="text"}), or .txt.gz (\code{fileType="zippedText"}) file. Any text files should not have row or column names.
#' @param processSeparately Step 0 parameter: Logical scalar giving whether the multiple raw data files should be
#' processed individually, versus all at once. Processing the files separately may be preferable for larger videos.
#' Default value is \code{TRUE}; this argument is ignored if the raw data is saved in a single file.
#' @param minSize,maxSize Step 1 parameter: The minimum and maximum size, respectively, for a preliminary dictionary element with default values
#' of 25 and 500, respectively.
#' @param maxWidth,maxHeight Step 1 parameter: The maximum width and height, respectively, for a preliminary dictionary element with default values of 30.
#' @param removeBorder Step 3 parameter: A logical scalar indicating whether the dictionary elements containing pixels in the 10-pixel
#' border of the video should be removed prior to fitting the sparse group lasso. The default value is \code{FALSE}.
#' @param alpha Step 3 parameter: The value of alpha to use when fitting the sparse group lasso. The default value is 0.9.
#' @param thresholdVec Optional advanced user argument: Step 1 parameter: A vector with the desired thresholds to use for image segmentation. If not specified, the default is to
#' use the negative of the minimum of the processed Y data, the negative of the 0.1\% quantile of the processed Y data, and the mean of these. If there were multiple raw data
#' files that were processed separately, these values are calculated on only the first part of data, and then these thresholds are used for the remaining parts.
#' @param maxSizeToCluster Optional advanced user argument: Step 2 parameter: The maximum number of preliminary dictionary elements to cluster at once. We attempt to cluster each
#' overlapping set of preliminary dictionary elements, but if one of these sets is very large (e.g., >10,000), memory issues may
#' result. Thus we perform a two-stage clustering in which we first cluster together random sets of size
#' approximately equaling \code{maxSizeToCluster} and then cluster together the representatives from the first stage.
#' Finally, we recalculate the representatives using all of the preliminary dictionary elements in the final clusters. The default value is 3000.
#' If \code{maxSizeToCluster} is set to \code{NULL}, single-stage clustering is done, regardless of the size of the overlapping sets.
#' Memory issues may result when using this option to force single-stage clustering if the size of
#' the largest overlapping set of preliminary dictionary elements is very large (e.g., >10,000).
#'
#' @return An object of class \code{scalpel}, which can be summarized using \code{\link{summary}}, used to rerun SCALPEL Steps 1-3 with new parameters using \code{\link{scalpelStep1}}, \code{\link{scalpelStep2}}, and \code{\link{scalpelStep3}},
#' or can be used with any of the plotting functions: \code{\link{plotFrame}}, \code{\link{plotThresholdedFrame}}, \code{\link{plotVideoVariance}}, \code{\link{plotCandidateFrame}},
#' \code{\link{plotCluster}}, \code{\link{plotResults}}, \code{\link{plotResultsAllLambda}}, \code{\link{plotSpatial}},
#' \code{\link{plotTemporal}}, and \code{\link{plotBrightest}}.
#' The individual elements are described in detail in the documentation for the corresponding step: \code{\link{scalpelStep0}}, \code{\link{scalpelStep1}}, \code{\link{scalpelStep2}}, and \code{\link{scalpelStep3}}.
#'
#' @details Several files containing data from the pipeline, as well as summaries of each step,
#' are saved in various subdirectories of "outputFolder".
#'
#' @seealso The individual steps in the pipeline can be run using the \code{\link{scalpelStep0}},
#' \code{\link{scalpelStep1}}, \code{\link{scalpelStep2}}, and \code{\link{scalpelStep3}} functions.
#' Results can be summarized using \code{\link{summary}}, loaded at a later time using \code{\link{getScalpel}}, and plotted using \code{\link{plotResults}},
#' \code{\link{plotSpatial}}, \code{\link{plotTemporal}}, \code{\link{plotCluster}}, \code{\link{plotVideoVariance}},
#' \code{\link{plotFrame}}, \code{\link{plotThresholdedFrame}}, \code{\link{plotCandidateFrame}}, and \code{\link{plotBrightest}}.
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #existing folder to save results (update this to an existing folder on your computer)
#' outputFolder = "scalpelResults"
#' #location on computer of raw data in R package to use
#' rawDataFolder = gsub("Y_1.rds", "", system.file("extdata", "Y_1.rds", package = "scalpel"))
#' #video height of raw data in R package
#' videoHeight = 30
#' #run SCALPEL pipeline
#' scalpelOutput = scalpel(outputFolder = outputFolder, rawDataFolder = rawDataFolder,
#'                        videoHeight = videoHeight)
#' #summarize each step
#' summary(scalpelOutput, step = 0)
#' summary(scalpelOutput, step = 1)
#' summary(scalpelOutput, step = 2)
#' summary(scalpelOutput, step = 3)
#' }
#' @import Matrix
#' @export
scalpel = function(outputFolder, rawDataFolder, videoHeight, minClusterSize=1, lambdaMethod="trainval", lambda=NULL, cutoff=0.18,
                   omega=0.2, fileType="R", processSeparately=TRUE, minSize=25, maxSize=500, maxWidth=30,
                   maxHeight=30, removeBorder=FALSE, alpha=0.9, thresholdVec=NULL, maxSizeToCluster=3000) {

  if (substr(outputFolder, nchar(outputFolder), nchar(outputFolder))!="/") outputFolder = paste0(outputFolder, "/")
  if (!dir.exists(outputFolder)) stop("Specify an existing folder for 'outputFolder'")
  if (!dir.exists(paste0(outputFolder,"timings/"))) dir.create(paste0(outputFolder,"timings/"))
  utils::write.table(1,paste0(outputFolder,"timings/SCALPEL_Step0_start.txt"))
  step0Output = scalpelStep0(outputFolder=outputFolder, videoHeight=videoHeight,
                             rawDataFolder=rawDataFolder, processSeparately=processSeparately, fileType=fileType)
  utils::write.table(1,paste0(outputFolder,"timings/SCALPEL_Step0_end.txt"))

  utils::write.table(1,paste0(outputFolder,"timings/SCALPEL_Step1_start.txt"))
  step1Output = scalpelStep1(step0Output=step0Output, minSize=minSize, maxSize=maxSize, maxWidth=maxWidth,
                             maxHeight=maxHeight, thresholdVec=thresholdVec)
  utils::write.table(1,paste0(outputFolder,"timings/SCALPEL_Step1_end.txt"))

  utils::write.table(1,paste0(outputFolder,"timings/SCALPEL_Step2_start.txt"))
  step2Output = scalpelStep2(step1Output=step1Output, cutoff=cutoff, omega=omega, maxSizeToCluster=maxSizeToCluster)
  utils::write.table(1,paste0(outputFolder,"timings/SCALPEL_Step2_end.txt"))

  utils::write.table(1,paste0(outputFolder,"timings/SCALPEL_Step3_start.txt"))
  step3Output = scalpelStep3(step2Output=step2Output, lambdaMethod=lambdaMethod, minClusterSize=minClusterSize,
                             alpha=alpha, removeBorder=removeBorder, lambda=lambda)
  utils::write.table(1,paste0(outputFolder,"timings/SCALPEL_Step3_end.txt"))

  output = getScalpel(outputFolder=outputFolder, version=step1Output$version, cutoff=cutoff, omega=omega,
                      lambdaMethod=step3Output$lambdaMethod, lambda=step3Output$lambda, minClusterSize=minClusterSize,
                      alpha=alpha, removeBorder=removeBorder, excludeReps=step3Output$excludeReps)
  return(output)
}
