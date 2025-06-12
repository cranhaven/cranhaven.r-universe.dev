#functions for plotting SCALPEL results

#EXPORTED FUNCTIONS
#exported functions at top with helper functions below

#' Plot temporal components from Step 3 of SCALPEL.
#'
#' We plot the temporal components, displaying the estimated fluorescence over time for each spatial component,
#' which result from running Step 3 of SCALPEL.
#'
#' @param scalpelOutput An object returned by one of these SCALPEL functions:
#' \code{\link{scalpel}} or \code{\link{scalpelStep3}}.
#' @param neuronsToDisplay Vector giving which neurons' temporal components to plot. The indices refer to which rows
#' of \code{scalpelOutput$Zhat} to plot. By default, all components are plotted. Users may also specify \code{"kept"},
#' which will exclude all dictionary elements discarded using a previous call to \code{\link{reviewNeurons}} or \code{\link{reviewNeuronsInteractive}}.
#' @param colVec Vector of colors to use, which are chosen automatically if the default value of NULL is used.
#' @param ylab Label for the y-axis.
#' @param title Label for the title.
#' @param fileName If provided, the plot will be saved to the specified location.
#' @param lambdaIndex Optional advanced user argument: Index of lambda value for which results will be plotted. Default is
#' to use lambda value of \code{scalpelOutput$lambda} but specifying this will use the lambda value of \code{scalpelOutput$lambdaSeq[lambdaIndex]}.
#'
#' @return None
#'
#' @details If \code{lambdaIndex} is \code{NULL}, each temporal component is scaled by its largest value. If
#' \code{lambdaIndex} is specified, each temporal component is scaled by its largest value across all of the lambda values.
#' Temporal components that were zeroed out in the sparse group lasso are omitted from the plot.
#'
#' @seealso \code{\link{scalpelStep3}}, \code{\link{scalpel}}, \code{\link{plotResults}}
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "scalpel" function
#'
#' #simplest example with default parameters:
#' plotTemporal(scalpelOutput = scalpelOutput)
#'
#' #example with optional parameters:
#' #plot only two of the neurons and add a title
#' plotTemporal(scalpelOutput = scalpelOutput, neuronsToDisplay = c(1,2),
#'              title = "First two neurons")
#' }
#' @export
plotTemporal = function(scalpelOutput, neuronsToDisplay=NULL, colVec=NULL, ylab="", title="", fileName=NULL, lambdaIndex=NULL) {

  if (is.null(lambdaIndex)) {
    Zhat = scalpelOutput$Zhat
    #maxVal = max(Zhat)
    #max fluorescence for each neuron over lambda of interest
    maxVal = apply(Zhat, 1, max)
  } else {
    Zhat = scalpelOutput$ZhatList[[lambdaIndex]]
    #maxVal = max(unlist(scalpelOutput$ZhatList))
    #max fluorescence for each neuron over all lambda
    maxVal = apply(sapply(1:nrow(Zhat), function(i, ZhatList) sapply(1:length(ZhatList), function(j) max(ZhatList[[j]][i,])), ZhatList=scalpelOutput$ZhatList), 2, max)
  }
  #check function arguments
  if (class(scalpelOutput)!="scalpel" & class(scalpelOutput)!="scalpelStep3") stop("The class of 'scalpelOutput' must be 'scalpel' or 'scalpelStep3'")
  if (!is.null(neuronsToDisplay)) if (length(neuronsToDisplay)==1) if (neuronsToDisplay=="kept") {
    if (!is.null(scalpelOutput$excludeReps)) fileAppend = paste0("_excluded_",paste0(sort(scalpelOutput$excludeReps), collapse = "_")) else fileAppend = ""
    keepFilename = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,
                          "/Step3_lambdaMethod_",scalpelOutput$lambdaMethod,"_lambda_",round(scalpelOutput$lambda,4),"_minClusterSize_",scalpelOutput$minClusterSize,
                          "_alpha_",scalpelOutput$alpha,"_removeBorder_",(scalpelOutput$removeBorder),fileAppend,"/Step3Data/keep.rds")
    if (!file.exists(keepFilename)) stop(paste0("When 'neuronsToDisplay' is set to 'kept', the file ", keepFilename, " must exist"))
    keep = readRDS(keepFilename)
    neuronsToDisplay = which(is.na(keep) | keep!="no")
  }
  if (is.null(neuronsToDisplay)) neuronsToDisplay = 1:nrow(Zhat)
  if (max(neuronsToDisplay)>nrow(Zhat)) stop(paste0("All elements of 'neuronsToDisplay' must be integers in 1 to ",nrow(Zhat)))
  if (!is.null(lambdaIndex)) if (lambdaIndex>length(scalpelOutput$lambdaSeq) | lambdaIndex<1) stop(paste0("Specify 'lambdaIndex' as an integer from 1 to ",length(scalpelOutput$lambdaSeq)))

  if (is.null(colVec)) {
    colVec = c("dodgerblue", "darkorchid3",  "orange", "seagreen2", "deepskyblue4", "deeppink", "darkorchid1",
                                  "firebrick2", "forestgreen",  "darkslateblue", "yellow")
    zeroedOut = which(apply(Zhat, 1, max)==0)
  } else zeroedOut = c()
  neuronsToDisplay = neuronsToDisplay[which(!(neuronsToDisplay %in% zeroedOut))]

  K = nrow(Zhat); Kplot = length(neuronsToDisplay)
  if (K>length(colVec)) colVec = rep(colVec, ceiling(K/length(colVec)))

  if (!is.null(fileName)) grDevices::pdf(fileName, width=10, height=10)
  origMar = graphics::par()$mar
  if (ylab=="") graphics::par(mar=c(5,2,3,2)) else graphics::par(mar=c(5,4,3,2))
  graphics::plot(0, type="n", ylim=c(0,Kplot+1), xlim=c(1,ncol(Zhat)), xlab="Frame", ylab=ylab, yaxt="n", main=title)
  graphics::axis(side=2, at=Kplot:1, labels=neuronsToDisplay, las=2, cex.axis=0.7)
  for (i in neuronsToDisplay) {
    vec = Zhat[i,]
    #if (maxVal!=0) vec = vec/maxVal
    if (maxVal[i]!=0) vec = vec/maxVal[i]
    if (max(vec)==0) col="gray" else col=colVec[i]
    graphics::points(1:ncol(Zhat), vec+(Kplot-which(i==neuronsToDisplay)+1)-0.5, type="l", col=col, lty=1)
  }
  if (!is.null(fileName)) grDevices::dev.off()
  graphics::par(mar=origMar)
}

#' Plot spatial components from Steps 2 or 3 of SCALPEL.
#'
#' We plot the dictionary elements obtained from Step 2 of SCALPEL, or the filtered dictionary elements from Step 3 of SCALPEL.
#'
#' @param scalpelOutput An object returned by one of these SCALPEL functions:
#' \code{\link{scalpel}}, \code{\link{scalpelStep2}}, or \code{\link{scalpelStep3}}.
#' @param neuronSet Which set of neurons should be plotted:
#' use \code{"A"} for the dictionary elements resulting from \code{\link{scalpelStep2}} and saved as \code{scalpelOutput$A},
#' or use \code{"Afilter"} for the filtered dictionary elements resulting from \code{\link{scalpelStep3}} and saved as \code{scalpelOutput$Afilter}.
#' @param neuronsToDisplay Vector giving which neurons' spatial components to plot. The indices refer to which columns to plot
#' of \code{scalpelOutput$Afilter} (if \code{neuronSet="Afilter"}), or \code{scalpelOutput$A} (if \code{neuronSet="A"}). By default, all components are plotted.
#' Users may also specify \code{"kept"}, which will exclude all dictionary elements discarded using a previous call to \code{\link{reviewNeurons}} or \code{\link{reviewNeuronsInteractive}}.
#' @param colVec Vector of colors to use, which are chosen automatically if the default value of NULL is used.
#' @param title Label for the title.
#' @param fileName If provided, the plot will be saved to the specified location.
#' @param pctTransp The percent transparency (in [0,1]) for the colors used to plot the neurons. The default value is 0.7.
#' @param number Logical value indicating whether the neurons should be numbered.
#' @param addToPlot Logical value indicating whether these neurons should be plotted to an existing plot.
#' @param border Logical value indicating whether only the borders of the neurons should be plotted.
#' @param zoom Logical value indicating whether the plot should be zoomed in to exclude any area not containing neurons.
#' @param A Optional advanced user argument: A matrix containing the spatial components to plot, where the ith column of \code{A}
#' is a vector of 1's and 0's, indicating whether each pixel is contained in the ith spatial component. By default,
#' this argument is ignored and the dictionary elements saved in \code{scalpelOutput$A} or \code{scalpelOutput$Afilter} are plotted. If \code{A} is provided,
#' \code{scalpelOutput} will be ignored and \code{neuronsToDisplay} will refer to the columns of \code{A}.
#' @param videoHeight The height of the video (in pixels). This only needs to be specified if the user is plotting \code{A}.
#'
#' @return None
#'
#' @details When \code{neuronSet="Afilter"}, spatial components corresponding to temporal components that were zeroed out in the sparse group lasso are plotted in gray, unless \code{colVec} is specified by the user.
#' @seealso \code{\link{scalpelStep2}}, \code{\link{scalpelStep3}}, \code{\link{scalpel}}, \code{\link{plotResults}}
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "scalpel" function
#'
#' #simplest example with default parameters:
#' plotSpatial(scalpelOutput = scalpelOutput, neuronSet = "Afilter")
#'
#' #example with optional parameters:
#' #plot only two of the neurons, add a title, do not number neurons,
#' #and draw the outlines of the neurons
#' plotSpatial(scalpelOutput = scalpelOutput, neuronsToDisplay = c(1,2), neuronSet = "Afilter",
#'            title = "First two neurons", number = FALSE, border = TRUE)
#' }
#' @export
plotSpatial = function(scalpelOutput=NULL, neuronSet="", neuronsToDisplay=NULL, colVec=NULL, title="", fileName=NULL, pctTransp=0.7, number=TRUE, addToPlot=FALSE, border=FALSE, zoom=FALSE, A=NULL, videoHeight=NULL){

  #check function arguments
  if (is.null(A) & is.null(scalpelOutput)) stop("Must specify either 'scalpelOutput' or 'A'")
  if (!is.null(A) & is.null(videoHeight) & is.null(scalpelOutput)) stop("Must specify 'videoHeight'")
  if (!is.null(scalpelOutput)) if (class(scalpelOutput)!="scalpel" & class(scalpelOutput)!="scalpelStep3" & class(scalpelOutput)!="scalpelStep2") stop("The class of 'scalpelOutput' must be 'scalpel', 'scalpelStep2', or 'scalpelStep3'")
  if (is.null(videoHeight)) videoHeight = scalpelOutput$videoHeight
  if (is.null(A)) {
    if (neuronSet!="A" & neuronSet!="Afilter") stop("Must specify 'A' or 'Afilter' for 'neuronSet'")
    if (class(scalpelOutput)=="scalpelStep2" & neuronSet!="A") neuronSet = "A"
  }
  if (!is.null(neuronsToDisplay)) if (length(neuronsToDisplay)==1) if (neuronsToDisplay=="kept") {
    if (neuronSet=="Afilter") {
      if (!is.null(scalpelOutput$excludeReps)) fileAppend = paste0("_excluded_",paste0(sort(scalpelOutput$excludeReps), collapse = "_")) else fileAppend = ""
      keepFilename = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,
                            "/Step3_lambdaMethod_",scalpelOutput$lambdaMethod,"_lambda_",round(scalpelOutput$lambda,4),"_minClusterSize_",scalpelOutput$minClusterSize,
                            "_alpha_",scalpelOutput$alpha,"_removeBorder_",(scalpelOutput$removeBorder),fileAppend,"/Step3Data/keep.rds")
    } else if (neuronSet=="A") {
      keepFilename = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,"/Step2Data/keep.rds")
    }
    if (!file.exists(keepFilename)) stop(paste0("When 'neuronsToDisplay' is set to 'kept', the file ", keepFilename, " must exist"))
    keep = readRDS(keepFilename)
    neuronsToDisplay = which(is.na(keep) | keep!="no")
  }
  if (is.null(colVec)) {
    colVec = c("dodgerblue", "darkorchid3",  "orange", "seagreen2", "deepskyblue4", "deeppink", "darkorchid1",
               "firebrick2", "forestgreen",  "darkslateblue", "yellow")
    if (is.null(A) & neuronSet=="Afilter") zeroedOut = which(apply(scalpelOutput$Zhat, 1, max)==0) else zeroedOut = c()
  } else zeroedOut = c()

  if (is.null(A)) {
    if (neuronSet=="Afilter") A = scalpelOutput$Afilter else A = scalpelOutput$A
  }
  if (is.null(neuronsToDisplay)) neuronsToDisplay = 1:ncol(A)
  if (max(neuronsToDisplay)>ncol(A)) stop(paste0("All elements of 'neuronsToDisplay' must be integers in 1 to ",ncol(A)))

  if (pctTransp>1 | pctTransp<0) stop("Specify 'pctTransp' in [0,1]")
  if (ncol(A)>length(colVec)) colVec = rep(colVec, ceiling(ncol(A)/length(colVec)))
  colVec[zeroedOut] = "gray"
  if (!is.logical(number)) stop("Specify TRUE or FALSE for 'number'")
  if (!is.logical(addToPlot)) stop("Specify TRUE or FALSE for 'addToPlot'")
  if (!is.logical(border)) stop("Specify TRUE or FALSE for 'border'")
  if (!is.logical(zoom)) stop("Specify TRUE or FALSE for 'zoom'")

  if (zoom==TRUE) {
    union = matrix(rowSums(A[,neuronsToDisplay,drop=FALSE]), nrow=videoHeight)
    #figure out area to zoom in on
    minCol = min(which(colSums(union)>0)); maxCol = max(which(colSums(union)>0))
    minRow = min(which(rowSums(union)>0)); maxRow = max(which(rowSums(union)>0))
    pixelsPlot = as.vector(matrix(1:nrow(A), nrow=videoHeight)[max(c(minRow-5,1)):min(c(maxRow+5,videoHeight)),
                                                                                               max(c(minCol-5,1)):min(c(maxCol+5,nrow(A)/videoHeight))])
    videoHeight = min(c(maxRow+5,videoHeight)) - max(c(minRow-5,1)) + 1
    A = A[pixelsPlot,,drop=FALSE]
  }
  if (!is.null(fileName)) grDevices::pdf(fileName, width=10, height=10)
  videoWidth = nrow(A)/videoHeight
  origMar = graphics::par()$mar
  graphics::par(mar=c(0.5,0.5,3,0.5))

  if (addToPlot==FALSE) {
    #set up plot to use
    image(z=t(matrix(rep(1, nrow(A)), nrow=videoHeight))[,videoHeight:1], zlim=c(0,1),
          axes=FALSE, col=grDevices::grey(seq(0, 1, length = 256)), main=title)
    graphics::box()
  }
  for (i in neuronsToDisplay) {
    if (!is.na(colVec[i])) {
      plotNeuronOnFrame(A=A, neuron=i, videoHeight=videoHeight, col=colVec[i], border = border, pctTransp=pctTransp)
      if (number==TRUE) {
        centroid = getCentroid(componentVec = A[,i], videoHeight=videoHeight)
        #image plot has centers at seq(0, 1), getCentroid gives values in [1,videoHeight] or [1,videoWidth]
        graphics::text(x=(centroid[1]-1)/(videoWidth-1), y=(videoHeight-centroid[2])/(videoHeight-1), labels = i, cex = 0.5)
      }
    }
  }
  if (!is.null(fileName)) grDevices::dev.off()
  graphics::par(mar=origMar)
}

#' Plot both the spatial and temporal components from Step 3 of SCALPEL.
#'
#' We plot the temporal components, displaying the estimated fluorescence over time for each spatial component, along with a map of the spatial components.
#'
#' @param scalpelOutput An object returned by one of the SCALPEL functions:
#' \code{\link{scalpel}} or \code{\link{scalpelStep3}}.
#' @param neuronsToDisplay Vector giving which neurons' spatial and temporal components to plot. The indices refer to which columns
#' of \code{scalpelOutput$Afilter} to plot. By default, all components are plotted. Users may also specify \code{"kept"},
#' which will exclude all dictionary elements discarded using a previous call to \code{\link{reviewNeurons}} or \code{\link{reviewNeuronsInteractive}}.
#' @param colVec Vector of colors to use, which are chosen automatically if the default value of NULL is used.
#' @param titleA Label for the title of the spatial components plot.
#' @param titleZ Label for the title of the temporal components plot.
#' @param ylabZ Label for the y-axis of the temporal components plot.
#' @param fileName If provided, the plot will be saved to the specified location.
#' @param pctTransp The percent transparency (in [0,1]) for the colors used to plot the neurons. The default value is 0.7.
#' @param number Logical value indicating whether the neurons should be numbered.
#' @param border Logical value indicating whether only the borders of the neurons should be plotted.
#'
#' @return None
#'
#' @details If \code{lambdaIndex} is \code{NULL}, each temporal component is scaled by its largest value. If
#' \code{lambdaIndex} is specified, each temporal component is scaled by its largest value across all of the lambda values.
#' Temporal components that were zeroed out in the sparse group lasso are omitted from the plot, and their corresponding
#' spatial components are shown in gray.
#'
#' @seealso \code{\link{scalpelStep3}}, \code{\link{scalpel}}, \code{\link{plotSpatial}}, \code{\link{plotTemporal}}
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "scalpel" function
#'
#' #simplest example with default parameters:
#' plotResults(scalpelOutput = scalpelOutput)
#'
#' #example with optional parameters:
#' #plot only two of the neurons, do not number neurons, draw the outlines of the neurons
#' plotResults(scalpelOutput = scalpelOutput, neuronsToDisplay = c(1,2),
#'            number = FALSE, border = TRUE)
#' }
#' @export
plotResults = function(scalpelOutput, neuronsToDisplay=NULL, colVec=NULL, titleA="", titleZ="", ylabZ="", fileName=NULL, pctTransp=0.7, number=TRUE, border=FALSE){

  #check function arguments

  if (class(scalpelOutput)!="scalpel" & class(scalpelOutput)!="scalpelStep3") stop("The class of 'scalpelOutput' must be 'scalpel' or 'scalpelStep3'")
  if (!is.null(neuronsToDisplay)) if (length(neuronsToDisplay)==1) if (neuronsToDisplay=="kept") {
    if (!is.null(scalpelOutput$excludeReps)) fileAppend = paste0("_excluded_",paste0(sort(scalpelOutput$excludeReps), collapse = "_")) else fileAppend = ""
    keepFilename = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,
                          "/Step3_lambdaMethod_",scalpelOutput$lambdaMethod,"_lambda_",round(scalpelOutput$lambda,4),"_minClusterSize_",scalpelOutput$minClusterSize,
                          "_alpha_",scalpelOutput$alpha,"_removeBorder_",(scalpelOutput$removeBorder),fileAppend,"/Step3Data/keep.rds")
    if (!file.exists(keepFilename)) stop(paste0("When 'neuronsToDisplay' is set to 'kept', the file ", keepFilename, " must exist"))
    keep = readRDS(keepFilename)
    neuronsToDisplay = which(is.na(keep) | keep!="no")
  }
  if (is.null(neuronsToDisplay)) neuronsToDisplay = 1:ncol(scalpelOutput$Afilter)
  if (max(neuronsToDisplay)>ncol(scalpelOutput$Afilter)) stop(paste0("All elements of 'neuronsToDisplay' must be integers in 1 to ",ncol(scalpelOutput$Afilter)))
  if (pctTransp>1 | pctTransp<0) stop("Specify 'pctTransp' in [0,1]")
  if (!is.logical(number)) stop("Specify TRUE or FALSE for 'number'")
  if (!is.logical(border)) stop("Specify TRUE or FALSE for 'border'")

  if (!is.null(fileName)) grDevices::pdf(fileName, width=15, height=8)
  graphics::par(mfrow=c(1,2))
  plotSpatial(scalpelOutput = scalpelOutput, neuronsToDisplay = neuronsToDisplay, colVec = colVec, neuronSet = "Afilter",
              title = titleA, pctTransp = pctTransp, number = number, addToPlot = FALSE, border = border)
  plotTemporal(scalpelOutput = scalpelOutput, neuronsToDisplay = neuronsToDisplay, colVec = colVec, ylab = ylabZ, title = titleZ)
  if (!is.null(fileName)) grDevices::dev.off()
  graphics::par(mfrow=c(1,1))
}

#' Plot both the spatial and temporal components for the sequence of lambda values from Step 3 of SCALPEL.
#'
#' We plot the temporal components, displaying the estimated fluorescence over time for each spatial component, along with a map of the spatial components for a whole sequence of lambda values.
#'
#' @param scalpelOutput An object returned by one of the SCALPEL functions:
#' \code{\link{scalpel}} or \code{\link{scalpelStep3}}.
#' @param neuronsToDisplay Vector giving which neurons' spatial and temporal components to plot. The indices refer to which columns
#' of \code{scalpelOutput$Afilter} to plot. By default, all components are plotted. Users may also specify \code{"kept"},
#' which will exclude all dictionary elements discarded using a previous call to \code{\link{reviewNeurons}} or \code{\link{reviewNeuronsInteractive}}.
#' @param colVec Vector of colors to use, which are chosen automatically if the default value of NULL is used.
#' @param titleA Label for the title of the spatial components plot.
#' @param ylabZ Label for the y-axis of the temporal components plot.
#' @param fileName If provided, the plot will be saved to the specified location.
#' @param pctTransp The percent transparency (in [0,1]) for the colors used to plot the neurons. The default value is 0.7.
#' @param number Logical value indicating whether the neurons should be numbered.
#' @param border Logical value indicating whether only the borders of the neurons should be plotted.
#'
#' @return None
#'
#' @seealso \code{\link{scalpelStep3}}, \code{\link{scalpel}}, \code{\link{plotSpatial}}, \code{\link{plotTemporal}}
#'
#' @details Temporal components that were zeroed out in the sparse group lasso and their corresponding spatial components
#' are shown in gray for both plots.
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "scalpel" function
#'
#' #simplest example with default parameters:
#' plotResultsAllLambda(scalpelOutput = scalpelOutput)
#'
#' #example with optional parameters:
#' #plot only two of the neurons, do not number neurons, draw the outlines of the neurons
#' plotResultsAllLambda(scalpelOutput = scalpelOutput, neuronsToDisplay = c(1,2),
#'                     number = FALSE, border = TRUE)
#' }
#' @export
plotResultsAllLambda = function(scalpelOutput, neuronsToDisplay=NULL, colVec=NULL, titleA="", ylabZ="", fileName=NULL, pctTransp=0.7, number=TRUE, border=FALSE){

  #check function arguments
  if (class(scalpelOutput)!="scalpel" & class(scalpelOutput)!="scalpelStep3") stop("The class of 'scalpelOutput' must be 'scalpel' or 'scalpelStep3'")
  if (!is.null(neuronsToDisplay)) if (length(neuronsToDisplay)==1) if (neuronsToDisplay=="kept") {
    if (!is.null(scalpelOutput$excludeReps)) fileAppend = paste0("_excluded_",paste0(sort(scalpelOutput$excludeReps), collapse = "_")) else fileAppend = ""
    keepFilename = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,
                          "/Step3_lambdaMethod_",scalpelOutput$lambdaMethod,"_lambda_",round(scalpelOutput$lambda,4),"_minClusterSize_",scalpelOutput$minClusterSize,
                          "_alpha_",scalpelOutput$alpha,"_removeBorder_",(scalpelOutput$removeBorder),fileAppend,"/Step3Data/keep.rds")
    if (!file.exists(keepFilename)) stop(paste0("When 'neuronsToDisplay' is set to 'kept', the file ", keepFilename, " must exist"))
    keep = readRDS(keepFilename)
    neuronsToDisplay = which(is.na(keep) | keep!="no")
  }
  if (is.null(neuronsToDisplay)) neuronsToDisplay = 1:ncol(scalpelOutput$Afilter)
  if (max(neuronsToDisplay)>ncol(scalpelOutput$Afilter)) stop(paste0("All elements of 'neuronsToDisplay' must be integers in 1 to ",ncol(scalpelOutput$Afilter)))
  if (pctTransp>1 | pctTransp<0) stop("Specify 'pctTransp' in [0,1]")
  if (!is.logical(number)) stop("Specify TRUE or FALSE for 'number'")
  if (!is.logical(border)) stop("Specify TRUE or FALSE for 'border'")

  if (!is.null(fileName)) grDevices::pdf(fileName, width=15, height=8)
  graphics::par(mfrow=c(1,2))
  for (lambdaIndex in seq(scalpelOutput$lambdaSeq)) {
    #set up color vector
    if (is.null(colVec)) {
      colVecSet = c("dodgerblue", "darkorchid3",  "orange", "seagreen2", "deepskyblue4", "deeppink", "darkorchid1",
                                    "firebrick2", "forestgreen",  "darkslateblue", "yellow")
      if (ncol(scalpelOutput$Afilter)>length(colVecSet)) colVecSet = rep(colVecSet, ceiling(ncol(scalpelOutput$Afilter)/length(colVecSet)))
      #make gray if neuron is zeroed out
      zeroedOut = which(apply(scalpelOutput$ZhatList[[lambdaIndex]], 1, max)==0)
      colVecSet[zeroedOut] = "gray"
    } else colVecSet = colVec

    titleZ = bquote(paste(lambda," = ",.(round(scalpelOutput$lambdaSeq[lambdaIndex],4))))
    plotSpatial(scalpelOutput = scalpelOutput, neuronsToDisplay = neuronsToDisplay, colVec = colVecSet, neuronSet = "Afilter",
                title = titleA, pctTransp = pctTransp, number = number, addToPlot = FALSE, border = border)
    plotTemporal(scalpelOutput = scalpelOutput, neuronsToDisplay = neuronsToDisplay, colVec = colVecSet, ylab = ylabZ, title = titleZ, lambdaIndex=lambdaIndex)
    if (is.null(fileName)) keepPlotting = UIinputYesNo("Enter 'Y' for next plot and 'N' to stop plotting: \n")
    if (is.null(fileName)) if (keepPlotting==FALSE) break()
  }
  if (!is.null(fileName)) grDevices::dev.off()
  graphics::par(mfrow=c(1,1))
}

#' Plot a frame of the video.
#'
#' We plot a specified frame of the raw video that we began with in Step 0 of SCALPEL,
#' or the processed video that results from Step 0 of SCALPEL.
#'
#' @param scalpelOutput An object returned by one of the SCALPEL functions:
#' \code{\link{scalpel}}, \code{\link{scalpelStep0}}, \code{\link{scalpelStep1}}, \code{\link{scalpelStep2}}, or \code{\link{scalpelStep3}}.
#' @param frame The frame to plot.
#' @param videoType Specify whether to plot the processed data from Step 0 (default; \code{videoType="processed"}) or raw data (\code{videoType="raw"}).
#' This is ignored if \code{Y} is provided.
#' @param shrinkLargest Logical value indicating whether the values above \code{shrinkCutoff} should be shrunk when plotting. Shrinking these values
#' allows us to better visualize the areas with the largest fluorescence.
#' @param shrinkCutoff The value above which pixel values will be shrunk. By default, this will be chosen as
#' \code{scalpelOutput$lowThreshold} if \code{class(scalpelOutput)=="scalpelStep0"} or \code{min(scalpelOutput$thresholdVec)} otherwise.
#' @param title Label for the title. By default, it is the frame number.
#' @param col Vector of colors to use, which by default is grayscale.
#' @param addToPlot Logical value indicating whether to add to the current plot.
#' @param Y An object of class \code{scalpelY}, which results from running the \code{\link{getY}} function. When not specified, \code{Y}
#' is automatically read in, but specifying \code{Y} is recommended when the user
#' would like to call this function many times, as this avoids reading the video into memory repeatedly.
#'
#' @return None
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
#'
#' #simplest example with default parameters:
#' plotFrame(scalpelOutput = scalpelOutput, frame = 100)
#'
#' #example with optional parameters:
#' #plot raw data instead of processed
#' plotFrame(scalpelOutput = scalpelOutput, frame = 100, videoType = "raw")
#'
#' #same plot but if you have video data read in already
#' #using 'getY' function, you can provide it
#' rawY = getY(scalpelOutput = scalpelOutput, videoType = "raw")
#' plotFrame(scalpelOutput = scalpelOutput, frame = 100, Y = rawY)
#' }
#' @export
plotFrame = function(scalpelOutput, frame, videoType="processed", shrinkLargest=FALSE, shrinkCutoff=NULL, title=NULL, col=grDevices::grey(seq(0, 1, length = 256)), addToPlot=FALSE, Y=NULL) {

  #check function arguments
  if (frame>sum(scalpelOutput$nFramesRaw)) stop(paste0("Specify 'frame' to be a number from 1 to ",sum(scalpelOutput$nFramesRaw)))
  if (!(class(scalpelOutput) %in% c("scalpel", "scalpelStep0", "scalpelStep1", "scalpelStep2", "scalpelStep3"))) stop("The class of 'scalpelOutput' must be 'scalpel', 'scalpelStep0', 'scalpelStep1', 'scalpelStep2', or 'scalpelStep3'")
  if (videoType!="raw" & videoType!="processed") stop("Specify 'raw' or 'processed' for 'videoType'")
  if (!is.logical(addToPlot)) stop("Specify TRUE or FALSE for 'addToPlot'")
  if (!is.logical(shrinkLargest)) stop("Specify TRUE or FALSE for 'shrinkLargest'")
  if (!is.null(Y)) {
    if (class(Y)!="scalpelY") stop("The class of 'Y' must be 'scalpelY'")
    videoType = Y$videoType
  }
  if (shrinkLargest==TRUE & is.null(shrinkCutoff)) {
    if (class(scalpelOutput)=="scalpelStep0") {
      shrinkCutoff = scalpelOutput$lowThreshold
    } else shrinkCutoff = min(scalpelOutput$thresholdVec)
  }

  #Default title
  if (is.null(title)) title = paste0("Frame ", frame)

  #min and max for plot
  if (videoType=="raw") {
    min = scalpelOutput$minRaw
    max = scalpelOutput$maxRaw
  } else {
    min = scalpelOutput$minDeltaf
    max = scalpelOutput$maxDeltaf
  }
  if (!is.null(shrinkCutoff)) {
    if (shrinkCutoff<min | shrinkCutoff>max) {
      warning("The specified 'shrinkCutoff' is outside the range of the data, and thus will be ignored.")
      shrinkLargest = FALSE
    }
  }
  if (is.null(Y)) {
    if (videoType=="raw") { #raw video
      #figure out which part of the video contains the frame of interest, and frame in that part to look at
      part = min(which(frame <= cumsum(scalpelOutput$nFramesRaw)))
      if (part>1) relativeFrame = frame - sum(scalpelOutput$nFramesRaw[1:(part-1)]) else relativeFrame = frame
      fileName = paste0(scalpelOutput$rawDataFolder,"Y_",part,ifelse(scalpelOutput$fileType=="R", ".rds", ifelse(scalpelOutput$fileType=="matlab", ".mat", ifelse(scalpelOutput$fileType=="text", ".txt", ".txt.gz"))))
      #read in Y matrix if not provided
      if (is.null(Y)) {
        if (scalpelOutput$fileType=="R") {
          Y = readRDS(fileName)
        } else if (scalpelOutput$fileType=="matlab") {
          data = R.matlab::readMat(fileName)
          Y = with(data, get(names(data)))
        } else {
          Y = utils::read.table(fileName)
        }
      }
    } else { #processed video
      #figure out which part of the video contains the frame of interest, and frame in that part to look at
      part = min(which(frame <= cumsum(scalpelOutput$nFramesDeltaf)))
      if (part>1) relativeFrame = frame - sum(scalpelOutput$nFramesDeltaf[1:(part-1)]) else relativeFrame = frame
      fileName = paste0(scalpelOutput$outputFolder,"Step0Data/Ydeltaf_part",part,".rds")
      #read in Y matrix if not provided
      if (is.null(Y)) Y = readRDS(fileName)
    }
  } else { #Y is already provided
    #requested frame in terms of part that is read in
    relativeFrame = match(frame, Y$currentFrames)
    if (is.na(relativeFrame)) stop("Provide 'frame' that is in 'Y$currentFrames'")
    Y = Y$Y
  }
  frameVec = Y[,relativeFrame]
  rm(Y)

  if (shrinkLargest==TRUE) {
    frameVec[frameVec>shrinkCutoff] = shrinkCutoff + 0.3 * (frameVec[frameVec>shrinkCutoff] - shrinkCutoff)
    max = shrinkCutoff + 0.3 * (max - shrinkCutoff)
  }
  frameVec = (frameVec - min) / (max - min) #between 0 and 1
  origMar = graphics::par()$mar
  graphics::par(mar=c(0.5,0.5,3,0.5))

  image(z=t(matrix(frameVec, nrow=scalpelOutput$videoHeight))[,scalpelOutput$videoHeight:1], zlim=c(0,1),
        axes=FALSE, col=col, main=title, add=addToPlot)
  if (addToPlot==FALSE) {
    graphics::box()
  }
  graphics::par(mar=origMar)
}

#' Plot a summary of the fluorescence in the video.
#'
#' We plot a heat map of the variance of each pixel across the frames.
#'
#' @param scalpelOutput An object returned by one of the SCALPEL functions:
#' \code{\link{scalpel}}, \code{\link{scalpelStep0}}, \code{\link{scalpelStep1}}, \code{\link{scalpelStep2}}, or \code{\link{scalpelStep3}}.
#' @param neuronSet This argument is ignored unless the class of \code{scalpelOutput} is \code{scalpel} or \code{scalpelStep3}, and \code{neuronsToOutline} is not \code{"none"}. It gives which set of neurons should be plotted:
#' use \code{"A"} for those resulting from \code{\link{scalpelStep2}} and saved as \code{scalpelOutput$A}, or use \code{"Afilter"} for those resulting from
#' \code{\link{scalpelStep3}} and saved as \code{scalpelOutput$Afilter}.
#' @param videoType Specify whether to plot the processed data from Step 0 (default; \code{videoType="processed"}) or raw data (\code{videoType="raw"}).
#' This is ignored if \code{Y} is provided.
#' @param neuronsToOutline Specify whether to plot outlines of all neurons (default; \code{neuronsToOutline="all"}),
#' none of the neurons (\code{neuronsToOutline="none"}), or outlines of only the neurons kept
#' using a previous call to \code{\link{reviewNeurons}} or \code{\link{reviewNeuronsInteractive}} (\code{neuronsToOutline="kept"}).
#' If \code{scalpelOutput} is not of the class \code{scalpel}, \code{scalpelStep2}, or \code{scalpelStep3}, this argument is ignored.
#' @param shrinkLargest Logical value indicating whether the values above \code{shrinkQuantile} should be shrunk when plotting. Shrinking these values
#' allows us to better visualize the areas with the highest variance fluorescence.
#' @param shrinkQuantile The quantile value above which pixel values will be shrunk. By default, this is the 95th quantile.
#' @param title Label for the title.
#' @param Y An object of class \code{scalpelY}, which results from running the \code{\link{getY}} function. When not specified, \code{Y}
#' is automatically read in, but specifying \code{Y} is recommended when the user
#' would like to call this function many times, as this avoids reading the video into memory repeatedly.
#'
#' @return None
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
#'
#' #simplest example with default parameters:
#' plotVideoVariance(scalpelOutput = scalpelOutput, neuronSet = "Afilter")
#'
#' #example with optional parameters:
#' #previous showed summary of processed data,
#' #can instead show raw data, not outline the neurons found, and add a title
#' plotVideoVariance(scalpelOutput = scalpelOutput, videoType = "raw",
#'                  neuronsToOutline = "none", title = "Raw Data")
#'
#' #if you have video data read in already using 'getY' function, you can provide it
#' rawY = getY(scalpelOutput = scalpelOutput, videoType = "raw")
#' plotVideoVariance(scalpelOutput = scalpelOutput, neuronSet = "Afilter", Y = rawY)
#' }
#' @export
plotVideoVariance = function(scalpelOutput, neuronSet="", videoType="processed", neuronsToOutline="all", shrinkLargest=FALSE, shrinkQuantile=0.95, title="", Y=NULL) {

  #check function arguments
  if (neuronsToOutline!="all" & neuronsToOutline!="kept" & neuronsToOutline!="none") stop("Specify 'all', 'kept', or 'none' for 'neuronsToOutline'")
  if (neuronsToOutline!="none" & neuronSet!="A" & neuronSet!="Afilter") {
    if (class(scalpelOutput) %in% c("scalpel", "scalpelStep3")) {
      stop("Specify 'A' or 'Afilter' for 'neuronSet'")
    } else neuronSet = "A"
  }
  if (!(class(scalpelOutput) %in% c("scalpel", "scalpelStep0", "scalpelStep1", "scalpelStep2", "scalpelStep3"))) stop("The class of 'scalpelOutput' must be 'scalpel', 'scalpelStep0', 'scalpelStep1', 'scalpelStep2', or 'scalpelStep3'")
  if (videoType!="raw" & videoType!="processed") stop("Specify 'raw' or 'processed' for 'videoType'")
  if (!is.null(Y)) {
    if (class(Y)!="scalpelY") stop("The class of 'Y' must be 'scalpelY'")
    if (!is.null(Y$part)) {
      if (length(Y$part)!=length(Y$allParts)) warning("It is recommended that you use the whole video for this function. \n To do this, run 'getY' with 'part=NULL'.")
    }
    videoType = Y$videoType
  }
  if (neuronsToOutline!="none" & (class(scalpelOutput)=="scalpelStep0" | class(scalpelOutput)=="scalpelStep1")) {
    warning("Given the class of 'scalpelOutput', the neurons will not be outlined.")
    neuronsToOutline = "none"
  }
  if (neuronsToOutline!="none" & class(scalpelOutput)=="scalpelStep2" & neuronSet=="Afilter") {
    neuronSet = "A"
  }
  if (neuronsToOutline=="kept") {
    if (neuronSet=="A") {
      keepFilename = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,"/Step2Data/keep.rds")
    } else if (neuronSet=="Afilter") {
      if (!is.null(scalpelOutput$excludeReps)) fileAppend = paste0("_excluded_",paste0(sort(scalpelOutput$excludeReps), collapse = "_")) else fileAppend = ""
      keepFilename = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,
                            "/Step3_lambdaMethod_",scalpelOutput$lambdaMethod,"_lambda_",round(scalpelOutput$lambda,4),"_minClusterSize_",scalpelOutput$minClusterSize,
                            "_alpha_",scalpelOutput$alpha,"_removeBorder_",(scalpelOutput$removeBorder),fileAppend,"/Step3Data/keep.rds")
    }
    if (!file.exists(keepFilename)) stop(paste0("When 'neuronsToOutline' is set to 'kept', the file ", keepFilename, " must exist"))
    keep = readRDS(keepFilename)
  }

  #read in Y matrix (either raw or processed) if not provided
  if (is.null(Y)) {
    if (videoType=="raw") {
      partialFileName = paste0(scalpelOutput$rawDataFolder,"Y_")
      parts = scalpelOutput$partsRaw
    } else {
      partialFileName = paste0(scalpelOutput$outputFolder,"Step0Data/Ydeltaf_part")
      parts = scalpelOutput$partsDeltaf
    }
    if (scalpelOutput$fileType=="R" | videoType=="processed") {
      Y = readRDS(paste0(partialFileName,1,".rds"))
    } else if (scalpelOutput$fileType=="text") {
      Y = utils::read.table(paste0(partialFileName,1,".txt"))
    } else if (scalpelOutput$fileType=="zippedText") {
      Y = utils::read.table(paste0(partialFileName,1,".txt.gz"))
    } else {
      data = R.matlab::readMat(paste0(partialFileName,1,".mat"))
      Y = with(data, get(names(data)))
    }
    if (length(parts)>1) {
      for (part in parts[-1]) {
        if (scalpelOutput$fileType=="R" | videoType=="processed") {
          Y = cbind(Y, readRDS(paste0(partialFileName, part, ".rds")))
        } else if (scalpelOutput$fileType=="text") {
          Y = cbind(Y, utils::read.table(paste0(partialFileName, part, ".txt")))
        } else if (scalpelOutput$fileType=="zippedText") {
          Y = cbind(Y, utils::read.table(paste0(partialFileName, part, ".txt.gz")))
        } else {
          data = R.matlab::readMat(paste0(partialFileName, part, ".mat"))
          Y = cbind(Y, with(data, get(names(data))))
        }
      }
    }
  } else Y = Y$Y
  frameVec = apply(Y, 1, stats::var)
  rm(Y)
  origMar = graphics::par()$mar
  graphics::par(mar=c(0.5,0.5,3,0.5))
  if (shrinkLargest==TRUE) {
    cut = stats::quantile(frameVec, p=shrinkQuantile)
    frameVec[frameVec>cut] = cut + 0.3 * (frameVec[frameVec>cut] - cut)
  }
  frameVec = (frameVec - min(frameVec)) / (max(frameVec) - min(frameVec)) #between 0 and 1
  image(z=t(matrix(frameVec, nrow=scalpelOutput$videoHeight))[,scalpelOutput$videoHeight:1], zlim=c(0,1),
        axes=FALSE, col=grDevices::grey(seq(0, 1, length = 256)), main=title, add=FALSE)
  if (neuronsToOutline=="all") {
    plotSpatial(scalpelOutput = scalpelOutput, neuronSet = neuronSet, addToPlot = TRUE, pctTransp = 0.5, border = TRUE, number = FALSE)
  } else if (neuronsToOutline=="kept") {
    plotSpatial(scalpelOutput = scalpelOutput, neuronSet = neuronSet, neuronsToDisplay = which(is.na(keep) | keep!="no"), addToPlot = TRUE, pctTransp = 0.5, border = TRUE, number = FALSE)
  }
  graphics::box()
  graphics::par(mar=origMar)
}

#' Plot the most active frames for a given neuron.
#'
#' For a given neuron, we plot the frames with the highest estimated fluorescence, which
#' results from fitting the sparse group lasso in Step 3 of SCALPEL.
#'
#' @param scalpelOutput An object returned by one of the SCALPEL functions:
#' \code{\link{scalpel}} or \code{\link{scalpelStep3}}.
#' @param AfilterIndex Scalar giving the neuron for which to plot the brightest frames. The index refers to which column
#' of \code{scalpelOutput$Afilter} is of interest.
#' @param videoType Specify whether to plot the processed data from Step 0 (default; \code{videoType="processed"}) or raw data (\code{videoType="raw"}).
#' This is ignored if \code{Y} is provided.
#' @param neuronsToOutline Specify whether to plot outlines of all neurons (default; \code{neuronsToOutline="all"}),
#' only the outline for \code{neuron} (\code{neuronsToOutline="main"}), outlines of only the neurons kept
#' using a previous call to \code{\link{reviewNeurons}} or \code{\link{reviewNeuronsInteractive}} (\code{neuronsToOutline="kept"}),
#'  or none (\code{neuronsToOutline="none"}).
#' @param brightIndex Scalar giving which of the ordered brightest frames to plot. The default is 1, i.e., the brightest frame.
#' @param shrinkLargest Logical value indicating whether the values above \code{shrinkCutoff} should be shrunk when plotting. Shrinking these values
#' allows us to better visualize the areas with the largest fluorescence.
#' @param shrinkCutoff The value above which pixel values will be shrunk. By default, this will be chosen as
#' \code{min(scalpelOutput$thresholdVec)}.
#' @param title Label for the title. The default is frame number.
#' @param Y An object of class \code{scalpelY}, which results from running the \code{\link{getY}} function. When not specified, \code{Y}
#' is automatically read in, but specifying \code{Y} is recommended when the user
#' would like to call this function many times, as this avoids reading the video into memory repeatedly.
#'
#' @return None
#'
#' @seealso \code{\link{scalpel}}, \code{\link{scalpelStep3}}
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "scalpel" function
#' #simplest example with default parameters:
#' plotBrightest(scalpelOutput = scalpelOutput, AfilterIndex = 2)
#'
#' #example with optional parameters:
#' #only outline neuron corresponding to frame, plot 5th brightest with raw data
#' plotBrightest(scalpelOutput = scalpelOutput, AfilterIndex = 2, videoType = "raw",
#'              neuronsToOutline = "main", brightIndex = 5)
#'
#' #same plot but if you have video data read in already
#' #using 'getY' function, you can provide it
#' rawY = getY(scalpelOutput = scalpelOutput, videoType = "raw")
#' plotBrightest(scalpelOutput = scalpelOutput, AfilterIndex = 2, Y = rawY,
#'              neuronsToOutline = "main", brightIndex = 5)
#' }
#' @export
plotBrightest = function(scalpelOutput, AfilterIndex, videoType="processed", neuronsToOutline="all", brightIndex=1, shrinkLargest=FALSE, shrinkCutoff=NULL, title=NULL, Y=NULL) {

  #check function arguments
  if (class(scalpelOutput)!="scalpel" & class(scalpelOutput)!="scalpelStep3") stop("The class of 'scalpelOutput' must be 'scalpel' or 'scalpelStep3'")
  if (videoType!="raw" & videoType!="processed") stop("Specify 'raw' or 'processed' for 'videoType'")
  if (AfilterIndex>ncol(scalpelOutput$Afilter)) stop(paste0("'AfilterIndex' must be an integer in 1 to ",ncol(scalpelOutput$Afilter)))
  if (!is.null(Y)) {
    if (class(Y)!="scalpelY") stop("The class of 'Y' must be 'scalpelY'")
    videoType = Y$videoType
  }
  if (neuronsToOutline!="main" & neuronsToOutline!="all" & neuronsToOutline!="none" & neuronsToOutline!="kept") stop("Specify 'all', 'kept', 'main', or 'none' for 'neuronsToOutline'")
  if (neuronsToOutline=="kept") {
    if (!is.null(scalpelOutput$excludeReps)) fileAppend = paste0("_excluded_",paste0(sort(scalpelOutput$excludeReps), collapse = "_")) else fileAppend = ""
    keepFilename = paste0(scalpelOutput$outputFolder,"Step1_",scalpelOutput$version,"/Step2_omega_",scalpelOutput$omega,"_cutoff_",scalpelOutput$cutoff,
                          "/Step3_lambdaMethod_",scalpelOutput$lambdaMethod,"_lambda_",round(scalpelOutput$lambda,4),"_minClusterSize_",scalpelOutput$minClusterSize,
                          "_alpha_",scalpelOutput$alpha,"_removeBorder_",(scalpelOutput$removeBorder),fileAppend,"/Step3Data/keep.rds")
    if (!file.exists(keepFilename)) stop(paste0("When 'neuronsToOutline' is set to 'kept', the file ", keepFilename, " must exist"))
    keep = readRDS(keepFilename)
  }

  #find the brightest frames
  Z = scalpelOutput$Zhat
  brightest = order(Z[AfilterIndex,], decreasing = TRUE)
  nBright = sum(Z[AfilterIndex,]>0)
  if (nBright==0) stop("The selected 'AfilterIndex' was estimated to be non-active when fitting the sparse group lasso. Choose a different 'AfilterIndex'")
  if (brightIndex>nBright) stop(paste0("There are only ",nBright," non-zero frames. Choose a smaller 'brightIndex'."))
  frame = brightest[brightIndex]
  if (is.null(title)) title = paste0("Frame ",frame)
  #plot the indicated brightest frame
  plotFrame(scalpelOutput = scalpelOutput, frame = frame, videoType = videoType, title = title, Y = Y, shrinkLargest=shrinkLargest, shrinkCutoff=shrinkCutoff)
  if (neuronsToOutline=="all") {
    colVec = rep("dodgerblue", ncol(scalpelOutput$Afilter))
    colVec[AfilterIndex] = "orange"
    plotSpatial(scalpelOutput = scalpelOutput, neuronSet = "Afilter", addToPlot = TRUE, colVec = colVec, pctTransp = 0.5, border = TRUE, number = FALSE)
  } else if (neuronsToOutline=="kept") {
    colVec = rep("dodgerblue", ncol(scalpelOutput$Afilter))
    colVec[AfilterIndex] = "orange"
    plotSpatial(scalpelOutput = scalpelOutput, neuronSet = "Afilter", neuronsToDisplay = which(keep!="no" | is.na(keep)), addToPlot = TRUE, colVec = colVec, pctTransp = 0.5, border = TRUE, number = FALSE)
    if (!is.na(keep[AfilterIndex])) if (keep[AfilterIndex]=="no") plotSpatial(scalpelOutput = scalpelOutput, neuronSet = "Afilter", neuronsToDisplay = AfilterIndex, addToPlot = TRUE, colVec = "orange", pctTransp = 0.5, border = TRUE, number = FALSE)
  } else if (neuronsToOutline=="main") {
    plotSpatial(scalpelOutput = scalpelOutput, neuronSet = "Afilter", neuronsToDisplay = AfilterIndex, addToPlot = TRUE, colVec = "orange", pctTransp = 0.5, border = TRUE, number = FALSE)
  }
}

#' Plot a frame of the video with shading.
#'
#' We plot a specified frame of the processed video, which results from Step 0 of SCALPEL,
#' with shading to indicate values above a specified threshold.
#'
#' @param scalpelOutput An object returned by one of the SCALPEL functions:
#' \code{\link{scalpel}}, \code{\link{scalpelStep0}}, \code{\link{scalpelStep1}}, \code{\link{scalpelStep2}}, or \code{\link{scalpelStep3}}.
#' @param frame The frame to plot.
#' @param threshold Value above which pixels will be shaded.
#' @param shrinkLargest Logical value indicating whether the values above \code{shrinkCutoff} should be shrunk when plotting. Shrinking these values
#' allows us to better visualize the areas with the largest fluorescence.
#' @param shrinkCutoff The value above which pixel values will be shrunk. By default, this will be chosen as
#' \code{scalpelOutput$lowThreshold} if \code{class(scalpelOutput)=="scalpelStep0"} or \code{min(scalpelOutput$thresholdVec)} otherwise.
#' @param title Label for the title. By default, it gives the threshold value.
#' @param col Color of shading to use, which is yellow by default.
#' @param Y An object of class \code{scalpelY}, which results from running the \code{\link{getY}} function. When not specified, \code{Y}
#' is automatically read in, but specifying \code{Y} is recommended when the user
#' would like to call this function many times, as this avoids reading the video into memory repeatedly.
#'
#' @return None
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
#'
#' #simplest example with default parameters:
#' plotThresholdedFrame(scalpelOutput = scalpelOutput, frame = 100,
#'                     threshold = scalpelOutput$thresholdVec[1])
#'
#' #example with optional parameters:
#' #change shading to purple and add a title
#' plotThresholdedFrame(scalpelOutput = scalpelOutput, frame = 100, col = "purple",
#'                    threshold = scalpelOutput$thresholdVec[2])
#'
#' #if you have video data read in already using 'getY' function, you can provide it
#' processedY = getY(scalpelOutput = scalpelOutput, videoType = "processed")
#' plotThresholdedFrame(scalpelOutput = scalpelOutput, frame = 100,
#'                     threshold = scalpelOutput$thresholdVec[1], Y = processedY)
#' }
#' @export
plotThresholdedFrame = function(scalpelOutput, frame, threshold, shrinkLargest=FALSE, shrinkCutoff=NULL, title=NULL, col="yellow", Y=NULL) {

  #check function arguments
  if (frame>sum(scalpelOutput$nFramesRaw)) stop(paste0("Specify 'frame' to be a number from 1 to ",sum(scalpelOutput$nFramesRaw)))
  if (!(class(scalpelOutput) %in% c("scalpel", "scalpelStep0", "scalpelStep1", "scalpelStep2", "scalpelStep3"))) stop("The class of 'scalpelOutput' must be 'scalpel', 'scalpelStep0', 'scalpelStep1', 'scalpelStep2', or 'scalpelStep3'")
  if (is.null(title)) title = paste0("Threshold = ",round(threshold,3))
  if (!is.null(Y)) {
    if (class(Y)!="scalpelY") stop("The class of 'Y' must be 'scalpelY'")
    if (Y$videoType=="raw") stop("Specify 'Y' with 'Y$videoType' of 'processed'")
  }
  if (shrinkLargest==TRUE & is.null(shrinkCutoff)) {
    if (class(scalpelOutput)=="scalpelStep0") {
      shrinkCutoff = scalpelOutput$lowThreshold
    } else shrinkCutoff = min(scalpelOutput$thresholdVec)
  }

  videoType = "processed"
  if (videoType=="raw") {
    min = scalpelOutput$minRaw
    max = scalpelOutput$maxRaw
  } else {
    min = scalpelOutput$minDeltaf
    max = scalpelOutput$maxDeltaf
  }
  if (!is.null(shrinkCutoff)) {
    if (shrinkCutoff<min | shrinkCutoff>max) {
      warning("The specified 'shrinkCutoff' is outside the range of the data, and thus will be ignored.")
      shrinkLargest = FALSE
    }
  }
  if (is.null(Y)) {
    if (videoType=="raw") {
      #figure out which part of the video contains the frame of interest, and frame in that part to look at
      part = min(which(frame <= cumsum(scalpelOutput$nFramesRaw)))
      if (part>1) relativeFrame = frame - sum(scalpelOutput$nFramesRaw[1:(part-1)]) else relativeFrame = frame
      fileName = paste0(scalpelOutput$rawDataFolder,"Y_",part,ifelse(scalpelOutput$fileType=="R", ".rds", ifelse(scalpelOutput$fileType=="matlab", ".mat", ifelse(scalpelOutput$fileType=="text", ".txt", ".txt.gz"))))
      #read in Y matrix if not provided
      if (is.null(Y)) {
        if (scalpelOutput$fileType=="R") {
          Y = readRDS(fileName)
        } else if (scalpelOutput$fileType=="matlab") {
          data = R.matlab::readMat(fileName)
          Y = with(data, get(names(data)))
        } else {
          Y = utils::read.table(fileName)
        }
      }
    } else {
      #figure out which part of the video contains the frame of interest, and frame in that part to look at
      part = min(which(frame <= cumsum(scalpelOutput$nFramesDeltaf)))
      if (part>1) relativeFrame = frame - sum(scalpelOutput$nFramesDeltaf[1:(part-1)]) else relativeFrame = frame
      fileName = paste0(scalpelOutput$outputFolder,"Step0Data/Ydeltaf_part",part,".rds")
      #read in Y matrix if not provided
      if (is.null(Y)) Y = readRDS(fileName)
    }
  } else { #Y is already provided
    #requested frame in terms of part that is read in
    relativeFrame = match(frame, Y$currentFrames)
    if (is.na(relativeFrame)) stop("Provide 'frame' that is in 'Y$currentFrames'")
    Y = Y$Y
  }
  frameVec = Y[,relativeFrame]
  rm(Y)

  thresY = (frameVec>=threshold)
  thresY[thresY==0] = NA

  if (shrinkLargest==TRUE) {
    frameVec[frameVec>shrinkCutoff] = shrinkCutoff + 0.3 * (frameVec[frameVec>shrinkCutoff] - shrinkCutoff)
    max = shrinkCutoff + 0.3 * (max - shrinkCutoff)
  }
  frameVec = (frameVec - min) / (max - min) #between 0 and 1
  origMar = graphics::par()$mar
  graphics::par(mar=c(0.5,0.5,3,0.5))
  image(z=t(matrix(frameVec, nrow=scalpelOutput$videoHeight))[,scalpelOutput$videoHeight:1], zlim=c(0,1),
        axes=FALSE, col=grDevices::grey(seq(0, 1, length = 256)), main=title, add=FALSE)
  image(z=t(matrix(thresY, nrow=scalpelOutput$videoHeight))[,scalpelOutput$videoHeight:1], zlim=c(0,1),
        axes=FALSE, col=c("white",transp(col, 0.3)), main=title, add=TRUE)
  graphics::box()
  graphics::par(mar=origMar)
}

#' Plot preliminary dictionary element from Step 1 of SCALPEL and its corresponding frame.
#'
#' We plot the specified preliminary dictionary element, along with the frame of Y from which the component was derived in Step 1 of SCALPEL.
#'
#' @param scalpelOutput An object returned by one of the SCALPEL functions:
#' \code{\link{scalpel}}, \code{\link{scalpelStep1}}, \code{\link{scalpelStep2}}, or \code{\link{scalpelStep3}}.
#' @param AzeroIndex The preliminary dictionary element of interest. The index refers to the column of \code{scalpelOutput$Azero}.
#' Specify only one of the following: \code{AzeroIndex}, \code{AIndex}, or \code{AfilterIndex}.
#' @param AIndex The dictionary element (i.e., cluster) of interest. The index refers to the column of \code{scalpelOutput$A}.
#' Note that the class of \code{scalpelOutput} must be \code{scalpel}, \code{scalpelStep2}, or \code{scalpelStep3} if specifying \code{AIndex}, and \code{member} must also be specified.
#' Specify only one of the following: \code{AzeroIndex}, \code{AIndex}, or \code{AfilterIndex}.
#' @param AfilterIndex The sparse group lasso component of interest. The index refers to the column of \code{scalpelOutput$Afilter}.
#' Note that the
#' class of \code{scalpelOutput} must be \code{scalpel} or \code{scalpelStep3} if specifying \code{AfilterIndex}, and \code{member} must also be specified.
#' Specify only one of the following: \code{AzeroIndex}, \code{AIndex}, or \code{AfilterIndex}.
#' @param member Which member of the cluster corresponding to \code{AIndex} or \code{AfilterIndex} to plot. Ignored if \code{AzeroIndex} is specified.
#' @param videoType Specify whether to plot the processed data from Step 0 (default; \code{videoType="processed"}) or raw data (\code{videoType="raw"}).
#' This is ignored if \code{Y} is provided.
#' @param shrinkLargest Logical value indicating whether the values above \code{shrinkCutoff} should be shrunk when plotting. Shrinking these values
#' allows us to better visualize the areas with the largest fluorescence.
#' @param shrinkCutoff The value above which pixel values will be shrunk. By default, this will be chosen as
#' \code{min(scalpelOutput$thresholdVec)}.
#' @param Y An object of class \code{scalpelY}, which results from running the \code{\link{getY}} function. When not specified, \code{Y}
#' is automatically read in, but specifying \code{Y} is recommended when the user
#' would like to call this function many times, as this avoids reading the video into memory repeatedly.
#'
#' @return None
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
#' #simplest example with default parameters:
#' plotCandidateFrame(scalpelOutput = scalpelOutput, AzeroIndex = 10)
#'
#' #example with optional parameters:
#' #plot raw data instead of processed
#' plotCandidateFrame(scalpelOutput = scalpelOutput, AzeroIndex = 10, videoType = "raw")
#'
#' #same plot but if you have video data read in already
#' #using 'getY' function, you can provide it
#' rawY = getY(scalpelOutput = scalpelOutput, videoType = "raw")
#' plotCandidateFrame(scalpelOutput = scalpelOutput, AzeroIndex = 10, Y = rawY)
#' }
#' @export
plotCandidateFrame = function(scalpelOutput, AzeroIndex=NULL, AIndex=NULL, AfilterIndex=NULL, member=NULL, videoType="processed", shrinkLargest=FALSE, shrinkCutoff=NULL, Y=NULL) {

  #check function arguments
  if (!(class(scalpelOutput) %in% c("scalpel", "scalpelStep1", "scalpelStep2", "scalpelStep3"))) stop("The class of 'scalpelOutput' must be 'scalpel', 'scalpelStep1', 'scalpelStep2', or 'scalpelStep3'")
  if ((is.null(AzeroIndex)+is.null(AIndex)+is.null(AfilterIndex))!=2) stop("Specify only one of the following: 'AzeroIndex', 'AIndex', or 'AfilterIndex'")
  if (!is.null(AIndex) & class(scalpelOutput)=="scalpelStep1") stop("When 'AIndex' is specified, the class of 'scalpelOutput' must be 'scalpel', 'scalpelStep2', or 'scalpelStep3'")
  if (!is.null(AfilterIndex) & (class(scalpelOutput)=="scalpelStep1" | class(scalpelOutput)=="scalpelStep2")) stop("When 'AfilterIndex' is specified, the class of 'scalpelOutput' must be 'scalpel' or 'scalpelStep3'")

  if (is.null(AzeroIndex) & is.null(member)) stop("When 'AIndex' or 'AfilterIndex' is specified, 'member' must also be specified'")
  if (!is.null(Y)) {
    if (class(Y)!="scalpelY") stop("The class of 'Y' must be 'scalpelY'")
    videoType = Y$videoType
  }

  #figure out candidate based on inputs
  if (!is.null(AzeroIndex)) {
    candidate = AzeroIndex
    if (candidate > ncol(scalpelOutput$Azero)) stop(paste0("Specify 'AzeroIndex' to be an integer in 1 to ", ncol(scalpelOutput$Azero)))
  } else if (!is.null(AIndex)) {
    numMembers = length(which(scalpelOutput$clusterID==AIndex))
    if (AIndex > ncol(scalpelOutput$A)) stop(paste0("Specify 'AIndex' to be an integer in 1 to ", ncol(scalpelOutput$A)))
    if (member > numMembers) stop(paste0("Specify 'member' to be an integer in 1 to ", numMembers))
    candidate = which(scalpelOutput$clusterID==AIndex)[member]
    repMember = match(scalpelOutput$repComps[AIndex], which(scalpelOutput$clusterID==AIndex))
    message("There ", numMembers, " members in the requested cluster. Member ", repMember, " is the representative for the cluster.")
  } else {
    numMembers = length(which(scalpelOutput$clusterID==scalpelOutput$clustersUse[AfilterIndex]))
    if (AfilterIndex > ncol(scalpelOutput$Afilter)) stop(paste0("Specify 'AfilterIndex' to be an integer in 1 to ", ncol(scalpelOutput$Afilter)))
    if (member > numMembers) stop(paste0("Specify 'member' to be an integer in 1 to ", numMembers))
    candidate = which(scalpelOutput$clusterID==scalpelOutput$clustersUse[AfilterIndex])[member]
    repMember = match(scalpelOutput$repComps[scalpelOutput$clustersUse[AfilterIndex]], which(scalpelOutput$clusterID==scalpelOutput$clustersUse[AfilterIndex]))
    message("There ", numMembers, " members in the requested cluster. Member ", repMember, " is the representative for the cluster.")
  }

  graphics::par(mfrow=c(1,2))
  frame = scalpelOutput$AzeroFrames[candidate]
  #plot preliminary dictionary element
  plotSpatial(A = scalpelOutput$Azero, videoHeight = scalpelOutput$videoHeight, neuronsToDisplay = candidate, title = paste0("Prelim. dictionary\nelement ",candidate), number = FALSE)
  #plot frame from which preliminary dictionary element was derived
  plotFrame(scalpelOutput = scalpelOutput, frame = frame, videoType = videoType, title = paste0("Frame ",frame), Y = Y, shrinkLargest=shrinkLargest, shrinkCutoff=shrinkCutoff)
  graphics::par(mfrow=c(1,1))
}

#' Plot a summary of a given cluster from Step 2 of SCALPEL.
#'
#' We plot the preliminary dictionary elements that correspond to a given dictionary element, derived during Step 2 of SCALPEL, or a
#' given component included in the sparse group lasso of Step 3.
#'
#' @param scalpelOutput An object returned by one of the SCALPEL functions:
#' \code{\link{scalpel}}, \code{\link{scalpelStep2}}, or \code{\link{scalpelStep3}}.
#' @param AIndex The dictionary element (i.e., cluster) of interest. The index refers to the column of \code{scalpelOutput$A}, which is part
#' of the output from \code{\link{scalpelStep2}}. Specify \code{AIndex} or \code{AfilterIndex}, not both.
#' @param AfilterIndex The refined dictionary element of interest. The index refers to the column of \code{scalpelOutput$Afilter}, which is part
#' of the output from \code{\link{scalpelStep3}}. Note that the
#' class of \code{scalpelOutput} must be \code{scalpel} or \code{scalpelStep3} if specifying \code{AfilterIndex}. Specify \code{AIndex} or \code{AfilterIndex}, not both.
#' @param pctTransp The percent transparency (in [0,1]) for the colors used to plot the preliminary dictionary elements. The default value is 0.01.
#'
#' @return None
#'
#' @seealso \code{\link{scalpelStep2}}, \code{\link{scalpel}}
#'
#' @details The left plot shows the dictionary element of interest in orange, with the other dictionary elements
#'  shown in blue. The middle plot shows all of the preliminary dictionary elements corresponding to the
#' dictionary element plotted transparently. The right plot shows the dictionary element in orange, along with
#' the union of all of the preliminary dictionary elements in gray. Note that the plots in the middle and on the right are
#' zoomed-in, compared to the plot on the left that shows the entire field of view for the video.
#'
#' @examples
#' \dontrun{
#' ### many of the functions in this package are interconnected so the
#' ### easiest way to learn to use the package is by working through the vignette,
#' ### which is available at ajpete.com/software
#'
#' #assumes you have run the example for the "scalpel" function
#' #plots the cluster for the 2nd dictionary element (i.e., scalpelOutput$A[,2])
#' plotCluster(scalpelOutput = scalpelOutput, AIndex = 2)
#' #plots the cluster for the 2nd component included in SGL (i.e., scalpelOutput$Afilter[,2])
#' plotCluster(scalpelOutput = scalpelOutput, AfilterIndex = 2)
#' }
#' @export
plotCluster = function(scalpelOutput, AIndex=NULL, AfilterIndex=NULL, pctTransp=0.01) {

  #check function arguments
  if (!(class(scalpelOutput) %in% c("scalpel", "scalpelStep2", "scalpelStep3"))) stop("The class of 'scalpelOutput' must be 'scalpel', 'scalpelStep2', or 'scalpelStep3'")
  if (is.null(AIndex) & is.null(AfilterIndex)) stop("Must specify 'AIndex' or 'AfilterIndex'")
  if (!is.null(AIndex) & !is.null(AfilterIndex)) stop("Only specify 'AIndex' or 'AfilterIndex', not both")
  if (!is.null(AIndex)) {
    if (AIndex>ncol(scalpelOutput$A)) stop(paste0("'AIndex' must be an integer in 1 to ", ncol(scalpelOutput$A)))
    A = scalpelOutput$A
    clusterToPlot = AIndex
  } else {
    if (class(scalpelOutput)=="scalpelStep2") stop("The class of 'scalpelOutput' must be 'scalpel' or 'scalpelStep3' when 'AfilterIndex' is used")
    if (AfilterIndex>ncol(scalpelOutput$Afilter)) stop(paste0("'AfilterIndex' must be an integer in 1 to ", ncol(scalpelOutput$Afilter)))
    A = scalpelOutput$Afilter
    AIndex = AfilterIndex
    clusterToPlot = scalpelOutput$clustersUse[AfilterIndex]
  }

  graphics::par(mfrow=c(1,3))
  #plot spatial map highlighting dictionary element of interest
  colVec = rep("dodgerblue", ncol(A))
  colVec[AIndex] = "orange"
  plotSpatial(A = A, videoHeight = scalpelOutput$videoHeight, colVec = colVec, number = FALSE, title = paste0("Cluster ",clusterToPlot))
  #plot all preliminary dictionary elements in cluster of interest
  union = matrix(rowSums(scalpelOutput$Azero[,which(scalpelOutput$clusterID==clusterToPlot),drop=FALSE]), nrow=scalpelOutput$videoHeight)
  #figure out area to zoom in on
  minCol = min(which(colSums(union)>0))
  maxCol = max(which(colSums(union)>0))
  minRow = min(which(rowSums(union)>0))
  maxRow = max(which(rowSums(union)>0))
  pixelsPlot = as.vector(matrix(1:nrow(scalpelOutput$Azero), nrow=scalpelOutput$videoHeight)[max(c(minRow-5,1)):min(c(maxRow+5,scalpelOutput$videoHeight)),
                                                                 max(c(minCol-5,1)):min(c(maxCol+5,nrow(scalpelOutput$A)/scalpelOutput$videoHeight))])
  videoHeightZoom = min(c(maxRow+5,scalpelOutput$videoHeight)) - max(c(minRow-5,1)) + 1
  plotSpatial(A = scalpelOutput$Azero[pixelsPlot,which(scalpelOutput$clusterID==clusterToPlot),drop=FALSE], videoHeight = videoHeightZoom, number = FALSE, pctTransp = pctTransp, title = paste0(length(which(scalpelOutput$clusterID==clusterToPlot)), " prelim. dictionary elements"))
  #plot dictionary element vs. union
  plotSpatial(A = matrix(as.vector(union>0)[pixelsPlot], ncol=1), videoHeight = videoHeightZoom, colVec = "lightgray", number = FALSE, title = "Union of prelim. dictionary\n elements vs. representative")
  plotSpatial(A = A[pixelsPlot,], videoHeight = videoHeightZoom, neuronsToDisplay = AIndex, colVec = "orange", number = FALSE, addToPlot = TRUE)
  graphics::par(mfrow=c(1,1))
}

#HELPER FUNCTIONS (not exported)

#calculate the x-y position of the centroid for a spatial component
getCentroid = function(componentVec, videoHeight) {
  mat = matrix(componentVec, nrow=videoHeight)
  xval = sum(1:ncol(mat)*colSums(mat))/sum(colSums(mat))
  yval = sum(1:nrow(mat)*rowSums(mat))/sum(rowSums(mat))
  return(c(xval,yval))
}

#function for plotting transparent color
transp = function(color, pctTransp=0.7) {
  rgbNum = grDevices::col2rgb(color)
  col = grDevices::rgb(rgbNum[1], rgbNum[2], rgbNum[3], alpha=pctTransp*256, max=256)
  return(col)
}

#helper function for plotting border of neuron
getBorderMat = function(mat) {
  possible = which(mat>0)
  videoHeight = nrow(mat); videoWidth = ncol(mat)
  bordermat = matrix(0, nrow(mat), ncol(mat))
  for (index in possible) {
    if (index==videoHeight*(videoWidth-1)+1) { #top right corner
      neighbors = c(index+1, index-videoHeight)
    } else if (index==videoHeight*videoWidth) { #bottom right corner
      neighbors = c(index-1, index-videoHeight)
    } else if (index==1) { #top left corner
      neighbors = c(index+1, index+videoHeight)
    } else if (index==videoHeight) { #bottom left corner
      neighbors = c(index-1, index+videoHeight)
    } else if (index %% videoHeight==1) { #border pixel on top
        neighbors = c(index+1, index-videoHeight, index+videoHeight)
    } else if (index %% videoHeight==0) { #border pixel on bottom
      neighbors = c(index-1, index-videoHeight, index+videoHeight)
    } else if (index < videoHeight) { #border pixel on left
      neighbors = c(index-1, index+1, index+videoHeight)
    } else if (index > videoHeight*(videoWidth-1)) { #border pixel on right
      neighbors = c(index-1, index+1, index-videoHeight)
    } else { #interior pixel
      neighbors = c(index-1, index+1, index-videoHeight, index+videoHeight)
    }
    if (min(mat[neighbors])==0) bordermat[index] = 1
  }
  return(bordermat)
}

#plots specified neuron on existing plot
#only the border of the neuron if border==TRUE
plotNeuronOnFrame = function(A, neuron, videoHeight, col, pctTransp=1, border=TRUE) {
  if (border==TRUE) {
    toPlot = as.vector(getBorderMat(mat = matrix(A[,neuron], nrow = videoHeight)))
  } else toPlot = (A[,neuron]==1)
  toPlot[which(toPlot==0)] = NA
  col = transp(col, pctTransp)
  image(z=t(matrix(toPlot, nrow=videoHeight))[,videoHeight:1], zlim=c(0,1),
        axes=FALSE, col=c("white",col), add=TRUE)
}

