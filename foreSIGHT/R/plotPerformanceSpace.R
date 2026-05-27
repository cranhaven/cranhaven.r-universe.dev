getAttXY <- function(attPerturb, attX, attY) {
  if (length(attPerturb) < 2) stop(paste0("sim should contain two or more perturbed attributes to plot a performance space."))

  if (is.null(attX) & is.null(attY)) {
    attX <- attPerturb[1]
    attY <- attPerturb[2]
    message(paste0("Using attX = ", attX, ", and attY = ", attY, ". Specify attX and attY in the function call to choose alternate attributes."))
  } else {
    attInd <- which(attPerturb %in% c(attX, attY))
    if (!identical(attInd, integer(0))) {
      attPerturb <- attPerturb[-attInd]
    }
    if (is.null(attX)) {
      attX <- attPerturb[1]
      message(paste0("Using attX = ", attX, ". Specify attX in the function call to choose an alternate attribute."))
    } else {
      attY <- attPerturb[1]
      message(paste0("Using attY = ", attY, ". Specify attY in the function call to choose an alternate attribute."))
    }
  }

  return(list(
    attX = attX,
    attY = attY
  ))
}




# returns the sum of fitness as a matrix
getSimFitness <- function(sim) {
  # unpacking sim metadata
  repNames <- names(sim[grep("Rep", names(sim))])
  if (is.null(repNames)) stop("There are no replicates in sim.")
  tarNames <- names(sim[[repNames[1]]])

  nRep <- length(repNames)
  nTar <- length(tarNames)

  # get the fitness value
  fitName <- "score"
  simFitness <- matrix(NA, nrow = length(tarNames), ncol = length(repNames))
  for (r in 1:nRep) {
    simFitness[, r] <- sapply(sim[[repNames[r]]], function(x) {
      sum(x[["score"]])
    })
  }
  return(simFitness)
}


getPerfStat <- function(performance,
                        simFitness,
                        topReps,
                        nRep,
                        statFUN = mean, ...) {
  nTar <- nrow(performance)
  # rank in order of fitness (take top X)
  # this ordering is done for each target
  if (!is.null(topReps)) {
    if (topReps < nRep) {
      sortedPerf <- matrix(0, nrow = dim(performance)[1], ncol = dim(performance)[2])
      for (i in 1:nTar) {
        ind <- order(simFitness[i, ], decreasing = TRUE) # make the best performance 1st
        sortedPerf[i, ] <- performance[i, ind]
      }
      # performance as the average of nReps (in terms of scenario fit) of the set (USER INPUT)
      performanceStat <- as.data.frame(apply(sortedPerf[, 1:topReps], 1, FUN = statFUN, ...))
    } else {
      performanceStat <- as.data.frame(apply(performance, 1, FUN = statFUN, ...))
    }
  } else {
    performanceStat <- as.data.frame(apply(performance, 1, FUN = statFUN, ...))
  }
  return(performanceStat)
}

checkAttPert <- function(targetMat, attX, attY) {
  if (attX == attY) {
    stop("attY should be different from attX.")
  }

  # Check attX and attY
  attNames <- colnames(targetMat)
  colAttX <- which(attNames == attX)
  colAttY <- which(attNames == attY)

  if (length(colAttX) == 0 | colAttY == 0) {
    stop("attX or attY is not available in sim.")
  }

  # tarAttX <- targetMat[ ,colAttX]
  # tarAttY <- targetMat[ ,colAttY]

  # May not need two attributes - can plot a single tile using just one sample?
  # if (length(unique(tarAttX)) < 2) stop("Perturbation of attX should contain at least two samples to plot the performance space.")
  # if (length(unique(tarAttY)) < 2) stop("Perturbation of attY should contain at least two samples to plot the performance space.")

  # Check the rest of the attributes - commented since it does not work for some reason
  # tarAttRest <- targetMat[ , -c(colAttX, colAttY)]
  # for (a in 1:ncol(tarAttRest)) {
  #   allPointsAtt <- sort(unique(tarAttRest[ ,a]))
  #   if (!(all( abs(allPointsAtt - mean(allPointsAtt)) < 0.000005 ))) {
  #     aName <- colnames(tarAttRest)[a]
  #     warning(paste0(aName, " is not perturbed on a regular grid. The performance space will be averaged across the irregular perturbations in this attribute."))
  #   }
  # }
  return(invisible())
}

# function to return the indices to subset the exposure space
# will be used to subset targetMat and performance
getSliceIndices <- function(expSpace, attSlices) {
  if (!(is.list(attSlices))) stop("attSlices should be a list.")
  attIn <- names(attSlices)
  if (is.null(attIn)) stop("attSlices should be named using the attribute tags to be sliced.")

  if (sum(attIn %in% c(expSpace[["attPerturb"]], expSpace[["attHold"]])) != length(attIn)) stop("attSlices should contain only attributes present in the expSpace of sim.")

  # identify whether perturbed or held
  attPInd <- which(attIn %in% expSpace[["attPerturb"]])
  attHInd <- which(attIn %in% expSpace[["attHold"]])

  # slices on attHold can only be 0(for Temp) or 1(other variables)
  # user is not expected to input these, in case they do input it the following code ensures that it does not result in an error.
  if (!identical(attHInd, integer(0))) {
    for (i in 1:length(attHInd)) {
      iAtt <- attIn[attHInd[i]]
      iAttVar <- strsplit(iAtt, "_")[[1]][1]
      if (iAttVar == "Temp") {
        if (attSlices[[iAtt]] != 0) stop(paste0(iAtt, " is a held attribute. attSlices[[\"", iAtt, "\"]] should be 0 or NULL."))
      } else {
        if (attSlices[[iAtt]] != 1) stop(paste0(iAtt, " is a held attribute. attSlices[[\"", iAtt, "\"]] should be 1 or NULL."))
      }
    }
  }

  targetMat <- expSpace[["targetMat"]]

  if (!identical(attPInd, integer(0))) {
    # perturbed attributes, this is where the actual slicing happens
    sliceIndicesList <- list()
    for (i in 1:length(attPInd)) {
      iAtt <- attIn[attPInd[i]]
      iSlice <- attSlices[[iAtt]]
      # single value to slice
      if (length(iSlice) == 1) {
        iSliceInd <- which(targetMat[[iAtt]] == iSlice)
        # a range to slice
      } else if (length(iSlice == 2)) {
        sMin <- min(iSlice)
        sMax <- max(iSlice)
        iSliceInd <- which(targetMat[[iAtt]] >= sMin & targetMat[[iAtt]] <= sMax)
      }
      if (identical(iSliceInd, integer(0))) stop(paste0("attSlices[[\"", iAtt, "\"]] is not valid for this expSpace."))
      sliceIndicesList[[i]] <- iSliceInd
    }
    sliceIndices <- Reduce(intersect, sliceIndicesList)
  } else {
    sliceIndices <- c(1:nrow(targetMat))
  }
  return(sliceIndices)
}

#' Plots a performance space using the system performance and scenarios as input
#'
#' \code{plotPerformanceSpace} uses the system model performance calculated 
#' using the function \code{runSystemModel} and the summary of the simulation 
#' generated using the functions \code{generateScenarios} 
#' & \code{getSimSummary} as input to plot the performance space of the system.
#' The user may specify the attributes to be used as the axes of the performance space.
#' @param performance a named list; contains the system model performance 
#' calculated using \code{runSystemModel}.
#' If the list contains more than one performance metric, the argument 
#' \code{metric} can be used to specify the metric to be used.
#' @param sim a list; summary of the simulation containing the scenarios 
#' generated using the function \code{generateScenarios} that is used to 
#' run the system model using \code{runSystemModel}.
#' The summary of the simulation may be obtained by using the function 
#' \code{getSimSummary} on the full simulation. The summary object is much 
#' smaller in size for ease of storage and use with the performance
#' plotting functions like \code{plotPerformanceSpace}.
#' @param metric a string; the name of the performance metric to be plotted. 
#' The argument can be used to select a metric from \code{performance} for plotting.
#' If \code{NULL} (the default), the first metric in the list will be used.
#' @param attX a string; the tag of the perturbed attribute to plot on the xaxis. 
#' The attribute must be one of the perturbed attributes of \code{sim}.
#' Type \code{sim$expSpace$attPerturb} to view all perturbed attributes of 
#' \code{sim}. If \code{NULL} (default), the first perturbed attribute of 
#' \code{sim} will be used.
#' @param attY a string; the tag of the perturbed attribute to plot on the yaxis. 
#' The attribute must be another perturbed attribute of \code{sim}.
#' If \code{NULL}, the second perturbed attribute of \code{sim} will be used.
#' @param topReps an integer (default is \code{NULL}); the number of "top" 
#' replicates in terms of simulation fitness to be used. If \code{topReps} is 
#' specified, \code{topReps}
#' number of replicates will be identified for each target and the average 
#' performance across these replicates will be plotted. If \code{NULL}, the 
#' average performance across all the replicates will be plotted.
#' @param perfThresh a number; the minimum or maximum threshold value of the 
#' performance metric. A line will be drawn to mark this threshold value in the performance space.
#' @param perfThreshLabel a string; the text to label \code{perfThresh}.
#' @param attSlices a list; used to subset perturbed attributes in \code{sim} 
#' for the plot. This argument would typically be used in cases where there are 
#' more than two perturbed attributes.
#' The elements of the list correspond to the perturbed attributes to be 
#' subsetted and must be named using the attribute tag. Each element may 
#' contain a single value or a two-element vector specifying the minimum-maximum values.
#' If the element is a single value, the exposure space is sliced on this 
#' single value of the attribute. If minimum-maximum values are specified, 
#' the exposure space will be sliced to subset this range.
#' If \code{attSlices} includes \code{attX} or \code{attY}, these attributes 
#' will be sliced and the resulting plot will be a "zoomed-in" space.
#' @param climData data.frame; the values of attX and attY from other sources 
#' like climate models. This data will be plotted as points in the performance space.
#' The data frame may contain columns with values of the performance metric to 
#' be plotted and the "Name" of the dataset.
#' If the performance metric is available in the data.frame, the points will be 
#' coloured based on the performance \code{colMap} scale.
#' If the \code{Name} of the data is available in the data.frame, the points 
#' will be identified using the \code{Name}.
#' Please refer data provided with the package that may be loaded using 
#' \code{data("egClimData")} for an example of the expected format of \code{climData}.
#' @param colMap a vector of colours; to specify the colourmap to be used. 
#' If \code{NULL}, the default foreSIGHT colourmap is used.
#' @param colLim a vector of 2 values; the minimum and maximum limits of the colour scale.
#' @param contourBreaks a vector; specifies breaks in the performance metric
#' @param nContour a number; specifies number of contours in the performance 
#' metric (if contourBreaks not specified)
#' @param axesPercentLabel a string; indicates display format for x and y axes. 
#' To display as a fraction, use "fraction", to display as a percentage change 
#' use "percentage.change", and to display as a percentage increase or decrease 
#' use "percentage.total".
#' @param type a string; indicates type of plot as "heat.plot" (default) 
#' or "filled.contour"
#' @param noPlot logical; indicates whether plots will be printed (\code{TRUE}) 
#' or not printed (\code{FALSE}) and only saved as an object.
#' @details If the space contains more than two perturbed attributes, the 
#' performance values are averaged across the perturbations in the attributes 
#' other than \code{attX} and \code{attY}.
#' The user may specify argument \code{attSlices} to slice the performance 
#' space at specific values of the other perturbed attributes. If 
#' \code{attSlices} are used to
#' specify minimum-maximum values to subset other perturbed attributes, 
#' the performance values are averaged across the subsetted perturbations 
#' in these attributes.
#' If the input performance list contains multiple performance metrics, 
#' the function plots the first metric.
#' The function may be called with \code{performance} argument specifying 
#' the metric to be plotted \code{plotPerformanceSpace(performance[2], sim)} 
#' to plot other metrics.
#' @return The plot of the performance space and the ggplot object.
#' @seealso \code{runSystemModel}, \code{generateScenarios}, \code{getSimSummary}, 
#' \code{plotPerformanceOAT}
#' @examples
#' \dontrun{
#' # load example datasets
#' data("egSimSummary") # summary of stochastic simulation
#' data("egSimPerformance") # system performance calculated using the stochastic simulation
#' data("egClimData") # alternate climate data and system performance
#' colnames(egClimData)[6] = "average daily deficit (L)" # change metric name to match egSimPerformance
#' 
#' plotPerformanceSpace(performance = egSimPerformance[2], sim = egSimSummary)
#'
#' # change plot style to "filled.contour" and specify contours - show contours from
#' # 0.76 to 0.9 in increments of 0.02
#' plotPerformanceSpace(
#'   performance = egSimPerformance[2],
#'   sim = egSimSummary, contourBreaks = seq(0.76, 0.84, 0.02),type='filled.contour'
#' )
#'
#'
#' # adding climate data, using top 10 replicates
#' plotPerformanceSpace(
#'   performance = egSimPerformance[1], sim = egSimSummary,
#'   topReps = 10, climData = egClimData 
#' )
#'
#' # adding a threshold
#' plotPerformanceSpace(
#'   performance = egSimPerformance, sim = egSimSummary, metric = "average daily deficit (L)",
#'   climData = egClimData, perfThresh = 27.5, perfThreshLabel = "Max Avg. Deficit",type = "heat.plot"
#' )
#'
#' # user specified colMap
#' plotPerformanceSpace(
#'   performance = egSimPerformance[1], sim = egSimSummary,
#'   climData = egClimData, perfThresh = 27.5,
#'   perfThreshLabel = "Max Avg. Deficit",
#'   colMap = viridisLite::inferno(100)
#' )
#'
#' # modify theme to change axes positioning to stacked vertically and left aligned
#' plotPerformanceSpace(
#'   performance = egSimPerformance[1], sim = egSimSummary,
#'   climData = egClimData, perfThresh = 27.5,
#'   perfThreshLabel = "Max Avg. Deficit",
#'   colMap = viridisLite::inferno(100)
#' ) +
#'   ggplot2::theme(
#'     legend.box = "vertical",
#'     legend.position = "bottom",
#'     legend.box.just = "left",
#'     legend.margin = ggplot2::margin(t = 0.01, r = 0.1, b = 0.01, l = 0.1, "cm"),
#'     legend.justification = c(0.01, 0.01)
#'   )
#'
#' # display fractional changes axes as percentage change
#' plotPerformanceSpace(
#'   performance = egSimPerformance, sim = egSimSummary,
#'   metric = "average daily deficit (L)",
#'   climData = egClimData, perfThresh = 27.5,
#'   perfThreshLabel = "Max Avg. Deficit",
#'   axesPercentLabel = "percentage.change"
#' )
#'
#' # change displayed contours on performance space - show contours 
#' # from 18 to 34 in increments of 2 L
#' plotPerformanceSpace(
#'   performance = egSimPerformance, sim = egSimSummary,
#'   metric = "average daily deficit (L)",
#'   climData = egClimData, perfThresh = 27.5,
#'   perfThreshLabel = "Max Avg. Deficit",
#'   contourBreaks = seq(18, 34, 2)
#' )
#'
#' # change plot type to filled.contour style
#' plotPerformanceSpace(
#'   type = "filled.contour", performance = egSimPerformance,
#'   sim = egSimSummary, metric = "average daily deficit (L)",
#'   climData = egClimData, perfThresh = 27.5,
#'   perfThreshLabel = "Max Avg. Deficit",
#'   contourBreaks = seq(18, 34, 2)
#' )
#'
#' # example overlay points manually from a dataset in a similar style to egClimData
#' ptStyle <- 21:25 # select set of pt styles (e.g. circle, square, triangle)
#' plotPerformanceSpace(performance = egSimPerformance[1],
#'                      sim = egSimSummary) +
#'   ggplot2::geom_point(
#'     data = egClimData,
#'     mapping = ggplot2::aes(
#'       x = .data[["P_day_all_tot_m"]],
#'       y = .data[["P_day_all_seasRatioMarAug"]],
#'       shape = .data[["Name"]]
#'     ),
#'     show.legend = TRUE, size = 5, colour = "black", fill = "lightgray"
#'   ) +
#'   ggplot2::scale_shape_manual(
#'     name = NULL, values = ptStyle,
#'     guide = ggplot2::guide_legend(order = 2, nrow = 1)
#'   ) +
#'   # one row of legend for specified ptStyle types
#'   ggplot2::theme(
#'     legend.box = "vertical", # vertical arrangement of items in legends
#'     legend.position = "bottom", # position legends base of figure
#'     legend.justification = c(0, 0)
#'   ) # justification according to the plot area
#'
#' # example of performance generated using simple scaled simulation
#' data("egScalPerformance")
#' data("egScalSummary")
#' data("egClimData")
#' plotPerformanceSpace(
#'   performance = egScalPerformance[2], sim = egScalSummary,
#'   perfThresh = 0.8, perfThreshLabel = "Reliability threshold"
#' )
#' }
#' @export

plotPerformanceSpace <- function(performance, # system model performance, matrix of size targets x replicates
                                 sim, # simulation containing all targets and replicates
                                 metric = NULL, # name of the performance metric
                                 attX = NULL, # attribute to be plotted on x-axis
                                 attY = NULL, # attribute to be plotted on y-axis
                                 topReps = NULL, # number of topReps based on fitness (i.e., the objective function score which is -ve)
                                 perfThresh = NULL, # desired performance threshold; plot would contain a contour to mark this threshold
                                 perfThreshLabel = "Threshold", # label text for the threshold
                                 attSlices = NULL, # list containing the slices of attributes to use for plotting
                                 climData = NULL, # changes in climate attributes from other sources - can include label. If the performance measure being plotted is a column in the data.frame, the points will be coloured accordingly
                                 colMap = NULL, # alternate colormap
                                 colLim = NULL, # if null, the full limit is used
                                 contourBreaks = NULL, # if null, default number of contours used, otherwise accepts vector of breaks
                                 nContour = perfSpace_nContour, # number of contours
                                 axesPercentLabel = "fraction", # or 'percentage.change' or 'percentage.total'
                                 type = "heat.plot", # plotting options "heat.plot", "filled.contour"
                                 noPlot = F) {
  # assuming that performance is a list with a name
  # it may also be a matrix without a name; will be named "performance"
  if (is.list(performance)) {
    if (!is.null(metric)) {
      if (!is.character(metric)) stop("`metric` should be specified as a string.")
      if (length(metric) > 1) stop("A single `metric` should be specified.")
      performance <- performance[metric]
      if (is.null(performance[[1]])) stop(paste0("Cannot find metric `", metric, "` in performance."))
      perfName <- metric
    } else {
      if (is.null(names(performance))) {
        perfName <- rep("Performance", length(performance))
      } else {
        perfName <- names(performance)
      }
    }
  } else if (is.matrix(performance)) {
    # already a matrix
    perfName <- "Performance"
    # change to list
    perfMat <- performance
    performance <- list()
    performance[[1]] <- perfMat
    rm(perfMat)
  }

  # unpacking sim metadata
  repNames <- names(sim[grep("Rep", names(sim))])
  if (is.null(repNames)) stop("There are no replicates in sim.")
  tarNames <- names(sim[[repNames[1]]])
  nRep <- length(repNames)
  nTar <- length(tarNames)
  targetMat <- sim[["expSpace"]][["targetMat"]]

  if (is.list(sim[["controlFile"]])) {
    # get the simulation fitness if stochastic
    simFitness <- getSimFitness(sim)

    # check that the performance is calculated from this simulation
    if (!identical(dim(simFitness), dim(performance[[1]]))) {
      stop("The number of targets and replicates in sim and performance should match. Is the performance calculated using sim?")
    }

    if (!is.null(topReps)) {
      if (topReps > nRep) {
        message(paste0("sim does not contain ", topReps, " replicates. Using all replicates available in sim.\n"))
      }
    }
  } else {
    if (!(sim[["controlFile"]] == "scaling")) stop("sim$controlFile is unrecognized.")
    # scaling
    simFitness <- NULL
    topReps <- NULL
  }

  # subset targets if attSlices are specified
  if (!is.null(attSlices)) {
    # indices to subset the rows of the targetMat
    tarInd <- getSliceIndices(sim[["expSpace"]], attSlices)

    # subset targetMat
    targetMat <- targetMat[tarInd, ]
  } else {
    tarInd <- NULL
  }

  # if null get appropriate tags
  if (is.null(attX) | is.null(attY)) {
    attPerturb <- sim[["expSpace"]][["attPerturb"]]
    attList <- getAttXY(attPerturb, attX, attY)
    attX <- attList[["attX"]]
    attY <- attList[["attY"]]
  }

  # check the perturbations in all attributes
  checkAttPert(targetMat, attX, attY)

  # identify x and y columns
  attNames <- colnames(targetMat)
  colAttX <- which(attNames == attX)
  colAttY <- which(attNames == attY)

  nPerf <- length(performance)
  perfPlots <- list()

  if (nPerf > 1) message("performance contains more than one performance metric, the first metric is plotted.")

  # loop over multiple metrics - can do this, but becomes complicated due for multiple colMap, colLim, and threshold labels
  # for (p in 1) {
  # subset performance based on slices
  if (!is.null(tarInd)) {
    perfMatrix <- performance[[1]][tarInd, ]
  } else {
    perfMatrix <- performance[[1]]
  }

  # get the average performance to be plotted
  performanceAv <- getPerfStat(perfMatrix, simFitness, topReps, nRep)

  # name appropriately
  names(performanceAv) <- perfName[1]

  # create data.frame for plotting
  perfPlotData <- cbind(targetMat[colAttX], targetMat[colAttY], performanceAv)

  # Base R: not used
  # plotPerfQuiltPlot(perfPlotData, nx, ny, colLim = colLim, colBar = colBar, perfThresh = perfThresh, climData = climData)

  if (type == "heat.plot") {
    perfPlots <- heatPlot(
      plotData = perfPlotData,
      colLim = colLim,
      colMap = colMap,
      perfThresh = perfThresh,
      perfThreshLabel = perfThreshLabel,
      climData = climData,
      contourBreaks = contourBreaks,
      nContour = nContour,
      axesPercentLabel = axesPercentLabel
    )
  } else if (type == "filled.contour") {
    perfPlots <- filledContourPlot(
      plotData = perfPlotData,
      colLim = colLim,
      colMap = colMap,
      perfThresh = perfThresh,
      perfThreshLabel = perfThreshLabel,
      climData = climData,
      contourBreaks = contourBreaks,
      nContour = nContour,
      axesPercentLabel = axesPercentLabel
    )
  } else {
    print("Warning: Invalid type specified in plotPerformanceSpace()")
    perfPlots <- NULL
  }

  # print(perfPlots[[1]])

  # }
  if (!noPlot) {
    print(perfPlots)
  }
  return(invisible(perfPlots))
}



heatPlot <- function(plotData,
                     colLim = NULL,
                     colMap = NULL,
                     perfThresh = NULL,
                     perfThreshLabel = "Threshold",
                     climData = NULL,
                     contourBreaks = NULL,
                     nContour = perfSpace_nContour,
                     axesPercentLabel = "fraction") {
  level <- NULL

  xyAtts <- colnames(plotData)[1:2]
  perfName <- colnames(plotData)[3]

  xyAttDefs <- mapply(tagBlender, xyAtts, USE.NAMES = FALSE)

  # aggregate data
  tempMat <- plotData
  names(tempMat) <- c("x", "y", "z")
  plotDataMean <- stats::aggregate(. ~ x + y, tempMat, mean)
  names(plotDataMean) <- names(plotData)

  if (is.null(colLim)) {
    colLimIn <- c(min(plotDataMean[, 3]), max(plotDataMean[, 3]))
  } else {
    colLimIn <- colLim
  }

  if (!is.null(perfThresh)) {
    tempData <- plotDataMean[, 3]
    threshName <- paste0("Thresh", names(plotDataMean)[3])
    names(tempData) <- threshName
    plotDataMean <- cbind(plotDataMean, tempData)
    names(plotDataMean) <- c(names(plotData), threshName)
  }

  varNames <- sapply(strsplit(xyAtts, "_"), `[[`, 1)
  varUnits <- getVarUnits(varNames)


  # Modify axes that currently display as "fraction" to display in terms of "%"
  if (axesPercentLabel == "percentage.total") {
    # modify if atribute is a fraction
    tempInd <- which(varUnits == "fraction")
    varUnits[tempInd] <- "%" # change label to %
    xyLabels <- paste0(xyAttDefs, " (", varUnits, ")")
    xlimits <- c(min(plotDataMean[, 1]), max(plotDataMean[, 1]))
    ylimits <- c(min(plotDataMean[, 2]), max(plotDataMean[, 2]))

    # modify depending on whether x, y or both
    if (length(tempInd) == 2) { # if both x- & y-axis
      p1 <- ggplot() +
        geom_tile(data = plotDataMean, aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], fill = .data[[perfName]])) +
        labs(x = xyLabels[1], y = xyLabels[2]) +
        scale_x_continuous(expand = c(0, 0), labels = scales::percent_format(accuracy = 0.1)) + # no extra space on x and y axes
        scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(accuracy = 0.1)) +
        coord_cartesian(xlim = xlimits, ylim = ylimits) +
        theme_heatPlot()
    } else if (tempInd[1] == 1) { # if only x-axis
      p1 <- ggplot() +
        geom_tile(data = plotDataMean, aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], fill = .data[[perfName]])) +
        labs(x = xyLabels[1], y = xyLabels[2]) +
        scale_x_continuous(expand = c(0, 0), labels = scales::percent_format(accuracy = 0.1)) + # no extra space on x and y axes
        scale_y_continuous(expand = c(0, 0)) +
        coord_cartesian(xlim = xlimits, ylim = ylimits) +
        theme_heatPlot()
    } else { # if only y-axis
      p1 <- ggplot() +
        geom_tile(data = plotDataMean, aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], fill = .data[[perfName]])) +
        labs(x = xyLabels[1], y = xyLabels[2]) +
        scale_x_continuous(expand = c(0, 0)) + # no extra space on x and y axes
        scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(accuracy = 0.1)) +
        coord_cartesian(xlim = xlimits, ylim = ylimits) +
        theme_heatPlot()
    }
  } else if (axesPercentLabel == "percentage.change") { # display as +/- % change i.e., -20% instead of 80%
    # modify if atribute is a fraction

    plotDataMean[, 1] <- plotDataMean[, 1] - 1 # convert to just the percentage increase or decrease
    plotDataMean[, 2] <- plotDataMean[, 2] - 1
    tempInd <- which(varUnits == "fraction")
    varUnits[tempInd] <- "%" # change label to %
    xyLabels <- paste0(xyAttDefs, " (", varUnits, ")")
    xlimits <- c(min(plotDataMean[, 1]), max(plotDataMean[, 1]))
    ylimits <- c(min(plotDataMean[, 2]), max(plotDataMean[, 2]))

    # modify depending on whether x, y or both
    if (length(tempInd) == 2) { # if both x- & y-axis
      p1 <- ggplot() +
        geom_tile(data = plotDataMean, aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], fill = .data[[perfName]])) +
        labs(x = xyLabels[1], y = xyLabels[2]) +
        scale_x_continuous(expand = c(0.01, 0.01), labels = scales::percent_format(accuracy = 0.1)) + # no extra space on x and y axes - need a tiny bit or labels were getting cut off
        scale_y_continuous(expand = c(0.01, 0.01), labels = scales::percent_format(accuracy = 0.1)) +
        coord_cartesian(xlim = xlimits, ylim = ylimits) +
        theme_heatPlot()
    } else if (tempInd[1] == 1) { # if only x-axis
      p1 <- ggplot() +
        geom_tile(data = plotDataMean, aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], fill = .data[[perfName]])) +
        labs(x = xyLabels[1], y = xyLabels[2]) +
        scale_x_continuous(expand = c(0.01, 0.01), labels = scales::percent_format(accuracy = 0.1)) + # no extra space on x and y axes
        scale_y_continuous(expand = c(0.01, 0.01)) +
        coord_cartesian(xlim = xlimits, ylim = ylimits) +
        theme_heatPlot()
    } else { # if only y-axis
      p1 <- ggplot() +
        geom_tile(data = plotDataMean, aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], fill = .data[[perfName]])) +
        labs(x = xyLabels[1], y = xyLabels[2]) +
        scale_x_continuous(expand = c(0.01, 0.01)) + # no extra space on x and y axes
        scale_y_continuous(expand = c(0.01, 0.01), labels = scales::percent_format(accuracy = 0.1)) +
        coord_cartesian(xlim = xlimits, ylim = ylimits) +
        theme_heatPlot()
    }

  } else if (axesPercentLabel == "fraction") { # if no percentage axes modification
    xyLabels <- paste0(xyAttDefs, " (", varUnits, ")")
    xlimits <- c(min(plotDataMean[, 1]), max(plotDataMean[, 1]))
    ylimits <- c(min(plotDataMean[, 2]), max(plotDataMean[, 2]))

    p1 <- ggplot() +
      geom_tile(data = plotDataMean, aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], fill = .data[[perfName]])) +
      labs(x = xyLabels[1], y = xyLabels[2]) +
      scale_x_continuous(expand = c(0, 0)) + # no extra space on x and y axes
      scale_y_continuous(expand = c(0, 0)) +
      coord_cartesian(xlim = xlimits, ylim = ylimits) +
      theme_heatPlot()
  }

  # contours
  if (perfSpace_contours) { # TRUE or FALSE default setting
    # print(colLimIn)

    if (is.null(contourBreaks)) {
      contourBreaks <- pretty(colLimIn, nContour)
    } # create breaks if none-specified
    # print(contourBreaks)

    # not able to add bins here - fix later
    p1 <- p1 + geom_contour(data = plotDataMean, aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], z = .data[[perfName]]), colour = "black", breaks = contourBreaks) + # ,breaks=seq(0.6,0.8,0.01),alpha=0.5
      directlabels::geom_dl(
        data = plotDataMean, aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], z = .data[[perfName]], label = after_stat(level)), # edited 20/06/2018
        method = list("first.points", "calc.boxes", "enlarge.box", box.color = NA, fill = "transparent", vjust = -0.5, hjust = -0.5, "draw.rects"),
        stat = "contour", breaks = contourBreaks
      ) # ,breaks=seq(0.6,0.8,0.01)
  }

  if (!is.null(perfThresh)) {
    p1 <- addContourThreshold(p1, plotDataMean, perfThresh, perfThreshLabel, xyAtts, perfName)
  }

  p2List <- addClimData(p1, climData, perfName, xyAtts, xlimits, ylimits, colLim, colLimIn)
  p2 <- p2List[[1]]
  colLimIn <- p2List[[2]]

  # Does not work if there are more than one contour line
  # if (!is.null(perfThresh)) {
  #   p2 <- p2 +
  #   directlabels::geom_dl(data = plotDataMean, aes(x = !!as.symbol(xyAtts[1]), y = !!as.symbol(xyAtts[2]), z = !!as.symbol(perfName), label = perfThreshLabel), #edited 20/06/2018
  #                         #method = list("first.points", "calc.boxes", "enlarge.box", box.color = "black", fill = "white", "draw.rects"),stat="contour", breaks = perfThresh)  #,breaks=seq(0.6,0.8,0.01)
  #                         # method = list(box.color = NA, "angled.boxes"),stat="contour", breaks = perfThresh)  #,breaks=seq(0.6,0.8,0.01)
  #                         method = list("smart.grid", box.color = "black", fill = "white", "draw.rects"), stat = "contour", breaks = perfThresh)
  #   #p2 <- directlabels::direct.label(p2, method = "angled.boxes", stat = "contour", breaks = perfThresh)
  # }

  # if (is.null(colMap)) {
  #   coloursIn <- foreSIGHT.colmap(perfSpace_nlevel)
  # } else {
  #   coloursIn <- colMap
  # }

  # Align colours with contours # needs fixing to align
  if (is.null(colMap)) {
    coloursIn <- foreSIGHT.colmap(perfSpace_nlevel)
  } else if (is.function(colMap)) {
    coloursIn <- colMap(perfSpace_nlevel)
  } else {
    coloursIn <- colMap
  }

  # p2 <- p2 + scale_fill_gradientn(colours = grDevices::adjustcolor(coloursIn, alpha.f = perfSpace_alpha), limits = colLimIn,
  #                                 guide = guide_colorbar(title = perfName, title.position = "right", order = 1, barwidth = 12, barheight = 0.6)) + labs(tag = tag_text)

  p2 <- p2 + scale_fill_gradientn(
    colours = coloursIn, limits = colLimIn,
    guide = guide_colorbar(title = perfName, title.position = "right", order = 1, barwidth = 12, barheight = 0.6)
  ) #+ labs(tag = tag_text)

  # print(p2)
  return(p2)
}

# pretty_breaks_exclude_zero <- function(x, n = 10) {
#   # Generate pretty breaks
#   breaks <- pretty(range(x, finite = TRUE), n = n)
#
#   # If 0 is exactly one of the breakpoints, shift the sequence slightly
#   if (0 %in% breaks) {
#     step <- diff(breaks)[1]  # assume equal spacing
#     # Shift by half a step to exclude 0
#     breaks <- breaks + step / 2
#     # Round again to nice numbers
#     breaks <- pretty(breaks, n = n)
#   }
#
#   return(breaks)
# }

# Bree: Added 17Nov2021
# Legend currently turned off
filledContourPlot <- function(plotData,
                              colLim = NULL,
                              colMap = NULL,
                              perfThresh = NULL,
                              perfThreshLabel = "Threshold",
                              climData = NULL,
                              contourBreaks = NULL,
                              nContour = perfSpace_nContour,
                              axesPercentLabel = "fraction") {
  level <- NULL

  xyAtts <- colnames(plotData)[1:2]
  perfName <- colnames(plotData)[3]

  xyAttDefs <- mapply(tagBlender, xyAtts, USE.NAMES = FALSE)

  # aggregate data
  tempMat <- plotData
  names(tempMat) <- c("x", "y", "z")
  plotDataMean <- stats::aggregate(. ~ x + y, tempMat, mean)
  names(plotDataMean) <- names(plotData)

  if (is.null(colLim)) {
    colLimIn <- c(min(plotDataMean[, 3]), max(plotDataMean[, 3]))
  } else {
    colLimIn <- colLim
  }

  # ADD CONTOUR BREAKS IF NONE SPECIFIED
  if (is.null(contourBreaks)) { # create breaks if none-specified
    contourBreaks <- pretty(colLimIn, nContour)
    #    contourBreaks <- pretty_breaks_exclude_zero(colLimIn, perfSpace_nContour)
  }

  # Align colours with contours # needs fixing to align
  if (is.null(colMap)) {
    coloursIn <- foreSIGHT.colmap(length(contourBreaks) - 1)
  } else if (is.function(colMap)) {
    coloursIn <- colMap((length(contourBreaks) - 1))
  } else {
    coloursIn <- colMap
  }

  # check matching colours and contours
  if (length(coloursIn) < (length(contourBreaks) - 1)) {
    print(paste0("Warning: ", length(coloursIn), " colours supplied for ", length(contourBreaks), " contour breaks."))
  }

  if (!is.null(perfThresh)) {
    tempData <- plotDataMean[, 3]
    threshName <- paste0("Thresh", names(plotDataMean)[3])
    names(tempData) <- threshName
    plotDataMean <- cbind(plotDataMean, tempData)
    names(plotDataMean) <- c(names(plotData), threshName)
  }

  varNames <- sapply(strsplit(xyAtts, "_"), `[[`, 1)
  varUnits <- getVarUnits(varNames)

  # Stick together break labels
  nbreaks <- length(contourBreaks)
  breaklab <- paste0(contourBreaks[1:(nbreaks - 1)], "-", contourBreaks[2:nbreaks])

  # Modify axes that currently display as "fraction" to display in terms of "%"
  if (axesPercentLabel == "percentage.change") {
    # modify if atribute is a fraction
    tempInd <- which(varUnits == "fraction")
    varUnits[tempInd] <- "%" # change label to %
    xyLabels <- paste0(xyAttDefs, " (", varUnits, ")")
    xlimits <- c(min(plotDataMean[, 1]), max(plotDataMean[, 1]))
    ylimits <- c(min(plotDataMean[, 2]), max(plotDataMean[, 2]))

    # modify depending on whether x, y or both
    if (length(tempInd) == 2) { # if both x- & y-axis
      p1 <- ggplot() +
        geom_contour_filled(
          data = plotDataMean,
          aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], z = .data[[perfName]]),
          breaks = contourBreaks, show.legend = TRUE
        ) +
        labs(x = xyLabels[1], y = xyLabels[2]) +
        scale_x_continuous(expand = c(0, 0), labels = scales::percent_format(accuracy = 0.1)) + # no extra space on x and y axes
        scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(accuracy = 0.1)) +
        coord_cartesian(xlim = xlimits, ylim = ylimits) +
        scale_fill_manual(drop = FALSE, values = coloursIn, labels = breaklab, name = perfName) +
        guides(fill = guide_legend(order = 1, override.aes = list(shape = rep(NA, length(breaklab))))) +
        theme_heatPlot() +
        theme(legend.position = "right")
    } else if (tempInd[1] == 1) { # if only x-axis
      p1 <- ggplot() +
        geom_contour_filled(
          data = plotDataMean,
          aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], z = .data[[perfName]]),
          breaks = contourBreaks, show.legend = TRUE
        ) +
        labs(x = xyLabels[1], y = xyLabels[2]) +
        scale_x_continuous(expand = c(0, 0), labels = scales::percent_format(accuracy = 0.1)) + # no extra space on x and y axes
        scale_y_continuous(expand = c(0, 0)) +
        coord_cartesian(xlim = xlimits, ylim = ylimits) +
        scale_fill_manual(drop = FALSE, values = coloursIn, labels = breaklab, name = perfName) +
        guides(fill = guide_legend(order = 1, override.aes = list(shape = rep(NA, length(breaklab))))) +
        theme_heatPlot() +
        theme(legend.position = "right")
    } else { # if only y-axis
      p1 <- ggplot() +
        geom_contour_filled(
          data = plotDataMean,
          aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], z = .data[[perfName]]),
          breaks = contourBreaks, show.legend = TRUE
        ) +
        labs(x = xyLabels[1], y = xyLabels[2]) +
        scale_x_continuous(expand = c(0, 0)) + # no extra space on x and y axes
        scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(accuracy = 0.1)) +
        coord_cartesian(xlim = xlimits, ylim = ylimits) +
        scale_fill_manual(drop = FALSE, values = coloursIn, labels = breaklab, name = perfName) +
        guides(fill = guide_legend(order = 1, override.aes = list(shape = rep(NA, length(breaklab))))) +
        theme_heatPlot() +
        theme(legend.position = "right")
    }
  } else if (axesPercentLabel == "percentage.total") {
    # modify if atribute is a fraction

    plotDataMean[, 1] <- plotDataMean[, 1] - 1
    plotDataMean[, 2] <- plotDataMean[, 2] - 1
    tempInd <- which(varUnits == "fraction")
    varUnits[tempInd] <- "%" # change label to %
    xyLabels <- paste0(xyAttDefs, " (", varUnits, ")")
    xlimits <- c(min(plotDataMean[, 1]), max(plotDataMean[, 1]))
    ylimits <- c(min(plotDataMean[, 2]), max(plotDataMean[, 2]))


    # modify depending on whether x, y or both
    if (length(tempInd) == 2) { # if both x- & y-axis
      p1 <- ggplot() +
        geom_contour_filled(
          data = plotDataMean,
          aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], z = .data[[perfName]]),
          breaks = contourBreaks, show.legend = TRUE
        ) +
        labs(x = xyLabels[1], y = xyLabels[2]) +
        scale_x_continuous(expand = c(0.0001, 0.0001), labels = scales::percent_format(accuracy = 0.1)) + # no extra space on x and y axes
        scale_y_continuous(expand = c(0.0001, 0.0001), labels = scales::percent_format(accuracy = 0.1)) +
        coord_cartesian(xlim = xlimits, ylim = ylimits) +
        scale_fill_manual(drop = FALSE, values = coloursIn, labels = breaklab, name = perfName) +
        guides(fill = guide_legend(order = 1, override.aes = list(shape = rep(NA, length(breaklab))))) +
        theme_heatPlot() +
        theme(legend.position = "right")
    } else if (tempInd[1] == 1) { # if only x-axis
      p1 <- ggplot() +
        geom_contour_filled(
          data = plotDataMean,
          aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], z = .data[[perfName]]),
          breaks = contourBreaks, show.legend = TRUE
        ) +
        labs(x = xyLabels[1], y = xyLabels[2]) +
        scale_x_continuous(expand = c(0.0001, 0.0001), labels = scales::percent_format(accuracy = 0.1)) + # no extra space on x and y axes
        scale_y_continuous(expand = c(0.0001, 0.0001)) +
        coord_cartesian(xlim = xlimits, ylim = ylimits) +
        scale_fill_manual(drop = FALSE, values = coloursIn, labels = breaklab, name = perfName) +
        guides(fill = guide_legend(order = 1, override.aes = list(shape = rep(NA, length(breaklab))))) +
        theme_heatPlot() +
        theme(legend.position = "right")
    } else { # if only y-axis
      p1 <- ggplot() +
        geom_contour_filled(
          data = plotDataMean,
          aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], z = .data[[perfName]]),
          breaks = contourBreaks, show.legend = TRUE
        ) +
        labs(x = xyLabels[1], y = xyLabels[2]) +
        scale_x_continuous(expand = c(0.0001, 0.0001)) + # no extra space on x and y axes
        scale_y_continuous(expand = c(0.0001, 0.0001), labels = scales::percent_format(accuracy = 0.1)) +
        coord_cartesian(xlim = xlimits, ylim = ylimits) +
        scale_fill_manual(drop = FALSE, values = coloursIn, labels = breaklab, name = perfName) +
        guides(fill = guide_legend(order = 1, override.aes = list(shape = rep(NA, length(breaklab))))) +
        theme_heatPlot() +
        theme(legend.position = "right")
    }
  } else if (axesPercentLabel == "fraction") { # if no percentage axes modification
    xyLabels <- paste0(xyAttDefs, " (", varUnits, ")")
    xlimits <- c(min(plotDataMean[, 1]), max(plotDataMean[, 1]))
    ylimits <- c(min(plotDataMean[, 2]), max(plotDataMean[, 2]))

    # p1 <- ggplot() +
    #   geom_contour_filled(data = plotDataMean,
    #                       aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], z = .data[[perfName]]),
    #                       breaks = contourBreaks,show.legend=TRUE)+
    #   labs(x = xyLabels[1], y = xyLabels[2]) +
    #   scale_x_continuous(expand=c(0, 0)) +                                          # no extra space on x and y axes
    #   scale_y_continuous(expand=c(0, 0)) +
    #   coord_cartesian(xlim=xlimits, ylim=ylimits) +
    #   scale_fill_manual(drop=FALSE,values=coloursIn,labels=breaklab,name=perfName)+
    #   guides(fill=guide_legend(order=1,override.aes = list(shape=rep(NA,length(breaklab)))))+
    #   theme_heatPlot() +
    #   theme(legend.position="right")

    # Step 1: Build a temporary plot just to get the levels
    p_temp <- ggplot() +
      geom_contour_filled(
        data = plotDataMean,
        aes(
          x = .data[[xyAtts[1]]],
          y = .data[[xyAtts[2]]],
          z = .data[[perfName]]
        ),
        breaks = contourBreaks
      )

    p1 <- p_temp +
      scale_fill_manual(values = coloursIn, name = perfName, drop = F) +
      labs(x = xyLabels[1], y = xyLabels[2]) +
      coord_cartesian(xlim = xlimits, ylim = ylimits) +
      theme_heatPlot()
  }

  # CONTOUR LINES
  if (perfSpace_contours) { # TRUE or FALSE default setting
    # print(colLimIn)

    # if(is.null(contourBreaks)){contourBreaks <- pretty(colLimIn, perfSpace_nContour)} # create breaks if none-specified
    # print(contourBreaks)

    # not able to add bins here - fix later
    p1 <- p1 + geom_contour(data = plotDataMean, aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], z = .data[[perfName]]), colour = "black", breaks = contourBreaks) +
      directlabels::geom_dl(
        data = plotDataMean, aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], z = .data[[perfName]], label = after_stat(level)),
        method = list("first.points", "calc.boxes", "enlarge.box", box.color = NA, fill = "transparent", vjust = -0.5, hjust = -0.5, "draw.rects"),
        stat = "contour", breaks = contourBreaks
      )
  }

  if (!is.null(perfThresh)) {
    p1 <- addContourThreshold(p1, plotDataMean, perfThresh, perfThreshLabel, xyAtts, perfName)
  }

  p2List <- addClimData(p1, climData, perfName, xyAtts, xlimits, ylimits, colLim, colLimIn, pointColouring = FALSE)
  p2 <- p2List[[1]]
  colLimIn <- p2List[[2]]

  # LEGEND OPTION PLOTS ON RIGHT FOR NOW
  # p2 <- p2 + scale_fill_manual(values=coloursIn,drop=FALSE) #test edit
  # p2 <- p1 #+
  # labs(tag = tag_text)+ #add foreSIGHT TAG

  # print(p2)
  return(p2)
}

addContourThreshold <- function(p1, plotDataMean, perfThresh, perfThreshLabel, xyAtts, perfName) {
  p1 <- p1 + geom_contour(
    data = plotDataMean, aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], z = .data[[perfName]]),
    breaks = perfThresh, colour = perfSpace_threshCol, size = threshLineSize
  ) #+ #,breaks=seq(0.6,0.8,0.01)
  # directlabels::geom_dl(data = plotDataMean, aes(x = !!as.symbol(xyAtts[1]), y = !!as.symbol(xyAtts[2]), z = !!as.symbol(perfName), label = perfThreshLabel), #edited 20/06/2018
  #                       #method = list("first.points", "calc.boxes", "enlarge.box", box.color = "black", fill = "white", "draw.rects"),stat="contour", breaks = perfThresh)  #,breaks=seq(0.6,0.8,0.01)
  #                       # method = list(box.color = NA, "angled.boxes"),stat="contour", breaks = perfThresh)  #,breaks=seq(0.6,0.8,0.01)
  #                       method = list("smart.grid", box.color = "black", fill = "white", "draw.rects"), stat = "contour", breaks = perfThresh)

  # Had to take this roundabout way to label the threshold - rethink
  # performance threshold
  nx <- length(unique(plotDataMean[, 1]))
  ny <- length(unique(plotDataMean[, 2]))
  look <- fields::as.image(plotDataMean[, 3], ind = cbind(plotDataMean[, 1], plotDataMean[, 2]), nx = nx, ny = ny)
  lineThresh <- grDevices::contourLines(x = look$x, y = look$y, z = look$z, levels = perfThresh)
  if (!(length(lineThresh) == 0)) {
    # lineNo <- NULL
    # lineLen <- NULL
    # if (length(lineThresh) > 1) {
    #   for (i in 1:length(lineThresh)) {
    #     nPts <- length(lineThresh[[i]])
    #     if (!(lineThresh[[i]]$x[1] == lineThresh[[i]]$x[nPts] | lineThresh[[i]]$y[1] == lineThresh[[i]]$y[nPts])) {
    #       lineLenNew <- length(lineThresh[[i]]$x)
    #       if (!is.null(lineNo)) {
    #         if (lineLenNew > lineLen) {
    #           lineNo <- i
    #           lineLen <- lineLenNew
    #         }
    #       } else {
    #         lineNo <- i
    #         lineLen <- lineLenNew
    #       }
    #     }
    #   }
    # } else {
    #   lineNo <- 1
    # }
    lineNo <- 1

    perfThreshData <- data.frame(x = lineThresh[[lineNo]]$x, y = lineThresh[[lineNo]]$y)
    perfThreshData$Tname <- perfThreshLabel
    nLines <- nrow(perfThreshData)

    p1 <- p1 + # geom_line(perfThreshData, mapping = aes(x = x, y = y, colour = Tname), colour = perfSpace_threshCol, size = threshLineSize) +
      geom_label(data = perfThreshData[nLines, ], mapping = aes(x = .data$x, y = .data$y, label = .data$Tname), size = threshLabelSize, vjust = "inward", hjust = "inward") # , colour = threshCol, fill = "black")
  } else {
    message("perfThresh is not plotted becasue it is outside the performance space.")
  }
  return(p1)
}

# Add contour function for plotOptions. The contour based on the original performance metric values is added on top of heatPlots created using the difference data
# So the threshold is a line based on the original performance metric - added on heatMaps plotted using the differences (option 2 - option 1)
# Similar to this function, ggplot_build can be used instead of fields::look to get the contour lines from a single contour line
addThresholdLines <- function(p1, plotDataMean, perfThresh, perfThreshLabel, xyAtts, perfName, lineCol, lineSize, lineAlpha, label = "beginning") {
  p1 <- p1 + geom_contour(
    data = plotDataMean, aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], z = .data[[perfName]]),
    breaks = perfThresh, colour = lineCol, size = lineSize, alpha = lineAlpha
  ) #+ #,breaks=seq(0.6,0.8,0.01)
  p2 <- ggplot() +
    geom_contour(
      data = plotDataMean, aes(x = .data[[xyAtts[1]]], y = .data[[xyAtts[2]]], z = .data[[perfName]]),
      breaks = perfThresh, colour = lineCol, size = lineSize, alpha = lineAlpha
    ) #+ #,breaks=seq(0.6,0.8,0.01)
  # Code to get the x-y co-ordinates of the contour line from p2 and plot it on p1
  p2Build <- ggplot_build(p2)
  threshContour <- data.frame(x = p2Build[["data"]][[1]]$x, y = p2Build[["data"]][[1]]$y)
  # threshContour <- as.data.frame(spline(p2Build[["data"]][[1]]$x, p2Build[["data"]][[1]]$y))

  if (nrow(threshContour) > 0) {
    if (label == "beginning") {
      nLine <- 1
    } else {
      nLine <- nrow(threshContour) - 1
      if (nLine == 0) nLine <- 1
    }

    threshContour$Tname <- perfThreshLabel
    p1 <- p1 + # geom_line(threshContour, mapping = aes(x = x, y = y), colour = lineCol, size = threshLineSize) +
      geom_label(data = threshContour[nLine, ], mapping = aes(x = .data$x, y = .data$y, label = .data$Tname), size = threshLabelSize, vjust = "inward", hjust = "inward") # , colour = threshCol, fill = "black")

    # # threshold label
    # nx <- length(unique(plotDataMean[, 1]))
    # ny <- length(unique(plotDataMean[, 2]))
    # look <- fields::as.image(plotDataMean[ ,3], ind = cbind(plotDataMean[ ,1], plotDataMean[ ,2]), nx = nx, ny = ny)
    # lineThresh <- contourLines(x = look$x, y = look$y, z = look$z, levels = perfThresh)
    # if (!(length(lineThresh) == 0)) {
    #   lineNo <- 1
    #
    #   perfThreshData <- data.frame(x = lineThresh[[lineNo]]$x, y = lineThresh[[lineNo]]$y)
    #   perfThreshData$Tname <- perfThreshLabel
    #   nLines <- nrow(perfThreshData)
    #
    #   p1 <- p1 + #geom_line(perfThreshData, mapping = aes(x = x, y = y, colour = Tname), colour = perfSpace_threshCol, size = threshLineSize) +
    #     geom_label(aes(x = x, y = y, label = Tname), data = perfThreshData[nLines, ], size = threshLabelSize, vjust = "inward", hjust = "inward") #, colour = threshCol, fill = "black")
  } else {
    message("perfThresh is not plotted becasue it is outside the performance space.")
  }
  return(p1)
}


