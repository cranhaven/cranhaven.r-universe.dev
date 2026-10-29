#' Plot method for Epoch objects
#'
#' @param x An Epoch object
#' @param y Not used (for S4 method compatibility)
#' @param gap Numeric value specifying the gap between electrode traces (default: 2)
#' @param groupIndex Integer or string. A group of electrodes to show together in a different color. If NULL(default), all electrodes are shown in the same color. 
#' @param timeResolution Maximum number of time points to keep for each electrode (default: 2048)
#' @param maxLabels Maximum number of electrode labels to display on the y-axis (default: 50)
#' @param linewidth Line width for the electrode traces (default: 0.2)
#' @param x.lab.size Size of the x-axis label text (default: 10)
#' @param y.lab.size Size of the y-axis label text (default: 10)
#' @param standardize If the parameter is a logical value, it indicates whether to standardize the iEEG data across time for each electrode. If it is a logical vector with length equal to the number of electrodes, it indicates whether to standardize each electrode individually. If it is a numeric vector with length equal to the number of electrodes, it indicates the standard deviation to use for standardization for each electrode. (default: TRUE).
#' @param ... Additional arguments (not currently used)
#' @return `plot`: A ggplot object showing iEEG electrode traces
#'
#' @examples
#' # Create an Epoch object
#' epoch_data <- matrix(rnorm(1000), nrow = 10)
#' rownames(epoch_data) <- paste0("Electrode_", 1:10)
#' epoch <- Epoch(epoch_data, startTime = 0, samplingRate = 100)
#' 
#' # Plot the epoch
#' plot(epoch)
#' 
#' 
#' @family Epoch methods
#' @export
setMethod("plot", signature(x = "Epoch", y = "missing"), 
    function(x, y, gap = 2, 
    groupIndex = NULL, timeResolution = 2048, 
    maxLabels = 50, linewidth = 0.2, x.lab.size = 10, y.lab.size = 10, standardize = TRUE, ...) {
    elecNames <- rownames(x)
    data <- tblData(x)
    elecNum <- nrow(data)
    timesNum <- ncol(data)

    plotData <- data
    ## The indices of the time points to plot
    if (timesNum > timeResolution) {
        indices <- floor(seq(1, timesNum, length.out = timeResolution))
    }else{
        indices <- seq_len(timesNum)
    }
    plotData <- plotData[, indices, drop = FALSE]

    # ticks for x-axis
    timePoints <- .times(x)
    if (is.null(timePoints) || all(is.na(timePoints))) {
        xlabel <- "Time Index"
        timeTicks <- indices
    } else {
        xlabel <- "Time (s)"
        timeTicks <- timePoints[indices]
    }

    # group electrodes
    groupIndex <- .checkIndex(groupIndex, elecNames)
    group1 <- groupIndex
    group2 <- setdiff(seq_len(elecNum), groupIndex)

    # group colors
    elecColor <- rep("blue", elecNum)
    elecColor[seq_along(group2)] <- "black"

    # reorder the electrodes
    plotData <- plotData[c(group1, group2), , drop = FALSE]
    newElecNames <- c(elecNames[group1], elecNames[group2])

    # Standardize the data
    if (length(standardize) == elecNum){
        names(standardize) <- elecNames
        standardize <- standardize[newElecNames]
    }
    plotData <- t(.standardizeIEEG(plotData, standardize, gap))
    plotData <- as.data.frame(plotData)
    plotData$timeTicks <- timeTicks

    # Add gap between electrodes for visual separation
    breakplot <- (seq_len(elecNum) - 1) * gap
    elecNamesReversed <- rev(newElecNames)
    for (i in seq_along(elecNamesReversed)) {
        elec <- elecNamesReversed[i]
        plotData[[elec]] <- plotData[[elec]] + (i-1) * gap
    }

    
    ## limit the number of labels on y-axis
    ylabels <- elecNamesReversed
    if (length(ylabels) > maxLabels) {
        by_num <- ceiling(length(ylabels)/maxLabels)
        label_idx <- seq(length(ylabels), 1, by=-by_num)
        ylabels[-label_idx] <- ""
    }

    ## Turn the data into long format for ggplot
    plotData_long <- reshape(
        plotData,
        varying = elecNamesReversed,
        v.names = "Signal",
        timevar = "Electrode",
        times = elecNamesReversed,
        direction = "long"
        )
    
    plotData_long$Electrode <- factor(plotData_long$Electrode, levels = elecNamesReversed)

    ggplot(
        plotData_long, 
        aes(x = .data$timeTicks, y = .data$Signal, group = .data$Electrode)
    ) +
    geom_line(linewidth = linewidth) + 
    labs(x = xlabel, y = "Electrode") +
    scale_y_continuous(labels = ylabels, breaks = breakplot) +
    theme(
        axis.text.y = element_markdown(colour = elecColor),
        axis.title.x = element_text(size = x.lab.size),
        axis.title.y = element_text(size = y.lab.size)
    ) -> p

    p 
    }
)
