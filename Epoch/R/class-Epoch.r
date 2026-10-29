setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("arrayOrNULL", c("array", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("data.frameOrNULL", c("data.frame", "NULL"))

#' @title Epoch Class
#' @description S4 class to handle epoch data with electrodes and time points
#' @slot table a matrix containing iEEG data (columns=time points, rows=electrodes)
#' @slot colData a data frame containing metadata for columns (time points)
#' @slot rowData a data frame containing metadata for rows (electrodes)
#' @slot metaData a list containing metadata for the Epoch object
#' @exportClass Epoch
.Epoch <- setClass("Epoch",
    slots = list(),
    contains = "TableContainer",
)

.TableContainer2Epoch <- function(x) {
    if (!is(x, "TableContainer")) {
        return(x)
    }
    # Create a new Epoch object
    .Epoch(
        table = tblData(x),
        rowData = rowData(x),
        colData = colData(x),
        metaData = metaData(x)
    )
}

#' Constructor for Epoch class
#' @param table Matrix containing epoch data (rows=electrodes, columns=time points)
#' @param electrodes Optional character vector for electrode names, if not provided, row names of data are used. If row names are also not available, there will be no electrode names.
#' @param times Optional numeric vector of time points.
#' @param startTime Optional numeric value for start time, if provided, times will be calculated based on this and samplingRate.
#' @param samplingRate Optional numeric value for sampling rate, if provided, times will be calculated based on this and startTime.
#' @param rowData Optional data frame containing metadata for rows (electrodes).
#' @param colData Optional data frame containing metadata for columns (time points).
#' @param metaData Optional list containing metadata for the Epoch object. Element name "SamplingRate" is reserved by the Epoch class.
#' @return An Epoch object
#'
#' @examples
#' epoch_data <- matrix(rnorm(1000), nrow = 10)
#' rownames(epoch_data) <- paste0("Electrode_", 1:10)
#' epoch <- Epoch(epoch_data, startTime = 0, samplingRate = 100)
#'
#' @export
Epoch <- function(
    table,
    electrodes = NULL, times = NULL,
    startTime = NULL, samplingRate = NULL,
    rowData = NULL, colData = NULL, metaData = NULL) {
    stopifnot(is.matrix(table))
    stopifnot(is.list(metaData) || is.null(metaData))
    if (!xor(is.null(times), is.null(startTime))) {
        stop("You must specify exactly one of 'times' or 'startTime'")
    }

    if (!is.null(times)) {
        if (length(times) != ncol(table)) {
            stop("Length of 'times' must match number of columns in 'table'")
        }
    }

    if (!is.null(startTime)) {
        # If startTime is provided, we need to calculate times based on samplingRate
        if (is.null(samplingRate)) {
            stop("If 'startTime' is provided, 'samplingRate' must also be provided")
        }
        times <- startTime + seq(0, ncol(table) - 1) / samplingRate
    } else {
        # If times is provided, we need to estimate samplingRate
        estSamplingRate <- (length(times) -1)/ (times[length(times)] - times[1])
        if (is.null(samplingRate)){
            samplingRate <- estSamplingRate
        } else {
            if (abs(estSamplingRate - samplingRate) > 1e-6) {
                stop(
                    glue("Estimated sampling rate {estSamplingRate} does not match provided sampling rate {samplingRate}.")
                )
            }
        }
    }

    if (is.null(rowData)) {
        rowData <- data.frame()
    }
    if (is.null(colData)) {
        colData <- data.frame()
    }

    if (!is(rowData, "data.frame")) {
        stop("rowData must be a data.frame")
    }

    if (!is(colData, "data.frame")) {
        stop("colData must be a data.frame")
    }

    # set the time points of the table
    if (!is.null(times)) {
        colnames(table) <- times
    }

    # set the electrodes of the table
    if (!is.null(electrodes)) {
      if (!is.character(electrodes) || length(electrodes) != nrow(table)) {
        stop("`electrodes` must be a character vector of length nrow(table).", call. = FALSE)
      }
        rownames(table) <- electrodes
    } else if (!is.null(rownames(table))) {
        electrodes <- rownames(table)
    }

    # reserved metaData name
    metaData$samplingRate <- samplingRate

    # Create new Epoch object
    .Epoch(
        table = table,
        rowData = rowData,
        colData = colData,
        metaData = metaData
    )
}

.times <- function(x) {
    as.numeric(colnames(tblData(x)))
}

.samplingRate <- function(x) {
    if (!is.null(metaData(x)$samplingRate)) {
        return(metaData(x)$samplingRate)
    }
    stop("Sampling rate is not defined in metaData")
}

`.samplingRate<-` <- function(x, value) {
    metaData(x)$samplingRate <- value
    x
}


###############################
## other Methods
###############################

#' Obtain the time points for the Epoch matrix
#' 
#' @param x An Epoch object
#' @rdname coltimes-Epoch-method
#' @family Epoch methods
#' @export
setGeneric("coltimes", function(x) standardGeneric("coltimes"))

#' @rdname coltimes-Epoch-method
#' @examples 
#' # Create an Epoch object
#' epoch_data <- matrix(rnorm(1000), nrow = 10)
#' rownames(epoch_data) <- paste0("Electrode_", 1:10)
#' epoch <- Epoch(epoch_data, startTime = 0, samplingRate = 100)
#' 
#' # get the time points of an Epoch object
#' coltimes(epoch)
#' 
#' @return A numeric vector of time points, or column indices if time points are not defined
#' @export
setMethod("coltimes", "Epoch", function(x) {
    tms <- .times(x)
    if (!length(tms)) {
        tms <- seq_len(ncol(x))
    }
    tms
})
