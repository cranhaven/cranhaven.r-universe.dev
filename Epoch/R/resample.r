#' Generic function for resampling objects
#' 
#' This function allows you to resample an object to a different sampling frequency.
#' 
#' @rdname resample-Epoch-method
#' @export
setGeneric("resample", function(x, ...) standardGeneric("resample"))


#' Resample an Epoch object to a different frequency
#' 
#' This function allows you to resample an Epoch object to a different sampling frequency.
#' 
#' @param x An `Epoch` object to be resampled.
#' @param samplingRate The new sampling frequency (unit: Hertz).
#' @param ... Additional arguments passed to `gsignal::resample`
#' 
#' @return An `Epoch` object with the resampled data.
#' 
#' @examples 
#' # Create an Epoch object
#' epoch_data <- matrix(rnorm(1000), nrow = 10)
#' rownames(epoch_data) <- paste0("Electrode_", 1:10)
#' epoch <- Epoch(epoch_data, startTime = 0, samplingRate = 100)
#' 
#' # downsample the epoch to 50 Hz
#' resample(epoch, samplingRate = 50)
#' 
#' # upsample the epoch to 200 Hz
#' resample(epoch, samplingRate = 200)
#' 
#' @rdname resample-Epoch-method
#' @family Epoch methods
#' @export 
setMethod("resample", "Epoch", function(x, samplingRate, ...) {
  
    if (!is.numeric(samplingRate) || samplingRate <= 0) {
      stop("samplingRate must be a positive numeric value.")
    }
  
    oldSamplingRate <- .samplingRate(x)
    electrodes <- rownames(x)
    timeRange <- range(coltimes(x))
    ntimes <- length(coltimes(x))
    newTimes <- seq(timeRange[1], timeRange[2], length.out = ntimes * samplingRate / oldSamplingRate)
    mat <- tblData(x)
    colnames(mat) <- NULL
    
    colMeta <- colData(x)
    if (!is.null(colMeta) && nrow(colMeta) > 0) {
        warning("Column metadata will be lost during resampling. Consider re-adding it after resampling.")
        colData(x) <- NULL
    }
    newMat <- gsignal::resample(
        t(mat),
        p = samplingRate,
        q = oldSamplingRate,
        ...
    )
    newMat <- t(newMat)
    colnames(newMat) <- newTimes
    tblData(x) <- newMat
    .samplingRate(x) <- samplingRate
    x
})
