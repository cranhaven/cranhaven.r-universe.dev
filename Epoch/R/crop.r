
#' Methods for Epoch class
#'
#' Truncating iEEG data to a specific time range.
#'
#' @param x An Epoch object
#' @param start Numeric value specifying start of new time range
#' @param end Numeric value specifying end of new time range
#' @param checkTimeRange Logical. Whether to check if the time range is correct. This includes (1) Check if the Epoch object has time information (2) check if `start` is less than or equal to `end`. (3) check if `start` and `end` are within the bounds of the available time points.(4) check if the time range will result in an empty Epoch. If `checkTimeRange = TRUE`, the function will raise a warning when the time range is not correct.
#' @param ... Not used
#' 
#' @return clip the time range of the Epoch object
#' @rdname crop-Epoch-method
#' @family Epoch methods
#' 
#' @examples 
#' # Create an Epoch object
#' epoch_data <- matrix(rnorm(1000), nrow = 10)
#' rownames(epoch_data) <- paste0("Electrode_", 1:10)
#' epoch <- Epoch(epoch_data, startTime = 0, samplingRate = 100)
#' 
#' # crop the epoch
#' crop(epoch, start = 0.5, end = 1.5)
#' 
#' @export
setGeneric("crop", function(x, start, end, ...) standardGeneric("crop"))

#' @rdname crop-Epoch-method
#' @param checkTimeRange Logical, whether to check the validity of the time range. This includes checking if the time range is empty, if `start` is greater than `end`, and if `start` or `end` are out of bounds. Default is TRUE.
#' @export
setMethod("crop", "Epoch", function(x, start, end, checkTimeRange = TRUE) {
    times <- coltimes(x)
    # Check 1: type validation
    if (!is.numeric(start) || !is.numeric(end)) {
        stop("crop function: `start` and `end` must be numeric.")
    }

    if (checkTimeRange) {
        # Check 2: empty times
        if (length(times) == 0) {
            warning("crop function: The Epoch object has no time information.")
        }

        # Check 3: logic error
        if (start > end) {
            warning("crop function: `start` is greater than `end`. Empty Epoch is returned.")
        }

        # Check 4: out-of-bounds time range
        if (start < min(times) || end > max(times)) {
            warning(glue::glue(
                "crop function: `start` or `end` is outside the available time range [{min(times)}, {max(times)}]."
            ))
        }
    }

    indices <- which(times >= start & times <= end)

    if (length(indices) == 0) {
        warning("crop function: No time points found in the specified range. Returning an empty Epoch object.")
    }

    x[, indices]
})
