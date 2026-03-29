# filtering_functions.R
# a file with a few option for filtering output data.
#---------------------------------
#' Apply a median absolute deviation filter
#'
#' Median absolute deviation filter of Brock 1986, with user specified
#' width and magnitude thresholds.
#'
#' @author Rich Fiorella \email{rfiorella@@lanl.gov}
#'
#' @param data Vector to filter.
#' @param width Width of filter, in rows.
#' @param threshold Only filter values that are `abs(threshold)`
#'              away from median
#'
#' @return Returns filtered vector.
#'
#' @importFrom stats median
filter_median_brock86 <- function(data, width = 7, threshold = 5) {

  # get rolling median of data.
  filt <- zoo::rollapply(data, width, median, na.rm = TRUE, fill = NA)

  # get logical vector of what indices are valid.
  spikes <- abs(data - filt) > threshold

  # set spikes as missing - consider interpolation in future release?
  data[spikes == TRUE] <- NA

  # return data vector back
  return(data)

}
