#' @keywords internal
#' @noRd

Hampel_flt <- function(var) {
  I_low <- max(median(var,na.rm = T) - 3*mad(var,na.rm = T), min(var,na.rm=T))
  I_upp <- min(median(var,na.rm = T) + 3*mad(var,na.rm = T), max(var,na.rm=T))
  outlier_ind_low <- which(var < I_low)
  outlier_ind_upp <- which(var > I_upp)

  return(list(I_low = I_low,I_upp = I_upp,
              outlier_ind_upp = outlier_ind_upp,
              outlier_ind_low = outlier_ind_low))
}

# The Hampel Filter detects and removes the outliers of the input signal by using the Hampel
# identifier. The Hampel identifier is a variation of the three-sigma rule of statistics,
# which is robust against outliers. For each sample of the input signal, the block computes the
# median of a window composed of the current sample and Length−12 adjacent samples on each side of
# the current sample. Len is the window length you specify through the Window length parameter.
# The block also estimates the standard deviation of each sample about its window median by using
# the median absolute deviation. If a sample differs from the median by more than the threshold
# multiplied by the standard deviation, the filter replaces the sample with the median.
