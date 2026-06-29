
#' Compute wear/non-wear flag
#'
#' Compute wear/non-wear flag (\code{1}/\code{0}) for each minute of activity
#' counts data.
#'
#' @details
#' Method implements wear/non-wear detection algorithm closely following that of
#' Choi et al. (2011).
#'
#' The wear/non-wear flag is determined based on activity counts data.
#' A minute is classified as non-wear if it belongs to any
#' \code{nonwear_0s_minimum_window} minutes-long interval of consecutive values 0 in
#' activity counts data vector;
#' here, "any interval" implies that a particular minute may be located
#' at any location (beginning, middle, end) of interval of consecutive values 0
#' to be classified as a non-wear. Otherwise, a particular minute is classified
#' as wear.
#'
#' Similarly to recommendations in Discussion in Choi et al. (2011), the method
#' assumes
#' a threshold value of 0 for nonzero counts allowed during
#' a nonwear time interval (I.e., no activity count equal >= 1 is allowed).
#' The method also assumes 90 minutes as a default for
#' minimum time of consecutive zero counts for a window to be flagged nonwear.
#' Differently from recommendations in Discussion in Choi et al. (2011), it
#' does not
#' consider any "artifactual movement" interval of nonzero counts during
#' a nonwear time interval.
#'
#' @param acc A numeric vector. A minute-level activity counts data vector.
#' @param nonwear_0s_minimum_window A numeric scalar. A minimum number of consecutive
#' minutes with 0 activity count to be considered non-wear.
#'
#' @return An integer vector. It has value \code{1} for a wear
#' and \code{0} for non-wear flagged minute. It has the same vector length as
#' \code{acc} vector. If there is an \code{NA} entry in \code{acc} vector,
#' then the returned vector will have a corresponding entry set to  \code{NA} too.
#'
#' @importFrom runstats RunningMean
#' @export
#'
#' @references
#' Choi, L., Liu, Z., Matthews, C. E., & Buchowski, M. S. (2011). Validation of
#' accelerometer wear and nonwear time classification algorithm. Medicine and
#' Science in Sports and Exercise. https://doi.org/10.1249/MSS.0b013e3181ed61a3
#'
#' @examples
#' ## Read exemplary data
#' fpath_i <- system.file("extdata", extdata_fnames[1], package = "arctools")
#' dat_i   <- as.data.frame(data.table::fread(fpath_i))
#' acc     <- dat_i$vectormagnitude
#' acc_ts  <- lubridate::ymd_hms(dat_i$timestamp)
#' ## Get acc data vector in "midnight_to_midnight" format
#' acc <- midnight_to_midnight(acc, acc_ts)
#' ## Get wear/non-wear flag
#' wear_flag <- get_wear_flag(acc)
#'
get_wear_flag <- function(acc, nonwear_0s_minimum_window = 90){

  ## Define output vector (tentatively filled with 1's wear flag only)
  out <- rep(1, length(acc))

  ## Define vector with 1 if there is a non-zero value in accelerometry data
  ## vector and 0 otherwise
  ac_non0 <- (acc > 0) * 1
  ## Technical: replace NAs with 0 for RunningMean computation
  ac_non0[is.na(ac_non0)] <- 0

  ## Compute running mean vector. Each element is a value of mean computed
  ## within `nonwear_0s_minimum_window`-long interval starting at the element's index
  ac_non0_runningMean <- runstats::RunningMean(x = ac_non0, W = nonwear_0s_minimum_window, circular = FALSE)

  ## Get indices that are a 1st element of a `nonwear_0s_minimum_window`-long interval of consecutive zeros
  ## (1st element of a non-wear interval)
  nonwear_idx1_vec <- which(ac_non0_runningMean < 1/nonwear_0s_minimum_window)

  ## For each identified 1st index of a `nonwear_0s_minimum_window`-long non-wear interval,
  ## define a non-wear flag for all indices of that `nonwear_0s_minimum_window`-long non-wear interval
  ## in the output vector
  for (idx1 in nonwear_idx1_vec){
    idx_all <- idx1 + seq(0, by = 1, length.out = nonwear_0s_minimum_window)
    out[idx_all] <- 0
  }

  ## Replace with NAs for corresponding NAs in input vector
  out[is.na(acc)] <- NA

  return(out)
}
