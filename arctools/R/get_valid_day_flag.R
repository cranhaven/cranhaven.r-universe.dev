
#' Compute valid day flag
#'
#' Compute valid/non-valid day flag (\code{1}/\code{0}) for each minute of activity
#' counts data.
#'
#' @details
#' All minute-level observations
#' from one day are assigned the same value of valid day flag.
#' The flag is \code{1} if a day is
#' determined to be valid, and \code{0} otherwise.
#'
#' A day is determined to be valid if it has no more than
#' \code{validday_nonwear_maximum_window} minutes of missing data.
#' Data may be missing due to
#' identified sensor nonwear or because activity data collection has not started
#' yet/has finished already in a particular day.
#'
#' @param wear_flag An integer vector. The vector has value \code{1} if a minute
#' belongs to a wear time-interval, value \code{0} if a minute
#' belongs to a non-wear time-interval, and value \code{NA} to denote minutes
#' before/after data collection started/finished.
#'
#' Vector \code{wear_flag} is assumed
#' to be in midnight-to-midnight format, meaning its vector length
#' is a multiple of number of minutes in a full day
#'  (1440).
#'  See \code{arctools::midnight_to_midnight()},
#'  \code{arctools::get_wear_flag()}.
#'
#' @param validday_nonwear_maximum_window In integer scalar. Maxmimum number of minutes of non-wear/not
#' collecting data so as the day is still considered valid. Default is \code{144}
#' (10\% of 1440 minutes of a full day).
#'
#' @return An integer vector. It has value \code{1} if a minute
#' belongs to a valid day, and \code{0} otherwise.
#'
#' @export
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
#' ## Get valid/non-valid day flag
#' valid_day_flag <- get_valid_day_flag(wear_flag)
#'
get_valid_day_flag <- function(wear_flag, validday_nonwear_maximum_window = 144){

  ## Argument checks
  arg_check_wear_flag(wear_flag)

  ## Arrange valid/non-valid vector into a matrix where each row has
  ## 1440 entries and correspond to one full day
  wear_flag_fullday_mat <- matrix(wear_flag, ncol = 1440, byrow = TRUE)

  ## Number of valid minute-level observations per day
  valid_obs_cnt  <- apply(wear_flag_fullday_mat, MARGIN = 1, sum, na.rm = TRUE)

  ## Valid days minute-level flag
  valid_days     <- (valid_obs_cnt >= (1440 - validday_nonwear_maximum_window)) * 1
  valid_day_flag <- rep(valid_days, each = 1440)

  return(valid_day_flag)
}


get_valid_days_cnt <- function(valid_day_flag){
  valid_minutes_cnt <- sum(valid_day_flag)
  valid_days_cnt <- valid_minutes_cnt / 1440
  return(valid_days_cnt)
}
