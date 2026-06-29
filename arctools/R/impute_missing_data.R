

#' Impute missing data
#'
#' Impute missing data in minute-level activity counts data vector based on
#' "average day profile".
#'
#' @details
#' An "average day profile" is computed as average across minutes identified
#' as wear and from valid days
#' (see param. \code{imputeFromValidDaysOnly}). Activity counts data are imputed
#' from "average day profile" for minutes identified as non-wear in days
#' identified as valid, except for minutes before/after data
#' collection start/end which remain \code{NA}.
#'
#' Theoretically, it is possible that all valid days of data collection have
#' non-wear flag for the some minute(s) (i.e., somebody is always
#' taking off the watch for the same few minutes during a day) so there is no
#' data to use to compute imputation values from. If it happens, then method
#' uses 0 as imputation value(s).
#'
#' @param acc A numeric vector. A minute-level activity counts data vector.
#' It is assumed
#' to be in midnight-to-midnight format, meaning its vector length
#' is a multiple of number of minutes in a full day
#' (1440; see \code{midnight_to_midnight()}).
#'
#' @param wear_flag An integer vector.
#' Wear/non-wear flag (\code{1}/\code{0}) for each minute of activity
#' counts data. It is assumed
#' to be in midnight-to-midnight format, meaning its vector length
#' is a multiple of number of minutes in a full day
#' (1440). See \code{midnight_to_midnight()}, \code{get_wear_flag()}.
#'
#' @param valid_day_flag An integer vector.
#' Valid/non-valid day flag (\code{1}/\code{0}) for each minute of activity
#' counts data. It is assumed
#' to be in midnight-to-midnight format, meaning its vector length
#' is a multiple of number of minutes in a full day
#' (1440).
#'  See \code{arctools::midnight_to_midnight()},
#'  \code{arctools::get_valid_day_flag()}.
#'
#' @param imputeFromValidDaysOnly A logical scalar. Whether or not data from
#' valid days only should be used for computing "average day profile"
#' used for imputation.
#'
#' @return A numeric vector. A minute-level activity counts data vector
#' with data imputed for minutes identified as non-wear in days identified as valid
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
#' ## Impute missing data in acc data vector
#' acc_imputed <- impute_missing_data(acc, wear_flag, valid_day_flag)
#' ## Compare mean acc before/after imputation
#' c(mean(acc, na.rm = TRUE), mean(acc_imputed, na.rm = TRUE))
#'
impute_missing_data <- function(acc, wear_flag, valid_day_flag, imputeFromValidDaysOnly = TRUE){

  ## Argument checks
  arg_check_acc_m2m(acc)
  arg_check_wear_flag(wear_flag)
  arg_check_valid_day_flag(valid_day_flag)

  ## Return acc if no non-wear
  if (!any(wear_flag == 0, na.rm = TRUE)){
    return(acc)
  }

  ## Return acc if no valid days
  valid_days_cnt <- sum(valid_day_flag) / 1440
  if (valid_days_cnt == 0){
    return(acc)
  }

  ## Put data vector into [ndays x 1440] matrix matrix
  ## where eacch row corresponds to data collected during one full day
  acc1440  <- matrix(acc,  ncol = 1440, byrow = TRUE)
  wear_flag1440 <- matrix(wear_flag, ncol = 1440, byrow = TRUE)
  valid_day_flag1440  <- matrix(valid_day_flag,  ncol = 1440, byrow = TRUE)

  ## Compute accross-day means to substitute for missing data using only
  ## (1) wear-time flagged observations
  ## (2) (default `imputeFromValidDaysOnly` param value) valid days only
  acc1440_colmean_mask <- ifelse((wear_flag1440 == 1), 1, NA)
  if (imputeFromValidDaysOnly){
    acc1440_colmean_mask <- ifelse((acc1440_colmean_mask == 1) & (valid_day_flag1440 == 1), 1, NA)
  }
  acc1440_colmean <- apply(acc1440 * acc1440_colmean_mask, 2, mean, na.rm = TRUE)
  ## Replacce NA in accross-day means vector with 0's
  ## (theoretically possible; very rare potential corner condition in pracctice)
  acc1440_colmean[is.na(acc1440_colmean)] <- 0

  ## In eacch iteration, check condition for minutes of iteration-specific day:
  ## (1) minute is from a valid day,
  ## (2) non-wear occurs in this minute
  ## and impute if (1) and (2) are met.
  for (i in 1:nrow(acc1440)){ # i <- 1
    i_cond     <- (valid_day_flag1440[i, ] == 1) * (wear_flag1440[i, ] == 0)
    i_cond_idx <- which(i_cond == 1)
    if (length(i_cond_idx) == 0) next   ## Nothing to replacce
    acc1440[i, i_cond_idx] <- acc1440_colmean[i_cond_idx]
  }

  ## Collapse [ndays x 1440] matrix into a vector
  acc_imputed <- as.vector(t(acc1440))

  return(acc_imputed)
}



