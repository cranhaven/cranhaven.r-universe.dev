
#' @noRd
return_out_NAfilled <- function(N_days, N_valid_days, out_names_suffix, adjust_out_colnames){
  out_names <- c(
    'N_days', 'N_valid_days', 'wear_time_on_valid_days',
    'TAC', 'TLAC', 'LTAC',
    'astp', 'satp',
    'time_spent_active', 'time_spent_nonactive',
    'no_of_active_bouts', 'no_of_nonactive_bouts',
    'mean_active_bout', 'mean_nonactive_bout'
  )
  out <- data.frame(matrix(NA, nrow = 1, ncol = length(out_names)))
  names(out) <- out_names
  out$N_days <- N_days
  out$N_valid_days <- N_valid_days
  if (adjust_out_colnames){
    names(out)[-(1:3)] <- paste0(names(out)[-(1:3)], out_names_suffix)
  }
  names(out) <- tolower(names(out))
  return(out)
}



#' Compute physical activity summaries of minute level activity
#' data.
#'
#' @param acc A numeric vector. A minute-level activity counts data vector.
#' It is assumed
#' to be in midnight-to-midnight format, meaning its vector length
#' is a multiple of number of minutes in a full day
#' (1440). See \code{arctools::midnight_to_midnight()}.
#' @param acc_ts A POSIXct vector. Time of activity data collection, corresponding to
#' \code{acc} in its original format (not: midnight-to-midnight).
#' We strongly recommended to use \code{lubridate::ymd_hms()} function to
#' create \code{acc_ts} (see Examples below).
#' @param wear_flag An integer vector. It has value \code{1} if a minute
#' belongs to a wear time-interval, value \code{0} if a minute
#' belongs to a non-wear time-interval, and value \code{NA} to denote minutes
#' before/after data collection started/finished.
#' See \code{arctools::get_wear_flag()}.
#' @param valid_day_flag An integer vector. It has value \code{1} if a minute
#' belongs to a valid day, and \code{0} otherwise. See
#' \code{arctools::get_valid_day_flag()}.
#' @param sedentary_thresh A numeric scalar. If an activity count value falls
#' below it then a corresponding minute is characterized as sedentary; otherwise,
#' a corresponding minute is characterized as active. Default is \code{1853}.
#' @param subset_minutes Integer vector. Contains subset of a day's minutes
#' within which activity summaries are to be computed. May take values from
#' \code{1} (day's minute from 00:00 to 00:01) to
#' \code{1440} (day's minute from 23:59 to 00:00). Default is \code{NULL}, i.e.
#' no subset used (all day's minutes are used).
#' @param exclude_minutes Integer vector. Contains subset of a day's minutes
#' to be excluded from activity summaries computation.
#' May take values from
#' \code{1} (day's minute from 00:00 to 00:01) to
#' \code{1440} (day's minute from 23:59 to 00:00). Default is \code{NULL}, i.e.
#' no minutes excluded (all day's minutes are used).
#' @param subset_weekdays Integer vector. Specfies days of a week within which
#' activity summaries are to be computed. Takes values between 1 (Sunday) to
#' 7 (Saturday). Default is \code{NULL}, i.e.no subset used
#' (all days of a week are used).
#' @param in_bed_time A POSIXct vector. An estimated in-bed time start.
#' Together with a corresponding entry from \code{out_bed_time} vector,
#' it defines a
#' day-specific subset of "in bed time" minutes to be excluded from
#' activity summaries computation.  Default is \code{NULL}, i.e.
#' no minutes excluded.
#' @param out_bed_time A POSIXct vector. An estimated in-bed time end.
#' Together with a corresponding entry from \code{in_bed_time} vector,
#' it defines a
#' day-specific subset of "in bed time" minutes to be excluded from
#' activity summaries computation.  Default is \code{NULL}, i.e.
#' no minutes excluded.
#' @param adjust_out_colnames A logical scalar. Whether or not to
#' add an informative suffix to column names in the output data frame.
#' This may happen in case
#' any of the arguments:
#' \code{subset_minutes}, or
#' \code{exclude_minutes}, or
#' \code{in_bed_time} and \code{out_bed_time}
#' are set other than \code{NULL}.
#' Default is \code{TRUE}.
#'
#' @return A data frame with physical activity summaries of minute level activity
#' data. See README or vignette for summaries description.
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
#'
#' ## Example 1
#' ## Summarize PA
#' summarize_PA(acc, acc_ts, wear_flag, valid_day_flag)
#'
#' ## Example 2
#' ## Summarize PA within minutes range corresponding to 12:00 AM - 6:00 AM
#' subset_12am_6am <- 1 : (6 * 1440/24)
#' summarize_PA(acc, acc_ts, wear_flag, valid_day_flag, subset_minutes = subset_12am_6am)
#'
#' ## Example 3
#' ## Summarize PA without (i.e., excluding) minutes range corresponding to 11:00 PM - 5:00 AM.
#' subset_11pm_5am <- c(
#'   (23 * 1440/24 + 1) : 1440,   ## 11:00 PM - midnight
#'   1 : (5 * 1440/24)            ## midnight - 5:00 AM
#' )
#' summarize_PA(acc, acc_ts, wear_flag, valid_day_flag, exclude_minutes = subset_11pm_5am)
#'
summarize_PA = function(
  acc,
  acc_ts,
  wear_flag,
  valid_day_flag,
  sedentary_thresh = 1853,
  subset_minutes = NULL,
  exclude_minutes = NULL,
  subset_weekdays = NULL,
  in_bed_time = NULL,
  out_bed_time = NULL,
  adjust_out_colnames = TRUE)
{

  ## Checks for arguments
  arg_check_acc_m2m(acc)
  arg_check_acc_ts(acc_ts)
  arg_check_wear_flag(wear_flag)
  arg_check_valid_day_flag(valid_day_flag)
  arg_check_minutes_subset(subset_minutes)
  arg_check_minutes_subset(exclude_minutes)
  arg_check_bed_time(in_bed_time, out_bed_time)
  arg_check_subset_weekdays(subset_weekdays)

  out_names_suffix <- ""

  N_valid_days <- sum(valid_day_flag) / 1440
  N_days       <- length(acc) / 1440

  ## SUBSET (A): EXCLUDE MINUTES OF SLEEP BASED ON ACTILIFE-ESTIMATED IN/OUT BED TIME
  if (!is.null(in_bed_time) & !is.null(out_bed_time)){
    ## Transform ActiLife timestamp data to POSIXct objects,
    ## convert timestamp data to midnight-to-midnight long vector
    acc_ts_mtm <- midnight_to_midnight(acc = acc_ts, acc_ts = acc_ts)
    # Mask out in-bed-intervals
    for(j in 1:length(in_bed_time)){ # j <- 1
      acc[acc_ts_mtm >= in_bed_time[j] & acc_ts_mtm <= out_bed_time[j]] = NA
    }
    out_names_suffix <- paste0(out_names_suffix, '_InBedRemoved')
  }

  ## SUBSET (B): EXCLUDE FIXED MINUTES
  if (!is.null(exclude_minutes)){
    ## Vector to [n_days x n_minutes_per_day] form
    acc_mat <- matrix(acc, ncol = 1440, byrow = TRUE)
    ## Replace values on elements other than "subset_minutes" with NA
    acc_mat[, exclude_minutes] <- NA
    acc <- as.vector(t(acc_mat))
    ## Define output summary colnames suffix based on subset
    ## starting hour, finish hour
    h_start <- round(exclude_minutes[1] * 24/1440)
    h_end   <- round(exclude_minutes[length(exclude_minutes)] * 24/1440)
    out_names_suffix <- paste0(out_names_suffix, '_', h_start, 'to', h_end, 'Removed')
  }

  ## SUBSET (C): USE FIXED MINUTES ONLY
  if (!is.null(subset_minutes)){
    ## Vector to [n_days x n_minutes_per_day] form
    acc_mat <- matrix(acc, ncol = 1440, byrow = TRUE)
    ## Replace values on elements other than "subset_minutes" with NA
    acc_mat[, -subset_minutes] <- NA
    acc <- as.vector(t(acc_mat))
    ## Define output summary colnames suffix based on subset
    ## starting hour, finish hour
    h_start <- round(subset_minutes[1] * 24/1440)
    h_end   <- round(subset_minutes[length(subset_minutes)] * 24/1440)
    out_names_suffix <- paste0(out_names_suffix, '_', h_start, 'to', h_end, 'Only')
  }

  ## SUBSET (D): USE FIXED WEEKDAYS ONLY
  ## (case different than the above as we exclude full days here)
  if (!is.null(subset_weekdays)){
    # Define mapping of timestamp vector onto unique day dates
    acc_ts_unq_date <- sort(unique(date(acc_ts)))
    acc_ts_unq_date_weekday <- wday(acc_ts_unq_date)
    ## 1. UPDATE acc
    ## Replace with NA values on elements corresponding to days other than provided subset
    acc_mat <- matrix(acc, ncol = 1440, byrow = TRUE)
    acc_mat[which(!(acc_ts_unq_date_weekday %in% subset_weekdays)), ] <- NA
    acc <- as.vector(t(acc_mat))
    ## 2. UPDATE valid_day_flag
    valid_day_flag_mat <- matrix(valid_day_flag, ncol = 1440, byrow = TRUE)
    valid_day_flag_mat[which(!(acc_ts_unq_date_weekday %in% subset_weekdays)), ] <- 0
    valid_day_flag <- as.vector(t(valid_day_flag_mat))
    ## 3. UPDATE N_days
    N_days       <- sum(acc_ts_unq_date_weekday %in% subset_weekdays)
    ## 4. UPDATE N_valid_days
    N_valid_days <- sum(valid_day_flag_mat) / 1440
    ## Define output summary colnames suffix based on subset
    weekdays_sub <- paste0(sort(unique(subset_weekdays)), collapse = "")
    out_names_suffix <- paste0(out_names_suffix, '_Weekdays', weekdays_sub, 'Only')
  }

  ## Replace non-valid days with NA
  acc_validOnly <- acc
  acc_validOnly[which(valid_day_flag == 0)] <- NA

  ## If no valid days, return empty data frame
  if (N_valid_days == 0){
    message("0 valid days identified. Returning data frame with NAs.")
    out <- return_out_NAfilled(N_days, N_valid_days, out_names_suffix, adjust_out_colnames)
    return(out)
  }

  ## If all values are NA, return empty data frame (this can happen i.e. when
  ## we want to compute data from weekends only but there is no weekend data)
  if (all(is.na(acc_validOnly))){
    message("0 days to compute summaries from. Returning data frame with NAs.")
    out <- return_out_NAfilled(N_days, N_valid_days, out_names_suffix, adjust_out_colnames)
    return(out)
  }

  ## COMPUTE PA VOLUME STATISTICS
  TAC  <- sum(acc_validOnly, na.rm = TRUE)/N_valid_days
  TLAC <- sum(log(1 + acc_validOnly), na.rm = TRUE)/N_valid_days
  LTAC <- log(TAC)

  ## Compute wear time on valid days
  wear_time_on_valid_days <- sum(wear_flag[valid_day_flag == 1], na.rm = T)/N_valid_days


  ## COMPUTE PA FRAGMENTATION METRICS
  is_active <- (acc_validOnly >= sedentary_thresh) * 1
  rle_out <- rle(is_active)
  ## Vector of lengths of all active bouts
  actbout_len_vec  <- rle_out$lengths[which(rle_out$values == 1)]
  ## Vector of lengths of all non-active bouts
  nonactbout_len_vec <- rle_out$lengths[which(rle_out$values == 0)]

  if (length(actbout_len_vec) == 0){
    astp <- NA
    time_spent_active  <- 0
    no_of_active_bouts <- 0
    mean_active_bout   <- 0
  } else {
    astp <- 1/mean(actbout_len_vec)
    time_spent_active  <- sum(actbout_len_vec)/N_valid_days
    no_of_active_bouts <- length(actbout_len_vec)/N_valid_days
    mean_active_bout   <- mean(actbout_len_vec)
  }

  if (length(nonactbout_len_vec) == 0){
    satp <- NA
    time_spent_nonactive  <- 0
    no_of_nonactive_bouts <- 0
    mean_nonactive_bout   <- 0
  } else {
    satp <- 1/mean(nonactbout_len_vec)
    time_spent_nonactive  <- sum(nonactbout_len_vec)/N_valid_days
    no_of_nonactive_bouts <- length(nonactbout_len_vec)/N_valid_days
    mean_nonactive_bout   <- mean(nonactbout_len_vec)
  }

  ## Define output data frame
  out <- data.frame(
    N_days, N_valid_days, wear_time_on_valid_days,
    TAC, TLAC, LTAC,
    astp, satp,
    time_spent_active, time_spent_nonactive,
    no_of_active_bouts, no_of_nonactive_bouts,
    mean_active_bout, mean_nonactive_bout
  )

  ## Format column names
  if (adjust_out_colnames){
    names(out)[-(1:3)] <- paste0(names(out)[-(1:3)], out_names_suffix)
  }
  names(out) <- tolower(names(out))

  return(out)
}
