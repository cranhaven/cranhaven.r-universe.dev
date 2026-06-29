
#' Compute physical activity summaries of minute level activity
#' data
#'
#' Process minute level actigraphy-measured activity counts data and extract
#' commonly used physical activity volume and fragmentation metrics.
#'
#' @details
#' Physical activity statistics are aggregated from "valid" days, i.e. days
#' with no more than 10% of the non-wear time. Wear time is determined based on
#' wear/non-wear detection algorithm closely
#' following that of Choi et al. (2011).
#' See \code{arctools::get_wear_flag()} for details.
#'
#' Data imputation is recommended for valid days for non-wear time periods and
#' is a default setting (see \code{impute_missing} arg).
#' Count values are imputed
#' from an "average day profile" -- a minute-specific activity counts average
#' computed across valid days within wear time.
#'
#' @param acc A numeric vector. A minute-level activity counts data vector.
#' @param acc_ts A POSIXct vector. A minute-level time of \code{acc}
#' data collection. We strongly recommended to use \code{lubridate::ymd_hms()}
#' function to create \code{acc_ts} (see Examples below).
#' @param impute_missing A logical scalar. Whether or not to perform missing
#' data imputation (see Details). Default is \code{TRUE}.
#' @param sedentary_thresh A numeric scalar. If an activity count value falls
#' below it then a corresponding minute is characterized as sedentary; otherwise,
#' a corresponding minute is characterized as active. Default is \code{1853}.
#' @param nonwear_0s_minimum_window A numeric scalar. A minimum number of consecutive
#' minutes with 0 activity count to be considered non-wear.
#' @param validday_nonwear_maximum_window In integer scalar. Maximum number of minutes of non-wear/not
#' collecting data so as the day is still considered valid. Default is \code{144}
#' (10\% of 1440 minutes of a full day).
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
#' fpath_i <- system.file("extdata", extdata_fnames[1], package = "arctools")
#' dat_i   <- as.data.frame(data.table::fread(fpath_i))
#' acc     <- dat_i$vectormagnitude
#' acc_ts  <- lubridate::ymd_hms(dat_i$timestamp)
#'
#' ## Example 1
#' ## Summarize PA
#' activity_stats(acc, acc_ts)
#'
#' ## Example 2
#' ## Summarize PA within minutes range corresponding to 12:00 AM - 6:00 AM
#' subset_12am_6am <- 1 : (6 * 1440/24)
#' activity_stats(acc, acc_ts, subset_minutes = subset_12am_6am)
#'
#' ## Example 3
#' ## Summarize PA without (i.e., excluding) minutes range corresponding to 11:00 PM - 5:00 AM.
#' subset_11pm_5am <- c(
#'   (23 * 1440/24 + 1) : 1440,   ## 11:00 PM - midnight
#'   1 : (5 * 1440/24)            ## midnight - 5:00 AM
#' )
#' activity_stats(acc, acc_ts, exclude_minutes = subset_11pm_5am)
#'
#' @references
#' Varma, V. R., Dey, D., Leroux, A., Di, J., Urbanek, J., Xiao, L., Zipunnikov, V.
#' (2018). Total volume of physical activity: TAC, TLAC or TAC(lambda).
#' Preventive medicine, 106, 233–235. https://doi.org/10.1016/j.ypmed.2017.10.028
#'
#' Di, J., Leroux, A., Urbanek, J., Varadhan, R., Spira, A., Schrack, J.,
#' Zipunnikov, V. Patterns of sedentary and active time accumulation are associated
#' with mortality in US adults: The NHANES study. https://doi.org/10.1101/182337
#'
#' Choi, L., Liu, Z., Matthews, C. E., & Buchowski, M. S. (2011). Validation of
#' accelerometer wear and nonwear time classification algorithm. Medicine and
#' Science in Sports and Exercise. https://doi.org/10.1249/MSS.0b013e3181ed61a3
#'
#' Koster, A., Shiroma, E. J., Caserotti, P., Matthews, C. E., Chen, K. Y.,
#' Glynn, N. W., & Harris, T. B. (2016). Comparison of Sedentary Estimates
#' between activPAL and Hip- and Wrist-Worn ActiGraph. Medicine and science in
#' sports and exercise, 48(8), 1514–1522. https://doi.org/10.1249/MSS.0000000000000924
#'
activity_stats <- function(
  acc,
  acc_ts,
  impute_missing = TRUE,
  sedentary_thresh = 1853,
  nonwear_0s_minimum_window = 90,
  validday_nonwear_maximum_window = 144,
  subset_minutes = NULL,
  exclude_minutes = NULL,
  subset_weekdays = NULL,
  in_bed_time = NULL,
  out_bed_time = NULL,
  adjust_out_colnames = TRUE
)
{

  ## Checks for arguments
  arg_check_acc_ts(acc_ts)
  arg_check_acc_and_acc_ts(acc, acc_ts)
  arg_check_minutes_subset(subset_minutes)
  arg_check_minutes_subset(exclude_minutes)
  arg_check_bed_time(in_bed_time, out_bed_time)

  ## Get acc data vector in "midnight_to_midnight" format
  acc <- midnight_to_midnight(acc, acc_ts)

  ## Get wear/non-wear flag
  wear_flag <- get_wear_flag(acc, nonwear_0s_minimum_window = nonwear_0s_minimum_window)

  ## Get valid/non-valid day flag
  valid_day_flag <- get_valid_day_flag(wear_flag, validday_nonwear_maximum_window = validday_nonwear_maximum_window)

  ## Impute missing data in acc data vector
  if (impute_missing){
    acc <- impute_missing_data(acc, wear_flag, valid_day_flag)
  }

  ## Summarize PA
  out <- summarize_PA(
    acc = acc,
    acc_ts = acc_ts,
    wear_flag = wear_flag,
    valid_day_flag = valid_day_flag,
    sedentary_thresh = sedentary_thresh,
    subset_minutes = subset_minutes,
    exclude_minutes = exclude_minutes,
    subset_weekdays = subset_weekdays,
    in_bed_time = in_bed_time,
    out_bed_time = out_bed_time,
    adjust_out_colnames = adjust_out_colnames)

  return(out)
}
