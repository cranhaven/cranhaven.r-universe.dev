
#' Expand activity data vector into midnight-to-midnight format
#'
#' Expand activity data vector such that its length is
#' a multiple of number of minutes in a full day (1440).
#'
#' @details
#' In the returned vector,
#' 1st observation
#' corresponds to minute of 00:00-00:01 on the first day of data collection,
#' and last observation
#' corresponds to minute of 23:59-00:00 on the last day of data collection.
#' Entries corresponding to no data in original activity data vector
#' are filled with \code{NA}.
#'
#' @param acc A numeric vector. A minute-level activity counts data vector.
#' @param acc_ts A POSIXct vector. Time of activity data collection, corresponding to
#' \code{acc}. We strongly recommended to use \code{lubridate::ymd_hms()} function to create \code{acc_ts} (see Examples below).
#'
#' @return A numeric vector. A minute-level activity counts data vector in
#' midnight-to-midnight format.
#'
#' @import lubridate
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
#' ## Observe we have an integer number of days
#' length(acc) / 1440
#'
midnight_to_midnight <- function(acc, acc_ts){

  ## Argument check
  arg_check_acc_ts(acc_ts)

  ## Added 2021-01-06 as protective measure in case one fails to submit
  ## acc_ts as lubridate::ymd_hms()-generated is.POSIXct
  ## (note: base::as.POSIXct()-generated IS NOT OK!)
  acc_ts <- lubridate::ymd_hms(acc_ts)

  ## Define day-specific minute for each observation
  obs_df <- data.frame(acc = acc, acc_ts = acc_ts, stringsAsFactors = FALSE)
  obs_df$ts_date_idx    <- as.integer(factor(as.Date(obs_df$acc_ts)))
  obs_df$ts_minute_idx  <- as.integer(lubridate::hour(obs_df$acc_ts) * 60 + lubridate::minute(obs_df$acc_ts) + 1)

  ## Filter observation data frame to keep unique timestamp observations only
  ## (handle rare margin case of duplicates with fall daylight time change)
  obs_df_nodup <- obs_df[!duplicated(obs_df[c("acc_ts")]), ]
  if (nrow(obs_df_nodup) != nrow(obs_df)){
    dup_removed <- nrow(obs_df) - nrow(obs_df_nodup)
    warning(paste0("Removed ", dup_removed, " timestamp duplicate observations."))
  }

  ## Define data frame with full days grid (date index, minute index)
  grid_df <- expand.grid(unique(obs_df$ts_date_idx), 1:1440)
  names(grid_df) <- c("ts_date_idx", "ts_minute_idx")

  ## Merge full days grid data frame with observation data frame
  out_df <- merge(x = grid_df, y = obs_df_nodup,
                  by = c("ts_date_idx", "ts_minute_idx"),
                  all.x = TRUE, all.y = FALSE)

  ## Sort merged data frame by (date index, minute index)
  out_df <- out_df[order(out_df$ts_date_idx, out_df$ts_minute_idx), ]

  ## Return midnight-to-midnight vector of activity counts
  out_vec <- out_df$acc
  return(out_vec)
}
