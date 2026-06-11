#' @title Diary In/Out-bed Times
#'
#' @description Obtain table of in-bed and out-bed times from sleep diary data
#'
#' @param datain input dataset, must be a data frame
#' @param subj_idx index indicating subject
#' @param idx_bed array specifying indices for in-bed time data
#' @param idx_wake array specifying indices for out-bed time data
#' @param tz timezone, default is GMT
#'
#' @return dataframe with in-bed and out-bed times
#' @export
#'
#' @examples
#' data("SleepDiary1Week")
#'
#' SleepDiary1Week <-
#' ChangeTimeVar(SleepDiary1Week, c(5:18), format = "%m/%d/%Y %H:%M")
#'
#' colIdx_diary_bed <- c(1:7) * 2 - 2 + 5
#' colIdx_diary_wake <- c(1:7) * 2 - 1 + 5
#'
#' GetDiary_subj(
#' SleepDiary1Week,
#' 1,
#' colIdx_diary_bed,
#' colIdx_diary_wake,
#' tz = "GMT")
#'
GetDiary_subj <- function(datain, subj_idx, idx_bed, idx_wake, tz = "GMT")
  {
  datain <- data.frame(datain)
  diary_bed <- rep(as.POSIXct(datain[subj_idx, idx_bed[1]], tz = tz), 7)
  diary_wake <- rep(as.POSIXct(datain[subj_idx, idx_wake[1]], tz = tz), 7)

  # Populate child's sleep diary info
  for(d in 1:7)
    {
    diary_bed[d] <- as.POSIXct(datain[subj_idx, idx_bed[d]], tz = tz)
    diary_wake[d] <- as.POSIXct(datain[subj_idx, idx_wake[d]], tz = tz)
    }

  # Create a table of in-bed and out-bed times
  diary_time <- data.frame(bed = diary_bed, wake = diary_wake)
  return(diary_time)
  }
