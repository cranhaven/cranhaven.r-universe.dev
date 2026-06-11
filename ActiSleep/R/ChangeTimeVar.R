#' @title Reformat Time Variable
#'
#' @description Reformat the time variable to match analysis-ready format
#'
#' @param datain input dataset, must be a data frame
#' @param col_idx index of column to convert to date object
#' @param format format of input date data, default is YYYY-MM-DD HH:MM:SS
#'
#' @return dataframe with formatted time values
#' @export
#'
#' @examples
#' data("SleepDiary1Week")
#'
#' ChangeTimeVar(
#' SleepDiary1Week,
#' col_idx = c(5:18),
#' format = "%Y-%m-%d %H:%M:%S")
#'
ChangeTimeVar <- function(datain, col_idx, format = "%Y-%m-%d %H:%M:%S")
  {
  dataout <- datain

  # Reformat the time data
  for(i in col_idx)
    {
    dataout[,i] <- as.POSIXct(as.matrix(datain[,i]),
                              origin = "0001-01-01 00:00:00",
                              tz = "GMT",
                              format = format)
    }
  return(dataout)
  }
