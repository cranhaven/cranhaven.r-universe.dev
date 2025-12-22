#' @title
#' Calculation of daily, monthly and annual scales
#'
#' @description
#' Performs data processing on an hourly scale for daily, monthly or annual scales
#'
#' @param data Data frame containing the data
#' @param col_date String with the column of data containing the date (R default date: "\%Y-\%m-\%d")
#' @param col_sum String with the column of data to apply the sum process
#' @param col_mean String with the column of data to apply the averaging process
#' @param col_max String with data column to find maximum
#' @param col_min String with data column to find minimum
#' @param n.round Integer, number of decimal places
#' @param type string, receives "Daily", "Monthly" or "Yearly" ("Daily" default). Defines the scale of processing to be performed
#'
#' @return
#' Data frame with the defined scale
#'
#' @export
#'
#' @import dplyr
#' @import rlang
#'
#' @examples
#' address <-
#'  base::system.file("extdata",
#'                     "ex1_inmet.CSV",
#'                     package = "DataMetProcess")
#'
#' df <-
#' read.table(
#'   address,
#'   h=TRUE,
#'   sep = ";",
#'   dec = ",",
#'   skip = 8,
#'   na.strings = -9999,
#'   check.names = FALSE
#' )
#'
#' df$Data = as.Date(df$Data,format = "%d/%m/%Y")
#'
#' df.d <-
#'   calculateDMY(
#'     data = df,
#'     col_date = "Data",
#'     col_sum = colnames(df)[c(3,7)],
#'     col_mean = colnames(df)[-c(1,2,3,7)],
#'     type = "Daily"
#'   )
#'
#' df.m <-
#'   calculateDMY(
#'     data = df.d,
#'     col_date = "Date",
#'     col_sum = colnames(df.d)[c(2)],
#'     col_mean = colnames(df.d)[-c(1,2)],
#'     type = "Monthly"
#'   )
#'
#' df.a <-
#'   calculateDMY(
#'     data = df.m,
#'     col_date = "Date",
#'     col_sum = colnames(df.m)[c(2)],
#'     col_mean = colnames(df.m)[-c(1,2)],
#'     type = "Yearly"
#'   )
#'
#'


calculateDMY <- function(data = NULL,
                         col_date = NULL,
                         col_sum = NULL,
                         col_mean = NULL,
                         col_max = NULL,
                         col_min = NULL,
                         n.round = 2,
                         type = c("Daily", "Monthly", "Yearly")) {

  Date <- NULL

  # Select the specified columns from the data frame
  data <- data[c(col_date, col_sum, col_mean, col_max, col_min)]

  # Apply different transformations based on the type of summary
  switch(type[1],
         "Monthly" = {
           # If type is "Monthly", convert dates to the first day of the month
           data <-
             dplyr::mutate(
               data,
               {{col_date}} :=
                 base::as.Date(base::format(!!rlang::sym(col_date),
                                            format = "%Y-%m-01"))
             )
         },
         "Yearly" = {
           # If type is "Yearly", convert dates to the year only
           data <-
             dplyr::mutate(
               data, {{col_date}} :=
                 base::as.numeric(base::format(!!rlang::sym(col_date),
                                               format = "%Y"))
             )
         })

  # Summarize the data by the specified date column
  data <-
    dplyr::summarise(
      dplyr::group_by(
        data,
        Date = !!rlang::sym(col_date)
      ),
      # Calculate the sum specified columns
      dplyr::across({{col_sum}}, \(x) base::sum(x, na.rm = TRUE)),
      # Calculate the mean of specified columns
      dplyr::across({{col_mean}}, \(x) base::mean(x, na.rm = TRUE)),
      # Calculate the max of specified columns
      dplyr::across({{col_max}}, \(x) if (all(is.na(x))) NA else base::max(x, na.rm = TRUE)),
      # Calculate the min of specified columns
      dplyr::across({{col_min}}, \(x) if (all(is.na(x))) NA else base::min(x, na.rm = TRUE))
    )

  # Round numeric columns to the specified number of decimal places
  data <-
    dplyr::mutate(data, dplyr::across(dplyr::where(base::is.numeric),
                                      \(x) base::round(x, digits = n.round)))

  # Return the summarized data frame
  return(data)
}
