#' @name m5_get_raw
#' @title Load raw CSV files using [data.table::fread()] function
#'
#' @param path The directory with the unzipped M5 data files
#'
#' @returns
#' The function returns a list of five data.tables:
#' * sales_train (evaluation/validation)
#' * sales_test (evaluation/validation)
#' * sell_prices
#' * calendar
#' * weights (evaluation/validation)
#'
#' @references
#' [m5-forecasts repo by Nixtla](https://github.com/Nixtla/m5-forecasts)
#'
#' @examples
#' \dontrun{
#' library(m5)
#' library(zeallot)
#'
#' m5_download('data')
#' c(sales_train,
#'   sales_test,
#'   sell_prices,
#'   calendar,
#'   ) %<-% m5_get_raw_evaluation('data')
#' }
NULL

.abstract_get_raw <- function(path, files){
  files <- file.path(path, files)
  Map(data.table::fread, files)
}

#' @rdname m5_get_raw
#' @export
m5_get_raw_evaluation <- function(path){
  files <- c(
    'sales_train_evaluation.csv',
    'sales_test_evaluation.csv',
    'sell_prices.csv',
    'calendar.csv',
    'weights_evaluation.csv'
  )
  .abstract_get_raw(path, files)
}

#' @rdname m5_get_raw
#' @export
m5_get_raw_validation <- function(path){
  files <- c(
    'sales_train_validation.csv',
    'sales_test_validation.csv',
    'sell_prices.csv',
    'calendar.csv',
    'weights_validation.csv'
  )
  .abstract_get_raw(path, files)
}
