#' Prepare the ready-to-use M5 data in one data.frame
#'
#' It's a memory-efficient function, which uses `data.table` under the hood.
#' However, it still not recommended to use this function on PCs with < 16GB RAM.
#' In such case, consider to use a custom solution
#' based on `[arrow](https://arrow.apache.org/docs/r/)` or `[disk.frame](https://diskframe.com/index.html)`
#'
#' @param sales_train A data.frame with M5 train data
#' @param sales_test A data.frame with M5 test data
#' @param calendar A data.frame with M5 calendar
#' @param sell_prices A data.frame with M5 sell_prices
#'
#' @returns
#'
#' A `data.table` composed from input objects, which  contains the following columns:
#' - *item_id*
#' - *dept_id*
#' - *cat_id*
#' - *store_id*
#' - *state_id*
#' - *d* - day ordinal number
#' - *value* - number of sold items
#' - *wm_yr_wk* - week identifier
#' - *weekday* - weekday name (character)
#' - *wday* - weekday as an integer
#' - *month*
#' - *year*
#' - *event_name_1* - special event name, like holidays etc.
#' - *event_type_1* - special event type
#' - *event_name_2* - as above
#' - *event_type_2* - as above
#' - *snap* - promotion flag
#' - *sell_price*
#'
#' @examples
#' library(m5)
#' library(zeallot)
#' \donttest{
#'
#' m5_download('data')
#' c(sales_train,
#'   sales_test,
#'   sell_prices,
#'   calendar,
#'   weights) %<-% m5_get_raw_evaluation('data')
#'
#' m5_data  <-
#'    m5_prepare(sales_train, sales_test, calendar, sell_prices)
#' }
#'
#' @import data.table
#' @export
m5_prepare <- function(sales_train, sales_test,
                       calendar, sell_prices){

  # Optimized
  # See: https://stackoverflow.com/questions/34598139/left-join-using-data-table

  # A hack to pass CRAN check
  # See: https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  variable <- d <- state_id <- . <- wm_yr_wk <- weekday <- event_name_1 <-
    event_type_1 <- event_name_2 <- event_type_2 <- snap <- store_id <-
    item_id <- sell_price <- NULL

  # Pivot
  .cols <- colnames(sales_train)[6:length(colnames(sales_train))]
  sales_train_long <- melt(sales_train, measure.vars=.cols)

  .cols <- colnames(sales_test)[6:length(colnames(sales_test))]
  sales_test_long  <- melt(sales_test, measure.vars=.cols)

  setDT(sales_train_long)
  setDT(sales_test_long)

  sales_train_long[, variable := as.integer(variable)]
  sales_test_long[, variable := as.integer(variable)]

  # Prepare calendar
  calendar[, date := lubridate::as_date(date)]
  .cols <- colnames(calendar)[startsWith(colnames(calendar), 'snap')]
  calendar <- melt(calendar, measure.vars=.cols)
  calendar[, variable := stringi::stri_sub(variable, 6)]
  setnames(calendar, 'variable', 'state_id')
  setnames(calendar, 'value', 'snap')
  calendar[, d := 1:.N, by=state_id]

  # Prepare sales
  m5_data <-
    rbindlist(list(
      sales_train_long,
      sales_test_long
    ))
  setnames(m5_data, 'variable', 'd')

  rm(sales_train_long)
  rm(sales_test_long)
  gc()

  m5_data[calendar, on = .(d, state_id),
          `:=`(wm_yr_wk=wm_yr_wk,
               weekday=weekday,
               wday=wday,
               month=month,
               year=year,
               event_name_1=event_name_1,
               event_type_1=event_type_1,
               event_name_2=event_name_2,
               event_type_2=event_type_2,
               state_id=state_id,
               snap=snap)]
  gc()
  m5_data[sell_prices, on = .(store_id, item_id, wm_yr_wk),
          `:=`(sell_price=sell_price)]
  gc()

  m5_data
}
