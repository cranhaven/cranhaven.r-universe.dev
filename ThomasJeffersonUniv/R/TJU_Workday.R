

#' @title Thomas Jefferson University Workdays
#' 
#' @description
#' To summarize the number of workdays, weekends, holidays and vacations in a given time-span 
#' (e.g., a month or a quarter of a year).
#' 
#' @param x \link[base]{character} scalar or \link[base]{vector} (e.g.,
#' `'2021-01'` for January 2021,
#' `'2021 Q1'` for 2021 Q1 (January to March)), or
#' \link[base]{integer} scalar or \link[base]{vector} (e.g., `2021L` for year 2021);
#' The time-span to be summarized.
#' Objects of classes \link[zoo]{yearqtr} and \link[zoo]{yearmon} are also accepted.
#' 
#' @param vacations \link[base]{Date} \link[base]{vector}, vacation days
#' 
#' @details 
#' 
#' Function [TJU_Workday] summarizes the workdays, weekends,
#' Jefferson paid holidays 
#' (New Year’s Day, Martin Luther King, Jr. Day, Memorial Day, Fourth of July, Labor Day, Thanksgiving and Christmas)
#' and your vacation (e.g., sick, personal, etc.) days (if any),
#' in a given time-span.
#' 
#' Per Jefferson policy (source needed), 
#' if a holiday is on Saturday, then the preceding Friday is considered to be a weekend day.
#' If a holiday is on Sunday, then the following Monday is considered to be a weekend day.
#' 
#' @returns 
#' Function [TJU_Workday] returns a \link[base]{factor}.
#' 
#' @examples
#' table(TJU_Workday(c('2021-01', '2021-02')))
#' 
#' tryCatch(TJU_Workday(c('2019-10', '2019-12')), error = identity)
#' table(c(TJU_Workday('2019-10'), TJU_Workday('2019-12'))) # work-around
#' 
#' table(TJU_Workday('2022-12'))
#' 
#' table(TJU_Workday('2022 Q1', vacations = seq.Date(
#'  from = as.Date('2022-03-14'), to = as.Date('2022-03-18'), by = 1)))
#'  
#' table(TJU_Workday('2022 Q2', vacations = as.Date(c(
#'  '2022-05-22', '2022-05-30', '2022-06-01', '2022-07-04'))))
#'  
#' table(TJU_Workday(2021L))
#' 
#' @importFrom lubridate year
#' @importFrom timeDate holiday as.Date.timeDate USNewYearsDay USMLKingsBirthday USMemorialDay USIndependenceDay USLaborDay USThanksgivingDay USChristmasDay
#' @export
TJU_Workday <- function(x, vacations) {
  
  # https://hr.jefferson.edu/benefits-compensation/paid-time-off.html
  
  if (!length(nx <- length(x_dt <- allDates(x)))) return(invisible()) # use S3
  if (!all(diff.default(x_dt) == 1L)) stop('Input must be a consecutive time period')
  
  # add 1-day before and after, to deal with 'weekend & holiday' situation
  dt_ext <- c(x_dt[1L] - 1L, x_dt, x_dt[nx] + 1L)
  
  # https://hr.jefferson.edu/benefits-compensation/paid-time-off.html
  JeffHoliday <- c('USNewYearsDay', 'USMLKingsBirthday', 'USMemorialDay', 'USIndependenceDay', 'USLaborDay', 'USThanksgivingDay', 'USChristmasDay')
  
  wkd <- format.Date(dt_ext, format = '%a') # ?base::weekdays.Date
  id_holiday <- dt_ext %in% as.Date.timeDate(holiday(year = unique.default(year(dt_ext)), Holiday = JeffHoliday))
  id_weekend <- wkd %in% c('Sat', 'Sun')
  
  if (any(id_holiday & id_weekend)) {
    # holiday on weekend; Jefferson makes the closest weekday as weekend
    
    if (length(hSat <- setdiff(which(id_holiday & (wkd == 'Sat')), y = 1L))) {
      # holiday on Saturday
      id_weekend[hSat] <- FALSE # Saturday no longer considered as weekend; consider as holiday
      id_weekend[hSat - 1L] <- TRUE # previous (auxiliary) Friday considered as weekend
      # dont care when first day is Sunday and before-auxiliary Saturday is holiday or not
      # Takes care when last day is Friday and after-auxiliary Saturday is a holiday
    }
    
    if (length(wch_Sun <- setdiff(which(id_holiday & (wkd == 'Sun')), y = nx+2L))) {
      # holiday on Sunday
      id_weekend[wch_Sun] <- FALSE # Sunday no longer considered as weekend; consider as holiday
      id_weekend[wch_Sun + 1L] <- TRUE # next (auxiliary) Monday considered as weekend
      # dont care when last day is Saturday and after-auxiliary Sunday is holiday or not
      # Takes care when first day is Monday and before-auxiliary Sunday is a holiday
    }
    
  }
  
  id_holiday <- id_holiday[-c(1L, nx+2L)]
  id_weekend <- id_weekend[-c(1L, nx+2L)]
  if (any(id_holiday & id_weekend)) stop('should have been removed')
  
  out <- rep(1L, times = nx) # default: weekday
  out[id_weekend] <- 2L # weekend
  out[id_holiday] <- 3L # holiday
  
  if (!missing(vacations)) {
    if (!inherits(vacations, what = 'Date')) stop('`vacations` must be Date object')
    if (any(vholiday <- vacations %in% x_dt[id_holiday])) {
      message('Vacation day(s) ', sQuote(vacations[vholiday]), ' are holiday(s).')
      vacations <- vacations[!vholiday]
    }
    if (any(vweekend <- vacations %in% x_dt[id_weekend])) {
      message('Vacation day(s) ', sQuote(vacations[vweekend]), ' are weekend(s).')
      vacations <- vacations[!vweekend]
    }
    #if (any(vout <- !(vacations %in% x_dt))) {
    #  message('Vacation day(s) ', sQuote(vacations[vout]), ' are out of the timespan.')
    #  vacations <- vacations[!vout]
    #} # this does not change the result :)
    out[x_dt %in% vacations] <- 4L # vacation
  } # else do nothing
  
  ret <- structure(
    out,
    class = 'factor',
    levels = c('Workday', 'Weekend', 'Holiday', 'Vacation')
  )
  factor(ret) # remove zero-count
}




# Objects \link[zoo]{yearqtr} and \link[zoo]{yearmon} are type-double.

# dont forget
# base::month.abb
# base::month.name

#' @title All \link[base]{Date}s in a Time Interval
#' 
#' @description
#' Find all \link[base]{Date}s in a time interval.
#' 
#' @param x R objects, such as
#' \describe{
#' \item{\link[base]{integer}}{
#' year, e.g., `x = 2020L` returns all \link[base]{Date}s from 2020-01-01 to 2020-12-31}
#' \item{\link[zoo]{yearmon}}{
#' year-month object from package \CRANpkg{zoo}}
#' \item{\link[zoo]{yearqtr}}{
#' year-quarter object from package \CRANpkg{zoo}}
#' \item{\link[base]{character}}{
#' convertible to \link[zoo]{yearmon} or \link[zoo]{yearqtr} object}
#' }
#' 
#' @details
#' Function [allDates] returns all \link[base]{Date}s in a given time interval.
#' 
#' @returns 
#' Function [allDates] returns a \link[base]{Date} \link[base]{vector}.
#' 
#' @name allDates
#' @keywords internal
#' @export
allDates <- function(x) {
  if (!length(x)) return(invisible())
  if (inherits(x, what = 'Date')) return(x)
  if (anyNA(x)) stop('does not allow NA input')
  x <- unique(x) # ?base::unique.default ?zoo:::unique.yearmon ?zoo:::unique.yearqtr
  UseMethod('allDates')
}

#' @rdname allDates
#' @export allDates.integer
#' @export
allDates.integer <- function(x) { # `x` considered as year!
  do.call(c, args = lapply(x, FUN = function(i) {
    i1 <- as.Date.character(paste0(i, c('-01-01', '-12-31')), format = '%Y-%m-%d')
    seq.Date(from = i1[1L], to = i1[2L], by = 1L)
  }))
}

#' @rdname allDates
#' @importFrom zoo as.yearmon as.yearqtr
#' @export allDates.character
#' @export
allDates.character <- function(x) {
  if (!anyNA(x0 <- as.yearmon(x))) return(allDates.yearmon(x0)) # ?zoo:::as.yearmon.character, exception is NA (not error)
  if (!anyNA(x0 <- as.yearqtr(x))) return(allDates.yearqtr(x0)) # ?zoo:::as.yearqtr.character
  stop('Cannot be converted to Date: ', sQuote(x))
}

#' @rdname allDates
#' @importFrom zoo as.Date.yearmon
#' @export allDates.yearmon
#' @export
allDates.yearmon <- function(x) {
  do.call(c, args = lapply(x, FUN = function(i) {
    seq.Date(from = as.Date.yearmon(i), to = as.Date.yearmon(i + 1/12) - 1L, by = 1L)
  }))
}

#' @rdname allDates
#' @importFrom zoo as.Date.yearqtr
#' @export allDates.yearqtr
#' @export
allDates.yearqtr <- function(x) {
  do.call(c, args = lapply(x, FUN = function(i) {
    seq.Date(from = as.Date.yearqtr(i), to = as.Date.yearqtr(i + 1/4) - 1L, by = 1L)
  }))
}






