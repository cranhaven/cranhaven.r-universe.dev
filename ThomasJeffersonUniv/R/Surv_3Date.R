
#' @title Create \link[survival]{Surv} Object using Three \link[base]{Date}s
#' 
#' @description 
#' Create right-censored \link[survival]{Surv} object using start, stop and censoring dates.
#' 
#' @param start,stop,censor \link[base]{Date}, \link[base]{POSIXlt} or \link[base]{POSIXct} object
#' 
#' @param units (optional) \link[base]{character} scalar, time units
#' 
#' @param ... potential parameters, currently not in use
#' 
#' @returns 
#' Function [Surv_3Date] returns a \link[survival]{Surv} object.
#' 
#' @examples 
#' library(survival)
#' d1 = within(survival::udca, expr = {
#'   edp_yr = Surv_3Date(entry.dt, death.dt, last.dt, units = 'years')
#'   edp_mon = Surv_3Date(entry.dt, death.dt, last.dt, units = 'months') 
#' })
#' head(d1)
#' 
#' noout = within(survival::udca, expr = {
#'   edp_bug = Surv_3Date(entry.dt, death.dt, as.Date('1991-01-01'), units = 'months') 
#' })
#' subset(survival::udca, subset = entry.dt > as.Date('1991-01-01')) # check error as suggested
#' 
#' @importFrom survival Surv
#' @export
Surv_3Date <- function(
    start, stop, censor, 
    units = 'years',
    ...
) {
  
  start_nm <- deparse1(substitute(start))
  stop_nm <- deparse1(substitute(stop))
  censor_nm <- deparse1(substitute(censor))
  
  start <- as.Date(start)
  stop <- as.Date(stop)
  censor <- as.Date(censor)
  
  stop2 <- stop - start # recycled; may have NA
  if (any(unclass(stop2) < 0, na.rm = TRUE)) {
    message('`start` date later than `stop` date\nsee `subset_(, subset = ', start_nm, ' > ', stop_nm, ')`')
    return(invisible()) # dont stop; inspect multiple definition
  }
  
  censor2 <- censor - start # recycled; may have NA
  if (any(unclass(censor2) < 0, na.rm = TRUE)) {
    message('`start` date later than `censor` date\nsee `subset_(, subset = ', start_nm, ' > ', censor_nm, ')`')
    return(invisible()) # dont stop; inspect multiple definition
  }
  
  units <- match.arg(units, choices = names(timeUnits()))
  units_difftime(stop2) <- units
  units_difftime(censor2) <- units
  
  censor3 <- pmax(stop2, censor2, na.rm = TRUE) 
  # some clinicians do not know we must have `cencor >= stop`
  # ?base::pmax (not ?base::pmax.int) can 
  # ... take care of 'difftime' input
  # ... recycles the length
  
  Surv(time = pmin(stop2, censor3, na.rm = TRUE), event = !is.na(stop2)) # beautiful!
  
}



survOver <- function(object, over, ...) {
  if (!inherits(object, 'Surv') || ncol(object) != 2L) stop('`object` must be right censored')
  if (!is.numeric(over) || length(over) != 1L || is.na(over)) stop('`over` must be len-1 numeric')
  
  ret <- (object[,1L] > over)
  ret[object[,1L] < over & object[,2L] == 0] <- NA # censored before `over`
  return(ret)
}


