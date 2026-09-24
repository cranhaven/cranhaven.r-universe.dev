
#' @title Number of Anniversaries Between Two \link[base]{Date}s
#' 
#' @description
#' Number of anniversaries between two dates.
#' 
#' @param from an R object convertible to \link[base]{POSIXlt},
#' start date/time
#' 
#' @param to an R object convertible to \link[base]{POSIXlt},
#' end date/time
#' 
#' @details
#' \enumerate{
#' \item {Year difference between `from` and `to` dates are calculated}
#' \item {In either situation below, subtract one (1) year from the year difference obtained in Step 1.
#' \itemize{
#'  \item {Month of `from` is later than month of `to`;}
#'  \item {Months of `from` and `to` are the same, but day of `from` is later than day of `to`.}
#' }
#' In either of such situations, the anniversary of the current year has not been reached.
#' }
#' \item {If any element from Step 2 is negative, \link[base]{stop}.}
#' }
#' 
#' 
#' 
#' @returns
#' Function [anniversary] returns an \link[base]{integer} scalar or \link[base]{vector}.
#' 
#' @export
anniversary <- function(to, from) {
  
  to_nm <- deparse1(substitute(to))
  from_nm <- deparse1(substitute(from))
  
  if (!inherits(to, 'POSIXlt')) to <- as.POSIXlt(to)
  if (!inherits(from, 'POSIXlt')) from <- as.POSIXlt(from)
  
  if (!identical(
    attr(to, which = 'tz', exact = TRUE), 
    attr(from, which = 'tz', exact = TRUE)
  )) stop('time zone different')
  
  to0 <- unclass(to)
  from0 <- unclass(from)
  
  ret <- to0$year - from0$year
  id <- which(
    (to0$mon < from0$mon) |
      ((to0$mon == from0$mon) & (to0$mday < from0$mday))
  )
  ret[id] <- ret[id] - 1L
  
  if (any(ret < 0L, na.rm = TRUE)) {
    #stop('do not allow negative anniversary\nsee `.[](, subset = ', from_nm, ' > ', to_nm, ')`')
    stop('do not allow negative anniversary')
  }
  
  return(ret)
}