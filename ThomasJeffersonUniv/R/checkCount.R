
#' @title Positive Counts in a \link[base]{logical} vector 
#' 
#' @description
#' Number and percentage of positive counts in a \link[base]{logical} \link[base]{vector}.
#' 
#' 
#' 
#' @param x \link[base]{logical} \link[base]{vector}
#' 
#' @returns
#' Function [checkCount] returns a \link[base]{character} scalar.
#' 
#' @examples
#' checkCount(as.logical(infert$case))
#' @export
checkCount <- function(x) {
  #if (!is.logical(x)) return(x) # exception handling!!
  if (!is.logical(x)) stop('input must be logical')
  x0 <- x[!is.na(x)]
  if (!(n <- length(x0))) return('')
  y <- sum(x0)
  if (y == 0L) return('')
  return(sprintf(fmt = '%d/%d, %.1f%%', y, n, 1e2*y/n))
}