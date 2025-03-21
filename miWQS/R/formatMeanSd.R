#  In makeJournalTables: Don't export.
#  #' @title Formatting Means and Standard Deviations
# or MSE
#  #'
#  #' @name formatMeanSd
#  #' @family format
#  #' @keywords format
#  #'
#  #' @description \code{formatMeanSd} formats output from mean and sd functions by writing in standard format
#  #'        in a table; ideally:  "Mean ± SD". However, as CRAN does not support non-ASCII characters like "±" ,
#  #'        the output is actually written as "Mean +- SD".
#  #'
#  #' @param x      A matrix where mean is in 1st column and standard deviation is in second.
#  #' @param digits Number of significant digits
#  #' @param ...    Any parameters passed to \code{\link[base]{format}},  except for drop0trailing
#  #'          which is always FALSE. (The leading 0 is never removed)
#  #' @return A character matrix of formatted means/sd OR MSE.
#  #'
#  #' @examples
#  #' \dontrun{
#  #'  #Uses the mtcars dataset from dataset package
#  #'  data("mtcars")
#  #'
#  #'  #vector
#  #'  x.sum <- cbind(mean(mtcars$mpg), sd(mtcars$mpg) )
#  #'  x.sum
#  #'  formatMeanSd(x.sum)
#  #'  formatMeanSd(x.sum, digits = 2, nsmall = 2)
#  #'  formatMeanSd(x.sum, digits = 2, scientific = TRUE)
#  #'
#  #'  #matrix: formatMeanSd using with apply. Perhaps most useful
#  #'  f <- function(x) {cbind(mean(x), sd(x))}
#  #'  x.sum <- apply(mtcars, 2, f )
#  #'  x.sum
#  #'  formatMeanSd( t(x.sum))
#  #'
#  #'  #error: when you pass more than 2 statistics.
#  #'  \donttest{
#  #'       x.sum <- cbind(mean(mtcars$mpg), sd(mtcars$mpg), quantile(mtcars$mpg, 0.25))
#  #'       formatMeanSd(x.sum)
#  #'  }
#  #' }
#  #'
#  #'  In makeJournalTables: Don't export.

formatMeanSd <- function(x, digits = 2, ...) {
  # Warnings and Checks
  v <- x
  warning("Formatting as Mean +- SD. Mean is assumed to be in 1st column; standard deviation in second. \n")
  if (is.character(v) | is.numeric(v)) {  v <- as.matrix(v)}
  stopifnot(ncol(v) == 2)

  # Format mean and sd's.
  v.look <- format(v, trim = TRUE, digits = digits, drop0trailing = FALSE,  ...)

  # Combine v.look in character.
  v.formatted <- rep(NA, nrow(v))
  names(v.formatted) <- rownames(v)
  for (j in 1:nrow(v)) {
    v.formatted[j]   <-  paste0 (v.look[j, 1], " +- ",  v.look[j, 2])
  }

  return(as.matrix(v.formatted))
}
