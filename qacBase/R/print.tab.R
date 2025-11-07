#' @title Print a tab object
#' @description Print the results of calculating a frequency table
#' @param x An object of class \code{tab}
#' @param ... Parameters passed to the print function
#' @return
#' No return value, called for side effects
#' @examples
#' frequency <- tab(cardata, make, sort = TRUE, na.rm = FALSE)
#' print(frequency)
#' @rdname print.tab
#' @export

print.tab <- function(x, ...) {
  if(!inherits(x, "tab")) stop("Must be class 'tab'")
  digits <- attr(x, "digits")
  x$percent = paste(as.character(round(x$percent, digits)),
                     "%", sep = "")
  if (length(x) == 5)
    x$cum_percent = paste(as.character(round(x$cum_percent, digits)),
                           "%", sep = "")
  print.data.frame(x, row.names=FALSE, ...)
}

