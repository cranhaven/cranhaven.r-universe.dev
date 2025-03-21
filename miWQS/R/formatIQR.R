# Copy from makeJournalTables
#' Formatting Median and Interquartile Range (IQR)
#'
# Log: 12/13/19: Change "(" to "[" to be more consistent with literature.
#' @family format
#' @keywords format
#'
#' @description Formats output from quantile function by writing in standard format in a table:
#'                      median(Q25, Q75)  or median(Q75 - Q25)
#'
#' @note This function can be used with apply or aggregate so a matrix can be formatted.
#'
#' @param x A vector or matrix where the rows are from Q1, Median, Q3 from that order. Imported
#'          using \code{\link[stats]{quantile}} function for vectors and with apply for matrix.
#' @param format.type Character vector. Either "1" or "2".  If "1", looks like  Median (Q75 - Q25).
#'                     If "2", looks like Median (Q25, Q75).  Defaults to 2.
#' @param digits # of significant digits; passed to \code{\link[base]{format}}.
#' @param ... Additional parameters passed to \code{\link[base]{format}}, except for drop0trailing
#'         which is always FALSE. (The leading 0 is never removed)
#'
#' @return A character matrix consisting of formatted median with IQR.
#'
#' @examples
#' data("CO2")
#' # vector
#' x.sum <- quantile(mtcars$mpg, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
#' x.sum
#' formatIQR (x.sum, digits = 5, format.type = "2")
#' formatIQR (x.sum, digits = 5, format.type = "1")
#' formatIQR (x.sum, digits = 2)
#'
#' # a matrix
#' x.sum <- apply(mtcars, 2, quantile, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
#' formatIQR (x.sum, format.type = "1")
#' @noRd


formatIQR <- function(x, format.type = c("2", "1"), digits = 2, ...) {
  # Checking...
  v <- x
  format.type <- match.arg  (format.type)
  v <- if (is.character(v) | is.numeric(v)) {  as.matrix(v) }  # makes a 3 x 1 matrix
  stopifnot(nrow(v) == 3)
  # if(is.null( rownames(v) ) ) {
  warning("Assuming v is a matrix with 1st, 2nd, 3rd quantiles in order.")
  rownames(v) <- c("25%", "50%", "75%")
  # }

  v.look <- format(v, trim = TRUE, digits = digits, drop0trailing = FALSE,  ...)
  v.formatted <- rep(NA, ncol(v))
  names(v.formatted) <- colnames(v)

  for (j in 1:ncol(v)) {
    if (format.type == "2") { # median(Q75-Q25)
      v.formatted[j]   <- paste0(v.look[2, j], " [", v.look[3, j], "-", v.look[1, j], "]")
    } else { # if(format.type == "1"){  #median[ Q1, Q3]
      v.formatted[j]   <- paste0(v.look[2, j], " [", v.look[1, j], ", ", v.look[3, j], "]")
      # } else{
      #   stop("Incorrect Format Type. Please respecify")
      # }
    }
  }

  return(as.matrix(v.formatted))
}

# Orginially this was called format.median.iqr. However, the R CMD check thought it was a realization for
# generic format(), but the names were similar. I had to change names to distinguish from generic. Although formatIQR() is not a the parameter
# "x" needs to be used instead of "v"nso that this function is consistent with format().
