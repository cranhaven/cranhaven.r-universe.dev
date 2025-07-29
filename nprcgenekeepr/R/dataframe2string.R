#' dataframe2string converts a data.frame object to a character vector
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Adapted from print.data.frame
#'
#' @return A character vector representation of the data.frame provided to the
#' function.
#'
#' @param object dataframe
#' @param ... optional arguments to print or plot methods.
#' @param digits the minimum number of significant digits to be used:
#' see print.default.
#' @param addRowNames	logical (or character vector), indicating whether
#'  (or what) row names should be printed.
#' @importFrom stringi stri_length
#' @importFrom stringi stri_pad_both
#' @export
#' @examples
#' library(nprcgenekeepr)
#' dataframe2string(nprcgenekeepr::pedOne)
dataframe2string <- function(object, ..., digits = NULL, addRowNames = TRUE) {
  nRows <- length(row.names(object))
  if (length(object) == 0L) {
    return(paste0(
      sprintf(
        ngettext(
          nRows, "data frame with 0 columns and %d row",
          "data frame with 0 columns and %d rows"
        ),
        nRows
      ),
      "\\n"
    ))
  } else if (nRows == 0L) {
    return(gettext("<0 rows> (or 0-length row names)\\n"))
  } else {
    # get text-formatted version of the data.frame
    m <- as.matrix(format.data.frame(object,
      digits = digits,
      na.encode = TRUE
    ))
    # define rowNames (if required)
    if (isTRUE(addRowNames)) {
      rowNames <- dimnames(object)[[1L]]
      if (is.null(rowNames)) {
        # no row header available -> use row numbers
        rowNames <- as.character(seq_len(NROW(m)))
      }
      # add empty header (used with column headers)
      rowNames <- c("", rowNames)
    }
    # add column headers
    m <- rbind(dimnames(m)[[2L]], m)
    # add row headers
    if (isTRUE(addRowNames)) {
      m <- cbind(rowNames, m)
    }
    # max-length per-column
    maxLen <- apply(apply(m, c(1L, 2L), stri_length), 2L, max,
      na.rm = TRUE
    )

    # add right padding
    ##  t is needed because "If each call to FUN returns a vector
    ##  of length n, then apply returns an array of dimension
    ##  c(n, dim(X)[MARGIN])"
    m <- t(apply(m, 1L, stri_pad_both, width = maxLen))
    # merge columns
    m <- apply(m, 1L, paste, collapse = "")
    # merge rows (and return)
    return(paste(m, collapse = "\n"))
  }
}
