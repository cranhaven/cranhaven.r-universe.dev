#' Append the rows of one dataframe to another.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Pedigree Curation
#'
#' Appends the rows of df2 to df1, can handle cases where df2
#' has a subset of the columns of df1
#'
#' @return The appended dataframe with \code{NA} inserted into columns as
#' needed.
#'
#' @param df1 the target dataframe to append to.
#' @param df2 the the donor dataframe information should be appended from
#' @noRd
rbindFill <- function(df1, df2) {
  # Find columns in df1 not in df2
  addHeaders <- setdiff(names(df1), names(df2))

  # Add the missing columns to df2 (containing NA values)
  if (!isEmpty(addHeaders)) {
    for (i in seq_len(length(addHeaders))) {
      col <- df1[[addHeaders[i]]] # We want to extract not subset
      colType <- mode(col)
      if (colType == "numeric") {
        if (inherits(col, "Date")) {
          df2[, addHeaders[i]] <- as.Date(NA, origin = as.Date("1970-01-01"))
        } else {
          df2[, addHeaders[i]] <- NA
        }
      } else if (colType %in% c("character", "logical")) {
        df2[, addHeaders[i]] <- NA
      } else {
        stop(colType, " : unknown column type")
      }
    }
  }
  rbind(df1, df2)
}
