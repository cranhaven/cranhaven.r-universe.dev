#' @import methods
#' @importFrom utils read.table
NULL

#' @title Test type of a data frame.
#'
#' @description df_is
#' This function tests if the columns of a data frame are all of the same type.
#'
#' @param   df      The data frame.
#' @param   type    The type you expect the columns to have.
#'    It must be one of the R base types:
#'     - 'character' ;
#'     - 'factor' ;
#'     - 'integer' ;
#'     - 'numeric' ;
#'     - 'logical'.
#'
#' @return  \code{TRUE} or \code{FALSE}.
#'
#' @examples
#' # Test if a data frame contains only integers
#' df <- data.frame(a = c(1, 4), b = c(6, 5))
#' # should return FALSE since in R all integers are converted to
#' # numeric by default.
#' W4MRUtils::df_is(df, "integer")
#' # should return TRUE.
#' W4MRUtils::df_is(df, "numeric")
#'
#' @export
df_is <- function(df, type) {
  return(all(lapply(df, class) == type))
}

#' @title Convert data frame to numeric.
#'
#' @description df_force_numeric
#' Converts integer columns of a data frame into numeric.
#'
#' @param df    The data frame.
#' @param cols  The set of columns to convert to numeric.
#'    By default (when set to \code{NULL}) all integer columns are converted.
#'    Set it to a character vector containing the names of the columns you
#'    want to convert, or ton integer vector containing the indices of the
#'    columns. Can be used to force conversion of non integer columns.
#'
#' @return The converted \code{data.frame}.
#'
#' @examples
#' # Convert an integer data frame
#' df <- data.frame(a = as.integer(c(1, 4)), b = as.integer(c(6, 5)))
#' df <- W4MRUtils::df_force_numeric(df)
#'
#' @export
df_force_numeric <- function(df, cols = NULL) {
  if (!is.null(df)) {
    # Convert all columns
    if (is.null(cols)) {
      df <- as.data.frame(lapply(
        df,
        function(v) if (is.integer(v)) as.numeric(v) else v
      ))
    } else {
      # Convert only the specified columns
      for (c in cols) {
        df[, c] <- as.numeric(df[, c])
      }
    }
  }
  return(df)
}

#' @title Data frame loading from a file.
#'
#' @description df_read_table
#' Reads a data frame from a file and possibly convert integer columns to
#'    numeric. This function calls the built-in \code{read.table()} method and
#'    then \code{W4MRUtils::df_force_numeric()}.
#'
#' @param   file            The path to the file you want to load. See
#'    \code{read.table()} documentation for more information.
#' @param   force_numeric   If set to TRUE, all integer columns will be
#'    converted to numeric.
#' @param   ...   Parameter to transmit to the read.table function.
#' @return  The loaded data frame.
#'
#' @examples
#' # Load a data frame from a file and convert integer columns
#' file_path <- system.file(
#'   "extdata",
#'   "example_df_read_table.csv",
#'   package="W4MRUtils"
#' )
#' str(W4MRUtils::df_read_table(
#'   file_path,
#'   sep = ",",
#'   force_numeric = TRUE,
#'   header=TRUE
#' ))
#'
#' @export
df_read_table <- function(file, force_numeric = FALSE, ...) {
  warning(paste(
    "Please, use read.table instead of df_read_table.",
    "This function will be deleted in further version of W4MRUtils."
  ))
  df <- read.table(file, ...)
  if (is.logical(force_numeric) && force_numeric) {
    df <- df_force_numeric(df)
  } else if (is.integer(force_numeric) || is.character(force_numeric)) {
    df <- df_force_numeric(df, cols = force_numeric)
  }
  return(df)
}
