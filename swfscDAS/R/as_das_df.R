#' Coerce object to a das_df object
#'
#' Check if an object is of class \code{das_df}, or coerce it if possible.
#'
#' @param x an object to be coerced to class \code{das_df}
#'
#' @details Only data frames can be coerced to an object of class \code{das_df}.
#'   If \code{x} does not have column names and classes as specified in \code{\link{das_df-class}},
#'   then the function returns an error message detailing the first column that does not
#'   meet the requirements of a \code{das_df} object.
#'
#' @return An object of class `das_df`
#'
#' @seealso \code{\link{das_df-class}}
#'
#' @export
as_das_df <- function(x) UseMethod("as_das_df")

#' @name as_das_df
#' @export
as_das_df.das_df <- function(x) x

#' @name as_das_df
#' @export
as_das_df.data.frame <- function(x) {
  exp.class <- list(
    Event = "character",
    DateTime = c("POSIXct", "POSIXt"),
    Lat = "numeric",
    Lon = "numeric",
    OnEffort = "logical",
    Cruise = "numeric",
    Mode = "character",
    OffsetGMT = "integer",
    EffType = "character",
    ESWsides = "numeric",
    Course = "numeric",
    SpdKt = "numeric",
    Bft = "numeric",
    SwellHght = "numeric",
    WindSpdKt = "numeric",
    RainFog = "numeric",
    HorizSun = "numeric",
    VertSun = "numeric",
    Glare = "logical",
    Vis = "numeric",
    ObsL = "character",
    Rec = "character",
    ObsR = "character",
    ObsInd = "character",
    Data1 = "character",
    Data2 = "character",
    Data3 = "character",
    Data4 = "character",
    Data5 = "character",
    Data6 = "character",
    Data7 = "character",
    Data8 = "character",
    Data9 = "character",
    Data10 = "character",
    Data11 = "character",
    Data12 = "character",
    EffortDot = "logical",
    EventNum = "character",
    file_das = "character",
    line_num = "integer"
  )
  exp.class.names <- names(exp.class)

  x.class <- lapply(x, class)

  for (i in seq_along(exp.class)) {
    name.curr <- exp.class.names[i]
    x.curr <- x.class[[name.curr]]

    if (!identical(x.curr, exp.class[[i]])) {
      stop("The provided object (x) cannot be coerced to an object of class das_df ",
           "because it does not contain the correct columns. ",
           "Specifically, it must contain a column with the name '", names(exp.class)[i], "' ",
           "and class '", exp.class[[i]], "'\n",
           "Was x created using das_process()? ",
           "See `?as_das_df` or `?das_df-class` for more details.")
    }
  }

  # Check that no events are NA
  if (any(is.na(x$Event)))
    stop("The provided data cannot be coerced to an object of class das_df ",
         "because the following have NA Event value(s):\n",
         .print_file_line(x$file_das, x$line_num, which(is.na(x$Event))))

  class(x) <- c("das_df", setdiff(class(x), "das_df"))

  x
}
