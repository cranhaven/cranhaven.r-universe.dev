#' Coerce object to a airdas_dfr object
#'
#' Check if an object is of class \code{\link{airdas_dfr}}, or coerce it if possible.
#'
#' @param x An object to be coerced to class \code{airdas_dfr}
#'
#' @details Currently only data frames can be coerced to an object of class \code{\link{airdas_dfr}}.
#'   If \code{x} does not have column names and classes as specified in \code{\link{airdas_dfr}},
#'   then the function returns an error message detailing the first column that does not
#'   meet the \code{\link{airdas_dfr}} requirements.
#'
#' @return An object of class `airdas_dfr`
#'
#' @seealso \code{\link{airdas_dfr-class}}
#'
#' @export
as_airdas_dfr <- function(x) UseMethod("as_airdas_dfr")

#' @name as_airdas_dfr
#' @export
as_airdas_dfr.airdas_dfr <- function(x) x

#' @name as_airdas_dfr
#' @export
as_airdas_dfr.data.frame <- function(x) {
  # Check that columns have correct names and classes
  exp.class <- list(
    Event = "character",
    EffortDot = "logical",
    DateTime = c("POSIXct", "POSIXt"),
    Lat = "numeric",
    Lon = "numeric",
    Data1 = "character",
    Data2 = "character",
    Data3 = "character",
    Data4 = "character",
    Data5 = "character",
    Data6 = "character",
    Data7 = "character",
    EventNum = "character",
    file_das = "character",
    line_num = "integer",
    file_type = "character"
  )
  exp.class.names <- names(exp.class)
  
  x.class <- lapply(x, class)
  
  for (i in seq_along(exp.class)) {
    name.curr <- exp.class.names[i]
    x.curr <- x.class[[name.curr]]
    
    if (!identical(x.curr, exp.class[[i]])) {
      stop("The provided data cannot be coerced to an object of class airdas_dfr ",
           "because it does not contain the correct columns. ",
           "Specifically, it must contain a column with the name '", 
           names(exp.class)[i], "' ", "and class '", exp.class[[i]], "'\n",
           "Was x created using airdas_read()? ", 
           "See `?as_airdas_dfr` or `?airdas_dfr-class` for more details.")
    }
  }
  
  # Check that file_type column has an expected value
  file.type.acc <- c("turtle", "caretta", "survey", "phocoena")
  if (!all(x$file_type %in% file.type.acc))
    stop("The file_type column values all must be one of: ", 
         paste(file.type.acc, collapse = ", "))
  
  # Check that no events are NA
  if (any(is.na(x$Event)))
    stop("The provided data cannot be coerced to an object of class airdas_dfr ",
         "because the following have NA Event codes:\n", 
         .print_file_line(x$file_das, x$line_num, which(is.na(x$Event))))
  
  # Add class and return
  class(x) <- c("airdas_dfr", setdiff(class(x), "airdas_dfr"))
  
  x
}
