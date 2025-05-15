#' Coerce object to a airdas_df object
#'
#' Check if an object is of class \code{\link{airdas_df}}, or coerce it if possible.
#'
#' @param x An object to be coerced to class \code{airdas_df}
#'
#' @details Currently only data frames can be coerced to an object 
#'   of class \code{\link{airdas_df}}.
#'   If \code{x} does not have column names, classes, and contents 
#'   as specified in \code{\link{airdas_df}},
#'   then the function returns an error message detailing the first column that does not
#'   meet the \code{\link{airdas_df}} requirements. 
#'
#' @return An object of class \code{\link{airdas_df}}
#'
#' @seealso \code{\link{airdas_df-class}}
#'
#' @export
as_airdas_df <- function(x) UseMethod("as_airdas_df")

#' @name as_airdas_df
#' @export
as_airdas_df.airdas_df <- function(x) x

#' @name as_airdas_df
#' @export
as_airdas_df.data.frame <- function(x) {
  # Check that columns have correct names and classes
  exp.class <- list(
    Event = "character",
    DateTime = c("POSIXct", "POSIXt"),
    Lat = "numeric",
    Lon = "numeric",
    OnEffort = "logical",
    Trans = "character",
    Bft = "numeric",
    CCover = "numeric",
    Jelly = "numeric",
    HorizSun = "numeric",
    VertSun = "numeric",
    HKR = "character",
    Haze = "logical",
    Kelp = "logical",
    RedTide = "logical",
    AltFt = "numeric",
    SpKnot = "numeric",
    ObsL = "character",
    ObsB = "character",
    ObsR = "character",
    Rec = "character",
    VLI = "character",
    VLO = "character",
    VB = "character",
    VRI = "character",
    VRO = "character",
    Data1 = "character",
    Data2 = "character",
    Data3 = "character",
    Data4 = "character",
    Data5 = "character",
    Data6 = "character",
    Data7 = "character",
    EffortDot = "logical", 
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
      stop("The provided object (x) cannot be coerced to an object of class airdas_df ",
           "because it does not contain the correct columns. ",
           "Specifically, it must contain a column with the name '", names(exp.class)[i], "' ",
           "and class '", exp.class[[i]], "'\n",
           "Was x created using airdas_process()? ", 
           "See `?as_airdas_df` or `?airdas_df-class` for more details.")
    }
  }
  
  # Check that file_type column has an expected value
  file.type.acc <- c("turtle", "caretta", "survey", "phocoena")
  if (!all(x$file_type %in% file.type.acc))
    stop("The file_type column values all must be one of: ", 
         paste(file.type.acc, collapse = ", "))
  
  # Check that no events are NA
  if (any(is.na(x$Event)))
    stop("The provided data cannot be coerced to an object of class airdas_df ",
         "because the following line(s) have NA Event value(s):\n", 
         .print_file_line(x$file_das, x$line_num, which(is.na(x$Event))))
  
  # Check that all of OnEffort is either TRUE/FALSE; no NAs
  if (any(is.na(x$OnEffort))) 
    stop("The following line(s) have OnEffort values of NA, ", 
         "and thus this object cannot be coerced to an airdas_df object:\n",
         .print_file_line(x$file_das, x$line_num, which(is.na(x$OnEffort))))
  
  # Check for no datetime/lat/lon NAs in on-effort events
  x.oneff <- x[x$OnEffort, ]
  x.oneff.dtll.na <- which(is.na(x.oneff$Lat) | is.na(x.oneff$Lon) | is.na(x.oneff$DateTime))
  if (length(x.oneff.dtll.na) > 0)
    stop("The following have NA Lat, Lon, and/or DateTime values in the following, ", 
         "meaning this object cannot coerced to an airdas_df object:\n", 
         .print_file_line(x$file_das, x$line_num, x.oneff.dtll.na))
  
  # Check for no deleted events
  if (any(x$Event == "#"))
    warning("This airdas_df object has deleted events, meaning ", 
            "some \"#\" events which should be removed, at the following:\n", 
            .print_file_line(x$file_das, x$line_num, which(x$Event == "#")))
  
  # Add class and return
  class(x) <- c("airdas_df", setdiff(class(x), "airdas_df"))
  
  x
}
