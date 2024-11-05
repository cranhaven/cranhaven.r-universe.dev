#' Set/get global faux options
#' 
#' Global faux options are used, for example, to set the default separator for cell names.
#'
#' @param ... One of four: (1) nothing, then returns all options as a list; (2) a name of an option element, then returns its value; (3) a name-value pair which sets the corresponding option to the new value (and returns nothing), (4) a list with option-value pairs which sets all the corresponding arguments.
#'
#' @return a list of options, values of an option, or nothing
#' @export
#'
#' @examples
#' 
#' faux_options() # see all options
#' 
#' faux_options("sep") # see value of faux.sep
#' 
#' \dontrun{
#' # changes cell separator (e.g., A1.B2)
#' faux_options(sep = ".")
#' 
#' # changes cell separator back to default (e.g., A1_B2)
#' faux_options(sep = "_")
#' }
faux_options <- function (...) {
  # code from afex::afex_options
  dots <- list(...)
  if (length(dots) == 0) { 
    # get all faux options
    op <- options()
    faux_op <- op[grepl("^faux.", names(op))]
    names(faux_op) <- sub("^faux.", "", names(faux_op))
    return(faux_op)
  } else if (is.list(dots[[1]])) {
    # first item is a list, set from list if named
    newop <- dots[[1]]
    if (is.null(names(newop))) 
        stop("Format lists with names like list(sep = '.', verbose = FALSE)")
    names(newop) <- paste0("faux.", names(newop))
    options(newop)
  } else if (!is.null(names(dots))) {
    # dots have names, so set faux options
    newop <- dots
    names(newop) <- paste0("faux.", names(newop))
    options(newop)
  } else if (is.null(names(dots))) {
    # dots don't have names, so get faux options
    opnames <- paste0("faux.", unlist(dots))
    getop <- lapply(opnames, getOption)
    if (length(opnames) == 1) {
      getop <- getop[[1]]
    } else {
      names(getop) <- unlist(dots)
    }
    return(getop)
  } else {
    warning("Unsupported command to faux_options(), nothing done.", 
            call. = FALSE)
  }
}