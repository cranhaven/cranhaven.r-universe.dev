#' Validity check for ref_horodate
#'
#' @param object A ref_horodate object
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @keywords internal
validity_ref_horodate = function(object)
{
  rep1 = inherits(object@horodate[2], "POSIXt")
  
  return(ifelse(rep1, TRUE, FALSE))
}


#' Class ref_horodate
#'
#' choice of date with method to show current and previous year
#'
#'
#' @slot horodate a "POSIXt"
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ref_horodate", \dots{})}.
#' @author Cedric Briand \email{cedric.briand@eptb-vilaine.fr}
#' @family referential objects
setClass(
  Class = "ref_horodate",
  representation =
    representation(horodate = "POSIXt"),
  validity = validity_ref_horodate,
  prototype = prototype(horodate = Hmisc::roundPOSIXt(Sys.time(), "years"))
)


#' Choice_c method for ref_horodate
#' @aliases choice_c.ref_horodate
#' @param object An object of class \link{ref_horodate-class}
#' @param nomassign The name assigned in environment envir_stacomi
#' @param funoutlabel, text displayed by the interface
#' @param silent Default FALSE, should messages be displayed
#' @param horodate The horodate to set, formats "\%d/\%m/\%Y \%H:\%M:\%s", "\%d/\%m/\%y \%H:\%M:\%s", "\%Y-\%m-\%d  \%H:\%M:\%s" formats
#' can also be passed with the date set to the minute \%d/\%m/\%Y \%H:\%M or the day  \%d/\%m/\%Y
#' \dots are accepted. The choice_c method assigns and
#' @return An object of class \link{ref_horodate-class} with slot \emph{horodate} set,
#'  and assigns an object of class POSIXt with name nomassign in envir_stacomi
setMethod(
  "choice_c",
  signature = signature("ref_horodate"),
  definition = function(object,
                        nomassign = "horodate",
                        funoutlabel = "nous avons le choix dans la date\n",
                        #decal=0,
                        horodate,
                        silent = FALSE) {
    # horodate="2013-01-01"
    # parse the horohorodate
    if (length(horodate) > 1)
      stop("horodate should be a vector of length 1")
    if (is.null(horodate))
      stop("horodate should not be null")
    if (inherits(horodate, "character")) {
      if (grepl("/", horodate)) {
        .horodate = strptime(horodate, format = "%d/%m/%Y %H:%M:%s")
        if (is.na(.horodate)) {
          .horodate = strptime(horodate, format = "%d/%m/%y %H:%M:%s")
        }
        if (is.na(.horodate)) {
          .horodate = strptime(horodate, format = "%d/%m/%y %H:%M")
        }
        if (is.na(.horodate)) {
          .horodate = strptime(horodate, format = "%d/%m/%Y %H:%M")
        }
        if (is.na(.horodate)) {
          .horodate = strptime(horodate, format = "%d/%m/%y")
        }
        if (is.na(.horodate)) {
          .horodate = strptime(horodate, format = "%d/%m/%Y")
        }
      } else if (grepl("-", horodate)) {
        .horodate = strptime(horodate, format = "%Y-%m-%d  %H:%M:%s")
        if (is.na(.horodate)) {
          .horodate = strptime(horodate, format = "%d-%m-%Y  %H:%M:%s")
        }
        if (is.na(.horodate)) {
          .horodate = strptime(horodate, format = "%Y-%m-%d  %H:%M")
        }
        if (is.na(.horodate)) {
          .horodate = strptime(horodate, format = "%d-%m-%Y  %H:%M")
        }
        if (is.na(.horodate)) {
          .horodate = strptime(horodate, format = "%Y-%m-%d")
        }
        if (is.na(.horodate)) {
          .horodate = strptime(horodate, format = "%d-%m-%Y")
        }
      } else {
        stop(
          "Formatting problem, the character vector you are trying to pass as horodate could not be parsed. Check example or documentation"
        )
      }
      
    } else if (inherits(horodate, "Date")) {
      .horodate <- as.POSIXlt(horodate)
    } else if (inherits(horodate[2] , "POSIXt")) {
      .horodate = horodate
    }
    if (is.na(.horodate))
      stop(
        "Formatting problem, the character vector you are trying to pass as horodate could not be parsed. Check example or documentation"
      )
    object@horodate = .horodate
    validObject(object)
    assign(nomassign, object@horodate, envir_stacomi)
    if (!silent)
      funout(funoutlabel)
    return(object)
  }
)
