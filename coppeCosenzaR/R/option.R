

#' Option S4 Class
#'
#' Option S4 class represents a possible solution to projects. The object
#' includes a list of Option.resource, which is type checked.
#'
#' @slot name character (any other argument will be cast to character)
#'
#' @slot option.resources Option.resources
#'
#' @export
#'
#' @include option-resources.R
#'
setClass(
  "Option",
  representation(
    name = "character",
    option.resources = "Option.resources"),
  validity = function(object) {
    if (!methods::is(object@option.resources, "Option.resources"))
      stop("@option.resources must be a Option.resources S4 object")

    if (length(object@name) > 1) stop("@name cannot have more then 1 value")
    if (object@name == "") stop("@name cannot be void")
    if (grepl("^\\s*$", object@name)) stop("@name cannot be only blanc spaces")
    TRUE
  }
)



setMethod(
  f = "initialize",
  signature = "Option",
  definition = function(.Object,
                        name,
                        option.resources){
    #cat("~~~ Option: initializator ~~~ \n")
    # Assignment of the slots
    .Object@name <- as.character(name)
    .Object@option.resources = option.resources
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)

#' Option Constructor function
#'
#'
#' Constructs a Option S4 object, which  represents a possible solution to
#' projects. The object includes a list of Option.resource, which is type
#' checked.
#'
#' @param  name character character (any other argument will be cast to
#' character)
#' @param  option.resources Option.resources S4 object. Cannot be empty.
#'
#' @return a \code{\link{Option}} S4 object
#'
#' @export
#'
#' @examples
#' \dontrun{Option <- Option(name, option.resources)}
#'
#'
#'
Option <- function(name, option.resources){
  new("Option", name, option.resources)
}


#' getOptionFactorsNames
#'
#' This function returns a sorted vector with all the factors names in a Option
#' S4 object
#'
#' @param option an Option S4 object
#'
#' @return It provides a sorted vector with the names of factors in an option.
#' @export
#'
#' @examples
#' \dontrun{getOptionFactorsNames(option)}
#'
getOptionFactorsNames <- function(option){

  #type check
  if (!methods::is(option, "Option"))
    stop("option parameter must be an Option S4 object")

  list.of.factors.names <- list()
  for (option.factor.availability in
       option@option.resources@list.of.factor.availability) {
    list.of.factors.names <-
      list(list.of.factors.names, option.factor.availability@factor@name )

  }
  vector.of.factors.names <- unlist(list.of.factors.names)
  vector.of.factors.names <- sort(vector.of.factors.names, decreasing = FALSE)
  return(vector.of.factors.names)
}

#' @rdname show
#' @param Option Option
#' @export
setMethod("show", "Option",
          function(object){
            cat("\n")
            cat(paste0("Option: ", object@name, "\n"))
            print(object@option.resources)
            cat("\n")
          }
)

