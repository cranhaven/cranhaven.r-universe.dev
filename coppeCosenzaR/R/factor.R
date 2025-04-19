

#' Factor S4 Class
#'
#' Factor S4 class contains a single slot with the Factor name. A factor in the
#' COPPE-Cosenza model represents an item to be considered both in the options
#' and in projects.
#'
#' @slot name character
#'
#' @export
#'
setClass(
  "Factor",
  representation(
    name = "character"),
  validity = function(object) {
    if (length(object@name) > 1)
      stop("Factor@name cannot have more then 1 value")
    if (object@name == "") stop("Factor@name cannot be void")
    if (grepl("^\\s*$", object@name))
      stop("Factor@name cannot be only blanc spaces")
    TRUE
  }
)



setMethod(
  f = "initialize",
  signature = "Factor",
  definition = function(.Object,
                        name){
    #cat("~~~ Factor: initializator ~~~ \n")
    # Assignment of the slots
    .Object@name <- as.character(name)
    names(.Object@name) <- as.character(name)
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)

#' Factor Constructor
#'
#' Factor(name) is a constructor to Factor S4 objects.
#' Factor S4 class contains a single slot with the factor name.
#'
#' @param name character the factor namecharacter (any other argument will be
#' cast to character)
#'
#' @return a \code{\link{Factor}} S4 object
#'
#' @export
#'
#' @examples
#' factor <- Factor("name")
#' Factor("name")
#'
#'
Factor <- function(name){
  new("Factor", as.character(name))
}

#' @rdname show
#' @param Factor Factor
#' @export
setMethod("show", "Factor",
          function(object){
            cat(paste("Factor:", sep = " ", object@name))
}
)




