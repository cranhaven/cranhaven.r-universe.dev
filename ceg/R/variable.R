

#' Variable S4 Class
#'
#' Variable S4 class contains two slots with the Variable name and a list
#' of Categories. It is used to construct Stratified.vent.tree objects.
#'
#' @slot name character.
#' @slot categories list of Category S4 objects.
#'
#' @export

setClass(
  "Variable",
  representation(name = "character",
                 categories = "list"
  ),
  validity = function(object) {
    msg <- NULL
    if (is.null(object@name)) stop("Variable must have a name")
    if (object@name == "") stop("Variable@name cannot be void")
    if (grepl("^\\s*$", object@name)) stop("Variable@name cannot be only blanc spaces")
    if (is.null(object@categories)) stop("each Variable must have one category or more")
    for (category in object@categories) {
      if (!methods::is(category, "Category"))
        msg <-
          c(msg, "'@categories' must be a list of CEG::Category S4 objects")
    }
    if (is.null(msg))
      TRUE
    else
      stop(msg)
  }
)



setMethod(
  f = "initialize",
  signature = "Variable",
  definition = function(.Object,
                        name,
                        categories) {
    cat("~~~ Variable: initializator ~~~ \n")
    # Assignment of the slots
    .Object@name <- name
    .Object@categories <- categories
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)




#' Variable(name,categories)
#'
#' Variable(name,categories) is a function that act as constructor to Variable
#' S4 object. Variable S4 class contains two slots with the Variable name and a
#' list of Categories. It is used to construct Stratified.vent.tree objects.
#'
#' @param name character, the Variable name
#' @param categories a list of S4 Category objects.
#'
#' @return a \code{\link{Variable}} S4 object
#'
#' @export
#'
#' @examples
#' var <- Variable("variable.name", list(Category("cat1"), Category("cat2"),
#' Category("cat3")))
#'
#'
Variable <- function(name, categories){
  new("Variable", name, categories)
}

