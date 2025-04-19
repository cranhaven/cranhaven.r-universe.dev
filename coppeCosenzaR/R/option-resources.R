
#' Option.resources S4 Class
#'
#' Option.resources S4 class contains a list of one or more S4
#' Option.factor.availability objects. This list is type-checked and used to
#' construct Option objects.
#'
#' @slot list.of.factor.availability list of Option.factor.availability
#'
#' @export
#'
#' @include option-factor-availability.R
#'
setClass(
  "Option.resources",
  representation(
    list.of.factor.availability = "list"),
  validity = function(object) {

    # not null
    if (is.null(object@list.of.factor.availability))
      stop("@list.of.factor.availability cannot be NULL")
    #is.data.frame(df) && nrow(df)==0

    # is list and have elements
    if (!(is.list(object@list.of.factor.availability) &&
          length(object@list.of.factor.availability) > 0))
      stop("list.of.factor.availability must be a list with one or more
           Option.factor.availability")

    # all elements are Option.factor.availability
    for (option.factor.availability in object@list.of.factor.availability) {
      if (!methods::is(option.factor.availability, "Option.factor.availability"))
        stop("@list.of.factor.availability must be a list of
             Option.factor.availability S4 objects")
    }

    factor.names <- c()
    for (option.factor.availability in object@list.of.factor.availability) {
      factor.names <- c(factor.names, option.factor.availability@factor@name)}
    #print(factor.names)
    if (anyDuplicated(factor.names) > 0) stop("A factor cannot have more than
                                              one availablity evaluation
                                              informed -> ", factor.names )

  }
)



setMethod(
  f = "initialize",
  signature = "Option.resources",
  definition = function(.Object,
                        list.of.factor.availability){
    #cat("~~~ Option.resources: initializator ~~~ \n")
    # Assignment of the slots
    .Object@list.of.factor.availability <- list.of.factor.availability
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)

#' Option.resources Constructor
#'
#' A constructor to Option.resources S4 objects.
#'
#' @param list.of.factor.availability list of Option.factor.availability S4 objects
#'
#' @return a \code{\link{Option.resources}} S4 object
#'
#' @export
#'
#' @examples
#' \dontrun{Option.resources(list.of.factor.availability)}
#'
Option.resources <- function(list.of.factor.availability){
  new("Option.resources", list.of.factor.availability)
}

#' @rdname show
#' @param Option.resources Option.resources
#' @export
setMethod("show", "Option.resources",
          function(object){
            df <- NULL
            for (i in 1:length(object@list.of.factor.availability)) {
              x <- object@list.of.factor.availability[[i]]
              df <- rbind(
                df,
                data.frame(
                  as.character(x@factor@name),
                  x@availability
                )
              )
            }
            row.names(df) <- df[, 1]
            df <- df[, 2, drop = FALSE]
            names(df) <- c("availability")
            print(df)
          }
)





