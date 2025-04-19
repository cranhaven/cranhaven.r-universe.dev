

#' Factors.of.interest S4 Class
#'
#' Factors.of.interest S4 class contains a list of S4 Factor objects.
#' This list is used as parameter when construction the output from Coppe-Cosenza
#'  method.
#'
#'
#'
#' @slot list.of.factors list of Factor.
#' Has one or more distinct S4 Factor objects.
#'
#' @export
#'
#' @include factor.R
#'
setClass(
  "Factors.of.interest",
  representation(
    list.of.factors = "list"),
  validity = function(object) {
      msg <- NULL
      if (is.null(object@list.of.factors))
        stop("Factors.of.interest must have one or more Factors")
      for (factor in object@list.of.factors) {
        if (!methods::is(factor, "Factor"))
          msg <-
            c(msg, "'@all' must be a list of Factor S4 objects")
      }
      if (is.null(msg))
        TRUE
      else
        stop(msg)
    }
)



setMethod(
  f = "initialize",
  signature = "Factors.of.interest",
  definition = function(.Object,
                        list.of.factors){
    # cat("~~~ Factors.of.interest: initializator ~~~ \n")
    # Assignment of the slots
    .Object@list.of.factors <- list.of.factors
     methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)

#' Factors.of.interest Constructor
#'
#' Factors.of.interest is a constructor. Factor elements inserted in
#' list.of.factors are type-checked as S4 coppeCosenza::Factor objects. They
#' must have distinct names.
#'
#'
#' @param list.of.factors list of Factor S4 objects
#'
#' @return a \code{\link{Factors.of.interest}} S4 object
#'
#' @export
#'
#' @examples
#' Factors.of.interest(list(Factor("factor1"), Factor("factor2"),
#' Factor("factor3")))
#'
#' @include factor.R
#'
Factors.of.interest <- function(list.of.factors){
  new("Factors.of.interest", list.of.factors)
}



#' getFactorsOfInterestNames
#'
#' It provides a sorted vector with the names of factors.
#'
#' @param factors.of.interest S4 Factors.of.interest object
#'
#' @return vector of character
#' @export
#'
#' @examples
#' \dontrun{getFactorsOfInterestNames(factors.of.interest)}
#'
getFactorsOfInterestNames <- function(factors.of.interest){

  #type check
  if (!methods::is(factors.of.interest, "Factors.of.interest"))
    stop("factors.of.interest parameter must be a
         Factors.of.interest S4 object")

  vector.of.factors.names <- NULL
  for (factor in factors.of.interest@list.of.factors) {
    vector.of.factors.names <- c(vector.of.factors.names, factor@name)
  }
  vector.of.factors.names <- sort(vector.of.factors.names, decreasing = FALSE)
  vector.of.factors.names <- unique(vector.of.factors.names)
  return(vector.of.factors.names)
}


#' @rdname show
#' @param Factors.of.interest Factors.of.interest
#' @export
setMethod("show", "Factors.of.interest",
          function(object){
            cat("\nFactors.of.interest:\n")
            cat(
                unlist(lapply(object@list.of.factors, function(x)paste0(x@name, " ")))
                )
            cat("\n")
          }
)




