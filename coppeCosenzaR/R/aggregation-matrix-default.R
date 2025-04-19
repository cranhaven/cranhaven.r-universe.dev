#' Aggregation.matrix.default
#'
#' This class represents extends the Aggregation.matrix S4 and is the default
#' aggregation matrix, that presents a zero value, when the option does not
#' provide an adequate level of the required factor.  In other words, if the
#' option level is below the required one, the evaluation of the criteria for
#' the studied option will be zero. Such matrix provides a low compensatory
#' effect. Nevertheless for problems which allows greater compensatory effects,
#' the package allows using different aggregation matrices.
#'
#' @seealso Aggregation.matrix
#'
#'
#' @include aggregation-matrix.R
#'
#' @export
#'
setClass(
  "Aggregation.matrix.default",
  representation( ),
  contains = "Aggregation.matrix",
  prototype = list(name = "Aggregation.matrix.default"),
  validity = function(object) {
    TRUE
  }

)



setMethod(
  f = "initialize",
  signature = "Aggregation.matrix.default",
  definition = function(.Object)
  {
    # cat("~~~ Aggregation.matrix.default: initializator ~~~ \n")
    # Assignment of the slots
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)





#' @rdname Aggregate
#'
#' @export
#'
setMethod(
  "Aggregate",
  signature = c( "Aggregation.matrix.default",
                   "character",
                   "character",
                   "logical",
                   "numeric"),
  function(
    aggregation.matrix,
    factor.evaluation,
    resource.evaluation,
    factor.is.specific,
    nrfactors){
    # factor.evaluation  - character
    # resource.evaluation - character
    # factor.is.specific - logical
    # nrfactors - numeric
    if (factor.evaluation == "Cr") {
      if (resource.evaluation == "Ex") return(1)
      if (factor.is.specific) return(NA)
      return(0) # if -> "G", "R", "W", "Em", "Z", "In"
    }

    if (factor.evaluation == "C") {
      if (resource.evaluation == "Ex") return(1 + 1/nrfactors)
      if (resource.evaluation == "G") return(1)
      if (factor.is.specific) return(NA)
      return(0) # "R", "W", "Em", "Z", "In"
    }

    if (factor.evaluation == "LC") {
      if (resource.evaluation == "Ex") return(1 + 2/nrfactors)
      if (resource.evaluation == "G") return(1 + 1/nrfactors)
      if (resource.evaluation == "R") return(1)
      if (factor.is.specific) return(NA)
      return(0) # "W", "Em", "Z", "In"
    }

    if (factor.evaluation == "I") {
      if (resource.evaluation == "Ex") return(1 + 3/nrfactors)
      if (resource.evaluation == "G") return(1 + 2/nrfactors)
      if (resource.evaluation == "R") return(1 + 1/nrfactors)
      if (resource.evaluation == "W") return(1)
      if (resource.evaluation == "Em") return(0.01)
      if (resource.evaluation == "Z") return(0.001)
      if (factor.is.specific) return(NA)
      return(0) # "Inexistent - In"
    }

    stop("fail when agregating  - invalid factor or resource evaluation")
  }
)




#' @rdname show
#' @param Aggregation.matrix.default Aggregation.matrix.default
#' @export
setMethod("show", "Aggregation.matrix.default",
          function(object){
            cat("\nAgregation Matrix Default:\n")
            cat("general evaluation matrix")
            cr <- c(         "1",          "0",          "0", "0",    "0",
                             "0", "0")
            c  <- c("(1 + 1/nf)",          "1",          "0", "0",    "0",
                    "0", "0")
            lc <- c("(1 + 2/nf)", "(1 + 1/nf)",          "1", "0",    "0",
                    "0", "0")
            i  <- c("(1 + 3/nf)", "(1 + 2/nf)", "(1 + 1/nf)", "1", "0.01",
                    "0.001", "0")

            df <- rbind(cr, c, lc, i)
            colnames(df) <- c("Ex", "G", "R", "W", "Em", "Z", "In")
            row.names(df) <- c("Cr", "C", "LC", "I")

            cat("\n")

            print(df)

            cat("\nSpecfics factors to a project must achieve a value bigger
                then 0, otherwise they are evaluated as NA and causes the full
                option not being acceptable.\n ")
            cat("More information about the model see:  Cosenza, Carlos Alberto
                Nunes, Francisco Antonio Doria, and Leonardo Antonio Monteiro
                Pessoa. Hierarchy Models for the Organization of Economic
                Spaces. Procedia Computer Science 55 (2015): 82-91.
                https://doi.org/10.1016/j.procs.2015.07.010")
          }
)
