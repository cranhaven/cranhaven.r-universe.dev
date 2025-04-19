#' Aggregation.matrix.membership.difference
#'
#' This class represents extends the Aggregation.matrix S4 and implements  the
#' Membership Difference aggregation matrix.
#'
#' @seealso Aggregation.matrix
#'
#'
#' @include aggregation-matrix.R
#'
#' @export
#'
setClass(
  "Aggregation.matrix.membership.difference",
  representation( ),
  contains = "Aggregation.matrix",
  prototype = list(name = "Aggregation.matrix.membership.difference"),
  validity = function(object) {
    TRUE
  }

)



setMethod(
  f = "initialize",
  signature = "Aggregation.matrix.membership.difference",
  definition = function(.Object)
  {
    # cat("~~~ Aggregation.matrix.membership.difference: initializator ~~~ \n")
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
  signature = c( "Aggregation.matrix.membership.difference",
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

   ###### colnames(df) <- c("Ex", "G", "R", "W", "Em", "Z", "In")
   #######row.names(df) <- c("Cr", "C", "LC", "I")



    if (factor.evaluation == "Cr") {
      if (resource.evaluation == "Ex") return(1.00)
      if (resource.evaluation == "G")  return(0.75)
      if (resource.evaluation == "R")  return(0.50)
      if (resource.evaluation == "W")  return(0.25)
      if (factor.is.specific) return(NA)
      return(0) # if -> "Em", "Z", "In"
    }



    if (factor.evaluation == "C") {
      if (resource.evaluation == "Ex") return(1.25)
      if (resource.evaluation == "G")  return(1.00)
      if (resource.evaluation == "R")  return(0.75)
      if (resource.evaluation == "W")  return(0.50)
      if (factor.is.specific) return(NA)
      return(0) # if -> "Em", "Z", "In"
    }



    if (factor.evaluation == "LC") {
      if (resource.evaluation == "Ex") return(1.50)
      if (resource.evaluation == "G")  return(1.25)
      if (resource.evaluation == "R")  return(1.00)
      if (resource.evaluation == "W")  return(0.75)
      if (factor.is.specific) return(NA)
      return(0) # if -> "Em", "Z", "In"
    }



    if (factor.evaluation == "I") {
      if (resource.evaluation == "Ex") return(1.75)
      if (resource.evaluation == "G")  return(1.50)
      if (resource.evaluation == "R")  return(1.25)
      if (resource.evaluation == "W")  return(1.00)
      if (factor.is.specific) return(NA)
      return(0) # if -> "Em", "Z", "In"
    }





    stop("fail when agregating  - invalid factor or resource evaluation")
  }
)




#' @rdname show
#' @param Aggregation.matrix.membership.difference Aggregation.matrix.membership.difference
#' @export
setMethod("show", "Aggregation.matrix.membership.difference",
          function(object){
            cat("\nAgregation Matrix Membership Difference:\n")
            cat("general evaluation matrix")
            cr <- c(         "1.00",          "0.75",          "0.50", "0.25",    "0",
                             "0", "0")
            c  <- c("1.25",          "1.00",          "0.75", "0.50",    "0",
                    "0", "0")
            lc <- c("1.50", "1.25",          "1.00", "0.75",    "0",
                    "0", "0")
            i  <- c("1.75", "1.50", "1.25", "1.00", "0",
                    "0", "0")

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
