#' Aggregation.matrix S4 Class
#'
#' This class was included to act as an abstract class to be inherited by
#' concrete classes that implement their matrix in constructors.
#'
#'
#' @slot name character
#'
#' @export
#'
setClass(
  "Aggregation.matrix",
  representation(
    name = "character"),
  validity = function(object) {
    TRUE
  }

)



setMethod(
  f = "initialize",
  signature = "Aggregation.matrix",
  definition = function(.Object)
  {
    # cat("~~~ Aggregation.matrix: initializator ~~~ \n")
    # Assignment of the slots

    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)






#' AggregateMatrix
#'
#' S4 method to perform Aggregation.Matrix inheirited objects.
#' If a implementation is not provided to a specific aggregation matrix, this
#' implementation will be used.
#' So it allows using different aggregation matrices. Such feature provides
#' flexibility regarding compensatory effects among the criteria.  Therefore it
#' can be adjusted to different multicriteria problems. It is important to
#' highlight that the entries must be compliant to the original described
#' categories,  using the same linguistic variables present in the default
#' aggregation matrix.
#'
#' @param aggregation.matrix aggregation.matrix
#' @param project.portfolio.as.data.frame project.portfolio.as.data.frame
#' @param project.portfolio.specifics.as.data.frame project.portfolio.specifics.as.data.frame
#' @param option.portfolio.as.data.frame option.portfolio.as.data.frame
#'
#' @export
#'
#' @return data.frame
#'
#' @include aggregation-matrix.R
#'
setGeneric(
  "AggregateMatrix",
  function(
    aggregation.matrix,
    project.portfolio.as.data.frame,
    project.portfolio.specifics.as.data.frame,
    option.portfolio.as.data.frame)
    standardGeneric("AggregateMatrix"),
  signature = c("aggregation.matrix",
                "project.portfolio.as.data.frame",
                "project.portfolio.specifics.as.data.frame",
                "option.portfolio.as.data.frame")
)




#' @rdname AggregateMatrix
#'
#' @export
#'
#' @return data.frame
#'
#' @include aggregation-matrix.R
#'
# Function to handle the solving of the agregate matrix. It call agregate for
# Project(j,i) and option(k,i), i vary from 1:number.of.factors
# This function needs a well-behaved set of parameters, said: no NA, all
# values already checked and valid, and all project and options evaluated.
#
# If only one project is provided, the result will be detailed on its factors.
# The same if only one option is provided.
#
setMethod(
  "AggregateMatrix",
  signature = c(
    "Aggregation.matrix",
    "data.frame",
    "data.frame",
    "data.frame"),
  function(
    aggregation.matrix,
    project.portfolio.as.data.frame,
    project.portfolio.specifics.as.data.frame,
    option.portfolio.as.data.frame){

    nrfactors <- length(colnames(project.portfolio.as.data.frame))

    # First possibility:
    # If Project.portfolio contain only one project, then aggregation provides
    # detailed information about options to this project. The result
    # will show a matrix project factors x option
    if (length(row.names(project.portfolio.as.data.frame)) == 1) {
      result <-
        data.frame(
          matrix(
            ncol = length(row.names(option.portfolio.as.data.frame)),
            nrow = nrfactors
          )
        )

      for (j in 1:length(row.names(option.portfolio.as.data.frame))) {
        result[,j]  <- unlist(lapply(1:nrfactors, function(x)
          (Aggregate(aggregation.matrix,
                     project.portfolio.as.data.frame[1, x],  # only 01 project
                     option.portfolio.as.data.frame[j, x],
                     project.portfolio.specifics.as.data.frame[1, x],  # only 01 project
                     nrfactors))))
      }

      #setting column and row names
      names(result) <- row.names(option.portfolio.as.data.frame)
      row.names(result) <- names(project.portfolio.as.data.frame)

      return(result)
    }

    # Second possibility:
    # If Option.portfolio contain only one option, then aggregation provides
    # detailed information about projects related to this option.  The result
    # will show a matrix option factors x projects

    if (length(row.names(option.portfolio.as.data.frame)) == 1) {

      result <-
        data.frame(
          matrix(
            ncol = length(row.names(project.portfolio.as.data.frame)),
            nrow = nrfactors
          )
        )

      for (i in 1:length(row.names(project.portfolio.as.data.frame))) {
        j <- 1
        result[,i]  <- unlist(lapply(1:nrfactors, function(x)
          (Aggregate(aggregation.matrix,
                     project.portfolio.as.data.frame[i, x],
                     option.portfolio.as.data.frame[j, x],
                     project.portfolio.specifics.as.data.frame[i, x],
                     nrfactors))))
      }



    # setting column and row names
      names(result) <- row.names(project.portfolio.as.data.frame)
      row.names(result) <- names(option.portfolio.as.data.frame)

        return(result)
    }


    # Third possibility:
    # If Option.portfolio an Project.portfolio with n> 1 elements. The result
    # will show a matrix projects x option

    result <-
      data.frame(
        matrix(
          ncol = length(row.names(option.portfolio.as.data.frame)),
          nrow = length(row.names(project.portfolio.as.data.frame))
        )
      )



    for (i in 1:length(row.names(project.portfolio.as.data.frame))) {
      for (j in 1:length(row.names(option.portfolio.as.data.frame))) {
        temp.list.agregation <- lapply(1:nrfactors, function(x)
          (Aggregate(aggregation.matrix,
                     project.portfolio.as.data.frame[i, x],
                     option.portfolio.as.data.frame[j, x],
                     project.portfolio.specifics.as.data.frame[i, x],
                     nrfactors)))
        agregation <- NULL
        if (any(is.na(temp.list.agregation))) agregation <- NA
        else agregation <- sum(unlist(temp.list.agregation))
        result[i,j] <- agregation
      }
    }
    # setting column and row names
    names(result) <- row.names(option.portfolio.as.data.frame)
    row.names(result) <- row.names(project.portfolio.as.data.frame)

    return(result)
  }
)

#' Aggregate
#'
#' S4 method do not validate entries, since it is not exported and the data
#' is validated by the constructors. The validation here would be resource
#' consuming.
#'
#' @param factor.evaluation character factor evaluation from project
#' @param resource.evaluation character factor evaluation from option
#' @param factor.is.specific logic indicates that this factor is specific for
#' the project
#' @param nrfactors numeric number of factors evaluated for each project/option
#' @param aggregation.matrix aggregation.matrix
#'
#' @export
#'
#' @return numeric indicate the result factor per option. If a specific factor
#' is not achieved it returns -1
#'
#'
setGeneric(
  "Aggregate",
  function(
    aggregation.matrix,
    factor.evaluation,
    resource.evaluation,
    factor.is.specific,
    nrfactors)
    standardGeneric("Aggregate"),
  signature = c( "aggregation.matrix",
                 "factor.evaluation",
                 "resource.evaluation",
                 "factor.is.specific",
                 "nrfactors")
)




#' @rdname show
#' @param object Aggregation.matrix
#' @export
setMethod("show", "Aggregation.matrix", function(object) print(object@name))


#' show
#' @export
#' @import  methods
setGeneric("show", getGeneric("show", package = "methods"))
