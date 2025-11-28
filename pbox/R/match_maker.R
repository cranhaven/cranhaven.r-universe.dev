#' Generate Query Vector
#'
#' This function defines a generic function for creating a query vector to explore
#' the probabilistic space based on provided matches and data. It is used internally
#' to handle different types of inputs efficiently.
#'
#' @name match_maker
#' @docType methods
#' @export
#' @importFrom stats na.omit
#' @param varSet A data frame or list describing the variable set.
#' @param matches A data frame describing the matches with potential additional control parameters.
#' @param data A data frame representing the data to be queried.
#'
#' @return A modified version of `varSet` with values updated based on `matches`.
setGeneric("match_maker",
           def = function(varSet, matches, data) {
             standardGeneric("match_maker")
           })

#' Method for match_maker
#'
#' This method implements the `match_maker` function for handling specific types
#' of 'varSet', 'matches', and 'data'. It modifies the 'varSet' based on 'matches' which
#' can contain variable names and values to be matched or operations to be performed.
#' It supports operations and direct value assignment.
#'
#' @importFrom stats na.omit
#' @param varSet A data frame or list describing the variable set.
#' @param matches A data frame describing the matches with variable names and corresponding values or operators.
#' @param data A data frame representing the data to be queried.
#'
#' @return A modified version of `varSet` that integrates conditions or values from `matches`.
#' @seealso \code{\link{match_maker}} for the generic function and additional details.
setMethod("match_maker",
          definition= function(varSet, matches, data){

            if(!is.data.frame(varSet)){
              stop("'varSet' must be a data.frame!")
            }
            if(!is.data.frame(matches)){
              stop("'matches' must be a data.frame!")
            }
            if(!is.data.frame(data)){
              stop("'data' must be a data.frame!")
            }

            if(!all(na.omit(trimws(unlist(strsplit(c(matches$Varnames,matches$Varnames2),","))))  %in% varSet$Varnames)){
              stop("Your query mismatch with the variables names in the data!")
            }

            if ('Varnames' %in% names(matches)) {
              matchesVal <- na.omit(matches[, .(Varnames, Value)])
              varSet[match(matchesVal$Varnames, varSet$Varnames), ] <- matchesVal
            }
            if('Varnames2' %in% names(matches)){
              matchesOp <- na.omit(matches[, .(Operator, Varnames2)])
              varSet <- stats_calc(data, matches=matchesOp, varSet)
            }
            return(varSet)
          })
