#' Parse Query
#'
#' This function defines a generic function to parse a string query into structured
#' data that can be used to explore a pbox object. It extracts components of the query
#' using regular expression matching.
#'
#' @name q_parser
#' @export
#' @importFrom data.table as.data.table
#' @importFrom stringr str_match_all
#' @param query A string representing the query.
#' @return A data table with columns 'Varnames', 'Value', 'Operator', and 'Varnames2',
#'         where numeric values are converted to numeric type, and unnecessary columns are removed.
#' @examples
#' query <- "Vietnam:23"
#' q_parser(query)
setGeneric("q_parser",
           def = function(query) {
             standardGeneric("q_parser")
           })

#' Method for Parsing Queries
#'
#' Implements the `q_parser` function specifically for string input. It uses a regular
#' expression to split the query into its components, converting numeric strings to numeric
#' values where applicable, and structuring the result as a data table for easy manipulation.
#'
#' @param query A string representing the query.
#' @return A data table with the parsed elements of the query.
#' @seealso \code{\link{q_parser}} for the generic function definition.

setMethod("q_parser",
          definition= function(query){
  # Define the regular expression pattern
  #pattern <- "([a-zA-Z]+)(:)(\\d+)|(\\w+)([:])c\\(([^)]+)\\)"
  pattern <- "([a-zA-Z]+)(:)(\\d+\\.?\\d*)|(\\w+)([:])c\\(([^)]+)\\)"
  # Extract matches using the regular expression pattern
  matches <- as.data.table(str_match_all(query, pattern)[[1]])[, -1]
  colnames(matches) <- c("Varnames", "Colon1", "Value", "Operator", "Colon2", "Varnames2")
  matches$Value<-as.numeric(matches$Value)
  matches[,c('Colon1','Colon2'):=NULL]
  matches<-matches[, Filter(function(x) any(!is.na(x)), .SD)]

  return(matches)

})
