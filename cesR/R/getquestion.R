#' Prints out the survey question column label for a given column in a given dataset.
#'
#' @description get_question() prints out the survey question associated with a given
#' column in a given dataset name.
#'
#' @param do a CES survey data object (given as a character string)
#' @param q a column name in a CES survey data object (given as a character string)
#'
#' @details
#' The order of the parameters for `get_question` are data object then column name.
#' Both parameters must be character strings and cannot be reversed.
#' To return the associated survey question, the `get_question` function relies
#' on the `var_label` function from the `labelled` package.
#'
#' @return The associated question \code{q} in a loaded dataset \code{do}.
#'
#' @examples
#' # print out CES code calls
#' get_cescodes()
#'
#' # call CES survey
#' get_ces("ces1988")
#'
#' # request the survey question for column a6
#' get_question("ces1988", "a6")
#'
#' @seealso
#' `get_ces()` function help.
#' `get_cescodes()` help.

#library(labelled)

#' @export
# function to produce the column label for requested dataset and variable
# takes two parameters as character strings
# 'do' data object and 'q' question
get_question <- function(do, q){
  if(exists(do)){                                                     # if data object exists
    if(utils::hasName(get(do), q)){                                   # if data object has the name of the given question
      message(labelled::var_label(get(q, get(do))))                       # print out concatenation of the column label
                                                                      # the get function is required because it
                                                                      # returns the object from the provided character string
    }
    else{
      message("Warning: Variable is not in dataset")                      # else, print this warning if question does not exist
                                                                      # cat is used instead of stop because stop breaks the function
    }
  }
  else{
    message("Warning: Data object does not exist")                        # else, print this warning if data object does not exist
  }
}
