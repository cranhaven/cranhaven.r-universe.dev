#' @title Dictionary of Variable Attributes
#' @description Extracts a specified attribute from each column of a data frame and returns a dictionary as a data frame mapping variable names to their corresponding attribute values.
#'
#' @param df A data frame whose columns may have attached attributes.
#' @param attribute A character string specifying the name of the attribute to extract from each column (e.g., "label").
#' @param na.rm Logical; if \code{TRUE}, rows for which the attribute is missing (\code{NA}) are omitted from the output. Default is \code{TRUE}.
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{name}{The names of the variables in \code{df}.}
#'   \item{description}{The extracted attribute values from each variable.}
#' }
#'
#' @details The function iterates over each column in the input data frame \code{df} and retrieves the specified attribute using \code{attr()}. If the attribute is not found for a column, \code{NA} is returned as its description. The resulting data frame acts as a dictionary for the variables, which is particularly useful for documenting datasets during exploratory data analysis.
#'
#' @examples
#'   # Example: Generate a dictionary of variable labels using the USJudgeRatings dataset.
#'   # This dataset contains ratings on various performance measures for U.S. federal judges.
#'   data("USJudgeRatings")
#'
#'   # Assume that the dataset's variables have been annotated with "label" attributes.
#'   # which is the default label read by dictionary
#'   attr(USJudgeRatings$CONT, "label") <- "Content Quality"
#'   attr(USJudgeRatings$INTG, "label") <- "Integrity"
#'   attr(USJudgeRatings$DMNR, "label") <- "Demeanor"
#'   attr(USJudgeRatings$DILG, "label") <- "Diligence"
#'
#'   # Generate the dictionary of labels
#'   dict <- dictionary(USJudgeRatings, "label")
#'   print(dict)
#'
#' @export
#' @author E. F. Haghish

# extract the attributes for the given label and create the Description columns
dictionary <- function(df, attribute = "label", na.rm = TRUE) {
  vars <- names(df)
  descriptions <- sapply(df, function(x) {
    desc <- attr(x, attribute)
    if (is.null(desc)) NA else desc
  })
  dict_df <- data.frame(name = vars, description = descriptions,
                        stringsAsFactors = FALSE)
  rownames(dict_df) <- NULL

  if (na.rm) dict_df <- na.omit(dict_df)
  return(dict_df)
}

