#' Check and Preprocess Input Data
#'
#' This function checks whether input data is a data frame, matrix, vector,
#' or factor variable, and converts it to a numeric matrix.
#'
#' @param input A data frame, matrix, vector, or factor variable to be converted
#'  to a numeric matrix.
#'
#' @return A numeric matrix corresponding to the input data frame.
#'
#' @keywords internal

check_input <- function(input) {
  if (is.matrix(input)) {
    return(input)
  } else if (is.data.frame(input)) {
    # Convert data frame to a numeric matrix
    input <- dftomat(input)
  } else if (is.vector(input) && !is.list(input)) {
    # Convert vector to a data frame and then to a numeric matrix
    input <- dftomat(as.data.frame(input))
  } else if (is.factor(input)) {
    # Convert a factor variable into dummy variables
    var_name <- deparse(substitute(input))
    fac_to_df <- data.frame(input)
    fac_to_df <- dftomat(fac_to_df)

    # Rename dummy variable columns to include original variable name
    colnames(fac_to_df) <- paste(var_name, levels(input), sep = "_")
    input <- fac_to_df
  } else {
    # If input is none of the expected types, stop with an error
    stop("Input must be a data frame, matrix, vector, or factor variable.")
  }
  return(input)
}
