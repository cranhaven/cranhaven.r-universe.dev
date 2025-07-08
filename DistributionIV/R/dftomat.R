#' Convert Data Frame to Numeric Matrix
#'
#' This function converts a data frame into a numeric matrix.
#' If the data frame contains factor variables, they are first converted to
#' dummy variables (one-hot encoding).
#' If the data frame contains character variables, they are first converted to
#' factors and then to dummy variables.
#'
#' @param df A data frame to be converted to a numeric matrix.
#'
#' @return A numeric matrix corresponding to the input data frame.
#'
#' @keywords internal
#' @importFrom stats model.matrix

dftomat <- function(df) {
  for (col_name in names(df)) {
    if (is.character(df[[col_name]])) {
      # Convert character variables to factors
      df[[col_name]] <- as.factor(df[[col_name]])

      # Check for too many levels
      if (length(levels(df[[col_name]])) > 0.1 * nrow(df)) {
        stop(paste(col_name, "contains too many factor levels.
                   Check if variable type is correct."))
      }
    }

    if (is.factor(df[[col_name]])) {
      # Create dummy variables for the factor column (one level omitted)
      dummy_vars <- stats::model.matrix(~ df[[col_name]] - 1)  # encode to one-hot
      colnames(dummy_vars) <- make.names(paste(col_name,
                                               levels(df[[col_name]]), sep = "_"))

      # Remove the original factor column and append dummy variables
      df <- df[, !names(df) %in% col_name, drop = FALSE]
      df <- cbind(df, dummy_vars)
    } else {
      # Convert non-factor, non-character variables to numeric
      df[[col_name]] <- as.numeric(df[[col_name]])
    }
  }

  # Convert the data frame to a matrix
  df <- as.matrix(df)
  return(df)
}
