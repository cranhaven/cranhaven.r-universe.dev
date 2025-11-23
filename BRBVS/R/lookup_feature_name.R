#' Lookup Feature Name by Code
#'
#' This function searches for a given code in a feature matrix and returns the corresponding feature name.
#' If the code is not found in the feature matrix, `NA` is returned by default.
#'
#' @param code A single code (character) that needs to be matched in the feature matrix.
#' @param feature_matrix A matrix with two columns: the first column containing feature names and
#'   the second column containing corresponding codes. The function will search for the code in this matrix.
#'
#' @return Returns the feature name (character) corresponding to the provided code.
#'   If the code is not found, `NA` is returned. If you wish to retain the original code in case of no match,
#'   modify the function to return `code` instead of `NA`.
#' @noRd
lookup_feature_name <- function(code, feature_matrix) {
  if (code %in% feature_matrix[, "Code"]) {
    return(feature_matrix[feature_matrix[, "Code"]==code][1])
  } else {
    return(NA)  # or return(code) if you want to keep the original code when no match is found
  }
}
