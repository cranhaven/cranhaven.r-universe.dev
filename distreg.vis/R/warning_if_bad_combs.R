#' Internal: Function that checks whether chosen covariate combinations
#'   are in the range of original data. Returns true when there is a cov
#'   comb outside of data.
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @keywords internal

range_checker <- function(orig_data, newdata) {

  # If newdata has NA remove it
  newdata <- na.omit(newdata)

  # Only use numeric values
  numerics <- sapply(newdata, FUN = is.numeric)
  newdata <- newdata[, numerics]

  # Check for every column if one of the values lies outside orig min/max
  varnames <- colnames(newdata)
  conds <- sapply(varnames, simplify = FALSE, FUN = function(x)
    return(sapply(newdata[[x]], FUN = function(y, x)
      return(y > min(orig_data[[x]]) & y < max(orig_data[[x]])), x = x)))
  conds <- data.frame(conds, row.names = row.names(newdata))
  if (all(unlist(conds))) {
    result <- NULL
  } else {
    # Which rows have "outliers"?
    outlier_combs <- apply(conds, 1, function(x)
      return(!all(x))) %>%
      which() %>%
      names()
    result <- outlier_combs
  }
  return(result)
}

#' Internal: Function that constructs a warning message for the user when
#'   \code{\link{range_checker}} is TRUE.
#' @keywords internal

bad_range_warning <- function(outlier_combs) {
  if (length(outlier_combs) == 1)
    paste("Prediction", outlier_combs, "has covariate combinations",
          "\nwhich are out of the original data's range")
  else if (length(outlier_combs) > 1)
    paste("Predictions", paste(outlier_combs, collapse = ", "), "have",
          "covariate combinations",
          "\nwhich are out of the original data's range")
}

