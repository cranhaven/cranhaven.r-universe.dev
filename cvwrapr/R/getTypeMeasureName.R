#' Get full name of loss function
#'
#' Get the full name of the loss function from `type.measure` and `family`.
#'
#' @param type.measure Loss function to use for cross-validation.
#' @param family Model family.
#'
#' @return A named vector of length 1. The vector's value is the full name
#' of the loss function, while the name of that element is the short name
#' of the loss function.
getTypeMeasureName <- function(type.measure, family) {
  if ("family" %in% class(family)) family <- "GLM"

  if (type.measure == "deviance") {
    typename <- switch(family,
                       gaussian = "Mean-squared Error",
                       binomial = "Binomial Deviance",
                       poisson = "Poisson Deviance",
                       cox = "Partial Likelihood Deviance",
                       multinomial = "Multinomial Deviance",
                       mgaussian = "Mean-squared Error",
                       GLM = "GLM Deviance"
    )
  } else {
    typename <- switch(type.measure,
                       mse = "Mean-Squared Error",
                       mae = "Mean Absolute Error",
                       auc = "AUC",
                       class = "Misclassification Error",
                       C = "C-index"
    )
  }
  names(typename) <- type.measure
  return(typename)
}
