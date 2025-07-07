#' Check if loss function is valid for a given family
#'
#' Also throws error if family is invalid.
#'
#' @param type.measure Loss function to use for cross-validation.
#' @param family Model family.
#'
#' @return No return value; called for side effects. (If the function returns
#' instead of throwing an error, it means the loss function is valid for
#' that family.)
checkValidTypeMeasure <- function(type.measure, family) {
  if ("family" %in% class(family)) family <- "GLM"

  if (!(family %in% c("gaussian", "binomial", "poisson", "multinomial",
                      "cox", "mgaussian", "GLM")))
    stop("Invalid family argument")

  if (!(type.measure %in% availableTypeMeasures(family)))
    stop(paste("Invalid type.measure for this family;",
               "see availableTypeMeasures() for possibilities"))
}
