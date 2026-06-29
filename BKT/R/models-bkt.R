#' Bayesian Knowledge Tracing
#'
#' Create a BKT (Bayesian Knowledge Tracing) model object with initial parameters.
#' This function constructs a BKT model by taking in various parameters such as
#' parallelization options, number of fits, random seed, and other model-specific settings.
#' These parameters can later be modified during the fitting or cross-validation process.
#'
#' @param parallel Logical. Indicates whether to use parallel computation.
#'   If set to `TRUE`, multithreading will be used to speed up model training.
#' @param seed Numeric. Seed for the random number generator, which ensures reproducibility
#'   of results.
#' @param num_fits Integer. Number of fit iterations. The best model is selected from
#'   the total iterations.
#' @param folds Integer. Number of folds used for cross-validation.
#'   This parameter is used during cross-validation to divide the data into parts.
#' @param forgets Logical. Whether to include a forgetting factor in the model.
#'   If set to `TRUE`, the model will account for the possibility that learners may forget knowledge.
#' @param fixed List. A nested list specifying which parameters to fix for specific skills during
#'   model fitting. Each skill can have certain parameters, such as "guesses" and "slips", set to
#'   `TRUE` (to fix) or `FALSE` (to let them vary). For example:
#'   \code{list("skill_name" = list("guesses" = TRUE, "slips" = TRUE))}.
#' @param model_type Logical vector. Specifies model variants to use. There are four possible
#'   variants: 'multilearn', 'multiprior', 'multipair', and 'multigs'. Each corresponds to
#'   a different modeling strategy.
#' @param defaults List. The defaults parameter is a list that functions as a query dictionary.
#'   It is used to map column names in the data to the expected variables in the model.
#'   This helps ensure that the model can work with different datasets that may have varying column names.
#' @param ... Other parameters.
#' @return A BKT model object, which can be used by other functions such as fitting
#'   the model, cross-validation, or making predictions.
#' @examples
#' model <- bkt(seed = 42, parallel = FALSE, num_fits = 1)
#' @export
bkt <- function(parallel = TRUE, num_fits = 5, folds = 5, seed = sample(1:1e8, 1), model_type = rep(FALSE, 4), forgets = FALSE, fixed = NULL, defaults = NULL, ...) {
  new("Model", parallel = parallel, num_fits = num_fits, folds = folds, seed = seed, model_type = model_type, forgets = forgets, fixed = fixed, defaults = defaults, ...)
}
