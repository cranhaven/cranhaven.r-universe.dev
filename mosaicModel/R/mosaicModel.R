#' \code{mosaicModel} package
#'
#' Functions for teaching about modeling.
#' 
#' The package offers a handful of high-level functions for evaluating, displaying, 
#' and interpreting models that work in a consistent way across model architectures, e.g.
#' lm, glm, rpart, randomForest, knn3, caret-train, and so on.
#'
#'
#' * `mod_eval()` -- evaluate a model, that is, turn inputs into model values. For many model architectures, you can also get prediction or confidence intervals on the outputs.
#' * `mod_plot()` -- produce a graphical display of the "shape" of a model. There can be as many as 4 input variables shown, along with the output.
#' * `mod_effect()` -- calculate effect sizes, that is, how a change in an input variable changes the output
#' * `mod_error()` -- find the mean square prediction error (or the log likelihood)
#' * `mod_ensemble()` -- create an ensemble of bootstrap replications of the model, that is, models fit to resampled data from the original model.
#' * `mod_cv()` -- carry out cross validation on one or more models. 
#' * `mod_fun()` -- extract a function from a model that implements the inputs-to-output relationship.

#' `mosaicModel` stays out of the business of training models. You do that using functions, e.g.

#' - the familiar `lm` or `glm` provided by the `stats` package
#' - `train` from the `caret` package for machine learning
#' - `rpart`, `randomForest`, `rlm`, and other functions provided by other packages
#'
#' @docType package
#' @name mosaicModel
#' @importFrom dplyr %>%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("."))
  utils::globalVariables(c(".trial"))
  utils::globalVariables(c(".slope"))
  utils::globalVariables(c(".row"))
  utils::globalVariables(c("bind_cols"))
  utils::globalVariables(c("row_number"))
  utils::globalVariables(c("slope"))
}
