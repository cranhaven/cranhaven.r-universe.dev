#' compute the model comparison measures: DIC, LPML, or Pearson's residuals
#' 
#' `model_comp` is a generic function that computes the model comparison measures (DIC and LPML) or the Pearson's residuals. Note that the Pearson's residuals are not available for `bayes.nmr` when `df` is either random or fixed but smaller than 2 since the variance of the random effects is not finite.
#' 
#' @param object the output model from fitting a meta analysis/regression model
#' @param type the type of model comparison measure to compute; DIC or LPML
#' @param verbose FALSE by default; If TRUE, then progress bar will appear
#' @param ncores the number of CPU cores to use for parallel processing. It must not exceed the number of existing cores. If unspecified, it will default to 2 cores or the number of existing cores, whichever is smaller.
#' @return dataframe containing the compute the model comparison measures
#' 
#' 
#' @md
#' @export
"model_comp" <- function(object, type = "lpml", verbose=FALSE, ncores=NULL) {
	UseMethod("model_comp", object)
} 