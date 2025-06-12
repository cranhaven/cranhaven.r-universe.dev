#' Compare models with k-fold cross validation
#'
#' @param ... one or more models on which to perform the cross validation
#' @param k the k in k-fold. cross-validation will use k-1/k of the data for training.
#' @param ntrials how many random partitions to make. Each partition will be one case in the 
#' output of the function
#' @param error_type The kind of output to produce from each cross-validation. See \code{\link{mod_error}} for details.
#' 
#' @details The purpose of cross-validation is to provide "new" data on which to test a model's 
#' performance. In k-fold cross-validation, the data set used to train the model is broken into 
#' new training and testing data. This is accomplished simply by using most of the data for training while 
#' reserving the remaining data for evaluating the model: testing. Rather than training a single model, k models
#' are trained, each with its own particular testing set. The testing sets in the k models are arranged to cover the
#' whole of the data set. On each of the k testing sets, a performance output is calculated. Which output is 
#' most appropriate depends on the kind of model: regression model or classifier. The most basic measure is the mean square error: the 
#' difference between the actual response variable in the testing data and the output of the model 
#' when presented with inputs from the testing data. This is appropriate in many regression models.
#'

#' 
#' @export 
mod_cv <- function(..., k = 10, ntrials = 5, 
                   error_type = c("default", "mse", "sse", "mad", "LL", "mLL", "dev", "class_error")) {
  error_type = match.arg(error_type)
  
  # Get just the names of the models
  full_names <- as.character(lapply(lazyeval::lazy_dots(...), FUN = function(x) x$expr))
  # Now for the models themselves
  models <- list(...)
  # model can be a list. If so, repeat over all the models.
  
  result = NULL
  for (counter in 1:length(models)) {
    this_mod <- models[[counter]]
    truth <- response_values(this_mod)
    pred_error_results <- numeric(ntrials)
    for (this_trial in 1:ntrials) {
      # get the model outputs for each test group against
      # the rest of the data
      tmp <- kfold_trial(this_mod, type = error_type)
      if (this_trial == 1) error_type <- names(tmp)[1]
      pred_error_results[this_trial] <- tmp
    }
    from_this_mod <- data.frame(pred_error_results, model = full_names[counter],
                                stringsAsFactors = FALSE)
    names(from_this_mod)[1] <- error_type

    result <- rbind(result, from_this_mod)
  }

  
  result
}
