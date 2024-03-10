
#' @examples
#' 
#' # The examples below show the use of *conditional_effects_bgm* to plot  
#' # the population average and individual-specific distance and velocity 
#' # curves.
#' 
#' # Fit Bayesian SITAR model 
#' # data <- berkeley
#' # berkeley_fit <- bgm(x = age, y = height, id = id, data = data, df = 4,
#' #                     chains = 2, iter = 1000, thin = 10)
#' 
#' # To avoid running the model which takes some time, the fitted model has 
#' # already been saved as berkeley_fit.rda object. The model is fitted using 2 
#' # chain  with 1000  iteration per chain (to save time) and setting thin as 1 
#' # (to save memory also).
#' 
#' model <- berkeley_mfit
#' 
#' # Population average distance curve
#' conditional_effects_bgm(model, deriv = 0, re_formula = NA)
#' 
#' # Individual-specific distance curves
#' conditional_effects_bgm(model, deriv = 0, re_formula = NULL)
#' 
#' \donttest{
#' # Population average velocity curve
#' conditional_effects_bgm(model, deriv = 1, re_formula = NA)
#' 
#' # Individual-specific velocity curves
#' conditional_effects_bgm(model, deriv = 1, re_formula = NULL)
#' }
#' 
