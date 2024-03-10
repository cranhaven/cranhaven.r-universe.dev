#' @examples
#' 
#' # Fit Bayesian SITAR model 
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
#' # Population average distance and velocity curves with default options
#' plot_bgm(model, opt = 'dv')
#' 
#' \donttest{
#' # Individual-specific distance and velocity curves with default options
#' plot_bgm(model, opt = 'DV')
#' 
#' # Population average distance and velocity curves with APGV
#' plot_bgm(model, opt = 'dv', apv = TRUE)
#' 
#' # Individual-specific distance and velocity curves with APGV
#' plot_bgm(model, opt = 'DV', apv = TRUE)
#' 
#' # Population average distance curve, velocity curve, and APGV with CI bands
#' # To construct CI bands, growth parameters are first calculated for each  
#' # posterior draw and then summarized across draws. Therefore,summary 
#' # option must be set to FALSE
#' 
#' plot_bgm(model, opt = 'dv', apv = TRUE, bands = 'dvp', summary = FALSE)
#' 
#' # Adjusted and unadjusted individual curves
#' # Note ipts = NULL (i.e., no interpolation of curve for smoothness). 
#' # This is because it does not a make sense to interploate data when
#' # estimating adjusted curves.
#' 
#' plot_bgm(model, opt = 'au', ipts = NULL)
#' }