#' @title Trace plot the estimands of a `bartCause::bartc()` model
#' @description Returns a ggplot of the estimated effect over each iteration of the model fit. This is used to visually assess the convergence of Markov chain Monte Carlo (MCMC) sampling. Chains should be well mixed such that no single color is notably separate from others.
#'
#' @param .model a model produced by `bartCause::bartc()`
#' @author Joseph Marlo, George Perrett
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2 dplyr
#' @importFrom bartCause extract
#'
#' @examples
#' \donttest{
#' data(lalonde)
#' confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
#' model_results <- bartCause::bartc(
#'  response = lalonde[['re78']],
#'  treatment = lalonde[['treat']],
#'  confounders = as.matrix(lalonde[, confounders]),
#'  estimand = 'ate',
#'  commonSup.rule = 'none'
#' )
#' plot_trace(.model = model_results)
#' }
plot_trace <- function(.model){

  # ensure model is a of class bartcFit
  validate_model_(.model)

  n_chains <- seq_len(.model$n.chains)

  p <- .model %>%
    bartCause::extract('cate', combineChains = FALSE) %>%
    t() %>%
    as.data.frame() %>%
    tibble() %>%
    mutate(iteration = row_number()) %>%
    pivot_longer(n_chains) %>%
    mutate(Chain = factor(sub('V', '', name), levels = as.character(n_chains))) %>%
    ggplot(aes(x = iteration, y = value, color = Chain)) +
    geom_line(alpha = 0.8) +
    labs(title = 'Diagnostics: Trace plot',
         x = 'Iteration',
         y = toupper(.model$estimand),
         color = 'Chain')

  return(p)
}
