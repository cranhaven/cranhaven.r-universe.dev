#' @title Set momentum parameters for a portfolio.model
#' 
#' @description
#' \code{momentum} sets a new alpha for VaR and Expected Shortfall
#'
#' @param model the portfolio.model to be changed
#' @param n_momentum amount of momentum assets long
#' @param n_momentum.short amount of momentum assets short
#' 
#' @return the adapted portfolio.model
#' 
#' @author Ronald Hochreiter, \email{ronald@@algorithmic.finance}
#'
#' @export
momentum <- function(model, n_momentum, n_momentum.short=NULL) {
  model$momentum.long <- n_momentum
  model$momentum.short <- n_momentum.short
  return(model)
}
