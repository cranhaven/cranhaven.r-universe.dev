#' @title Set portfolio.model default values
#' 
#' @description
#' \code{aux_portfolio.default} sets portfolio.model default values
#'
#' @param model the portfolio.model to be reset
#' 
#' @return a portfolio.model with all default values set
#' 
#' @author Ronald Hochreiter, \email{ronald@@algorithmic.finance}
#'
#' @export
aux_portfolio.default <- function(model) {
  
  if(!("objective" %in% names(model))) model$objective <- "markowitz"
  if(!("precision" %in% names(model))) model$precision <- 8
  if(!("active.extension" %in% names(model))) model$active.extension <- FALSE
  if(!("sum.portfolio" %in% names(model))) model$sum.portfolio <- 1
  if(!("alpha" %in% names(model))) model$alpha <- 0.05
  if(!("min.mean" %in% names(model))) model$min.mean <- NULL
  if(!("max.mean" %in% names(model))) model$max.mean <- NULL
  if(!("fix.mean" %in% names(model))) model$fix.mean <- NULL
  if(!("sum.long" %in% names(model))) model$sum.long <- NULL
  if(!("sum.short" %in% names(model))) model$sum.short <- NULL
  if(!("momentum.long" %in% names(model))) model$momentum.long <- NULL
  if(!("momentum.short" %in% names(model))) model$momentum.short <- NULL

  return(model)
}
