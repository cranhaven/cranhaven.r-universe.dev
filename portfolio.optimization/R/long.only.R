#' @title Disable active extension portfolios
#' 
#' @description
#' \code{long.only} switches a portfolio.model back to long-only by
#' disabling the active extension
#'
#' @param model the portfolio.model to deactivate active extensions
#' 
#' @return portfolio.model with active extension disabled
#' 
#' @author Ronald Hochreiter, \email{ronald@@algorithmic.finance}
#'
#' @export 
long.only <- function(model) {  
  model$asset.bound.lower <- 0
  model$asset.bound.upper <- 1
  model$sum.portfolio <- 1
  model$sum.long <- NULL
  model$sum.short <- NULL
  
  model$active.extension <- FALSE
  
  return(model)
}
