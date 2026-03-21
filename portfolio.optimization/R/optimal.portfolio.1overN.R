#' @title 1 over N portfolio
#' 
#' @description
#' \code{optimal.portfolio.1overN} adds a 1 over N portfolio to the portfolio.model
#'
#' @param model the portfolio.model to compute the portfolio of
#' 
#' @return the portfolio.model including the newly computed optimal portfolio
#' 
#' @author Ronald Hochreiter, \email{ronald@@algorithmic.finance}
#'
#' @export
optimal.portfolio.1overN <- function(model) {
  portfolio <- list()
  portfolio$x <- rep(1/model$assets, model$assets)
  portfolio$x <- round(portfolio$x, model$precision)  
  model$portfolio <- portfolio
  return(model) 
}
