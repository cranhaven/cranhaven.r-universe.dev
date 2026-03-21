#' @title Momentum portfolio including momentum for active extensions
#' 
#' @description
#' \code{optimal.portfolio.momentum} adds a momentum portfolio to the portfolio.model
#'
#' @param model the portfolio.model to compute the portfolio of
#' 
#' @return the portfolio.model including the newly computed optimal portfolio
#' 
#' @author Ronald Hochreiter, \email{ronald@@algorithmic.finance}
#'
#' @export
optimal.portfolio.momentum <- function(model) {
  
  ### Parameter
  
  momentum.n <- 0.1 # if not specified within model
  
  ### Find momentum assets
  asset.mean <- apply(model$data, 2, mean)
  sorted.asset.mean.ix <- sort(asset.mean, decreasing=TRUE, index.return=TRUE)$ix

  ### Initialize weights
  
  weights <- rep(0, model$assets)
  
  ### Active Extension
  
  if (model$active.extension) {
    if (is.null(model$momentum.long)) { 
      n.long <- ceiling(model$assets * momentum.n)
    } else { n.long <- model$momentum.long }
    if (is.null(model$momentum.short)) { 
      n.short <- n.long 
    } else { n.short <- model$momentum.short }
    weights[sorted.asset.mean.ix[1:n.long]] <- model$sum.long/n.long 
    weights[sorted.asset.mean.ix[(model$assets-n.short+1):model$assets]] <- -model$sum.short/n.short
    
  ### No Active Extension
  
  } else {  
    if (is.null(model$momentum.long)) { 
      n.long <- ceiling(model$assets * momentum.n) 
    } else {
      n.long <- model$momentum.long
    }
    weights[sorted.asset.mean.ix[1:n.long]] <- model$sum.portfolio/n.long 
  }
  
  ### Add momentum portfolio to model
  
  portfolio <- list()
  portfolio$x <- weights
  portfolio$x <- round(portfolio$x, model$precision)  
  model$portfolio <- portfolio
  
  return(model) 
}
