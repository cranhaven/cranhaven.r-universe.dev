#' @title Portfolio Optimization minimizing Standard Deviation
#' 
#' @description
#' \code{portfolio.weights} conducts a Portfolio Optimization minimizing Standard 
#' Deviation based on Markowitz (1952).
#'
#' @param model the portfolio.model to compute the portfolio of
#' 
#' @return the portfolio.model including the newly computed optimal portfolio
#' 
#' @author Ronald Hochreiter, \email{ronald@@algorithmic.finance}
#'
#' @export
optimal.portfolio.markowitz <- function(model) {
  
  ### Variables: x[asset]
  
  n_var <- model$assets
  ix_x <- 1
  
  ### Objective function
  ### minimize { t(x) * Cov(data) * x }

  Objective <- list()
  Objective$quadratic <- cov(model$data)
  Objective$linear <- rep(0, model$assets)
  
  ### Constraints
  
  Constraints <- list(n=n_var, A=NULL, b=NULL, Aeq=NULL, beq=NULL)
  
  # sum(a) { x[a] } == sum.portfolio
  Constraints <- linear.constraint.eq(Constraints, c(1:model$assets), model$sum.portfolio)

  # sum(a) { x[a] * mean[a] } => min.mean
  if(!is.null(model$min.mean)) { Constraints <- linear.constraint.iq(Constraints, c((ix_x):(ix_x+model$assets-1)), -model$min.mean, -1*model$asset.means) }

  # sum(a) { x[a] * mean[a] } == fix.mean
  if(!is.null(model$fix.mean)) { Constraints <- linear.constraint.eq(Constraints, c((ix_x):(ix_x+model$assets-1)), model$fix.mean, model$asset.means) }
  
  ### Bounds
  Bounds <- list()
  Bounds$lower <- model$asset.bound.lower
  Bounds$upper <- model$asset.bound.upper

  ### Solve optimization problem using modopt.quadprog
  solution <- quadprog(Objective$quadratic, Objective$linear, Constraints$A, Constraints$b, Constraints$Aeq, Constraints$beq, Bounds$lower, Bounds$upper)

  ### Add optimal portfolio to model  
  portfolio <- list()
  portfolio$x <- solution$x
  portfolio$x <- round(portfolio$x, model$precision)  
  model$portfolio <- portfolio
  return(model) 
}
