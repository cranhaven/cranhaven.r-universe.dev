#' @title Portfolio Optimization minimizing MAD (Active Extension)
#' 
#' @description
#' \code{optimal.portfolio.mad.long.short} conducts a Portfolio Optimization minimizing Mean 
#' Absolute Deviation (MAD) based on Konno and Yamazaki (1991) including an
#' active extension
#'
#' @param model the portfolio.model to compute the portfolio of
#' 
#' @return the portfolio.model including the newly computed optimal portfolio
#' 
#' @author Ronald Hochreiter, \email{ronald@@algorithmic.finance}
#'
#' @export
optimal.portfolio.mad.long.short <- function(model) {

  ### Portfolio Optimization minimizing Mean Absolute Deviation (MAD)
  # Implementation based on [Konno and Yamazaki 1991]
  
  # minimize { sum(s) { p[s] * y[s] } }
  # for(s) { y[s] + sum(a) { a[s, a] * x[a] } >= 0 }
  # for(s) { y[s] - sum(a) { a[s, a] * x[a] } >= 0 }
  
  ### Variables: x[asset], xp[asset], xm[asset], y[scenario]

  n_var <- 3*model$assets + model$scenarios
  ix_x <- 1
  ix_xp <- ix_x + model$assets
  ix_xm <- ix_xp + model$assets
  ix_y <- ix_xm + model$assets
  
  ### Objective function
  
  # minimize { sum(s) { p[s] * y[s] } } // Konno/Yamazaki: minimize { sum(s) { y[s]/card(y) } }
  Objective <- list()
  Objective$linear <- rep(0, n_var)
  Objective$linear[ix_y:(ix_y+model$scenarios-1)] <- model$scenario.probabilities
  
  ### Constraints
  Constraints <- list(n=n_var, A=NULL, b=NULL, Aeq=NULL, beq=NULL)
  
  # sum(a) { x[a] } == sum.portfolio
  Constraints <- linear.constraint.eq(Constraints, c((ix_x):(ix_x+model$assets-1)), model$sum.portfolio)
  
  # sum(a) { x[a] * mean[a] } => min.mean
  if(!is.null(model$min.mean)) { Constraints <- linear.constraint.iq(Constraints, c((ix_x):(ix_x+model$assets-1)), -model$min.mean, -1*model$asset.means) }
  
  # sum(a) { x[a] * mean[a] } == fix.mean
  if(!is.null(model$fix.mean)) { Constraints <- linear.constraint.eq(Constraints, c((ix_x):(ix_x+model$assets-1)), model$fix.mean, model$asset.means) }
  
  ### Long/Short constraints
  
  # sum(a) { xp[a] } == sum.long
  Constraints <- linear.constraint.eq(Constraints, c(ix_xp:(ix_xp+model$assets-1)), model$sum.long)
  
  # sum(a) { xm[a] } == sum.short
  Constraints <- linear.constraint.eq(Constraints, c(ix_xm:(ix_xm+model$assets-1)), model$sum.short)
  
  # for(a) { xp[a] - xm[a] - x[a] == 0 } // portfolio == xp - xm
  for (a in 0:(model$assets-1)) { Constraints <- linear.constraint.eq(Constraints, c(ix_x+a, ix_xp+a, ix_xm+a), 0,  c(-1, 1, -1)) }
  
  ### MAD constraints
  
  # calculate a[s, a]
  at <- as.matrix(model$data - as.vector(colMeans(as.matrix(model$data))))
  
  # for(s) { y[s] + sum(a) { a[s, a] * x[a] } >= 0 }
  for (s in 0:(model$scenarios-1)) { Constraints <- linear.constraint.iq(Constraints, c((ix_x):(ix_x+model$assets-1), ix_y+s), 0, c(-at[(s+1),], -1)) }
  
  # for(s) { y[s] - sum(a) { a[s, a] * x[a] } >= 0 }
  for (s in 0:(model$scenarios-1)) { Constraints <- linear.constraint.iq(Constraints, c((ix_x):(ix_x+model$assets-1), ix_y+s), 0, c(at[(s+1),], -1)) }
  
  ### Bounds
  Bounds <- list()
  
  # All variables unbounded
  M <- 1e9
  Bounds$lower <- rep(-M, n_var)
  Bounds$upper <- rep(M, n_var)
  
  # Portfolio constrained to model parameters
  Bounds$lower[(ix_x):(ix_x+model$assets-1)] <- model$asset.bound.lower
  Bounds$upper[(ix_x):(ix_x+model$assets-1)] <- model$asset.bound.upper

  # xp >= 0;
  Bounds$lower[(ix_xp):(ix_xp+model$assets-1)] <- 0
  
  # xm >= 0;
  Bounds$lower[(ix_xm):(ix_xm+model$assets-1)] <- 0
  
  ### Solve optimization problem using modopt.linprog  
  solution <- linprog(Objective$linear, Constraints$A, Constraints$b, Constraints$Aeq, Constraints$beq, Bounds$lower, Bounds$upper)
  
  ### Add optimal portfolio to model  
  portfolio <- list()
  portfolio$x <- solution$x[ix_x:(ix_x+model$assets-1)]
  portfolio$x <- round(portfolio$x, model$precision)  
  model$portfolio <- portfolio
  return(model) 
}
