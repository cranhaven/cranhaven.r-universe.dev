#####################################################################################
# pt_est.R
# Provides functions for computing point estimates
# Philip Barrett
# Washington, DC, 26sep2017
#####################################################################################

gsc.unit.i <- function( wt, Y, D, b, i, sig.i=NULL, print.level=0 ){
  # Computes optimal weights for unit i given b
  
  NN <- dim(D)[1] ; TT <- dim(D)[3]   # Dimensions of the problem
  if( is.null(sig.i) ) sig.i <- rep( 1, NN )
  f.i <- function(x){
    wt.f <- wt ; wt.f[i,] <- x
    gsc_target( NN, TT, wt.f, Y, D, b, sig.i )
  } 
  f.grad.i <- function(x){
    wt.f <- wt ; wt.f[i,] <- x
    gsc_target_grad( NN, TT, wt.f, Y, D, b, sig.i )[i+NN*(0:(NN-3))]
  }
  f.grad.i.num <- function(x){
    wt.f <- wt ; wt.f[i,] <- x ; out <- 0 * x ; inc <- 1e-06
    for(i in 1:(NN-2)){
      x.up <- x ; x.up[i] <- x.up[i] + inc 
      x.down <- x ; x.down[i] <- x.down[i] - inc 
      out[i] <- .5 * ( f.i(x.up) - f.i(x.down) ) / inc
    }
    return(out)
  }  ## PURELY FOR DEBUG
  ub <- c( rep(1,NN-2) )
  lb <- c( rep(0,NN-2) )
  opts <- list( algorithm="NLOPT_LD_SLSQP", maxeval=2000, print_level=print.level
                # , check_derivatives = TRUE, check_derivatives_print = "all"
  )
  sol <- nloptr( wt[i,], f.i, f.grad.i, ub=ub, lb=lb, opts=opts )
  # The solution
  return( list( sol=sol$solution, err=f.grad.i(sol$solution) * (sol$solution > 1e-06 ) * 
                  (sol$solution < 1 - 1e-06 ) ) )
}

gsc.unit <- function( wt, Y, D, b, sig.i=NULL, print.level=0 ){
  # Computes optimal weights for all units given b
  NN <- nrow(wt) ; err <- matrix(NA,NN,NN-2)
  if( is.null(sig.i) ) sig.i <- rep( 1, NN )
  for( i in 1:NN ){
    sol <- gsc.unit.i(wt, Y, D, b, i, sig.i, print.level)
    wt[i,] <- sol$sol
    err[i,] <- sol$err
  }
  return( list( sol=wt, err=err ) )
}  

gsc.treat <- function( wt, Y, D, b, sig.i=NULL, print.level=0, g.i=NULL, g.i.grad=NULL ){
  # Computes the optimal b for a given set of weights
  NN <- dim(D)[1] ; MM <- length(b) ; TT <- dim(D)[3]   # Dimensions of the problem
  if( is.null(sig.i) ) sig.i <- rep( 1, NN )
  f.i <- function(x) gsc_target( NN, TT, wt, Y, D, x, sig.i )
  # f.grad.i <- function(x) tail( gsc_target_grad( NN, TT, wt, Y, D, x, sig.i ), MM )
  f.grad.i <- function(x) gsc_target_grad_b( NN, TT, wt, Y, D, x, sig.i )
  ub <- c( rep(Inf,MM) ) ; lb <- c( rep(-Inf,MM) )
  opts <- list( algorithm="NLOPT_LD_SLSQP", maxeval=2000, print_level=print.level
                # , check_derivatives = TRUE, check_derivatives_print = "all"
  )
  if(!is.null(g.i)){ 
    sol <- nloptr( b, f.i, f.grad.i, eval_g_eq = g.i, eval_jac_g_eq =g.i.grad, ub=ub, lb=lb, opts=opts )
  }else{
    sol <- nloptr( b, f.i, f.grad.i, ub=ub, lb=lb, opts=opts )
  }
  # The solution
  return( list( sol=sol$solution, err=f.grad.i(sol$solution) ) )
}

gsc.iter <- function( wt, Y, D, b, sig.i=NULL, max.it=10, tol=1e-05, print.level=0, 
                      g.i=NULL, g.i.grad=NULL ){
  # Iterates over the unit- and treatment-level functions to find a solution
  
  diff <- 2 * tol
  # Initialize tolerance
  if(print.level>=0) message('Iteration report:')
  for(it in 1:max.it){
    if(diff < tol) break
    sol.w <- gsc.unit( wt, Y, D, b, sig.i, print.level )
    sol.b <- gsc.treat( sol.w$sol, Y, D, b, sig.i, print.level, g.i, g.i.grad )
    # New solutions
    diff <- max( abs( b - sol.b$sol ) )
    # Difference between b estimates
    if(print.level>=0) message( '  ', it,  ': diff = ', signif( diff, 4 ) )
    b <- sol.b$sol ; wt <- sol.w$sol
    # Update wt, b
  }
  W <- W_extract( wt, nrow(wt) )
  names(b) <- colnames(D) ; rownames(W) <- colnames(W) <- rownames(D)
  out <- list( wt=wt, W=W, b=b, 
               diff=diff, err=max( abs( c( sol.w$err, sol.b$err ) ) ), it=it, sig.i=sig.i ) 
  class(out) <- 'gsc'
  return( out )
}

gsc.iter.i <- function( wt, Y, D, b, max.it=20, tol=1e-05, print.level=0 ){
  # Computes the solution for unit-level estimation, returning a b_i for each unit i
  NN <- nrow(wt) ; MM <- length(b)
  b.i <- matrix( NA, NN, MM ) ; wt.i <- matrix( NA, NN, NN-2 ) ; err.i <- rep(NA,NN)
  for( i in 1:NN ){
    sig.i <- rep( Inf, NN ) ; sig.i[i] <- 1
    # Weight only on this unit
    # sol.i <- gsc.iter( wt.init, Y, D, b, sig.i, max.it, tol, print.level-1 )
    sol.i <- gsc.iter( wt, Y, D, b, sig.i, max.it, tol, print.level-1 )
    b.i[i,] <- sol.i$b ; wt.i[i,] <- sol.i$wt[i,] ; err.i[i] <- sol.i$err
    message( 'i = ', i, ' ; err = ', signif( err.i[i], 4 ) )
  }
  return( list( b=b.i, wt=wt.i, err=err.i ) )
}

gsc.df.convert <- function( dta, dep.var='y', indep.var=c('D1','D2') ){
# Converts 
  Y <- acast( dta, n ~ t, value.var=dep.var, mean, na.rm=TRUE)
      # dependent variable
  NN <- nrow(Y) ; TT <- ncol(Y) ; MM <- length(indep.var)
      # Dimensions
  D.wide <- lapply( indep.var, function(x) acast( dta, n ~ t, value.var=x, mean, na.rm=TRUE) )
      # The independent variable
  D <- array( 0, dim=c(NN, MM, TT) )
  for( t in 1:TT ) D[ , , t] <- sapply(D.wide, function(x) x[,t] )
  rownames( D ) <- rownames(Y) ; colnames(D) <- indep.var
  return( list( Y=Y, D=D ) )
}


pgsc <- function(dta, dep.var, indep.var, b.init, method, sol.it=NULL, wt.init=NULL, 
                        print.level=0, g.i=NULL, g.i.grad=NULL, ... ){
#' Wrapper function for GSC estimation
#'
#' @param dta A data frame
#' @param dep.var A string defining the dependent variable
#' @param indep.var A vector of strings defining the independent (treatment) variables
#' @param b.init An initial value for the treatment variable coefficients. Must have same 
#'           length as `indep.var`
#' @param method The GSC iteration method to be used. Must be one of:
#' \itemize{
#'     \item{\code{onestep}: "Plain" GSC solution, without weights}
#'     \item{\code{twostep.aggte}: Observations weighted by unit MSEs from the one-step solution.}
#'     \item{\code{twostep.indiv}: Observations weighted by unit MSEs from individual, unit-by-unit unweighted solutions.}
#' }
#' @param sol.it The first step solution used in the two-step methods. If omitted, 
#'           a new one-step solution is computed.
#' @param wt.init An initial value for the weighting matrix
#' @param print.level The level of detail provided in the printed output
#' @param g.i A function defining a restriction on the parameters.  Used in hypothesis testing.
#' @param g.i.grad The gradient of \code{g.i}.
#' @param ... Other arguments to be passed to the optimization
#' 
#' @return Returns the point estimate of the model as a \code{gsc} object, a list with entries:
#' \describe{
#'     \item{b}{The point estimate of the coefficients on the dependent variables}
#'     \item{diff}{The difference between successive iterations}
#'     \item{err}{The maximum error on the within-iteration optimization problems}
#'     \item{it}{Number of iterations require to solve}
#'     \item{sig.i}{The unit-specific MSEs from the solution}
#'     \item{W}{The "full" weighting matrix for counterfactuals, containing own-unit weights (all zero) and unit-N weights}
#'     \item{wt}{The "minimal" weighting matrix, omitting own-unit weights and weights on unit N (which can be computed as one-minus-rowsum)}
#' }
#' 
#' @details See the vignette "Using \code{pgsc}" for an extended example.
#' @examples
#' data("pgsc.dta")
#' sol <- pgsc(pgsc.dta, dep.var = 'y', indep.var = c('D1','D2'), 
#' b.init = c(0,0), method='onestep' )
#' summary(sol)
#' g.i <- function(b) b[1] ; g.i.grad <- function(b) c(1,0)
#' sol.r <- pgsc(pgsc.dta, dep.var = 'y', indep.var = c('D1','D2'), 
#' b.init = sol$b, method='onestep', g.i=g.i, g.i.grad=g.i.grad )
#' summary(sol.r)
  
  l.Y.D <- gsc.df.convert( dta, dep.var, indep.var )
  Y <- l.Y.D$Y ; D <- l.Y.D$D
  NN <- dim(D)[1] ; MM <- dim(D)[2] ; TT <- dim(D)[3] 
      # Dimensions
  if(is.null(wt.init)) wt.init <- matrix( 1 / (NN-1), NN, NN-2 )
  # The initial weights
  
  if( method=='onestep' | is.null(sol.it) )
    sol.it <- gsc.iter(wt.init, Y, D, b.init, print.level=print.level, 
                       g.i=g.i, g.i.grad=g.i.grad, ... )
  # This computes the optimal W & b by iteratively solving for W conditional
  # on b and b conditional on W.
  if( method=='onestep' ){
    return(sol.it)
  }
  
  if( method=='twostep.aggte'){
    sig.i <- gsc_target_i( NN, TT, sol.it$wt, Y, D, matrix( sol.it$b, MM, NN ) )
        # The province-level fitted errors from the initial estimation
    sol.2.step <- gsc.iter( wt.init, Y, D, sol.it$b, sig.i, print.level=print.level, 
                            g.i=g.i, g.i.grad=g.i.grad, ... )
        # The two-step estimator using residual weights generated from an unweighted solution
    return(sol.2.step)
  }
  
  if( method=='twostep.indiv' ){
    sol.i <- gsc.iter.i( wt.init, Y, D, sol.it$b, print.level=print.level )
    # Now fit each unit alone
    sig.i <- gsc_target_i( NN, TT, sol.i$wt, Y, D, t( sol.i$b ) )
    # The fit from the separate, province-level estimation
    sol.2.step <- gsc.iter( wt.init, Y, D, sol.it$b, sig.i, print.level=print.level, 
                            g.i=g.i, g.i.grad=g.i.grad, ... )
    # The two-step estimator using the 
    return(sol.2.step)
  }
}

