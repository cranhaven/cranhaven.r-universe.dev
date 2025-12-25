#####################################################################################
# wald.R
# Provides functions for performing Wald tests on a restricted hypothesis
# Philip Barrett
# Washington, DC, 16sep2018
#####################################################################################



gsc.grad.cl.k <- function(W, Y, D, b, k.idx, sig.i=NULL ){
# Returns the matrix of time-cluster-specific gradients for the kth treatment parameter
  NN <- dim(D)[1] ; MM <- length(b) ; TT <- dim(D)[3]   # Dimensions of the problem
  if( is.null(sig.i) ) sig.i <- rep( 1, NN )
  out <- matrix( NA, nrow=NN, ncol=TT)
  for( nn in 1:NN ){
    for( tt in 1:TT ){
      out[nn,tt] <- 1 / sig.i[nn] * ( ( D[nn, k.idx, tt] - sum( D[,k.idx,tt] * W[nn,]) ) * 
                                        ( Y[nn,tt] - sum( D[nn,,tt] * b ) - sum( W[nn,] * ( Y[,tt] - D[,,tt] %*% b ) ) ) )
    }
  }
  return(out)
}

gsc.grad.cl <- function(W, Y, D, b, sig.i=NULL ){
# Returns the matrix of time-cluster-specific gradients for all treatment parameters
  NN <- dim(D)[1] ; KK <- length(b)
  if( is.null(sig.i) ) sig.i <- rep( 1, NN )
  out <- lapply( 1:KK, function(k) gsc.grad.cl.k(W, Y, D, b, k, sig.i) )
  return(out)
}

gsc.mu.k <- function( grad.cl ){
  # Computes the mu residuals for each cluster after regressing on all prior clusters
  NN <- nrow(grad.cl) ; out <- 0 * grad.cl ; out[1,] <- grad.cl[1,]
  out[2,] <- lm( grad.cl[2,] ~ 0 + grad.cl[1,] )$residuals
  for(i in 3:NN){
    out[i,] <- lm( grad.cl[i,] ~ 0 + t(grad.cl[1:(i-1),]) )$residuals
  }
  return(out)
}

gsc.mu <- function( l.grad.cl ){
# Computes the mu residuals for each cluster after regressing on all prior clusters
  return( lapply( l.grad.cl, gsc.mu.k ) )
}

gsc.s.i <- function(grad.cl){
# Computes the orthogonalized score from the gradients
  mu <- gsc.mu(grad.cl)
  s <- sapply( mu, function(mu.k) apply( mu.k, 1, mean ) )
  # if(is.null(dim(s))) s <- t(s)
  return( t(s) )
}

# gsc.s.i.k <- function( grad.cl ){
#   # Computes the mean for each cluster after regressing on all prior clusters
#   NN <- nrow(grad.cl) ; out <- rep( 0, NN ) ; out[1] <- mean(grad.cl[1,])
#   out[2] <- lm( grad.cl[2,] ~ grad.cl[1,] )$coefficients[1]
#   for(i in 3:NN){
#     out[i] <- lm( grad.cl[i,] ~ t(grad.cl[1:(i-1),]) )$coefficients[1]
#   }
#   return(out)
# }

gsc.wald <- function( s.i, W.d=NULL ){
# Computes the Wald statistic for the Rademacher weights W
  NN <- ncol(s.i)
  if(is.null(W.d)) W.d <- rep(1,NN)
  s.bar.0 <- apply(s.i, 1, mean)
  s.i <- s.i * ( rep( 1, nrow(s.i)) %*% t(W.d) )
  s.bar <- apply(s.i, 1, mean)
  # sig.hat <- Reduce( '+', lapply( 1:NN, function(i) 
  #   ( s.i[,i] - s.bar ) %*% t( ( s.i[,i] - s.bar ) ) ) ) / (NN-1)
  sig.hat <- var(t(s.i))
  # sig.hat <- sig.hat + 1e-09*diag(nrow(sig.hat))
  # Avoids numerical issues
  wald.s <- t(s.bar) %*% solve(sig.hat, s.bar)
  return(c(wald.s))
}

gsc.wald.boot <- function( s.i, n.it=1000, seed=42 ){
  # Bootstraps the Wald statistic with appropriate weights
  NN <- ncol(s.i) ; set.seed(seed)
  W.d <- -1 + 2 * matrix( rbinom(NN*n.it, 1, .5), nrow=NN, ncol=n.it )
  v.s <- rep(NA, n.it)
  for( i in 1:n.it ) v.s[i] <- gsc.wald( s.i, W.d[,i])
  return(v.s)
}

pgsc.wald.test <- function( dta, dep.var, indep.var, sol.rest, n.boot=10000, seed=42 ){
#' A wrapper for the wald test of a restricted solution
#' 
#' @param dta A dataframe
#' @param dep.var A vector of strings of names of dependent variables.
#' @param indep.var A vector of strings of names of independent (treatment) variables.
#' @param sol.rest A restricted solution which is being tested
#' @param n.boot The number of bootstrapped samples for the variance calculation. Default is 10000.
#' @param seed Randomization seed.  Default is 42.
#' 
#' @return Returns the wald test as \code{gsc.wald} object, a list with entries:
#' \describe{
#'     \item{b}{The point estimate of the coefficients on the dependent variables}
#'     \item{S}{The Wald statistic}
#'     \item{s.boot}{The bootstrapped Wald statistic}
#'     \item{p.value}{The p-value for the Wald statistic.}
#' }
#' @details See the vignette "Using \code{pgsc}" for an extended example.
#' @examples
#' data("pgsc.dta")
#' g.i <- function(b) b[1] ; g.i.grad <- function(b) c(1,0)
#' sol.r <- pgsc(pgsc.dta, dep.var = 'y', indep.var = c('D1','D2'), 
#' b.init = c(0,1), method='onestep', g.i=g.i, g.i.grad=g.i.grad )
#' wald <- pgsc.wald.test( pgsc.dta, 'y', indep.var = c('D1','D2'), sol.r )
#' summary(wald)
#' plot(wald)
  
  l.Y.D <- gsc.df.convert( dta, dep.var, indep.var )
  Y <- l.Y.D$Y ; D <- l.Y.D$D
  h.grad <- gsc.grad.cl( sol.rest$W, Y, D, sol.rest$b, sol.rest$sig.i)
      # The gradients of the objective function
  # mu.2 <- gsc.mu(h.grad)
  s.i <- gsc.s.i(h.grad)
  S.stat <- gsc.wald(s.i)
  v.S <- gsc.wald.boot( s.i, n.boot, seed )
  p.val <- mean( v.S > S.stat )
  b.rest <- sol.rest$b
  out <- list( S=S.stat, S.boot=v.S, p.val=p.val, b=b.rest )
  class(out) <- 'gsc.wald'
  return( out )
}