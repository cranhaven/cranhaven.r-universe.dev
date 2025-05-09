#' Repeated Cross-Validation Penalized Restricted Multidimensional Scaling Function
#'
#' \code{cv.fastmds} performs repeated cross-validation for a penalized restricted multidimensional scaling model.
#'
#' @param delta an n by n symmatric and hollow matrix containing dissimilarities.
#' @param w an identical sized matrix containing nonnegative weights (all ones when omitted).
#' @param p dimensionality (default = 2).
#' @param q independent variables (n by h).
#' @param b initial regression coefficients (h by p).
#' @param lambda regularization penalty parameter(s) (default = 0.0: no penalty).
#' @param alpha elastic-net parameter (default = 1.0: lasso only).
#' @param grouped boolean for lasso penalty (default = FALSE: ordinary lasso).
#' @param NFOLDS number of folds for the k-fold cross-validation.
#' @param NREPEATS number of repeats for the repeated k-fold cross-validation.
#' @param MAXITER maximum number of iterations (default = 1024).
#' @param FCRIT relative convergence criterion function value (default = 0.00000001).
#' @param ZCRIT absolute convergence criterion coordinates (default = 0.000001).
#' @param error.check extensive check validity input parameters (default = FALSE).
#' @param echo print intermediate algorithm results (default = FALSE).
#'
#' @return mserrors mean squared errors for different values of lambda.
#' @return stderrors standard errors for mean squared errors.
#' @return varnames labels of independent row variables.
#' @return coefficients list with final h by p matrices with regression coefficients (lambda order).
#' @return lambda sorted regularization penalty parameters.
#' @return alpha elastic-net parameter (default = 1.0: lasso only).
#' @return grouped boolean for lasso penalty (default = FALSE: ordinary lasso).
#'
#' @references de Leeuw, J., and Heiser, W. J. (1980). Multidimensional scaling with restrictions on the configuration.
#'             In P.R. Krishnaiah (Ed.), Multivariate analysis (Vol. 5, pp. 501–522).
#'             Amsterdam, The Netherlands: North-Holland Publishing Company.
#'
#'             Heiser,W. J. (1987a). Joint ordination of species and sites: The unfolding technique.
#'             In P. Legendre and L. Legendre (Eds.), Developments in numerical ecology (pp. 189–221).
#'             Berlin, Heidelberg: Springer-Verlag.
#'
#'             Busing, F.M.T.A. (2010). Advances in multidimensional unfolding.
#'             Unpublished doctoral dissertation, Leiden University, Leiden, the Netherlands.
#'
#' @importFrom stats var runif
#' @export
#' @useDynLib fmds, .registration = TRUE
cv.fastmds <- function( delta,
                        w = NULL,
                        p = 2,
                        q = NULL,
                        b = NULL,
                        lambda = 0.0,
                        alpha = 1.0,
                        grouped = FALSE,
                        NFOLDS = 10,
                        NREPEATS = 30,
                        MAXITER = 1024,
                        FCRIT = 0.00000001,
                        ZCRIT = 0.000001,
                        error.check = FALSE,
                        echo = FALSE )
{
  if ( inherits( delta, "dist" ) ) delta <- as.matrix( delta )
  if ( error.check == TRUE ) validate( delta = delta,
                                       w = w,
                                       p = p,
                                       r = q,
                                       b = b,
                                       lambda = lambda,
                                       alpha = alpha,
                                       grouped = grouped,
                                       MAXITER = MAXITER,
                                       FCRIT = FCRIT,
                                       ZCRIT = ZCRIT,
                                       error.check = error.check,
                                       echo = echo )
  n <- nrow( delta )
  if ( any( is.na( delta ) ) ) {
    if ( is.null( w ) ) {
      w <- 1 - diag( n )
      w[is.na( delta )] = 0.0
    }
    else w[is.na( delta )] = 0.0
  }
  if ( is.null( b ) ) {
    dq <- as.matrix( dist( q ) )
    dq <- dq * sqrt( sum( delta^2 ) / sum( dq^2 ) )
    d <- sqrt( 0.5 * ( d^2 + dq^2 ) )
    b <- pcoa( d, p = p, q = b )
  }
  seed <- runif( 1, 1, .Machine$integer.max )
  lambda <- sort( lambda, decreasing = TRUE )
  nlambda <- length( lambda )
  mse.folds <- rep( 0.0, NFOLDS )
  mse.repeats <- rep( 0.0, NREPEATS )
  var.repeats <- rep( 0.0, NREPEATS )
  mse.lambda <- rep( 0.0, nlambda )
  se.lambda <- rep( 0.0, nlambda )
  b.lambda <- array( 0, c( nrow( b ), p, nlambda ) )
  indices <- rep( 1:NFOLDS, length.out = n )

  if ( echo == TRUE ) cat( "running lambda = " )
  for ( l in 1:nlambda ) {
    if ( echo == TRUE ) cat( lambda[l], "\b..." )
    z <- q %*% b
    result <- fastmds( delta = delta, w = w, p = p, z = z, r = q, b = b, anchor = 0.0, lambda = lambda[l], alpha = alpha, grouped = grouped, MAXITER = 65536, FCRIT = 0.0, ZCRIT = 0.0, rotate = FALSE, faster = FALSE, error.check = TRUE, echo = FALSE )
    b <- result$coefficients
    b.lambda[,,l] <- b
    set.seed( seed )

    for ( rep in 1:NREPEATS ) {
      folds <- sample( indices )

      for ( fold in 1:NFOLDS ){
        delta.train <- delta[folds != fold, folds != fold]
        q.train <- q[folds != fold, folds != fold]
        delta.test <- delta[folds == fold, folds == fold]
        q.test <- q[folds == fold, folds == fold]
        z <- q.train %*% b
        result <- fastmds( delta = delta.train, w = w, p = p, z = z, r = q.train, b = b, anchor = 0.0, lambda = lambda[l], alpha = alpha, grouped = grouped, MAXITER = MAXITER, FCRIT = FCRIT, ZCRIT = ZCRIT, rotate = FALSE, faster = FALSE, error.check = TRUE, echo = FALSE )
        coordinates.hat <- q.test %*% result$coefficients
        d.hat.test <- as.matrix( dist( coordinates.hat ))
        mse.folds[fold] <- mean( ( delta.test - d.hat.test )^2 )  # mean squared error within one fold
      }

      mse.repeats[rep] <- mean( mse.folds )  # mean over folds
      var.repeats[rep] <- var( mse.folds )   # variance over folds
    }

    mse.lambda[l] <- mean( mse.repeats )                  # Yousef (2021). A leisurely look at versions and variants of the cross validation estimator
    se.lambda[l] <- sqrt( mean( var.repeats / NFOLDS ) )  # Yousef (2021). Estimating the standard error of cross-validation-based estimators of classifier performance

    if ( echo == TRUE ) cat( "\b\b\b, ")
  }
  if ( echo == TRUE ) cat( "\b\b, done.")

  # rotate non-penalized solution towards closest penalized solution
  if ( lambda[nlambda] == 0.0 && nlambda > 1 ){
    result <- svd( t( b.lambda[,,nlambda] ) %*% b.lambda[,,nlambda - 1] )
    rotation <- result$u %*% t( result$v )
    b.lambda[,,nlambda] <- b.lambda[,,nlambda] %*% rotation
  }

  r <- list( mserrors = mse.lambda,
             stderrors = se.lambda,
             varnames = colnames( q ),
             coefficients = b.lambda,
             lambda = lambda,
             alpha = alpha,
             grouped = grouped,
             call = match.call() )
  class( r ) <- "cv.fmds"
  r

} # cv.fastmds
