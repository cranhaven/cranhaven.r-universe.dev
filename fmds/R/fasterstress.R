#' Faster Stress Function
#'
#' \code{fasterstress} calculates stochastic normalized stress.
#' Neither data nor distances based on z are optimally scaled.
#'
#' @param data an n by m multivariate data matrix.
#' @param z n by p matrix with coordinates.
#' @param nsamples number of samples
#' @param samplesize sample size
#'
#' @return n.stress normalized stress, mean over samples and observations
#' @return se standard error of se, standard deviation over samples
#'
#' @references agrafiotis, and others, and busing
#'
#' @examples
#' n <- 10000
#' m <- 10
#' data <- matrix( runif( n * m ), n, m )
#' p <- 2
#' zinit <- matrix( runif( n * p ), n, p )
#' r <- fastermds( data = data, p = p, z = zinit )
#' s <- fasterstress( data = data, z = r )
#' print( s$n.stress )
#' print( s$se )
#'
#' @author Frank M.T.A. Busing
#'
#' @importFrom stats runif
#' @export
#' @useDynLib fmds, .registration = TRUE

fasterstress <- function( data = NULL,       # multivariate data matrix
                          z = NULL,          # coordinate matrix
                          nsamples = 100,    # number of samples
                          samplesize = 30 )  # sample size
{
  seed <- as.integer( runif( 1, 1, as.integer( .Machine$integer.max ) ) )
  data <- as.matrix( data )
  n <- nrow( data )
  m <- ncol( data )
  z <- as.matrix( z )
  p <- ncol( z )
  stress <- 0.0
  se <- 0.0
  result <- ( .C( "Cfasterstress", n=as.integer(n), m=as.integer(m), data=as.double(t(data)), p=as.integer(p), z=as.double(t(z)), nsamples=as.integer(nsamples), samplesize=as.integer(samplesize), seed=as.integer(seed), stress=as.double(stress), se=as.double(se), PACKAGE = "fmds" ) )
  return( list( n.stress = result$stress, se = result$se, ci.95 = c( result$stress - 1.96 * result$se, result$stress + 1.96 * result$se ) ) )
} # fasterstress
