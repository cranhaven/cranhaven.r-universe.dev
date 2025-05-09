#' Asymmetry Function
#'
#' \code{asymmetry} return statistics from asymmetry analyses.
#'
#' @param delta an n by n square asymmetric hollow matrix containing dissimilarities.
#' @param z coordinates (n by p) after symmetric analysis
#' @param error.check extensive check validity input parameters (default = FALSE).
#' @param echo print (intermediate) results (default = FALSE).
#' @return ssaf skew-symmetric sum-of-squares-accounted-for
#' @return vaf skew-symmetric variance-accounted-for
#' @return drifts drift vectors starting from coordinates in z (n by p matrix)
#' @return radii n vector with circle radii
#' @references Gower (1968).
#'
#' @author Frank M.T.A. Busing
#' @export
#' @useDynLib fmds, .registration = TRUE

asymmetry <- function( delta, z, error.check = FALSE, echo = FALSE )
{
  if ( error.check == TRUE ) validate( delta = delta, z = z, error.check = error.check, echo = echo )

  delta <- as.matrix( delta )
  z <- as.matrix( z )

  n <- nrow( z )
  p <- ncol( z )

  # decomposition
  symdelta <- 0.5 * ( delta + t( delta ) )
  skewdelta <- delta - symdelta

  # sum-of-squares accounted for skewness
  ssaf <- sum( skewdelta^2 ) / sum( delta^2 )

  # variance accounted for skewness
  s <- svd( skewdelta )
  idx <- 1:( ( n + 1 ) / 2 ) * 2 - 1
  upper <- sum( s$values[idx[idx <= p]])
  lower <- sum( s$values[idx] )
  vaf <- ifelse( lower != 0.0, upper / lower, 0.0 )

  # drift vectors model
  drifts <- matrix( 0.0, n, p );
  for ( i in 1:n ) {
    for ( j in 1:n ) {
      ap <- z[j,] - z[i,]                            # differences with point j
      alpha <- sqrt( sum( ap^2 ) )                   # sum-of-squared differences
      ap <- ifelse( alpha == 0.0, 0.0, ap / alpha )  # no difference between point i and point j
      ap <- ap * skewdelta[j,i]                      # average asymmetry over sources
      drifts[i,] <- drifts[i,] + ap                  # sum of all directions
    }
    drifts[i,] <- drifts[i,] / sqrt( n - 1 )         # mean direction
  }

  # radius distance model
  radii <- colMeans( skewdelta )
  radii <- radii - min( radii )

  if ( echo == TRUE ) {
    cat( "asymmetry results\n" )
    cat( "  sum-of-squares-accounted-for symmetry      :", 1.0 - ssaf, "\n" )
    cat( "                               skew-symmetry :", ssaf, "\n" )
    cat( "variance-accounted-for symmetry              :", 1.0 - vaf, "\n" )
    cat( "                       skew-symmetry         :", vaf, "\n" )
    cat( "radii                                        :\n" )
    print( radii )
    cat( "drift vectors                                :\n" )
    print( drifts )
    cat( "\n" )
  }

  return( list( ssaf = ssaf, vaf = vaf, driftvectors = drifts, radii = radii ) )

} # asymmetry
