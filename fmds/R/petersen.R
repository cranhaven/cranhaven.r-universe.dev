#' Mark-Recapture Population Size Estimator
#'
#' \code{petersen} returns the estimated population size based on two independent equally-sized samples
#'
#' @param first vector with first sample identifiers (local minima)
#' @param second vector with second sample identifiers (local minima)
#'
#' @return population size estimate
#'
#' @references Busing (2025).
#'             A Simple Population Size Estimator for Local Minima Applied to Multidimensional Scaling.
#'
#' @examples
#' seed <- 1041245
#' set.seed( seed )
#' population <- c( rep( 1:200, 10 ), rep( 201:400, 40 ) )
#' length( population )
#' length( unique( population ) )
#' s1 <- sample( population, 500 )
#' s2 <- sample( population, 500 )
#' Nhat <- petersen( s1, s2 )
#' print( Nhat )
#'
#' @author Frank M.T.A. Busing
#' @export
#' @useDynLib fmds, .registration = TRUE

petersen <- function( first, second )
{
  s1 <- unique( first )
  s2 <- unique( second )
  marked <- length( s1 )
  drawn <- length( s2 )
  oldmarks <- sum( s2 %in% s1 )
  ifelse( oldmarks == 0, NA, marked * ( drawn / oldmarks ) )
} # petersen
