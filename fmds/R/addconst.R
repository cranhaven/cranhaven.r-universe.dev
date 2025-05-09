#' Additive Constant Function for Classical Multidimensional Scaling
#'
#' \code{addconst} returns the smallest additive constant which,
#' added to the dissimilarities, makes the data true Euclidean distances.
#' Note: NA's are not allowed.
#'
#' @param delta an n by n square symmetric hollow matrix containing (non-negative) dissimilarities.
#' @param faster logical indicating faster but less precise procedure
#' @param error.check extensive check validity input (data) parameters (default = FALSE).
#'
#' @return additive constant
#'
#' @references Cailliez (1983)
#'
#' @examples
#' delta <- as.matrix( eurodist )
#' ac <- addconst( delta, faster = TRUE, error.check = TRUE )
#' print( ac )
#'
#' @author Frank M.T.A. Busing
#' @export
#' @useDynLib fmds, .registration = TRUE

addconst <- function( delta, faster = FALSE, error.check = FALSE )
{
  delta <- as.matrix( delta )
  if ( error.check == TRUE ) validate( delta = delta, faster = faster )
  n <- nrow( delta )
  ac <- 0.0
  if ( faster == FALSE ) result <- ( .C( "Caddconst", n=as.integer(n), delta=as.double(delta), ac=as.double(ac), PACKAGE = "fmds" ) )
  else result <- ( .C( "Cfastaddconst", n=as.integer(n), delta=as.double(delta), ac=as.double(ac), PACKAGE = "fmds" ) )
  return( result$ac )
} # addconst

