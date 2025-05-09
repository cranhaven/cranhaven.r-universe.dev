#' Linear Multidimensional Scaling Function
#'
#' \code{fastlinearmds} performs linear multidimensional scaling.
#' The function follows algorithms given by de Leeuw and Heiser (1980).
#' The data, dissimilarities and weights, are either symmetric or asymmetric.
#' The dissimilarities are may contain negative values, the weights may not.
#' The configuration is either unrestricted, (partly) fixed, or a linear combination of independent variables.
#' The dissimilarities are optimally linearly transformed.
#'
#' @param delta an n by n squares hollow matrix containing dissimilarities.
#' @param w an identical sized matrix containing non-negative weights (all ones when omitted).
#' @param p dimensionality (default = 2).
#' @param z n by p matrix with initial coordinates.
#' @param r restrictions on the configuration,
#'        either an n by p matrix with booleans indicating free (false) and fixed (true) coordinates
#'        or an n by h numerical matrix with h independent variables.
#' @param b h by p matrix with initial regression coefficients.
#' @param anchor boolean indicating the use of an intercept
#' @param MAXITER maximum number of iterations (default = 1024).
#' @param FCRIT relative convergence criterion function value (default = 0.00000001).
#' @param ZCRIT absolute convergence criterion coordinates (default = 0.000001).
#' @param rotate if TRUE: solution is rotated to principal axes.
#' @param faster logical indicating faster but less precise procedure
#' @param error.check extensive validity check input parameters (default = FALSE).
#' @param echo print intermediate algorithm results (default = FALSE).
#'
#' @return data original n by n matrix with dissimilarities.
#' @return weights original n by n matrix with weights.
#' @return transformed.data final n by n matrix with transformed dissimilarities.
#' @return anchor whether an intervept was used or not.
#' @return coordinates final n by p matrix with coordinates.
#' @return restriction either the fixed coordinates or the independent variables.
#' @return coefficients final h by p matrix with regression coefficients.
#' @return distances final n by n matrix with Euclidean distances between n rows of coordinates.
#' @return last.iteration final iteration number.
#' @return last.difference final function difference used for convergence testing.
#' @return n.stress final normalized stress value.
#' @return rotate if solution is rotated to principal axes.
#' @return faster if a faster procedure has been used.
#'
#' @references de Leeuw, J., and Heiser, W. J. (1980). Multidimensional scaling with restrictions on the configuration.
#'             In P.R. Krishnaiah (Ed.), Multivariate analysis (Vol. 5, pp. 501–522).
#'             Amsterdam, The Netherlands: North-Holland Publishing Company.
#'
#'             Heiser, W.J. (1991). A generalized majorization method for least squares multidimensional scaling of pseudo-distances that may be negative.
#'             Psychometrika, 55, pages 7-27.
#'
#'             Busing, F.M.T.A. (submitted). Node Localization by Multidimensional Scaling with Iterative Majorization: A Psychometric Perspective.
#'             Signal Processing, Elsevier.
#'
#' @examples
#' data( "colors" )
#' delta <- as.matrix( colors^3 )
#' n <- nrow( delta )
#' w <- 1 - diag( n )
#' p <- 2
#' z <- matrix( runif( n * p ), n, p )
#' r <- fastlinearmds( delta, w, p, z, echo = TRUE )
#' summary( r )
#' print( r )
#' plot( r )
#'
#' @author Frank M.T.A. Busing
#' @export
#' @useDynLib fmds, .registration = TRUE

fastlinearmds <- function( delta,
                           w = NULL,
                           p = 2,
                           z = NULL,
                           r = NULL,
                           b = NULL,
                           anchor = TRUE,
                           MAXITER = 1024,
                           FCRIT = 0.00000001,
                           ZCRIT = 0.000001,
                           rotate = TRUE,
                           faster = FALSE,
                           error.check = FALSE,
                           echo = FALSE )
{
  # constants
  FREE = 0
  MODEL = 1
  FIXED = 2
  status = ifelse( is.null( r ), FREE, ifelse( is.logical( r ), FIXED, MODEL ) )

  # arguments validation
  if ( inherits( delta, "dist" ) ) delta <- as.matrix( delta )
  if ( error.check == TRUE ) validate( delta = delta,
                                       w = w,
                                       p = p,
                                       z = z,
                                       r = r,
                                       b = b,
                                       anchor = anchor,
                                       MAXITER = MAXITER,
                                       FCRIT = FCRIT,
                                       ZCRIT = ZCRIT,
                                       rotate = rotate,
                                       faster = faster,
                                       error.check = error.check,
                                       echo = echo )
  if ( any( is.na( w ) ) ) w[is.na( w )] <- 0.0
  if ( any( is.na( delta ) ) ) w[is.na( delta )] <- 0.0
  if ( any( is.na( delta ) ) ) delta[is.na( delta )] <- 0.0

  # initialization
  delta <- as.matrix( delta )
  n <- nrow( delta )
  if ( status == FREE ) {
    r <- NULL
    b <- NULL
    if ( is.null( z ) ) z <- pcoa( delta, p = p, ac = 0.0, q = NULL, faster = faster, error.check = error.check )$coordinates
  }
  if ( status == FIXED ) {
    b <- NULL
    if ( is.null( z ) ) z <- pcoa( delta, p = p, ac = 0.0, q = NULL, faster = faster, error.check = error.check )$coordinates
    rotate <- FALSE
  }
  if ( status == MODEL ) {
    h <- ncol( r )
    if ( is.null( b ) ) b <- pcoa( delta, p = p, ac = 0.0, q = r, faster = FALSE, error.check = FALSE )$coefficients
    z <- r %*% b
  }
  d <- matrix( 0, n, n )
  fvalue <- 0.0

  # execution
  if ( is.null( w ) ) {
    if ( status == FREE  ) result <- ( .C( "Clinmds", n=as.integer(n), delta=as.double(delta), p=as.integer(p), z=as.double(z), d=as.double(d), anchor=as.integer(anchor), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), ZCRIT=as.double(ZCRIT), fvalue=as.double(fvalue), echo=as.integer( echo ), PACKAGE = "fmds" ) )
    if ( status == FIXED ) result <- ( .C( "Cfxdlinmds", n=as.integer(n), delta=as.double(delta), p=as.integer(p), z=as.double(z), r=as.integer(r), d=as.double(d), anchor=as.integer(anchor), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), ZCRIT=as.double(ZCRIT), fvalue=as.double(fvalue), echo=as.integer( echo ), PACKAGE = "fmds" ) )
    if ( status == MODEL ) result <- ( .C( "Cvarlinmds", n=as.integer(n), delta=as.double(delta), p=as.integer(p), h=as.integer(h), r=as.double(r), b=as.double(b), d=as.double(d), anchor=as.integer(anchor), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), ZCRIT=as.double(ZCRIT), fvalue=as.double(fvalue), echo=as.integer( echo ), PACKAGE = "fmds" ) )
  }
  else {
    if ( status == FREE  ) result <- ( .C( "Clinwgtmds", n=as.integer(n), delta=as.double(delta), w=as.double(w), p=as.integer(p), z=as.double(z), d=as.double(d), anchor=as.integer(anchor), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), ZCRIT=as.double(ZCRIT), fvalue=as.double(fvalue), echo=as.integer( echo ), PACKAGE = "fmds" ) )
    if ( status == FIXED ) result <- ( .C( "Cfxdlinwgtmds", n=as.integer(n), delta=as.double(delta), w=as.double(w), p=as.integer(p), z=as.double(z), r=as.integer(r), d=as.double(d), anchor=as.integer(anchor), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), ZCRIT=as.double(ZCRIT), fvalue=as.double(fvalue), echo=as.integer( echo ), PACKAGE = "fmds" ) )
    if ( status == MODEL ) result <- ( .C( "Cvarlinwgtmds", n=as.integer(n), delta=as.double(delta), w=as.double(w), p=as.integer(p), h=as.integer(h), r=as.double(r), b=as.double(b), d=as.double(d), anchor=as.integer(anchor), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), ZCRIT=as.double(ZCRIT), fvalue=as.double(fvalue), echo=as.integer( echo ), PACKAGE = "fmds" ) )
  }

  # finalization
  if ( status == MODEL ) {
    r <- matrix( result$r, n, h )
    b <- matrix( result$b, h, p )
    z <- r %*% b
  }
  else z <- matrix( result$z, n, p )
  if ( rotate == TRUE ) {
    if ( !is.null( w ) ) {
      wrow <- rowSums( w )
      v <- rotation( z, wrow )
    }
    else v <- rotation( z )
    z <- z %*% v
    if ( status == MODEL ) b <- b %*% v
  }

  r <- list( data = delta,
             weights = w,
             transformed.data = matrix( result$delta, n, n ),
             anchor = anchor,
             degree = 0,
             ninner = 0,
             knotstype = 0,
             iknots = NULL,
             approach = 0,
             coordinates = z,
             restriction = r,
             coefficients = b,
             lambda = 0.0,
             alpha = 1.0,
             grouped = FALSE,
             distances = matrix( result$d, n, n ),
             last.iteration = result$MAXITER,
             last.difference = result$FCRIT,
             n.stress = result$fvalue,
             rotate = rotate,
             faster = faster,
             call = match.call() )
  class( r ) <- "fmds"
  r

} # fastlinearmds
