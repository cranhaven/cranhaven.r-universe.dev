#' Stochastic Iterative Majorization Multidimensional Scaling Function
#'
#' \code{fastermds} performs multidimensional scaling using a stochastic iterative majorization algorithm.
#' The data are either dissimilarities (full or only lower triangular part) or multivariate data.
#' The dissimilarities and the weights may not contain negative values.
#' The configuration is either unrestricted or (partly) fixed.
#' Local multidimensional scaling is performed when a boundary is provided.
#' Interval multidimensional scaling is performed with a full dissimilarity matrix,
#' using the lower triangular part for the lower bound and the upper triangular part for the upper bound.
#'
#' One of the following three data formats need to be specified:
#' @param delta dissimilarity matrix, non-negative, square, and hollow.
#' @param lower lower triangular part of dissimilarity matrix.
#' @param data multivariate data matrix.
#' @param w non-negative weights per dissimilarity for delta and lower, and per object for data
#' @param p dimensionality (default = 2).
#' @param z n by p matrix with initial coordinates.
#' @param fixed n by p matrix with booleans indicating free (FALSE) or fixed (TRUE) coordinates.
#' @param linear boolean indicating whether linear is used.
#' @param boundary boundary value for local mds.
#' @param interval interval measurements for interval mds, requires delta data format.
#' @param NCYCLES number of cycles taken by the algorithm (default = 32).
#' @param MINRATE criterion rate of convergence (default = 0.01).
#' @param error.check extensive validity check input parameters (default = FALSE).
#'
#' @return n by p matrix with final coordinates.
#'
#' @references Agrafiotis, and others, and Busing
#'
#' @examples
#' n <- 1000
#' m <- 10
#' delta <- as.matrix( dist( matrix( runif( n * m ), n, m ) ) )
#' p <- 2
#' zinit <- matrix( runif( n * p ), n, p )
#' r <- fastermds( delta = delta, p = p, z = zinit, error.check = TRUE )
#' head( r )
#'
#' @author Frank M.T.A. Busing
#'
#' @importFrom stats runif
#' @export
#' @useDynLib fmds, .registration = TRUE

fastermds <- function( delta = NULL,                                        # dissimilarity matrix
                       lower = NULL,                                        # lower-triangular part of dissimilarity matrix
                       data = NULL,                                         # multivariate data matrix
                       w = NULL,                                            # format matches data format
                       p = 2,                                               # dimensionality (default = 2)
                       z = NULL,                                            # initial coordinates matrix must be provided
                       fixed = NULL,                                        # matrix indicating free (FALSE) or fixed (TRUE) coordinates
                       linear = FALSE,                                      # linear transformation of the dissimilarities
                       boundary = NULL,                                     # local mds is off by default
                       interval = FALSE,                                    # interval mds is off by default
                       NCYCLES = 32,                                        # number of algorithmic cycles
                       MINRATE = 0.01,                                      # minimum learning rate after NCYCLES
                       error.check = FALSE )                                # checks at the expense of runtime
{
  DELTA <- !is.null( delta )
  LOWER <- !is.null( lower )
  DATA <- !is.null( data )
  if ( DELTA + LOWER + DATA != 1 ) stop( "invalid delta/lower/data specification" )
  if ( DELTA && inherits( delta, "dist" ) ) delta <- as.matrix( delta )
  if ( LOWER && inherits( lower, "dist" ) ) lower <- as.vector( lower )
  if ( DATA && inherits( data, "dist" ) ) stop( "invalid data specification" )

  if ( error.check == TRUE ) validate( delta = delta,
                                       lower = lower,
                                       data = data,
                                       w = w,
                                       p = p,
                                       z = z,
                                       r = fixed,
                                       boundary = boundary,
                                       interval = interval,
                                       NCYCLES = NCYCLES,
                                       MINRATE = MINRATE )

  # set problem size
  if ( DELTA ) n <- nrow( delta )
  else if ( LOWER ) n <- ceiling( sqrt( 2 * length( lower ) ) )
  else if ( DATA ) {
    n <- nrow( data )
    m <- ncol( data )
  }

  # initialization
  seed <- as.integer( runif( 1, 1, as.integer( .Machine$integer.max ) ) )

  # .C execution
  if ( is.null( w ) ) {
    if ( is.null( fixed ) ) {
      if ( is.null( boundary ) ) {
        if ( interval == FALSE ) {

          # no weights, no fixed, no local, no interval
          if ( LOWER ) result <- ( .C( "Csimmds1", n=as.integer(n), lower=as.double(lower), p=as.integer(p), z=as.double(t(z)), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds"  ) )
          if ( DELTA ) result <- ( .C( "Csimmds2", n=as.integer(n), delta=as.double(delta), p=as.integer(p), z=as.double(t(z)), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
          if ( DATA ) result <- ( .C( "Csimmds3", n=as.integer(n), m=as.integer(m), data=as.double(t(data)), p=as.integer(p), z=as.double(t(z)), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
        }
        else {

          # no weights, no fixed, no local, yes interval
          if ( DELTA ) result <- ( .C( "Csimmds2interval", n=as.integer(n), delta=as.double(delta), p=as.integer(p), z=as.double(t(z)), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
        }
      }
      else {
        if ( interval == FALSE ) {

          # no weights, no fixed, yes local, no interval
          if ( LOWER ) result <- ( .C( "Csimmds1local", n=as.integer(n), lower=as.double(lower), p=as.integer(p), z=as.double(t(z)), boundary=as.double(boundary), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
          if ( DELTA ) result <- ( .C( "Csimmds2local", n=as.integer(n), delta=as.double(delta), p=as.integer(p), z=as.double(t(z)), boundary=as.double(boundary), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
          if ( DATA ) result <- ( .C( "Csimmds3local", n=as.integer(n), m=as.integer(m), data=as.double(t(data)), p=as.integer(p), z=as.double(t(z)), boundary=as.double(boundary), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
        }
        else {

          # no weights, no fixed, yes local, yes interval
          if ( DELTA ) result <- ( .C( "Csimmds2localinterval", n=as.integer(n), delta=as.double(delta), p=as.integer(p), z=as.double(t(z)), boundary=as.double(boundary), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
        }
      }
    }
    else {
      if ( is.null( boundary ) ) {
        if ( interval == FALSE ) {

          # no weights, yes fixed, no local, no interval
          if ( LOWER ) result <- ( .C( "Csimfxdmds1", n=as.integer(n), lower=as.double(lower), p=as.integer(p), z=as.double(t(z)), fz=as.integer(t(fixed)), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds"  ) )
          if ( DELTA ) result <- ( .C( "Csimfxdmds2", n=as.integer(n), delta=as.double(delta), p=as.integer(p), z=as.double(t(z)), fz=as.integer(t(fixed)), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds"  ) )
          if ( DATA ) result <- ( .C( "Csimfxdmds3", n=as.integer(n), m=as.integer(m), data=as.double(t(data)), p=as.integer(p), z=as.double(t(z)), fz=as.integer(t(fixed)), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds"  ) )
        }
        else {

          # no weights, yes fixed, no local, yes interval
          if ( DELTA ) result <- ( .C( "Csimfxdmds2interval", n=as.integer(n), delta=as.double(delta), p=as.integer(p), z=as.double(t(z)), fz=as.integer(t(fixed)), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds"  ) )
        }
      }
      else {
        if ( interval == FALSE ) {

          # no weights, yes fixed, yes local, no interval
          if ( LOWER ) result <- ( .C( "Csimfxdmds1local", n=as.integer(n), lower=as.double(lower), p=as.integer(p), z=as.double(t(z)), fz=as.integer(t(fixed)), boundary=as.double(boundary), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds"  ) )
          if ( DELTA ) result <- ( .C( "Csimfxdmds2local", n=as.integer(n), delta=as.double(delta), p=as.integer(p), z=as.double(t(z)), fz=as.integer(t(fixed)), boundary=as.double(boundary), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds"  ) )
          if ( DATA ) result <- ( .C( "Csimfxdmds3local", n=as.integer(n), m=as.integer(m), data=as.double(t(data)), p=as.integer(p), z=as.double(t(z)), fz=as.integer(t(fixed)), boundary=as.double(boundary), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds"  ) )
        }
        else {

          # no weights, yes fixed, yes local, yes interval
          if ( DELTA) result <- ( .C( "Csimfxdmds2localinterval", n=as.integer(n), delta=as.double(delta), p=as.integer(p), z=as.double(t(z)), fz=as.integer(t(fixed)), boundary=as.double(boundary), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds"  ) )
        }
      }
    }
  }
  else {
    if ( is.null( fixed ) ) {
      if ( is.null( boundary ) ) {
        if ( interval == FALSE ) {

          # yes weights, no fixed, no local, no interval
          if ( LOWER ) result <- ( .C( "Csimwgtmds1", n=as.integer(n), lower=as.double(lower), w=as.double(w), p=as.integer(p), z=as.double(t(z)), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
          if ( DELTA ) result <- ( .C( "Csimwgtmds2", n=as.integer(n), delta=as.double(delta), w=as.double(w), p=as.integer(p), z=as.double(t(z)), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
          if ( DATA ) result <- ( .C( "Csimwgtmds3", n=as.integer(n), m=as.integer(m), data=as.double(t(data)), w=as.double(w), p=as.integer(p), z=as.double(t(z)), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
        }
        else {

          # yes weights, no fixed, no local, yes interval
          if ( DELTA ) result <- ( .C( "Csimwgtmds2interval", n=as.integer(n), delta=as.double(delta), w=as.double(w), p=as.integer(p), z=as.double(t(z)), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
        }
      }
      else {
        if ( interval == FALSE ) {

          # yes weights, no fixed, yes local, no interval
          if ( LOWER ) result <- ( .C( "Csimwgtmds1local", n=as.integer(n), lower=as.double(lower), w=as.double(w), p=as.integer(p), z=as.double(t(z)), boundary=as.double(boundary), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
          if ( DELTA ) result <- ( .C( "Csimwgtmds2local", n=as.integer(n), delta=as.double(delta), w=as.double(w), p=as.integer(p), z=as.double(t(z)), boundary=as.double(boundary), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
          if ( DATA ) result <- ( .C( "Csimwgtmds3local", n=as.integer(n), m=as.integer(m), data=as.double(t(data)), w=as.double(w), p=as.integer(p), z=as.double(t(z)), boundary=as.double(boundary), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
        }
        else {

          # yes weights, no fixed, yes local, yes interval
          if ( DELTA ) result <- ( .C( "Csimwgtmds2localinterval", n=as.integer(n), delta=as.double(delta), w=as.double(w), p=as.integer(p), z=as.double(t(z)), boundary=as.double(boundary), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
        }
      }
    }
    else {
      if ( is.null( boundary ) ) {
        if ( interval == FALSE ) {

          # yes weights, yes fixed, no local, no interval
          if ( LOWER ) result <- ( .C( "Csimfxdwgtmds1", n=as.integer(n), lower=as.double(lower), w=as.double(w), p=as.integer(p), z=as.double(t(z)), fz=as.integer(t(fixed)), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
          if ( DELTA ) result <- ( .C( "Csimfxdwgtmds2", n=as.integer(n), delta=as.double(delta), w=as.double(w), p=as.integer(p), z=as.double(t(z)), fz=as.integer(t(fixed)), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
          if ( DATA ) result <- ( .C( "Csimfxdwgtmds3", n=as.integer(n), m=as.integer(m), data=as.double(t(data)), w=as.double(w), p=as.integer(p), z=as.double(t(z)), fz=as.integer(t(fixed)), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
        }
        else {

          # yes weights, yes fixed, no local, yes interval
          if ( DELTA ) result <- ( .C( "Csimfxdwgtmds2interval", n=as.integer(n), delta=as.double(delta), w=as.double(w), p=as.integer(p), z=as.double(t(z)), fz=as.integer(t(fixed)), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
        }
      }
      else {
        if ( interval == FALSE ) {

          # yes weights, yes fixed, yes local, no interval
          if ( LOWER ) result <- ( .C( "Csimfxdwgtmds1local", n=as.integer(n), lower=as.double(lower), w=as.double(w), p=as.integer(p), z=as.double(t(z)), fz=as.integer(t(fixed)), boundary=as.double(boundary), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
          if ( DELTA ) result <- ( .C( "Csimfxdwgtmds2local", n=as.integer(n), delta=as.double(delta), w=as.double(w), p=as.integer(p), z=as.double(t(z)), fz=as.integer(t(fixed)), boundary=as.double(boundary), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
          if ( DATA ) result <- ( .C( "Csimfxdwgtmds3local", n=as.integer(n), m=as.integer(m), data=as.double(t(data)), w=as.double(w), p=as.integer(p), z=as.double(t(z)), fz=as.integer(t(fixed)), boundary=as.double(boundary), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
        }
        else {

          # yes weights, yes fixed, yes local, yes interval
          if ( DELTA ) result <- ( .C( "Csimfxdwgtmds2localinterval", n=as.integer(n), delta=as.double(delta), w=as.double(w), p=as.integer(p), z=as.double(t(z)), fz=as.integer(t(fixed)), boundary=as.double(boundary), NCYCLES=as.integer(NCYCLES), MINRATE=as.double(MINRATE), seed=as.integer( seed ), PACKAGE = "fmds" ) )
        }
      }
    }
  }


  # finalization
  z <- matrix( result$z, n, p, byrow = TRUE )
  z

} # fastermds
