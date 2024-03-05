#' (Restricted) Multidimensional Unfolding Function
#'
#' \code{fastmdu} performs three types of multidimensional unfolding in different combination for row and column objects.
#' The function follows algorithms given by de Leeuw and Heiser (1980), Heiser (1987), and Busing (2010).
#'
#' @param delta an n by m rectangular matrix containing dissimilarities.
#' @param w an identical sized matrix containing nonnegative weights (all ones when omitted).
#' @param p dimensionality (default = 2).
#' @param x either initial or fixed row coordinates (n by p) or independent row variables (n by hx).
#' @param rx Row restriction. If omitted, x is free and x contains the initial row coordinates.
#'        If logical valued, x (n by p) contains the initial row coordinates and rx (n by p) indicates free (false) and fixed (true) row coordinates.
#'        If real valued, x (n by hx) contains hx independent row variables and rx (hx by p) contains the initial row regression coefficients.
#' @param y either initial or fixed column coordinates (m by p) or independent column variables (n by hy).
#' @param ry Column restriction. If omitted, y is free and y contains the initial column coordinates.
#'        If logical valued, y (m by p) contains the initial column coordinates and ry (m by p) indicated free (false) and fixed (true) column coordinates.
#'        If real valued, y (n by hy) contains hy independent column variables and ry (hy by p) contains the initial column regression coefficients.
#' @param MAXITER maximum number of iterations (default = 1024).
#' @param FCRIT relative convergence criterion (default = 0.00000001).
#' @param error.check extensive check validity input parameters (default = FALSE).
#' @param echo print intermediate algorithm results (default = FALSE).
#'
#' @return data original n by m matrix with dissimilarities.
#' @return weights original n by m matrix with dissimilarity weights.
#' @return row.coordinates final n by p matrix with row coordinates.
#' @return col.coordinates final m by p matrix with column coordinates.
#' @return row.coefficients if rx is real valued, final hx by p matrix with row regression coefficients.
#' @return col.coefficients if ry is real valued, final hy by p matrix with column regression coefficients.
#' @return distances final n by m matrix with distances.
#' @return last.iteration final iteration number.
#' @return last.difference final function difference used for convergence testing.
#' @return n.stress final normalized stress value.
#' @return stress.1 final stress-1 value.
#' @return call function call
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
#' @examples
#' library( smacof )
#' data( "breakfast" )
#' breakfast <- as.matrix( breakfast )
#' n <- nrow( breakfast )
#' m <- ncol( breakfast )
#' p <- 2
#' w <- matrix( 1, n, m )
#' x <- matrix( runif( n * p ), n, p )
#' y <- matrix( runif( m * p ), m, p )
#' r <- fastmdu( breakfast, w, p, x, NULL, y, NULL )
#' print( r )
#'
#' @import smacof
#' @export
#' @useDynLib fmdu, .registration=TRUE

fastmdu <- function( delta, w = NULL, p = 2, x = NULL, rx = NULL, y = NULL, ry = NULL, MAXITER = 1024, FCRIT = 0.00000001, error.check = FALSE, echo = FALSE )
{
  # constants
  FREE = 0
  MODEL = 1
  FIXED = 2

 # check for input errors
  if ( error.check == TRUE ) {

    # available
    if ( is.null( x ) ) stop( "missing row data")
    if ( is.null( y ) ) stop( "missing column data")

    # w
    if ( !is.null( w ) ) {
      if ( !is.numeric( w ) ) stop( "w is not numeric" )
      if ( any( w < 0.0 ) ) stop( "negative w not allowed" )
      if ( is.vector( w ) ) {
        if ( n != length( w ) ) stop( "number of rows delta and w do not match")
      }
      else if ( is.matrix( w ) ) {
        if ( n != nrow( w ) ) stop( "number of rows delta and w do not match")
        if ( m != ncol( w ) ) stop( "number of columns delta and d do not match")
      }
      else stop( "w is neither a vector nor a matrix" )
      if ( any( is.na( w ) ) ) w[is.na( w )] <- 0.0
    }

    # delta
    if ( !is.matrix( delta ) ) stop( "delta is not a matrix" )
    if ( !is.numeric( delta ) ) stop( "delta is not numeric" )
    n <- nrow( delta )
    m <- ncol( delta )
    if ( any( is.na( delta ) ) ) {
      if ( is.null( w ) ) {
        w <- matrix( 1, n, m )
        w[is.na( delta )] = 0.0
      }
      else w[is.na( delta )] = 0.0
    }

    # p
    if ( p <= 0 ) stop( "dimensionality p must be greater than 0")

    # x and rx
    if ( !is.matrix( x ) ) stop( "x is not a matrix" )
    if ( !is.numeric( x ) ) stop( "x is not numeric" )
    if ( any( is.na( x ) ) ) stop( "NA's not allowed in x" )
    if ( n != nrow( x ) ) stop( "number of rows x do not match number of rows either delta or w")
    if ( is.null( rx ) ) {
      if ( p != ncol( x ) ) stop( "number of columns x do not match dimensionality p")
    }
    else {
      if ( !is.matrix( rx ) ) stop( "rx is not a matrix" )
      if ( !is.numeric( rx ) && !is.logical( rx ) ) stop( "rx is neither numeric nor logical" )
      if ( any( is.na( rx ) ) ) stop( "NA's not allowed in rx" )
      if ( p != ncol( rx ) ) stop( "number of columns rx do not match dimensionality p")
      if ( is.numeric( rx ) ) {
        if ( ncol( x ) != nrow( rx ) ) stop( "number of rows rx do not match number of columns x")
      }
      if ( is.logical( rx ) ) {
        if ( n != nrow( rx ) ) stop( "number of rows rx do not match number of rows x")
      }
    }

    # y and ry
    if ( !is.matrix( y ) ) stop( "y is not a matrix" )
    if ( !is.numeric( y ) ) stop( "y is not numeric" )
    if ( any( is.na( y ) ) ) stop( "NA's not allowed in y" )
    if ( m != nrow( y ) ) stop( "number of rows y do not match number of columns either delta or w")
    if ( is.null( ry ) ) {
      if ( p != ncol( y ) ) stop( "number of columns y do not match dimensionality p")
    }
    else {
      if ( !is.matrix( ry ) ) stop( "ry is not a matrix" )
      if ( !is.numeric( ry ) && !is.logical( ry ) ) stop( "ry is neither numeric nor logical" )
      if ( any( is.na( ry ) ) ) stop( "NA's not allowed in ry" )
      if ( p != ncol( ry ) ) stop( "number of columns ry do not match dimensionality p")
      if ( is.numeric( ry ) ) {
        if ( ncol( y ) != nrow( ry ) ) stop( "number of rows ry do not match number of columns y")
      }
      if ( is.logical( ry ) ) {
        if ( m != nrow( ry ) ) stop( "number of rows ry do not match number of rows y")
      }
    }

    # MAXITER
    if ( MAXITER < 0 ) stop( "negative maximum number of iterations MAXITER not allowed")

    # FCRIT
    if ( FCRIT < 0.0 ) stop( "negative function convergence criterion not allowed" )
  }

  # initialization
  n <- nrow( delta )
  m <- ncol( delta )
  if ( is.vector( w ) ) w <- matrix( w, n, m )
  xstatus = ifelse( is.null( rx ), FREE, ifelse( is.logical( rx ), FIXED, MODEL ) )
  if ( xstatus == FREE ) {
    bx <- fx <- matrix( 0, n, p )
  }
  if ( xstatus == FIXED ) {
    if ( sum( rx ) == 0 ) xstatus = FREE
    else bx <- fx <- rx
  }
  if ( xstatus == MODEL ) {
    bx <- rx
    hx <- ncol( x )
  }
  ystatus = ifelse( is.null( ry ), FREE, ifelse( is.logical( ry ), FIXED, MODEL ) )
  if ( ystatus == FREE ) {
    by <- fy <- matrix( 0, m, p )
  }
  if ( ystatus == FIXED ) {
    if ( sum( ry ) == 0 ) ystatus = FREE
    else by <- fy <- ry
  }
  if ( ystatus == MODEL ) {
    by <- ry
    hy <- ncol( y )
  }
  d <- matrix( 0, n, m )
  fvalue <- 0.0

  # execution
  if ( is.null( w ) ) {
    if ( all( delta >= 0.0 ) ) {
      if ( xstatus == FREE  && ystatus == FREE  ) result <- ( .C( "Cmdu", n=as.integer(n), m=as.integer(m), delta=as.double(delta), p=as.integer(p), x=as.double(x), fx=as.integer(fx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE= "fmdu" ) )
      if ( xstatus == FREE  && ystatus == FIXED ) result <- ( .C( "Cmdu", n=as.integer(n), m=as.integer(m), delta=as.double(delta), p=as.integer(p), x=as.double(x), fx=as.integer(fx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == FREE  && ystatus == MODEL ) result <- ( .C( "Ccolresmdu", n=as.integer(n), m=as.integer(m), delta=as.double(delta), p=as.integer(p), x=as.double(x), fx=as.integer(fx), hy=as.integer(hy), qy=as.double(y), by=as.double(by), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == FIXED && ystatus == FREE  ) result <- ( .C( "Cmdu", n=as.integer(n), m=as.integer(m), delta=as.double(delta), p=as.integer(p), x=as.double(x), fx=as.integer(fx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == FIXED && ystatus == FIXED ) result <- ( .C( "Cmdu", n=as.integer(n), m=as.integer(m), delta=as.double(delta), p=as.integer(p), x=as.double(x), fx=as.integer(fx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == FIXED && ystatus == MODEL ) result <- ( .C( "Ccolresmdu", n=as.integer(n), m=as.integer(m), delta=as.double(delta), p=as.integer(p), x=as.double(x), fx=as.integer(fx), hy=as.integer(hy), qy=as.double(y), by=as.double(by), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == MODEL && ystatus == FREE  ) result <- ( .C( "Crowresmdu", n=as.integer(n), m=as.integer(m), delta=as.double(delta), p=as.integer(p), hx=as.integer(hx), qx=as.double(x), bx=as.double(bx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == MODEL && ystatus == FIXED ) result <- ( .C( "Crowresmdu", n=as.integer(n), m=as.integer(m), delta=as.double(delta), p=as.integer(p), hx=as.integer(hx), qx=as.double(x), bx=as.double(bx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == MODEL && ystatus == MODEL ) result <- ( .C( "Cresmdu", n=as.integer(n), m=as.integer(m), delta=as.double(delta), p=as.integer(p), hx=as.integer(hx), qx=as.double(x), bx=as.double(bx), hy=as.integer(hy), qy=as.double(y), by=as.double(by), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
    }
    else {
      if ( xstatus == FREE  && ystatus == FREE  ) result <- ( .C( "Cmduneg", n=as.integer(n), m=as.integer(m), delta=as.double(delta), p=as.integer(p), x=as.double(x), fx=as.integer(fx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == FREE  && ystatus == FIXED ) result <- ( .C( "Cmduneg", n=as.integer(n), m=as.integer(m), delta=as.double(delta), p=as.integer(p), x=as.double(x), fx=as.integer(fx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == FREE  && ystatus == MODEL ) result <- ( .C( "Ccolresmduneg", n=as.integer(n), m=as.integer(m), delta=as.double(delta), p=as.integer(p), x=as.double(x), fx=as.integer(fx), hy=as.integer(hy), qy=as.double(y), by=as.double(by), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == FIXED && ystatus == FREE  ) result <- ( .C( "Cmduneg", n=as.integer(n), m=as.integer(m), delta=as.double(delta), p=as.integer(p), x=as.double(x), fx=as.integer(fx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == FIXED && ystatus == FIXED ) result <- ( .C( "Cmduneg", n=as.integer(n), m=as.integer(m), delta=as.double(delta), p=as.integer(p), x=as.double(x), fx=as.integer(fx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == FIXED && ystatus == MODEL ) result <- ( .C( "Ccolresmduneg", n=as.integer(n), m=as.integer(m), delta=as.double(delta), p=as.integer(p), x=as.double(x), fx=as.integer(fx), hy=as.integer(hy), qy=as.double(y), by=as.double(by), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == MODEL && ystatus == FREE  ) result <- ( .C( "Crowresmduneg", n=as.integer(n), m=as.integer(m), delta=as.double(delta), p=as.integer(p), hx=as.integer(hx), qx=as.double(x), bx=as.double(bx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == MODEL && ystatus == FIXED ) result <- ( .C( "Crowresmduneg", n=as.integer(n), m=as.integer(m), delta=as.double(delta), p=as.integer(p), hx=as.integer(hx), qx=as.double(x), bx=as.double(bx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == MODEL && ystatus == MODEL ) result <- ( .C( "Cresmduneg", n=as.integer(n), m=as.integer(m), delta=as.double(delta), p=as.integer(p), hx=as.integer(hx), qx=as.double(x), bx=as.double(bx), hy=as.integer(hy), qy=as.double(y), by=as.double(by), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
    }
  }
  else {
    if ( all( delta >= 0.0 ) ) {
      if ( xstatus == FREE  && ystatus == FREE  ) result <- ( .C( "Cwgtmdu", n=as.integer(n), m=as.integer(m), delta=as.double(delta), W=as.double(w), p=as.integer(p), x=as.double(x), fx=as.integer(fx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == FREE  && ystatus == FIXED ) result <- ( .C( "Cwgtmdu", n=as.integer(n), m=as.integer(m), delta=as.double(delta), W=as.double(w), p=as.integer(p), x=as.double(x), fx=as.integer(fx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == FREE  && ystatus == MODEL ) result <- ( .C( "Ccolreswgtmdu", n=as.integer(n), m=as.integer(m), delta=as.double(delta), W=as.double(w), p=as.integer(p), x=as.double(x), fx=as.integer(fx), hy=as.integer(hy), qy=as.double(y), by=as.double(by), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == FIXED && ystatus == FREE  ) result <- ( .C( "Cwgtmdu", n=as.integer(n), m=as.integer(m), delta=as.double(delta), W=as.double(w), p=as.integer(p), x=as.double(x), fx=as.integer(fx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == FIXED && ystatus == FIXED ) result <- ( .C( "Cwgtmdu", n=as.integer(n), m=as.integer(m), delta=as.double(delta), W=as.double(w), p=as.integer(p), x=as.double(x), fx=as.integer(fx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == FIXED && ystatus == MODEL ) result <- ( .C( "Ccolreswgtmdu", n=as.integer(n), m=as.integer(m), delta=as.double(delta), W=as.double(w), p=as.integer(p), x=as.double(x), fx=as.integer(fx), hy=as.integer(hy), qy=as.double(y), by=as.double(by), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == MODEL && ystatus == FREE  ) result <- ( .C( "Crowreswgtmdu", n=as.integer(n), m=as.integer(m), delta=as.double(delta), W=as.double(w), p=as.integer(p), hx=as.integer(hx), qx=as.double(x), bx=as.double(bx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == MODEL && ystatus == FIXED ) result <- ( .C( "Crowreswgtmdu", n=as.integer(n), m=as.integer(m), delta=as.double(delta), W=as.double(w), p=as.integer(p), hx=as.integer(hx), qx=as.double(x), bx=as.double(bx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == MODEL && ystatus == MODEL ) result <- ( .C( "Creswgtmdu", n=as.integer(n), m=as.integer(m), delta=as.double(delta), W=as.double(w), p=as.integer(p), hx=as.integer(hx), qx=as.double(x), bx=as.double(bx), hy=as.integer(hy), qy=as.double(y), by=as.double(by), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
    }
    else {
      if ( xstatus == FREE  && ystatus == FREE  ) result <- ( .C( "Cwgtmduneg", n=as.integer(n), m=as.integer(m), delta=as.double(delta), W=as.double(w), p=as.integer(p), x=as.double(x), fx=as.integer(fx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == FREE  && ystatus == FIXED ) result <- ( .C( "Cwgtmduneg", n=as.integer(n), m=as.integer(m), delta=as.double(delta), W=as.double(w), p=as.integer(p), x=as.double(x), fx=as.integer(fx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == FREE  && ystatus == MODEL ) result <- ( .C( "Ccolreswgtmduneg", n=as.integer(n), m=as.integer(m), delta=as.double(delta), W=as.double(w), p=as.integer(p), x=as.double(x), fx=as.integer(fx), hy=as.integer(hy), qy=as.double(y), by=as.double(by), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == FIXED && ystatus == FREE  ) result <- ( .C( "Cwgtmduneg", n=as.integer(n), m=as.integer(m), delta=as.double(delta), W=as.double(w), p=as.integer(p), x=as.double(x), fx=as.integer(fx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == FIXED && ystatus == FIXED ) result <- ( .C( "Cwgtmduneg", n=as.integer(n), m=as.integer(m), delta=as.double(delta), W=as.double(w), p=as.integer(p), x=as.double(x), fx=as.integer(fx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == FIXED && ystatus == MODEL ) result <- ( .C( "Ccolreswgtmduneg", n=as.integer(n), m=as.integer(m), delta=as.double(delta), W=as.double(w), p=as.integer(p), x=as.double(x), fx=as.integer(fx), hy=as.integer(hy), qy=as.double(y), by=as.double(by), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == MODEL && ystatus == FREE  ) result <- ( .C( "Crowreswgtmduneg", n=as.integer(n), m=as.integer(m), delta=as.double(delta), W=as.double(w), p=as.integer(p), hx=as.integer(hx), qx=as.double(x), bx=as.double(bx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == MODEL && ystatus == FIXED ) result <- ( .C( "Crowreswgtmduneg", n=as.integer(n), m=as.integer(m), delta=as.double(delta), W=as.double(w), p=as.integer(p), hx=as.integer(hx), qx=as.double(x), bx=as.double(bx), y=as.double(y), fy=as.integer(fy), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
      if ( xstatus == MODEL && ystatus == MODEL ) result <- ( .C( "Creswgtmduneg", n=as.integer(n), m=as.integer(m), delta=as.double(delta), W=as.double(w), p=as.integer(p), hx=as.integer(hx), qx=as.double(x), bx=as.double(bx), hy=as.integer(hy), qy=as.double(y), by=as.double(by), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE = "fmdu" ) )
    }
  }

  # finalization
  if ( xstatus == MODEL ) {
    bx <- matrix( result$bx, hx, p )
    x <- x %*% bx
  }
  else x <- matrix( result$x, n, p )
  if ( ystatus == MODEL ) {
    by <- matrix( result$by, hy, p )
    y <- y %*% by
  }
  else y <- matrix( result$y, m, p )
  d <- matrix( result$d, n, m )
  lastiter <- result$MAXITER
  lastdif <- result$FCRIT
  fvalue <- result$fvalue

  r <- list( data = delta,
             weights = w,
             row.coordinates=x,
             col.coordinates=y,
             row.coefficients=bx,
             col.coefficients=by,
             distances=d,
             last.iteration=lastiter,
             last.difference=lastdif,
             n.stress=fvalue,
             stress.1=sqrt( fvalue),
             call = match.call())
  class(r) <- "fmdu"
  r
} # fastmdu
