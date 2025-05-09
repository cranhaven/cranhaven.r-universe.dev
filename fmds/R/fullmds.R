#' Full Multidimensional Scaling Function
#'
#' \code{fullmds} performs a complete multidimensional scaling analysis.
#' The function follows algorithms given by de Leeuw and Heiser (1980).
#' The data, dissimilarities and weights, are either symmetric or asymmetric.
#' The dissimilarities are may contain negative values, the weights may not.
#' The configuration is either unrestricted, (partly) fixed,
#' or a linear combination of independent variables.
#' The dissimilarities may be transformed by different functions, with related parameters.
#'
#' @param delta an n by n squares hollow matrix containing dissimilarities.
#' @param w an identical sized matrix containing non-negative weights (all ones when omitted).
#' @param p dimensionality (default = 2).
#' @param z n by p matrix with initial coordinates.
#' @param r restrictions on the configuration,
#'        either an n by p matrix with booleans indicating free (false) and fixed (true) coordinates
#'        or an n by h numerical matrix with h independent variables.
#' @param b h by p matrix with initial regression coefficients.
#' @param level type of dissimilarity transformation
#' @param anchor boolean indicating the use of an intercept
#' @param degree spline degree.
#' @param ninner number of interior knots.
#' @param knotstype type of knots, either a vector with knots or the type uniform, percentile, or midpercentile.
#' @param iknots user-provided interior knots
#' @param approach approach to ties: 1 = untie ties, 2 = keep ties tied.
#' @param lambda regularization penalty parameter (default = 0.0: no penalty).
#' @param alpha elastic-net parameter (default = 1.0: lasso only).
#' @param grouped boolean for grouped lasso penalty (default = FALSE: ordinary lasso).
#' @param MAXITER maximum number of iterations (default = 1024).
#' @param FCRIT relative convergence criterion function value (default = 0.00000001).
#' @param ZCRIT absolute convergence criterion coordinates (default = 0.000001).
#' @param rotate if TRUE: solution is rotated to principal axes.
#' @param faster logical indicating faster but less precise procedure.
#' @param error.check extensive validity check input parameters (default = FALSE).
#' @param echo print intermediate algorithm results (default = FALSE).
#'
#' @return data original n by n matrix with dissimilarities.
#' @return weights original n by n matrix with weights.
#' @return transformed.data final n by n matrix with transformed dissimilarities.
#' @return anchor whether an intercept was used or not.
#' @return degree spline degree.
#' @return ninner number of interior knots.
#' @return knotstype type of procedure creating the interior knot sequence.
#' @return iknots interior knots sequence.
#' @return approach apporach to ties: 1 = untie ties, 2 = keep ties tied.
#' @return coordinates final n by p matrix with coordinates.
#' @return restriction either the fixed coordinates or the independent variables.
#' @return coefficients final h by p matrix with regression coefficients.
#' @return lambda (optimal) penalty parameter.
#' @return alpha elastic-net penalty parameter.
#' @return grouped common or grouped lasso penalty.
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
#' delta <- as.matrix( ( colors )^3 )
#' n <- nrow( delta )
#' w <- 1 - diag( n )
#' p <- 2
#' zinit <- matrix( runif( n * p ), n, p )
#' r <- fullmds( delta = delta, w = w, p = p, z = zinit, echo = TRUE )
#' summary( r )
#' print( r )
#' plot( r )
#'
#' @author Frank M.T.A. Busing
#' @export
#' @useDynLib fmds, .registration = TRUE

fullmds <- function( delta,
                     w = NULL,
                     p = 2,
                     z = NULL,
                     r = NULL,
                     b = NULL,
                     level = c( "none", "linear", "power", "box-cox", "spline", "ordinal" ),
                     anchor = TRUE,
                     ninner = 0,
                     degree = 2,
                     knotstype = c( "none", "uniform", "percentile", "midpercentile" ),
                     iknots = NULL,
                     approach = 1,
                     lambda = 0.0,
                     alpha = 1.0,
                     grouped = FALSE,
                     MAXITER = 1024,
                     FCRIT = 0.00000001,
                     ZCRIT = 0.000001,
                     rotate = TRUE,
                     faster = FALSE,
                     error.check = FALSE,
                     echo = FALSE )
{
  level <- match.arg( level, c( "none", "linear", "power", "box-cox", "spline", "ordinal" ), several.ok = FALSE )

  if ( level == "none" ) result <- fastmds( delta, w = w, p = p, z = z, r = r, b = b, lambda = lambda, alpha = alpha, grouped = grouped, MAXITER = MAXITER, FCRIT = FCRIT, ZCRIT = ZCRIT, rotate = rotate, faster = faster, error.check = error.check, echo = echo )
  else if ( level == "linear" ) result <- fastlinearmds( delta, w = w, p = p, z = z, r = r, b = b, anchor = anchor, MAXITER = MAXITER, FCRIT = FCRIT, ZCRIT = ZCRIT, rotate = rotate, faster = faster, error.check = error.check, echo = echo )
  else if ( level == "power" ) result <- fastpowermds( delta, w = w, p = p, z = z, r = r, b = b, MAXITER = MAXITER, FCRIT = FCRIT, ZCRIT = ZCRIT, rotate = rotate, faster = faster, error.check = error.check, echo = echo )
  else if ( level == "box-cox" ) result <- fastboxcoxmds( delta, w = w, p = p, z = z, r = r, b = b, MAXITER = MAXITER, FCRIT = FCRIT, ZCRIT = ZCRIT, rotate = rotate, faster = faster, error.check = error.check, echo = echo )
  else if ( level == "spline" ) result <- fastsplinemds( delta, w = w, p = p, z = z, r = r, b = b, anchor = anchor, ninner = ninner, degree = degree, knotstype = knotstype, iknots = iknots, MAXITER = MAXITER, FCRIT = FCRIT, ZCRIT = ZCRIT, rotate = rotate, faster = faster, error.check = error.check, echo = echo )
  else if ( level == "ordinal" ) result <- fastordinalmds( delta, w = w, p = p, z = z, r = r, b = b, approach = approach, MAXITER = MAXITER, FCRIT = FCRIT, ZCRIT = ZCRIT, rotate = rotate, faster = faster, error.check = error.check, echo = echo )

  class( result ) <- "fmds"
  result

} # fullmds
