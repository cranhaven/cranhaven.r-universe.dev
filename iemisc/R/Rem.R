#' Remainder After Division (GNU Octave/MATLAB compatible)
#'
#' Obtain the remainder after division in a manner compatible with GNU
#' Octave/MATLAB.
#'
#'
#' @param x,n An R object (array, matrix, vector)
#'
#' @return "Return the remainder of the division x / y." Source: Eaton.
#'
#' @references
#' \enumerate{
#'    \item Samit Basu (2002-2006). FreeMat v4.0, \url{https://freemat.sourceforge.net/help/mathfunctions_rem.html}.
#'    \item John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 564.
#' }
#'
#'
#' @author Irucka Embry, Samit Basu (FreeMat)
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#'
#' @examples
#' 
#' # Example from GNU Octave
#' 
#' library(iemisc)
#' 
#' x <- 23.4
#' y <- 20
#' 
#' Rem(x, y)
#' 
#'
#' # Please refer to the iemisc: Examples from GNU Octave Rem, Mod, and
#' # fractdiff Compatible Functions vignette for additional examples
#'
#' 
#'
#' @importFrom checkmate qtest
#' @importFrom assertthat assert_that
#' @importFrom matlab repmat fix
#'
#'
#' @export
Rem <- function (x, n) {

assert_that(qtest(x, "N+[,]"), msg = "x is NA, NaN, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(n, "N+[,]"), msg = "n is NA, NaN, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails


  if (is.scalar(x)) {x <- repmat(x, size(n))}
  
  if (is.scalar(n)) {n <- repmat(n, size(x))}

  m <- fix(x / n)
  
  y <- x - m * n
  
  return(y)
}










#' Modulus Operation (GNU Octave/MATLAB compatible)
#'
#' Obtain the modulo of x and y in a manner compatible with GNU Octave/MATLAB.
#'
#'
#' @param x,n An R object (array, matrix, vector)
#'
#' @return Return the computed modulo of x and y.
#'
#' @references
#' \enumerate{
#'    \item Samit Basu (2002-2006). FreeMat v4.0, \url{https://freemat.sourceforge.net/help/mathfunctions_mod.html}.
#'    \item John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 564.
#' }
#'
#'
#' @author Irucka Embry, Samit Basu (FreeMat)
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#'
#' @examples
#'
#' # Example from FreeMat
#'
#' library(iemisc)
#' 
#' Mod_octave(18, 12)
#' 
#' # Please refer to the iemisc: Examples from GNU Octave Rem, Mod, and
#' # fractdiff Compatible Functions vignette for additional examples
#'
#' 
#' 
#' 
#'
#' @importFrom checkmate qtest
#' @importFrom assertthat assert_that is.scalar
#' @importFrom matlab repmat
#'
#'
#' @export
Mod_octave <- function (x, n) {

assert_that(qtest(x, "N+[,]"), msg = "x is NA, NaN, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(n, "N+[,]"), msg = "n is NA, NaN, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails


  if (is.scalar(x)) {x <- repmat(x, size(n))}

  if (is.scalar(n)) {n <- repmat(n, size(x))}

  m <- floor(x / n)
  
  y <- x - m * n
  
  y[n == 0] <- x[n == 0]
  
  return(y)
}
