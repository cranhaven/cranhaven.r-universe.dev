#' Fractional Differences (GNU Octave/MATLAB compatible)
#'
#' "Compute the fractional differences (1 - L)^d * x where L denotes the lag-
#' operator and d is greater than -1." Source: Eaton page 853.
#'
#'
#'
#' @param x A numeric vector
#' @param d A numeric scalar
#'
#' @return Return the fractional differences. 
#'
#'
#'
#' @author Irucka Embry
#'
#'
#' @encoding UTF-8
#'
#'
#' @source
#' \enumerate{
#'    \item r - How to not run an example using roxygen2? - Stack Overflow answered and edited by samkart on Jul 9 2017. (Also see the additional comments in response to the answer.) See \url{https://stackoverflow.com/questions/12038160/how-to-not-run-an-example-using-roxygen2}.
#'    \item devtools - Issues in R package after CRAN asked to replace dontrun by donttest - Stack Overflow answered by Hong Ooi on Sep 1 2020. (Also see the additional comments in response to the answer.) See \url{https://stackoverflow.com/questions/63693563/issues-in-r-package-after-cran-asked-to-replace-dontrun-by-donttest}.
#' }
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 853.
#'
#'
#'
#' @author Irucka Embry, GNU Octave Developers (GNU Octave code)
#'
#'
#'
#' @encoding UTF-8
#'
#'
#' @note
#' Note: Please refer to the iemisc: Examples from GNU Octave Rem, Mod, and
#' fractdiff Compatible Functions vignette for an example
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @importFrom assertthat assert_that is.scalar
#' @importFrom signal fftfilt
#' @importFrom matlab2r nargin
#'
#'
#' @export
fractdiff <- function (x, d) {

args <- nargin()

assert_that(args == 2, msg = "There are not exactly 2 arguments (x and d). Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(!any(is.vector(x) | is.scalar(d)) == FALSE, msg = "Either x is not a vector or d is not a scalar. x must be a vector and d must be a scalar. Please try again.")
# only proceed with vectors for both y and a and provide an error message if the check fails

  N <- 100;

  if (d >= 1) {
    for (k in 1:d) {
      x <- x[2:length(x)] - x[1:length(x) - 1]
    }
}

  if (d > -1) {

    d <- Rem(d, 1)

    if (d != 0) {

      n <- t(0:N)
      w <- Re(gamma (-d+n) / gamma (-d) / gamma (n+1))
      retval <- signal::fftfilt(w, x)
      retval <- retval[1:length(x)]

    } else if(d == 0) {
    
      retval <- x

    }

  } else if (d < -1) {

assert_that(d < -1, msg = "d must be > -1. Please try again.")
# only process with a single latitude point and provide a stop warning if not
    
}

return(retval)

}

