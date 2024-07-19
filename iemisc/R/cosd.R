#' Secant (in radians)
#'
#' Calculates the value of secant for each element of \code{x} in radians.
#'
#' @param x A numeric or complex vector containing values in radians
#'
#' @return The secant of each element of \code{x} in radians.
#'
#'
#'
#'
#'
#'
#' @author Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#' @note
#' Note: If you have a degree angle value, use \code{\link{secd}} instead.
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Examples
#' 
#' sec (seq(-2, 2, by = 1) * pi)
#'
#' sec ((3 * pi) / 4)
#'
#' sec (c((7/3) * pi, (5/2) * pi))
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom stringi stri_detect_fixed
#'
#' @export
sec <- function (x) {

# Check for x
assert_that(!any(qtest(x, "N+(,)") == FALSE), msg = "x is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values for x and provide a stop warning if x contains non-finite values


    y <- 1 / cos (x)

  return(y)

}





#' Cosine (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of cosine for each element of \code{x} in degrees in a
#' manner compatible with GNU Octave/MATLAB. Zero is returned for any 'elements
#' where (\code{x} - 90) / 180 is an integer.' Reference: Eaton.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The cosine of each element of \code{x} in degrees. Zero for any
#' 'elements where (\code{x} - 90) / 180 is an integer.'
#'
#'
#'
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
#'
#' @references
#' \enumerate{
#'    \item John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 553.
#'    \item Wikimedia Foundation, Inc. Wikipedia, 24 February 2019, "Radian", \url{https://en.wikipedia.org/wiki/Radian}.
#' }
#'
#'
#' @author David Bateman (GNU Octave cosd), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#' @note
#' Note: If you have a radian (rad) angle value, use \code{\link[base]{cos}} instead.
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Example from GNU Octave cosd
#'
#' cosd(seq(0, 80, by = 10))
#'
#'
#' \donttest{
#' # See Source 1 and Source 2
#' 
#' library(iemisc)
#'
#' try(cosd("90"))
#' }
#'
#'
#'
#'
#' @importFrom stringi stri_detect_fixed stri_detect_fixed
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#'
#'
#' @export
cosd <- function (x) {

# Check for x
assert_that(!any(qtest(x, "N+(,)") == FALSE), msg = "x is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values for x and provide a stop warning if x contains non-finite values


  I <- x / 180

  y <- cos (I * pi)

  I <- I + 0.5

  y <- ifelse(I == fix(I) & is.finite(I), 0, y)

  return(y)

}




#' Inverse cosine (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of inverse cosine for each element of \code{x} in
#' degrees in a manner compatible with GNU Octave/MATLAB.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The inverse cosine of each element of \code{x} in degrees.
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 554.
#'
#'
#'
#' @author David Bateman (GNU Octave acosd), Irucka Embry
#'
#'
#' @note
#' Note: If you have a radian (rad) angle value, use \code{\link[base]{acos}} instead.
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Examples from GNU Octave acosd
#' acosd (seq(0, 1, by = 0.1))
#'
#'
#' @importFrom stringi stri_detect_fixed stri_detect_fixed
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
acosd <- function (x) {

# Check for x
assert_that(!any(qtest(x, "N+(,)") == FALSE), msg = "x is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values for x and provide a stop warning if x contains non-finite values


  y <- acos (x) * 180 / pi

  return(y)

}



#' Sine (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of sine for each element of \code{x} in degrees in a
#' manner compatible with GNU Octave/MATLAB. Zero is returned for any "elements
#' where \code{x} / 180 is an integer." Reference: Eaton.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The sine of each element of \code{x} in degrees. Zero for any
#' "elements where \code{x} / 180 is an integer."
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 553.
#'
#'
#'
#' @author David Bateman (GNU Octave sind), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#' @note
#' Note: If you have a radian (rad) angle value, use \code{\link[base]{sin}} instead.
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Examples from GNU Octave sind
#' sind(seq(10, 90, by = 10))
#'
#' sind(c(0, 180, 360))
#'
#' sind(c(90, 270))
#'
#'
#' @importFrom stringi stri_detect_fixed
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
sind <- function (x) {

# Check for x
assert_that(!any(qtest(x, "N+(,)") == FALSE), msg = "x is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values for x and provide a stop warning if x contains non-finite values


I <- x / 180

y <- sin (I * pi)

y <- ifelse(I == fix(I) & is.finite(I), 0, y)

return(y)

}







#' Inverse sine (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of inverse sine for each element of \code{x} in degrees
#' in a manner compatible with GNU Octave/MATLAB.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The inverse sine of each element of \code{x} in degrees.
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 554.
#'
#'
#'
#' @author David Bateman (GNU Octave asind), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#' @note
#' Note: If you have a radian (rad) angle value, use \code{\link[base]{asin}} instead.
#'
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Examples from GNU Octave asind
#' asind(seq(0, 1, by = 0.1))
#'
#'
#' @importFrom stringi stri_detect_fixed
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
asind <- function (x) {

# Check for x
assert_that(!any(qtest(x, "N+(,)") == FALSE), msg = "x is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values for x and provide a stop warning if x contains non-finite values


  y <- asin (x) * 180 / pi

  return(y)

}




#' Tangent (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of tangent for each element of \code{x} in degrees in a
#' manner compatible with GNU Octave/MATLAB. Zero is returned for any "elements
#' where \code{x} / 180 is an integer and \code{Inf} for elements where
#' (\code{x} - 90) / 180 is an integer." Reference: Eaton.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The tangent of each element of \code{x} in degrees. Zero for any
#' "elements where \code{x} / 180 is an integer and \code{Inf} for elements where
#' (\code{x} - 90) / 180 is an integer."
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 553.
#'
#'
#'
#' @author David Bateman (GNU Octave tand), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#' @note
#' Note: If you have a radian (rad) angle value, use \code{\link[base]{tan}} instead.
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Examples from GNU Octave tand
#' tand(seq(10, 80, by = 10))
#'
#' tand(c(0, 180, 360))
#'
#' tand(c(90, 270))
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom stringi stri_detect_fixed
#'
#' @export
tand <- function (x) {

# Check for x
assert_that(!any(qtest(x, "N+(,)") == FALSE), msg = "x is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values for x and provide a stop warning if x contains non-finite values


  I0 <- x / 180

  I90 <- (x - 90) / 180

  y <- tan (I0 * pi)

y <- ifelse(I0 == fix(I0) & is.finite(I0), 0, y)

y <- ifelse(I90 == fix(I90) & is.finite(I90), Inf, y)

  return(y)

}



#' Inverse tangent (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of inverse tangent for each element of \code{x} in
#' degrees in a manner compatible with GNU Octave/MATLAB.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The inverse tangent of each element of \code{x} in degrees.
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 554.
#'
#'
#'
#' @author David Bateman (GNU Octave atand), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#' @note
#' Note: If you have a radian (rad) angle value, use \code{\link[base]{atan}} instead.
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Examples from GNU Octave atand
#' atand (seq(0, 90, by = 10))
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom stringi stri_detect_fixed
#'
#' @export
atand <- function (x) {

# Check for x
assert_that(!any(qtest(x, "N+(,)") == FALSE), msg = "x is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values for x and provide a stop warning if x contains non-finite values


  y <- 180 / pi * atan (x)

  return(y)

}



#' Secant (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of secant for each element of \code{x} in degrees in a
#' manner compatible with GNU Octave/MATLAB.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The secant of each element of \code{x} in degrees.
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 553.
#'
#'
#'
#' @author David Bateman (GNU Octave secd), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#' @note
#' Note: If you have a radian (rad) angle value, use \code{\link{sec}} instead.
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Examples from GNU Octave secd
#' secd (seq(0, 80, by = 10))
#'
#' secd (c(0, 180, 360))
#'
#' secd (c(90, 270))
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom stringi stri_detect_fixed
#'
#' @export
secd <- function (x) {

# Check for x
assert_that(!any(qtest(x, "N+(,)") == FALSE), msg = "x is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values for x and provide a stop warning if x contains non-finite values


    y <- 1 / cosd (x)

  return(y)

}




#' Inverse secant (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of inverse secant for each element of \code{x} in
#' degrees in a manner compatible with GNU Octave/MATLAB.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The inverse secant of each element of \code{x} in degrees.
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 554.
#'
#'
#'
#' @author David Bateman (GNU Octave asecd), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#' @note
#' Note: If you have a radian (rad) angle value, use \code{\link[pracma]{asec}} instead.
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Examples from GNU Octave asecd
#' asecd (seq(0, 90, by = 10))
#'
#'
#'
#' @importFrom stringi stri_detect_fixed
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom pracma asec
#'
#'
#' @export
asecd <- function (x) {

# Check for x
assert_that(!any(qtest(x, "N+(,)") == FALSE), msg = "x is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values for x and provide a stop warning if x contains non-finite values

    y <- asec (x) * 180 / pi

  return(y)

}



#' Cosecant (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of cosecant for each element of \code{x} in degrees in a
#' manner compatible with GNU Octave/MATLAB.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The cosecant of each element of \code{x} in degrees.
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 554.
#'
#'
#'
#' @author David Bateman (GNU Octave cscd), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#' @note
#' Note: If you have a radian (rad) angle value, use \code{\link[base]{asin}} instead.
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Examples from GNU Octave cscd
#' cscd (seq(0, 90, by = 10))
#'
#' cscd (c(0, 180, 360))
#'
#' cscd (c(90, 270))
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom stringi stri_detect_fixed
#'
#' @export
cscd <- function (x) {

# Check for x
assert_that(!any(qtest(x, "N+(,)") == FALSE), msg = "x is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values for x and provide a stop warning if x contains non-finite values


    y <- 1 / sind (x)

  return(y)

}




#' Inverse cosecant (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of inverse cosecant for each element of \code{x} in
#' degrees in a manner compatible with GNU Octave/MATLAB.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The inverse cosecant of each element of \code{x} in degrees.
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 554.
#'
#'
#'
#' @author David Bateman (GNU Octave acscd), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#' @note
#' Note: If you have a radian (rad) angle value, use \code{\link[pracma]{acsc}} instead.
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Examples from GNU Octave acscd
#' acscd (seq(0, 90, by = 10))
#'
#'
#' @importFrom stringi stri_detect_fixed
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom pracma acsc
#'
#'
#' @export
acscd <- function (x) {

# Check for x
assert_that(!any(qtest(x, "N+(,)") == FALSE), msg = "x is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values for x and provide a stop warning if x contains non-finite values

    y <- acsc (x) * 180 / pi

  return(y)

}





#' Cotangent (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of inverse secant for each element of \code{x} in
#' degrees in a manner compatible with GNU Octave/MATLAB.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The inverse secant of each element of \code{x} in degrees.
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 554.
#'
#'
#'
#' @author David Bateman (GNU Octave cotd), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Examples from GNU Octave cotd
#' cotd (seq(0, 80, by = 10))
#'
#' cotd (c(0, 180, 360))
#'
#' cotd (c(90, 270))
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom stringi stri_detect_fixed
#'
#' @export
cotd <- function (x) {

# Check for x
assert_that(!any(qtest(x, "N+(,)") == FALSE), msg = "x is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values for x and provide a stop warning if x contains non-finite values


    y <- 1 / tand (x)

  return(y)

}




#' Inverse cotangent (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of inverse cotangent for each element of \code{x} in
#' degrees in a manner compatible with GNU Octave/MATLAB.
#'
#' @param x A numeric vector containing values in degrees
#'
#' @return The inverse cotangent of each element of \code{x} in degrees.
#'
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 554.
#'
#'
#'
#' @author David Bateman (GNU Octave acotd), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#' @note
#' Note: If you have a radian (rad) angle value, use \code{\link[base]{atan}} instead.
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Examples from GNU Octave acotd
#' acotd (seq(10, 90, by = 10))
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom stringi stri_detect_fixed
#'
#' @export
acotd <- function (x) {

# Check for x
assert_that(!any(qtest(x, "N+(,)") == FALSE), msg = "x is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values for x and provide a stop warning if x contains non-finite values

    y <- atand (1 / x)

  return(y)

}






#' "Two-argument arc-tangent" (in degrees) [GNU Octave/MATLAB compatible]
#'
#' Calculates the value of the "two-argument arc-tangent" for each element of
#' (\code{y}, \code{x}) in degrees in a manner compatible with GNU Octave/MATLAB.
#'
#' @param x A numeric vector containing values in degrees
#' @param y A numeric vector containing values in degrees
#'
#' @return The "two-argument arc-tangent" of each element of (\code{y}, \code{x})
#'         in degrees. Note: "The arc-tangent of two arguments atan2(y, x)
#'         returns the angle between the x-axis and the vector from the origin
#'         to (x, y), i.e., for positive arguments atan2(y, x) == atan(y/x)."
#'         Source: \code{Trig} (base).
#'
#'
#'
#' @references
#' John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 554.
#'
#'
#'
#' @author Rik Wehbring (GNU Octave atan2d), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#' @note
#' Note: If you have a radian (rad) angle value, use \code{\link[base]{atan2}} instead.
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Examples from GNU Octave atan2d
#' atan2d (a <- seq(-1, 1, by = 0.1), b <- seq(1, -1, by = -0.1))
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom stringi stri_detect_fixed
#'
#' @export
atan2d <- function (y, x) {

# Check for x
assert_that(!any(qtest(x, "N+(,)") == FALSE), msg = "x is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values for x and provide a stop warning if x contains non-finite values

# Check for y
assert_that(!any(qtest(y, "N+(,)") == FALSE), msg = "y is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values for y and provide a stop warning if y contains non-finite values


    z <- 180 / pi * atan2 (y, x)

  return(z)

}
