#' Right triangle calculations
#'
#' This function computes the missing length (must have at least 2 sides) and
#' the interior angles (degrees) of a right triangle.
#'
#' Side \code{a} is the side adjacent to angle B and opposite angle A. Side \code{b}
#'   is the side adjacent to angle A and opposite angle B. Side c (hypotenuse)
#'   is opposite the right angle (angle C).
#'
#' This function makes the following calculations:
#' \enumerate{
#'    \item the length of the missing side using the Pythagorean theorem,
#'    \item the area of the right triangle,
#'    \item the altitude of the right triangle,
#'    \item the angle associated with the side named a (degrees),
#'    \item the angle associated with the side named b (degrees), and
#'    \item the angle associated with the side named c (degrees).
#'}
#'
#' @param a numeric vector that contains the known side a, if known
#' @param b numeric vector that contains the known side b, if known
#' @param c numeric vector that contains the known side c (hypotenuse),
#'   if known
#'
#' @return \code{\link[base]{list}} of known sides a, b, and c & the interior angles
#'   A, B, and C (right angle), in degrees, if and only if the given
#'   sides create a right triangle.
#'
#'
#' @source
#' \enumerate{
#'    \item r - Better error message for stopifnot? - Stack Overflow answered by Andrie on Dec 1 2011. See \url{https://stackoverflow.com/questions/8343509/better-error-message-for-stopifnot}.
#'    \item r - switch() statement usage - Stack Overflow answered by Tommy on Oct 19 2011 and edited by Tommy on Mar 6 2012. See \url{https://stackoverflow.com/questions/7825501/switch-statement-usage}.
#'    \item Using Switch Statement in R - Stack Overflow answered by Gavin Simpson on Jul 25 2013. See \url{https://stackoverflow.com/questions/17847034/using-switch-statement-in-r}.
#'    \item r - How to not run an example using roxygen2? - Stack Overflow answered and edited by samkart on Jul 9 2017. (Also see the additional comments in response to the answer.) See \url{https://stackoverflow.com/questions/12038160/how-to-not-run-an-example-using-roxygen2}.
#'    \item devtools - Issues in R package after CRAN asked to replace dontrun by donttest - Stack Overflow answered by Hong Ooi on Sep 1 2020. (Also see the additional comments in response to the answer.) See \url{https://stackoverflow.com/questions/63693563/issues-in-r-package-after-cran-asked-to-replace-dontrun-by-donttest}.
#' }
#'
#'
#' @references
#' \enumerate{
#'    \item r - Better error message for stopifnot? - Stack Overflow answered by Andrie on Dec 1 2011. See \url{https://stackoverflow.com/questions/8343509/better-error-message-for-stopifnot}.
#'    \item Masoud Olia, Ph.D., P.E. and Contributing Authors, \emph{Barron's FE (Fundamentals of Engineering Exam)}, 3rd Edition, Hauppauge, New York: Barron's Educational Series, Inc., 2015, page 44-45.
#'    \item Wikimedia Foundation, Inc. Wikipedia, 28 December 2015, "Pythagorean theorem", \url{https://en.wikipedia.org/wiki/Pythagorean_theorem}.
#'    \item Wikimedia Foundation, Inc. Wikipedia, 26 November 2015, "Radian", \url{https://en.wikipedia.org/wiki/Radian}.
#'    \item Wikimedia Foundation, Inc. Wikipedia, 9 December 2015, "Right triangle", \url{https://en.wikipedia.org/wiki/Right_triangle}.
#' }
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
#'
#'
#'
#'
#'
#' @examples
#'
#' library(iemisc)
#'
#' righttri(a = 3, b = 4, c = 5)
#'
#'
#'
#'
#' \donttest{
#' # See Source 4 and Source 5
#'
#' library(iemisc)
#'
#' try(righttri(0, 2)) # a = 0, b = 2
#'
#' try(righttri(1, 2)) # a = 1, b = 2
#'
#' try(righttri(a = 5, c = 10))
#'
#' try(righttri(a = 3, c = 10))
#' }
#'
#'
#'
#'
#'
#'
#' @importFrom checkmate qtest
#' @importFrom assertthat assert_that
#'
#' @export
righttri <- function (a = NULL, b = NULL, c = NULL) {

checks <- c(a, b, c)

assert_that(!any(all(qtest(checks, "N<=3(0,)") & qtest(checks, "N>1(0,)")) == FALSE), msg = "Either a, b, or c is 0, NA, NaN, Inf, -Inf, empty, or a string. Or, there are not at least 2 sides.  Please try again.")
# only process with finite values with a length of either 2 or 3 and provide an error message if the check fails


if (missing(c)) {

csquared <- a ^ 2 + b ^ 2 # csquared = c ^ 2

c <- sqrt(csquared) # sqrt(csquared) = sqrt(c ^ 2) = c (the missing hypotenuse)

} else if (missing(a)) {

asquared <- c ^ 2 - b ^ 2 # asquared = a ^ 2

a <- sqrt(asquared) # sqrt(asquared) = sqrt(a ^ 2) = a (the missing side)

} else if (missing(b)) {

bsquared <- c ^ 2 - a ^ 2 # bsquared = b ^ 2

b <- sqrt(bsquared) # sqrt(bsquared) = sqrt(b ^ 2) = b (the missing side)

}


csquared <- a ^ 2 + b ^ 2 # csquared = c ^ 2

c <- sqrt(csquared) # sqrt(csquared) = sqrt(c ^ 2) = c (the missing hypotenuse)

a1 <- a
b1 <- b



if (a <= b & b < c) {

a <- a
b <- b

} else {

# Source 2 and 3 begin
a <- switch("a", a = b1)
b <- switch("b", b = a1)
# Source 2 and 3 end
}

triarea <- 0.5 * a * b # length units^2

altitude <- (a * b) / c # length units

R <- c / 2 # radius of circumcircle

r <- (a + b - c) / 2 # radius of the incircle of a right triangle

s <- (a + b + c) / 2 # semi-perimeters

Aanglerad <- atan(a / b) # radians
Aangle <- Aanglerad * (180 / pi) # degrees

Banglerad <- atan(b / a) # radians
Bangle <- Banglerad * (180 / pi) # degrees

Cangle <- 180 - Aangle - Bangle

# not until sin can handle complex values
# Law of Sines
# Canglerad <- asin((c * sin(Bangle * pi / 180)) / a) # radians
# Cangle <- Canglerad * (180 / pi) # degrees

# check to see if this is a right triangle or not
check1a <- s == 2 * R + r
check1b <- a ^ 2 + b ^ 2 + c ^ 2 == 8 * R ^ 2
check2 <- sin(Aangle * pi / 180) ^ 2 + sin(Bangle * pi / 180) ^ 2 + sin(Cangle * pi / 180) ^ 2 == 2
check3 <- triarea == r * (2 * R + r)
check4 <- r == s - c
check5 <- altitude == (a * b) / c
check6 <- c == 2 * R
check7 <- Aangle + Bangle + Cangle == 180

checked <- c(check1a, check1b, check2, check3, check4, check5, check6, check7)

if (all(checked) == FALSE) {

assert_that(any(checked) == FALSE, msg = "This is not a right triangle so the Pythagorean theorem will not work.")
# message that the given sides do not produce a right triangle
  
} else {

return(list(a = a, b = b, c = c, Aangle = Aangle, Bangle = Bangle, Cangle = Cangle))
}
}
