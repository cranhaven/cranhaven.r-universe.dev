#' Calculating the Darcy friction factor (f) for pipes
#'
#' @description
#' The Darcy friction factor (f) is used in the "Darcy equation (also known as
#' the Weisbach equation or Darcy-Weisbach equation)" to determine the
#' "frictional energy loss for fluids" experiencing either laminar or turbulent
#' flow (which is based on the Reynolds number). [Reference: Lindeburg Manual]
#'
#' @details
#' Please consult the references for the equations:
#'
#' f1 - laminar flow equation (References: Lindeburg Manual and Zeghadnia)
#' f2 - Moody equation (References: Genić and Zeghadnia)
#' f3 - Romeo, et. al. equation (References: Genić and Zeghadnia)
#' f4 - Žarko Ćojbašića and Dejan Brkić equation (Reference: Zeghadnia)
#' f5 - Colebrook-White equation (References: Genić and Praks)
#' f6 - Swamee-Jaine equation (References: Genić and Zeghadnia)
#' f7 - Zigrang-Sylvester equation (References: Genić and Zeghadnia)
#' f8 - Vatankhah equation (Reference: Zeghadnia)
#'
#'
#'
#' @references
#' \enumerate{
#'    \item Steven C. Chapra, \emph{Applied Numerical Methods with MATLAB for Engineers and Scientists}, Second Edition, Boston, Massachusetts: McGraw-Hill, 2008, pages 157-161.
#'    \item Didier Clamond, "Efficient resolution of the Colebrook equation", \emph{Ind. Eng. Chem. Res.}, 2009, 48 (7), pages 3665-3671, \url{https://arxiv.org/abs/0810.5564} and \url{https://math.univ-cotedazur.fr/~didierc/DidPublis/ICR_2009.pdf}
#'    \item Srbislav Genić, Ivan Arandjelović, Petar Kolendić, Marko Jarić, Nikola Budimir, and Vojislav Genić, "A Review of Explicit Approximations of Colebrook's Equation", \emph{FME (Faculty of Mechanical Engineering, Belgrade) Transactions}, 2011, 39, pages 67-71, \url{https://www.mas.bg.ac.rs/_media/istrazivanje/fme/vol39/2/04_mjaric.pdf}
#'    \item Michael R. Lindeburg, PE, \emph{Civil Engineering Reference Manual for the PE Exam}, Twelfth Edition, Belmont, California: Professional Publications, Inc., 2011, pages 17-5 - 17-7.
#'    \item Michael R. Lindeburg, PE, \emph{Practice Problems for the Civil Engineering PE Exam: A Companion to the "Civil Engineering Reference Manual"}, Twelfth Edition, Belmont, California: Professional Publications, Inc., 2011, pages 17-1 and 17-8 - 17-9.
#'    \item Pavel Praks and Dejan Brkić, "Advanced Iterative Procedures for Solving the Implicit Colebrook Equation for Fluid Flow Friction", \emph{Advances in Civil Engineering}, Volume 2018, Article ID 5451034, 18 pages, \url{https://www.hindawi.com/journals/ace/2018/5451034/}
#'    \item Lotfi Zeghadnia, Jean Loup Robert, and Bachir Achour, "Explicit solutions for turbulent flow friction factor: A review, assessment and approaches classification", \emph{Ain Shams Engineering Journal}, March 2019, Volume 10, Issue 1, pages 243-252, \url{https://www.sciencedirect.com/science/article/pii/S2090447919300176}
#' }
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
#' @seealso \code{\link{Re1}}, \code{\link{Re2}}, \code{\link{Re3}}, and \code{\link{Re4}} for the Reynolds number and \code{\link{colebrook}}
#' for an accurate representation of the Colebrook-White equation
#'          
#'
#'
#'
#'
#' @examples
#'
#' library(iemisc)
#' 
#' # Examples
#' 
#' f1(200)
#' 
#' f1(1999)
#' 
#'
#'
#'


#' @title Laminar Flow Equation
#' @name f1
#'
#'
#' @param Re numeric vector that contains the Reynolds number (dimensionless)
#'
#' @return the dimensionless Darcy friction factor (f) as a numeric \code{\link[base]{vector}}
#'
#'
#' @seealso \code{\link{f2}}, \code{\link{f3}}, \code{\link{f4}}, \code{\link{f5}}, \code{\link{f6}}, \code{\link{f7}}, \code{\link{f8}}
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
f1 <- function(Re) {

# Check for Re
assert_that(!any(qtest(Re, "N+(0, 2100)") == FALSE), msg = "The Reynolds (Re) number is 0, NA, NaN, Inf, -Inf, empty, or a string. Or, the Reynolds (Re) number is not less than 2100 thus laminar flow is not represented. Please try again.")
# only process with finite values and provide an error message if the check fails

# Lindeburg reference
# Zeghadnia reference

f1 <- 64 / Re

f1
}





#' @title Moody Equation
#' @name f2
#'
#' @param Re numeric vector that contains the Reynolds number (dimensionless)
#' @param eps numeric vector that contains the specific roughness for the pipe
#'     material (m or ft)
#' @param D numeric vector that contains the inner diameters of the pipe (m or
#'     ft)
#'
#' @return the dimensionless Darcy friction factor (f) as a numeric \code{\link[base]{vector}}
#'
#'
#' @note
#' Note: Please refer to iemisc: Calculating the Friction Loss Examples vignette
#' for the examples
#' 
#' @seealso \code{\link{f1}} for the additional seealso, description, details, and references sections, \code{\link{f3}}, \code{\link{f4}}, \code{\link{f5}}, \code{\link{f6}}, \code{\link{f7}}, \code{\link{f8}}
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
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
f2 <- function(eps, Re, D = NULL) {

if (missing(D)) {

checks <- c(eps, Re)

# Check for eps and Re
assert_that(!any(qtest(checks, "N==2") == FALSE), msg = "There are more than or less than 2 known variables (eps and Re). Please try again.")
# only process with enough known variables and provide an error message if the check fails

assert_that(!any(qtest(Re, "N==1(4e03, 5e08)") == FALSE), msg = "The Reynolds (Re) number is NA, NaN, Inf, -Inf, empty, or a string. Or, the Reynolds (Re) number is not between 4000 and 5e08. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(!any(qtest(eps, "N==1(0, 1e-02)") == FALSE), msg = "The pipe specific roughness is 0, NA, NaN, Inf, -Inf, empty, or a string. Or, the specific roughness (epsilon) is not between 0 and 0.01. Please try again.")
# only process with finite values and provide an error message if the check fails


# Genić reference

f2 <- 0.0055 * (1 + (20000 * eps + 1e06 / Re) ^ (1 / 3))

return(f2)


} else {


check1 <- eps / D

checks <- c(eps, D, Re)


# Check for eps, D, and Re
assert_that(!any(qtest(checks, "N==3") == FALSE), msg = "There are more than or less than 3 known variables (eps, D, and Re). Please try again.")
# only process with enough known variables and provide an error message if the check fails

assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either eps, D, or Re is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(!any(qtest(Re, "N==1(4e03, 1e08)") == FALSE), msg = "The Reynolds (Re) number is NA, NaN, Inf, -Inf, empty, or a string. Or, the Reynolds (Re) number is not between 4000 and 1e08. Please try again.")
# only process with a value for Re between 4000 and 1e08 and provide a stop warning if false

assert_that(!any(qtest(check1, "N==1(0, 1e-02)") == FALSE), msg = "The ratio epsilon / D (specific roughness / pipe diameter)  is 0, NA, NaN, Inf, -Inf, empty, or a string. The ratio epsilon / D (specific roughness / pipe diameter) is not between 0 and 0.01. Please try again.")
# only process with a value for the ratio epsilon / D (specific roughness / pipe diameters) between 0 and 1e-02 and provide a stop warning if false


# Zeghadnia reference

f2 <- 0.0055 * (1 + (2e04 * (eps / D ) + (1e06 / Re)) ^ (1 / 3))

return(f2)

}
}





#' @title Romeo, et. al. Equation
#' @name f3
#'
#'
#' @param Re numeric vector that contains the Reynolds number (dimensionless)
#' @param eps numeric vector that contains the specific roughness for the pipe
#'     material (m or ft)
#' @param D numeric vector that contains the inner diameters of the pipe (m or
#'     ft)
#' @param x0 numeric vector that contains the starting value for pracma's
#'     newtonRaphson function
#'
#' @return the dimensionless Darcy friction factor (f) as a numeric \code{\link[base]{vector}}
#'
#'
#' @note
#' Note: Please refer to iemisc: Calculating the Friction Loss Examples vignette
#' for the examples
#' 
#' @seealso \code{\link{f1}} for the additional seealso, description, details, and references sections, \code{\link{f2}}, \code{\link{f4}}, \code{\link{f5}}, \code{\link{f6}}, \code{\link{f7}}, \code{\link{f8}} and \code{\link[pracma]{newtonRaphson}} for \code{x0}
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
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom pracma newtonRaphson
#'
#'
#' @export
f3 <- function(eps, Re, D = NULL, x0 = NULL) {

if (missing(D)) {

checks <- c(eps, Re)

# Check for eps and Re
assert_that(!any(qtest(checks, "N==2") == FALSE), msg = "There are more than or less than 2 known variables (eps and Re). Please try again.")
# only process with enough known variables and provide an error message if the check fails

assert_that(!any(qtest(Re, "N==1(3e03, 1.5e08)") == FALSE), msg = "The Reynolds (Re) number is NA, NaN, Inf, -Inf, empty, or a string. Or, the Reynolds (Re) number is not between 3000 and 1.5e08. Please try again.")
# only process with a value for Re between 3000 and 1.5e08 and provide a stop warning if false

assert_that(!any(qtest(eps, "N==1(0, 5e-02)") == FALSE), msg = "The pipe specific roughness is 0, NA, NaN, Inf, -Inf, empty, or a string. The specific roughness (epsilon) is not between 0 and 0.05. Please try again.")
# only process with a value for eps between 0 and 5e-02 and provide a stop warning if false



# Genić reference

f3 <- (-2 * log10(eps / 3.7065 - 5.0272 / Re * log10(eps / 3.827 - 4.567 / Re * log10((eps / 7.79) ^ 0.9924 + (5.3326 / (208.82 + Re)) ^ 0.9345)))) ^ -2

return(f3)


} else {


check1 <- eps / D

checks <- c(eps, D, Re)


# Check for eps, D, and Re
assert_that(!any(qtest(checks, "N==3") == FALSE), msg = "There are more than or less than 3 known variables (eps, D, and Re). Please try again.")
# only process with enough known variables and provide an error message if the check fails

assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either eps, D, or Re is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(!any(qtest(Re, "N==1(3e03, 1.5e08)") == FALSE), msg = "The Reynolds (Re) number is NA, NaN, Inf, -Inf, empty, or a string. Or, the Reynolds (Re) number is not between 3000 and 1.5e08. Please try again.")
# only process with a value for Re between 3000 and 1.5e08 and provide a stop warning if false

assert_that(!any(qtest(check1, "N==1(0, 5e-02)") == FALSE), msg = "The ratio epsilon / D (specific roughness / pipe diameter)  is 0, NA, NaN, Inf, -Inf, empty, or a string. The ratio epsilon / D (specific roughness / pipe diameter) is not between 0 and 0.05. Please try again.")
# only process with a value for the ratio epsilon / D (specific roughness / pipe diameters) between 0 and 5e-02 and provide a stop warning if false


# Zeghadnia reference

ifelse(missing(x0), x0 <- 0.001, x0 <- x0)

f3fun <- function (f3) { 1 / sqrt(f3) + 2 * log10(eps / 3.7065 * D - (5.0272 / Re) * log10(eps / 3.827 * D - (4.567 / Re) * log10((eps / 7.7918 * D) ^ 0.9924 + (5.3326 / (208.815 + Re)) ^ 0.9345))) }

f3 <- newtonRaphson(f3fun, x0 = x0)$root

return(f3)

}
}





#' @title Žarko Ćojbašića and Dejan Brkić Equation
#' @name f4
#'
#' @param Re numeric vector that contains the Reynolds number (dimensionless)
#' @param eps numeric vector that contains the specific roughness for the pipe
#'     material (m or ft)
#' @param D numeric vector that contains the inner diameters of the pipe (m or
#'     ft)
#' @param x0 numeric vector that contains the starting value for pracma's
#'     newtonRaphson function
#'
#' @return the dimensionless Darcy friction factor (f) as a numeric \code{\link[base]{vector}}
#'
#'
#' @note
#' Note: Please refer to iemisc: Calculating the Friction Loss Examples vignette
#' for the examples
#' 
#' @seealso \code{\link{f1}} for the additional seealso, description, details, and references sections, \code{\link{f2}}, \code{\link{f3}}, \code{\link{f5}}, \code{\link{f6}}, \code{\link{f7}}, \code{\link{f8}} and \code{\link[pracma]{newtonRaphson}} for \code{x0}
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
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom pracma newtonRaphson
#'
#'
#' @export
f4 <- function(eps, Re, D, x0 = NULL) {

check1 <- eps / D

checks <- c(eps, D, Re)


# Check for eps, D, and Re
assert_that(!any(qtest(checks, "N==3") == FALSE), msg = "There are more than or less than 3 known variables (eps, D, and Re). Please try again.")
# only process with enough known variables and provide an error message if the check fails

assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either eps, D, or Re is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(!any(qtest(Re, "N==1(2.3e03, 1e08)") == FALSE), msg = "The Reynolds (Re) number is NA, NaN, Inf, -Inf, empty, or a string. Or, the Reynolds (Re) number is not between 2300 and 1e08. Please try again.")
# only process with a value for Re between 2300 and 1e08 and provide a stop warning if false

assert_that(!any(qtest(check1, "N==1(0, 5e-02)") == FALSE), msg = "The ratio epsilon / D (specific roughness / pipe diameter)  is 0, NA, NaN, Inf, -Inf, empty, or a string. The ratio epsilon / D (specific roughness / pipe diameter) is not between 0 and 0.05. Please try again.")
# only process with a value for the ratio epsilon / D (specific roughness / pipe diameters) between 0 and 5e-02 and provide a stop warning if false



# Zeghadnia reference

ifelse(missing(x0), x0 <- 0.001, x0 <- x0)

f4fun <- function (f4) { 1 / sqrt(f4) + 2 * log10(( eps / 3.7106 * D) - 5 / Re * log10((eps / 3.8597 * D) - 4.795 / Re * log10((eps / 7.646 * D) ^ 0.9685 + (4.9755 / (206.2795 + Re)) ^ 0.8795))) }

f4 <- newtonRaphson(f4fun, x0 = x0)$root

return(f4)

}






#' @title Colebrook-White Equation
#' @name f5
#'
#'
#' @param Re numeric vector that contains the Reynolds number (dimensionless)
#' @param eps numeric vector that contains the specific roughness for the pipe
#'     material (m or ft)
#' @param D numeric vector that contains the inner diameters of the pipe (m or
#'     ft)
#' @param x0 numeric vector that contains the starting value for pracma's
#'     newtonRaphson function
#'
#' @return the dimensionless Darcy friction factor (f) as a numeric \code{\link[base]{vector}}
#'
#'
#' @note
#' Note: Please refer to iemisc: Calculating the Friction Loss Examples vignette
#' for the examples
#' 
#' @seealso \code{\link{f1}} for the additional seealso, description, details, and references sections, \code{\link{f2}}, \code{\link{f3}}, \code{\link{f4}}, \code{\link{f6}}, \code{\link{f7}}, \code{\link{f8}} and \code{\link[pracma]{newtonRaphson}} for \code{x0}
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
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom pracma newtonRaphson
#'
#'
#' @export
f5 <- function(eps, D = NULL, Re, x0 = NULL) {

ifelse(missing(x0), x0 <- 0.001, x0 <- x0)


if (missing(D)) {

checks <- c(eps, Re)

# Check for eps and Re
assert_that(!any(qtest(checks, "N==2") == FALSE), msg = "There are more than or less than 2 known variables (eps and Re). Please try again.")
# only process with enough known variables and provide an error message if the check fails

assert_that(!any(qtest(Re, "N==1(4e03, 1e08)") == FALSE), msg = "The Reynolds (Re) number is NA, NaN, Inf, -Inf, empty, or a string. Or, the Reynolds (Re) number is not between 4000 and 1e08. Please try again.")
# only process with a value for Re between 4000 and 1e08 and provide a stop warning if false

assert_that(!any(qtest(eps, "N==1(0, 5e-02)") == FALSE), msg = "The pipe specific roughness is 0, NA, NaN, Inf, -Inf, empty, or a string. The specific roughness (epsilon) is not between 0 and 0.05. Please try again.")
# only process with a value for eps between 0 and 5e-02 and provide a stop warning if false


# Genić reference

f5fun <- function (f5) { 1 / sqrt(f5) + 2 * log10(2.51 / Re * sqrt(f5) + eps / 3.7) }

f5 <- newtonRaphson(f5fun, x0 = x0)$root

return(f5)


} else {


check1 <- eps / D

checks <- c(eps, D, Re)


# Check for eps, D, and Re
assert_that(!any(qtest(checks, "N==3") == FALSE), msg = "There are more than or less than 3 known variables (eps, D, and Re). Please try again.")
# only process with enough known variables and provide an error message if the check fails

assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either eps, D, or Re is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(!any(qtest(Re, "N==1(4e03, 1e08)") == FALSE), msg = "The Reynolds (Re) number is NA, NaN, Inf, -Inf, empty, or a string. Or, the Reynolds (Re) number is not between 4000 and 1e08. Please try again.")
# only process with a value for Re between 4000 and 1e08 and provide a stop warning if false

assert_that(!any(qtest(check1, "N==1(0, 5e-02)") == FALSE), msg = "The ratio epsilon / D (specific roughness / pipe diameter)  is 0, NA, NaN, Inf, -Inf, empty, or a string. The ratio epsilon / D (specific roughness / pipe diameter) is not between 0 and 0.05. Please try again.")
# only process with a value for the ratio epsilon / D (specific roughness / pipe diameters) between 0 and 5e-02 and provide a stop warning if false


# Praks reference

f5fun <- function (f5) { 1 / sqrt(f5) + 2 * log10(2.51 / (Re * sqrt(f5)) + eps / (3.7 * D)) }

f5 <- newtonRaphson(f5fun, x0 = x0)$root

return(f5)

}
}





#' @title Swamee-Jaine Equation
#' @name f6
#'
#' @param Re numeric vector that contains the Reynolds number (dimensionless)
#' @param eps numeric vector that contains the specific roughness for the pipe
#'     material (m or ft)
#' @param D numeric vector that contains the inner diameters of the pipe (m or
#'     ft)
#' @param x0 numeric vector that contains the starting value for pracma's
#'     newtonRaphson function
#'
#' @return the dimensionless Darcy friction factor (f) as a numeric \code{\link[base]{vector}}
#'
#'
#' @note
#' Note: Please refer to iemisc: Calculating the Friction Loss Examples vignette
#' for the examples
#' 
#' @seealso \code{\link{f1}} for the additional seealso, description, details, and references sections, \code{\link{f2}}, \code{\link{f3}}, \code{\link{f4}}, \code{\link{f5}}, \code{\link{f7}}, \code{\link{f8}}
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
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#'
#' @export
f6 <- function(eps, D = NULL, Re, x0 = NULL) {

ifelse(missing(x0), x0 <- 0.001, x0 <- x0)


if (missing(D)) {

checks <- c(eps, Re)

# Check for eps and Re
assert_that(!any(qtest(checks, "N==2") == FALSE), msg = "There are more than or less than 2 known variables (eps and Re). Please try again.")
# only process with enough known variables and provide an error message if the check fails

assert_that(!any(qtest(Re, "N==1(5e03, 1e08)") == FALSE), msg = "The Reynolds (Re) number is NA, NaN, Inf, -Inf, empty, or a string. Or, the Reynolds (Re) number is not between 5000 and 1e08. Please try again.")
# only process with a value for Re between 5000 and 1e08 and provide a stop warning if false

assert_that(!any(qtest(eps, "N==1(1e-06, 0.05)") == FALSE), msg = "The pipe specific roughness is NA, NaN, Inf, -Inf, empty, or a string. The specific roughness (epsilon) is not between 1e-06 and 0.05. Please try again.")
# only process with a value for eps between 1e-06 and 5e-02 and provide a stop warning if false


# Genić reference

f6 <- (-2 * log10(eps / 3.7 + 5.74 / Re ^ 0.9)) ^ -2

return(f6)


} else {


check1 <- eps / D

checks <- c(eps, D, Re)


# Check for eps, D, and Re
assert_that(!any(qtest(checks, "N==3") == FALSE), msg = "There are more than or less than 3 known variables (eps, D, and Re). Please try again.")
# only process with enough known variables and provide an error message if the check fails

assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either eps, D, or Re is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(!any(qtest(Re, "N==1(5e03, 1e08)") == FALSE), msg = "The Reynolds (Re) number is NA, NaN, Inf, -Inf, empty, or a string. Or, the Reynolds (Re) number is not between 5000 and 1e08. Please try again.")
# only process with a value for Re between 5000 and 1e08 and provide a stop warning if false

assert_that(!any(qtest(check1, "N==1(1e-06, 1e-02)") == FALSE), msg = "The ratio epsilon / D (specific roughness / pipe diameter) is NA, NaN, Inf, -Inf, empty, or a string. The ratio epsilon / D (specific roughness / pipe diameter) is not between 1e-06 and 0.01. Please try again.")
# only process with a value for the ratio epsilon / D (specific roughness / pipe diameters) between 1e-06 and 1e-02 and provide a stop warning if false


# Zeghadnia reference

f6 <- (-2 * log10(eps / D / 3.7 + 5.74 / Re ^ 0.9)) ^ -2

f6

}
}





#' @title Zigrang-Sylvester Equation
#' @name f7
#'
#' @param Re numeric vector that contains the Reynolds number (dimensionless)
#' @param eps numeric vector that contains the specific roughness for the pipe
#'     material (m or ft)
#' @param D numeric vector that contains the inner diameters of the pipe (m or
#'     ft)
#' @param x0 numeric vector that contains the starting value for pracma's
#'     newtonRaphson function
#'
#' @return the dimensionless Darcy friction factor (f) as a numeric \code{\link[base]{vector}}
#'
#'
#' @note
#' Note: Please refer to iemisc: Calculating the Friction Loss Examples vignette
#' for the examples
#' 
#' @seealso \code{\link{f1}} for the additional seealso, description, details, and references sections, \code{\link{f2}}, \code{\link{f3}}, \code{\link{f4}}, \code{\link{f5}}, \code{\link{f6}}, \code{\link{f8}} and \code{\link[pracma]{newtonRaphson}} for \code{x0}
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
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom pracma newtonRaphson
#'
#'
#' @export
f7 <- function(eps, D = NULL, Re, x0 = NULL) {

ifelse(missing(x0), x0 <- 0.001, x0 <- x0)


if (missing(D)) {

checks <- c(eps, Re)

# Check for eps and Re
assert_that(length(checks) == 2, msg = "There are not 2 known variables. Please try again with 2 known variables (eps and Re).")
# only process with enough known variables and provide an error message if the check fails

assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either eps or Re is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(!any(qtest(Re, "N==1(4e03, 1e08)") == FALSE), msg = "The Reynolds (Re) number is NA, NaN, Inf, -Inf, empty, or a string. Or, the Reynolds (Re) number is not between 4000 and 1e08. Please try again.")
# only process with a value for Re between 4000 and 1e08 and provide a stop warning if false

assert_that(!any(qtest(eps, "N==1(4e-05, 5e-02)") == FALSE), msg = "The pipe specific roughness is NA, NaN, Inf, -Inf, empty, or a string. The specific roughness (epsilon) is not between 4e-05 and 0.05. Please try again.")
# only process with a value for eps between 4e-05 and 5e-02 and provide a stop warning if false



# Genić reference

f7 <- (-2 * (log10(eps / 3.7 - 5.02 / Re * log10(eps - 5.02 / Re * log10 (eps / 3.7 + 13 / Re ))))) ^ -2

f7


} else {


check1 <- eps / D

checks <- c(eps, D, Re)


# Check for eps, D, and Re
assert_that(!any(qtest(checks, "N==3") == FALSE), msg = "There are more than or less than 3 known variables (eps, D, and Re). Please try again.")
# only process with enough known variables and provide an error message if the check fails

assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either eps, D, or Re is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(!any(qtest(Re, "N==1(4e03, 1e08)") == FALSE), msg = "The Reynolds (Re) number is NA, NaN, Inf, -Inf, empty, or a string. Or, the Reynolds (Re) number is not between 4000 and 1e08. Please try again.")
# only process with a value for Re between 4000 and 1e08 and provide a stop warning if false

assert_that(!any(qtest(check1, "N==1(1e-05, 5e-02)") == FALSE), msg = "The ratio epsilon / D (specific roughness / pipe diameter) is NA, NaN, Inf, -Inf, empty, or a string. The ratio epsilon / D (specific roughness / pipe diameter) is not between 1e-05 and 0.05. Please try again.")
# only process with a value for the ratio epsilon / D (specific roughness / pipe diameters) between 1e-05 and 5e-02 and provide a stop warning if false


# Zeghadnia reference

f7fun <- function (f7) { 1 / sqrt(f7) + 2 * log10(eps / 3.7 * D - 5.02 / Re * log10(eps / 3.7 * D - 5.02 / Re * log10(eps / 3.7 * D + 13 / Re))) }

f7 <- newtonRaphson(f7fun, x0 = x0)$root

return(f7)

}
}




#' @title Vatankhah Equation
#' @name f8
#'
#' @param Re numeric vector that contains the Reynolds number (dimensionless)
#' @param eps numeric vector that contains the specific roughness for the pipe
#'     material (m or ft)
#' @param D numeric vector that contains the inner diameters of the pipe (m or
#'     ft)
#'
#' @return the dimensionless Darcy friction factor (f) as a numeric \code{\link[base]{vector}}
#'
#'
#' @note
#' Note: Please refer to iemisc: Calculating the Friction Loss Examples vignette
#' for the examples
#' 
#' @seealso \code{\link{f1}} for the additional seealso, description, details, and references sections, \code{\link{f2}}, \code{\link{f3}}, \code{\link{f4}}, \code{\link{f5}}, \code{\link{f6}}, \code{\link{f7}}
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
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#'
#' @export
f8 <- function(eps, D, Re) {

check1 <- eps / D

checks <- c(eps, D, Re)


# Check for eps, D, and Re
assert_that(!any(qtest(checks, "N==3") == FALSE), msg = "There are more than or less than 3 known variables (eps, D, and Re). Please try again.")
# only process with enough known variables and provide an error message if the check fails

assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either eps, D, or Re is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(!any(qtest(Re, "N==1(4e03, 1e08)") == FALSE), msg = "The Reynolds (Re) number is NA, NaN, Inf, -Inf, empty, or a string. Or, the Reynolds (Re) number is not between 4000 and 1e08. Please try again.")
# only process with a value for Re between 4000 and 1e08 and provide a stop warning if false

assert_that(!any(qtest(check1, "N==1(1e-06, 5e-02)") == FALSE), msg = "The ratio epsilon / D (specific roughness / pipe diameter) is NA, NaN, Inf, -Inf, empty, or a string. The ratio epsilon / D (specific roughness / pipe diameter) is not between 1e-06 and 0.05. Please try again.")
# only process with a value for the ratio epsilon / D (specific roughness / pipe diameters) between 1e-06 and 5e-02 and provide a stop warning if false


# Zeghadnia reference

delta <- 6.0173 / (Re * (0.07 * (eps / D) + Re ^ -0.885) ^ 0.109) + eps / D / 3.71

f8 <- ((2.51 / Re + 1.1513 * delta) / (delta - (eps / D) / 3.71 - 2.3026 * delta * log10(delta))) ^ 2

return(f8)

}
