#' Accurately calculate the Colebrook-White equation to obtain the Darcy-Weisbach friction factor
#'
#' This function "provides the fast, accurate, and robust computation of the
#' Colebrook-White equation" to determine the "Darcy-Weisbach friction factor
#' F". This method is "more efficient than the solution of the Colebrook
#' equation via the Lambert W-function, or the simple approximations." The
#' solution is accurate to "around machine precision for all R > 3 and for all
#' 0 <= K, i.e. in an interval exceeding all values of physical interest."
#' Reference: Clamond
#'
#'
#'
#' Colebrook-White equation is expressed as
#'
#' \deqn{\frac{1}{\sqrt{F}} = -2 * log10{\frac{K}{3.7} + \frac{2.51}{R * \sqrt{F}}}}
#'
#' \describe{
#'	\item{\emph{F}}{Darcy-Weisbach friction factor}
#'	\item{\emph{K}}{Equivalent sand roughness height (material specific
#'         roughness) divided by the hydraulic diameters}
#'	\item{\emph{R}}{the Reynolds' number (dimensionless)}
#' }
#'
#'
#' @param Re numeric vector that contains the Reynolds number [dimensionless],
#'   which should be >= 2300. Reference: Clamond
#' @param K numeric vector that contains the "equivalent sand roughness height
#'   sand roughness height (material specific roughness) divided by the
#'   hydraulic diameters", if known. If not known, the default value is 0.
#'   Reference: Clamond
#'
#'
#' @return F Return a numeric vector containing the Darcy-Weisbach friction
#'   factor. Reference: Clamond
#'
#'
#'
#' @references
#' \enumerate{
#'    \item Steven C. Chapra, \emph{Applied Numerical Methods with MATLAB for Engineers and Scientists}, Second Edition, Boston, Massachusetts: McGraw-Hill, 2008, pages 157-161.
#'    \item Didier Clamond, "Efficient resolution of the Colebrook equation", \emph{Ind. Eng. Chem. Res.}, 2009, 48 (7), pages 3665-3671 \url{https://arxiv.org/abs/0810.5564} and \url{https://math.univ-cotedazur.fr/~didierc/DidPublis/ICR_2009.pdf}
#' }
#'
#'
#'
#'
#' @author Didier Clamond (colebrook MATLAB function), Irucka Embry (colebrook R function)
#'
#'
#'
#' @encoding UTF-8
#'
#'
#' @seealso \code{\link{Re1}}, \code{\link{Re2}}, \code{\link{Re3}}, \code{\link{Re4}} for the Reynolds number and
#' \code{\link{f1}}, \code{\link{f2}}, \code{\link{f3}}, \code{\link{f4}}, \code{\link{f5}}, \code{\link{f6}}, \code{\link{f7}}, 
#' and \code{\link{f8}} for the Darcy friction factor
#'
#'
#'
#' @examples
#'
#' install.load::load_package("iemisc", "units")
#'
#' # Example 1 (Reference: Clamond)
#' F <- colebrook(c(3e3, 7e5, 1e100), 0.01)
#'
#' F
#'
#'
#' # Example 2
#' # 'Determine f for air flow through a smooth, thin tube. The parameters are
#' # rho = 1.23 kg/m^3, mu = 1.79 x 10^-5 N * s/m^2, D = 0.005 m, V = 40 m/s
#' # and epsilon = 0.0015 mm.' Reference: Chapra 158
#'
#' # Determine R (the Reynolds number) first using the following parameters:
#'
#' rho <- 1.23 # kg/m^3
#' V <- 40 # m/s
#' D <- 0.005 # m
#' mu <- 1.79 * 10^-5 # N * s/m^2
#'
#' eps <- 0.0015 # mm
#' eps <- set_units(eps, "mm")
#' units(eps) <- make_units(m)
#'
#' Re <- rho * V * D / mu
#'
#'
#' K <- drop_units(eps) / D
#'
#'
#' # with K
#' fr1 <- colebrook(Re, K); fr1
#'
#' # without K
#' fr2 <- colebrook(Re); fr2
#'
#'
#' # The solution on Chapra 159 and 160 is 'f = 0.02896781017144' which was
#' # computed using the Newton-Raphson method, Swamee-Jain approximation
#' # equation, and MATLAB's fzero function.
#'
#' # Thus,
#' fm <- 0.02896781017144
#'
#' # Compute the relative error between fr[1 and 2] (this function) and fm (Chapra).
#'
#' relerror(fr1, fm)
#'
#'
#' relerror(fr2, fm)
#'
#' # compare the relative error with and without K
#'
#'
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom sjmisc is_empty
#'
#'
#' @export
colebrook <- function (Re, K = NULL) {

# Check for Re
assert_that(all(!(qtest(Re, "N+(2300,)") == FALSE)), msg = "Re < 2300, NA, NaN, Inf, -Inf, empty, or a string. The Colebrook equation is only valid for Reynolds' numbers > 2300. Please try again.")
# only process with finite values and provide an error message if the check fails


# Check for K
if(is.null(K) == FALSE) {

assert_that(any(!(qtest(K, "N+[0,)") == FALSE)), msg = "K < 0, NA, NaN, Inf, -Inf, empty, or a string. The relative sand roughness can not be negative. Please try again.")
# only process with finite values and provide an error message if the check fails


} else {

K <- K

}


# Check if K is NULL or empty
if(is.null(K) | (is_empty(K) == FALSE)) {

K <- 0

}



# Initialization
X1 <- K * Re * log(10) / 18.574             # X1 <- K * Re * log(10) / 18.574
X2 <- log(Re * log(10) / 5.02)           # X2 <- log( Re * log(10) / 5.02 )

# Initial guess
F <- X2 - 0.2

# First iteration
E <- (log(X1 + F) - 0.2) / (1 + X1 + F)
F <- F - (1 + X1 + F + 0.5 * E) * E * (X1 + F) / (1 + X1 + F + E * (1 + E / 3))


# Second iteration (remove the next two lines for moderate accuracy)
E <- (log(X1 + F) + F - X2) / (1 + X1 + F)
F <- F - (1 + X1 + F + 0.5 * E) * E * (X1 + F) / (1 + X1 + F + E * (1 + E / 3))


# Finalized solution
F <- 0.5 * log(10) / F                   # F <- 0.5 * log(10) / F
friction_factor <- F * F                 # F <- Friction factor


return(friction_factor)
}
