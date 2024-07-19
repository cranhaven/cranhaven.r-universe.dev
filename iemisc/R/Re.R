#' Various Methods of Calculating the Reynolds number
#'
#' The Reynolds number, named after Osborne Reynolds, is a dimensionless number
#' that is used to determine the type of fluid flow (laminar, transition, or
#' turbulent). [References: Lindeburg Manual and Subramanian]
#'
#'
#'
#'
#' @param D numeric vector that contains the hydraulic diameters '(four times
#'   the area in flow divided by the wetted surface) is a characteristic
#'   length' (m or ft) [Reference: Lindeburg Manual]
#' @param V numeric vector that contains the average fluid velocity (m/s or
#'   ft/s) [Reference: Lindeburg Manual]
#' @param mu numeric vector that contains the absolute or dynamic viscosity of
#'   the fluid (Pa-s or lbf-sec/ft^2) [Reference: Lindeburg Manual]
#' @param gc numeric vector that contains the gravitational constant (32.2
#'   lbm-ft/lbf-sec^2) [Reference: Lindeburg Manual]
#' @param rho numeric vector that contains the fluid density (kg/m^3 or
#'   lbm/ft^3) [Reference: Lindeburg Manual]
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units, \code{Eng} for English units
#'   (United States Customary System in the United States and Imperial Units in
#'   the United Kingdom), and \code{slug} for the English unit slug which is a
#'   consistent unit of mass]
#'
#' @return the Reynolds number as a \code{\link{list}} for Re1
#'
#'
#' @details
#' Re1 - uses the absolute or dynamic viscosity (\\mu)
#' Re2 and Re4 - use kinematic viscosity (\\nu)
#' Re3 - uses the 'mass flow rate per unit area' (G) [Reference: Lindeburg Manual]
#'
#'
#'
#' The Reynolds number equation can be expressed in the following ways
#' [Reference: Lindeburg Manual]:
#'
#' \deqn{Re = \frac{inertial_forces}{viscous_forces}}
#'
#'
#'
#' \deqn{Re = \frac{DV\\rho}{\\mu}}
#'
#' \describe{
#'   \item{\emph{Re}}{the Reynolds number (dimensionless)}
#'   \item{\emph{D}}{the hydraulic diameters (m)}
#'   \item{\emph{V}}{average velocity of the fluid (m/s)}
#'   \item{\emph{\\rho}}{density of the fluid at a certain temperature (kg/m^3)}
#'   \item{\emph{\\mu}}{absolute or dynamic viscosity of the fluid at a certain temperature (Pa-s)}
#' }
#'
#'
#'
#' \deqn{Re = \frac{DV\\rho}{\\mug_c}}
#'
#' \describe{
#'   \item{\emph{Re}}{the Reynolds number (dimensionless)}
#'   \item{\emph{D}}{the hydraulic diameters (ft)}
#'   \item{\emph{V}}{average velocity of the fluid (ft/sec)}
#'   \item{\emph{\\rho}}{density of the fluid at a certain temperature (lbm/ft^3)}
#'   \item{\emph{\\mu}}{absolute or dynamic viscosity of the fluid at a certain temperature (lbf-sec/ft^2)}
#'   \item{\emph{g_c}}{gravitational constant (32.2 lbm-ft/lbf-sec^2) used for dimensional analysis so that the Reynolds number will be dimensionless with US Customary units}
#' }
#'
#'
#'
#' \deqn{Re = \frac{DG}{\\mu}}
#'
#' \describe{
#'   \item{\emph{Re}}{the Reynolds number (dimensionless)}
#'   \item{\emph{D}}{the hydraulic diameters (m)}
#'   \item{\emph{G}}{'mass flow rate per unit area' (kg/m^2-s)}
#'   \item{\emph{\\mu}}{absolute or dynamic viscosity of the fluid at a certain temperature (Pa-s)}
#' }
#'
#'
#'
#' \deqn{Re = \frac{DG}{g_c\\mu}}
#'
#' \describe{
#'   \item{\emph{Re}}{the Reynolds number (dimensionless)}
#'   \item{\emph{D}}{the hydraulic diameters (ft)}
#'   \item{\emph{G}}{'mass flow rate per unit area' (lbm/ft^2-sec)}
#'   \item{\emph{\\mu}}{absolute or dynamic viscosity of the fluid at a certain temperature (lbf-sec/ft^2)}
#'   \item{\emph{g_c}}{gravitational constant (32.2 lbm-ft/lbf-sec^2) used for dimensional analysis so that the Reynolds number will be dimensionless with US Customary units}
#' }
#'
#'
#' where
#' \deqn{G = {\\rhoV}}
#'
#' \describe{
#'   \item{\emph{G}}{'mass flow rate per unit area' (kg/m^2-s)}
#'   \item{\emph{\\rho}}{density of the fluid at a certain temperature (kg/m^3)}
#'   \item{\emph{V}}{average velocity of the fluid (m/s)}
#' }
#'
#'
#'
#' \deqn{Re = \frac{DV}{\\nu}}
#'
#' \describe{
#'   \item{\emph{Re}}{the Reynolds number (dimensionless)}
#'   \item{\emph{D}}{the hydraulic diameters (m)}
#'   \item{\emph{V}}{average velocity of the fluid (m/s)}
#'   \item{\emph{\\nu}}{kinematic viscosity of the fluid at a certain temperature (m^2/s)}
#' }
#'
#'
#'
#' \deqn{Re = \frac{DV}{\\nug_c}}
#'
#' \describe{
#'   \item{\emph{Re}}{the Reynolds number (dimensionless)}
#'   \item{\emph{D}}{the hydraulic diameters (ft)}
#'   \item{\emph{V}}{average velocity of the fluid (ft/sec)}
#'   \item{\emph{\\nu}}{absolute or dynamic viscosity of the fluid at a certain temperature (lbf-sec/ft^2)}
#'   \item{\emph{g_c}}{gravitational constant (32.2 lbm-ft/lbf-sec^2) used for dimensional analysis so that the Reynolds number will be dimensionless with US Customary units}
#' }
#'
#'
#' where
#' \deqn{\\nu = \frac{\\mu}{\\rho}}
#'
#' \describe{
#'   \item{\emph{\\nu}}{kinematic viscosity of the fluid at a certain temperature (m^2/s)}
#'   \item{\emph{\\mu}}{absolute or dynamic viscosity of the fluid at a certain temperature (Pa-s)}
#'   \item{\emph{\\rho}}{density of the fluid at a certain temperature (kg/m^3)}
#' }
#'
#'
#' where
#' \deqn{\\nu = \frac{\\mug_c}{\\rho}}
#'
#' \describe{
#'   \item{\emph{\\nu}}{kinematic viscosity of the fluid at a certain temperature (ft^2/sec)}
#'   \item{\emph{\\mu}}{absolute or dynamic viscosity of the fluid at a certain temperature (lbf-sec/ft^2)}
#'   \item{\emph{g_c}}{gravitational constant (32.2 lbm-ft/lbf-sec^2) used for dimensional analysis so that the kinematic viscosity units will work in US Customary units}
#'   \item{\emph{\\rho}}{density of the fluid at a certain temperature (lbm/ft^3)}
#' }
#'
#'
#'
#'
#'
#'
#' @references
#' \enumerate{
#'    \item Ven Te Chow, Ph.D., \emph{Open-Channel Hydraulics}, McGraw-Hill Classic Textbook Reissue, New York City, New York: McGraw-Hill Book Company, 1988, pages 7-8.
#'    \item Michael R. Lindeburg, PE, \emph{Civil Engineering Reference Manual for the PE Exam}, Twelfth Edition, Belmont, California: Professional Publications, Inc., 2011, pages 17-1, 17-5, 17-8 - 17-9.
#'    \item The NIST Reference on Constants, Units, and Uncertainty, Fundamental Constants Data Center of the NIST Physical Measurement Laboratory, "standard acceleration of gravity g_n", \url{https://web.archive.org/web/20230427133623/https://physics.nist.gov/cgi-bin/cuu/Value?gn}. Used the Internet Archive: Wayback Machine archived version for acceptance into CRAN.
#'    \item R. Shankar Subramanian, "Pipe Flow Calculations", page 9, Clarkson University Department of Chemical and Biomolecular Engineering, \url{https://web2.clarkson.edu/projects/subramanian/ch330/notes/Pipe\%20Flow\%20Calculations.pdf}.
#'    \item R. Shankar Subramanian, "Reynolds Number", page 1, Clarkson University Department of Chemical and Biomolecular Engineering, \url{https://web2.clarkson.edu/projects/subramanian/ch330/notes/Reynolds\%20Number.pdf}.
#'    \item Khanh Tuoc Trinh, "On the Critical Reynolds Number for Transition From Laminar to Turbulent Flow", page 2, \url{https://arxiv.org/abs/1007.0810}.
#'    \item Wikimedia Foundation, Inc. Wikipedia, 15 May 2019, "Conversion of units", \url{https://en.wikipedia.org/wiki/Conversion_of_units}.
#' }
#'
#'
#' @note
#' Please Note: The conventional wisdom that a Reynolds number less than 2100
#' is laminar flow, between 2100 and 4000 is transitional or critical flow, and
#' greater than 4000 is turbulent flow is not accurate. 'Reynolds himself
#' observed that turbulence was triggered by inlet disturbances to the pipe and
#' the laminar state could be maintained to Re \\u2248 12,000 if he took great care
#' in minimizing external disturbances to the flow. By careful design of pipe
#' entrances Ekman (1910) has maintained laminar pipe flow up to a Reynolds
#' number of 40,000 and Pfenniger (1961) up to 100,000 by minimising ambient
#' disturbances.' [References: Lindeburg Manual and Trinh]
#'
#' 'Numerous experiments have shown that the flow in a pipe changes from
#' laminar to turbulent in the range of R between the critical value of 2,000
#' and a value that may be as high as 50,000.* In these experiments the
#' diameters of the pipe was taken as the characteristic length in defining the
#' Reynolds number. When the hydraulic radius is taken as the characteristic
#' length, the corresponding range is from 500 to 12,500,* since the diameters
#' of a pipe is four times its hydraulic radius. * = It should be noted that
#' there is actually no definite upper limit.' [Reference: Chow]
#'
#' 
#'
#'
#'
#'
#' @note
#' Note: Units must be consistent
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
#' @seealso \code{\link{f1}}, \code{\link{f2}}, \code{\link{f3}}, \code{\link{f4}}, \code{\link{f5}}, \code{\link{f6}}, \code{\link{f7}}, and \code{\link{f8}}
#' for the Darcy friction factor (f) for pipes
#'
#'
#'
#'
#' @examples
#'
#' # from Lindeburg Reference page 17-8
#' # D = 0.3355 ft
#' # V = 7.56 ft/sec
#' 
#' # from the Chow reference, water at 68 F (20 C) has the following properties
#' 
#' library(iemisc)
#'
#' # mu (dynamic viscosity) = 2.09 * 10 ^ -5 slug/ft-sec
#' # rho (density) = 1.937 slug/ft^3
#' # v (kinematic viscosity) = mu / rho = 1.08 * 10 ^ -5
#'
#' Re1(D = 0.3355, V = 7.56, rho = 1.937, mu = 2.09 * 10 ^ -5, units = "slug")
#'
#'
#'
#'



#' @title Calculating the Reynolds Number 1
#' @name Re1
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#'
#' @seealso \code{\link{Re2}}, \code{\link{Re3}}, \code{\link{Re4}}
#' @export
Re1 <- function (D, V, rho, mu, gc = NULL, units = c("SI", "Eng", "slug")) {

checks1 <- c(D, V, rho, mu)

checks2 <- c(rho, mu)

units <- units


# Check for D, V, rho, mu, and units
assert_that(qtest(units, "S==1"), msg = "There is not an unit type or more than 1 unit type. Please specify either 'SI', 'Eng', or 'slug'. Please try again.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("SI", "Eng", "slug") %in% units)), msg = "The unit system has not been identified correctly as 'SI', 'Eng', or 'slug'. Please try again.")
# only process with a specified unit and provide a stop warning if not

assert_that(!any(qtest(checks1, "N==4(,)") == FALSE), msg = "Either D, V, rho, or mu is NA, NaN, Inf, -Inf, empty, or a string. Or, there are more than or less than 4 numeric values. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(!any(qtest(checks2, "N==2(0,)") == FALSE), msg = "Either rho or mu is 0, NA, NaN, Inf, -Inf, empty, or a string. Or, there are more than or less than 2 numeric values.  Please try again.")
# only process with finite values and provide an error message if the check fails


# [Reference: Lindeburg Manual]
if (units == "SI" | units == "slug") {

nu <- mu / rho

Re1 <- (D * V) / nu

return(list(nu = nu, Re1 = Re1))


} else {

ifelse(missing(gc), gc <- 9.80665 * (3937 / 1200), gc <- gc) # lbm-ft/lbf-sec^2

nu <- (mu  * gc / rho)

Re1 <- (D * V) / nu

return(list(nu = nu, Re1 = Re1))

}
}



#' @title Calculating the Reynolds Number 2
#' @name Re2
#'
#' @param D numeric vector that contains the hydraulic diameters "(four times
#'   the area in flow divided by the wetted surface) is a characteristic
#'   length" (m or ft)  [Reference: Lindeburg Manual]
#' @param V numeric vector that contains the average fluid velocity (m/s or
#'   ft/s)  [Reference: Lindeburg Manual]
#' @param nu numeric vector that contains the kinematic viscosity of the fluid
#'   (m^2/s or lbf-sec/ft^2)  [Reference: Lindeburg Manual]
#'
#' @return the Reynolds number as a numeric \code{\link[base]{vector}} for Re2
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
#' @examples
#' 
#' # from Lindeburg Reference page 17-8
#' # where
#' # D = 0.3355 ft
#' # V = 7.56 ft/sec
#' # nu = 1.41 * 10 ^ -5 ft^2 / sec
#' # and
#' # Re = 1.8 * 10 ^ 5
#' 
#' library(iemisc)
#'
#' Re2(D = 0.3355, V = 7.56, nu = 1.41 * 10 ^ -5)
#'
#' # compare to Re1(D = 0.3355, V = 7.56, rho = 1.937, mu = 2.09 * 10 ^ -5, units = "slug")
#'
#' Re2(D = 0.3355, V = 7.56, nu = 1.08 * 10 ^ -5)
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#'
#' @seealso \code{\link{Re1}} for the additional seealso, description, details, note, and references sections, \code{\link{Re3}}, \code{\link{Re4}}
#' @export
Re2 <- function (D, V, nu) {


checks1 <- c(D, V, nu)

checks2 <- nu


# Check for D, V, and nu
assert_that(!any(qtest(checks1, "N==3(,)") == FALSE), msg = "Either D, V, or nu is NA, NaN, Inf, -Inf, empty, or a string. Or, there are more than or less than 3 numeric values. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(!any(qtest(checks2, "N==1(0,)") == FALSE), msg = "Either nu is 0, NA, NaN, Inf, -Inf, empty, or a string. Or, there is more than or less than 1 numeric value.  Please try again.")
# only process with finite values and provide an error message if the check fails


# [Reference: Lindeburg Manual]
Re2 <- (D * V) / nu

return(Re2)

}






#' @title Calculating the Reynolds Number 3
#' @name Re3
#'
#' @param D numeric vector that contains the hydraulic diameters "(four times
#'   the area in flow divided by the wetted surface) is a characteristic
#'   length" (m or ft)  [Reference: Lindeburg Manual]
#' @param mu numeric vector that contains the absolute or dynamic viscosity of
#'   the fluid (Pa-s or lbf-sec/ft^2)  [Reference: Lindeburg Manual]
#' @param gc numeric vector that contains the gravitational constant (32.2
#'   lbm-ft/lbf-sec^2)  [Reference: Lindeburg Manual]
#' @param G numeric vector that contains the 'mass flow rate per unit area'
#'   (kg/m^2-s or lbm/ft^2-sec)  [Reference: Lindeburg Manual]
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units, \code{Eng} for English units
#'   (United States Customary System in the United States and Imperial Units in
#'   the United Kingdom), and \code{slug} for the English unit slug which is a
#'   consistent unit of mass]
#'
#' @return the Reynolds number as a numeric \code{\link[base]{vector}} for Re3
#'
#' @note
#' Note: Please see the Calculating the Reynolds number Examples vignette for
#' usage of Re3
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
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#'
#' @seealso \code{\link{Re1}} for the additional seealso, description, details, note, and references sections, \code{\link{Re2}}, \code{\link{Re4}}
#' @export
Re3 <- function (D, G, mu, gc = NULL, units = c("SI", "Eng")) {

checks1 <- c(D, G, mu)

checks2 <- c(D, mu)

units <- units


# Check for D, G, mu, and units
assert_that(qtest(units, "S==1"), msg = "There is not an unit type or more than 1 unit type. Please specify either 'SI' or 'Eng'. Please try again.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("SI", "Eng") %in% units)), msg = "The unit system has not been identified correctly as either 'SI' or 'Eng'. Please try again.")
# only process with a specified unit and provide a stop warning if not

assert_that(!any(qtest(checks1, "N==3(,)") == FALSE), msg = "Either D, G, or mu is NA, NaN, Inf, -Inf, empty, or a string. Or, there are more than or less than 3 numeric values.  Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(!any(qtest(checks2, "N==2(0,)") == FALSE), msg = "Either D or mu is 0, NA, NaN, Inf, -Inf, empty, or a string. Or, there are more than or less than 2 numeric values.  Please try again.")
# only process with finite values and provide an error message if the check fails



# [Reference: Lindeburg Mamual]
if (units == "SI") {

Re3 <- (G * 4) / (pi * D * mu)

return(Re3)


} else {

ifelse(missing(gc), gc <- 9.80665 * (3937 / 1200), gc <- gc) # lbm-ft/lbf-sec^2

Re3 <- (G * 4) / (pi * D * mu)

return(Re3)

}
}





#' @title Calculating the Reynolds Number 4
#' @name Re4
#'
#' @param D numeric vector that contains the hydraulic diameters "(four times
#'   the area in flow divided by the wetted surface) is a characteristic
#'   length" (m or ft)  [Reference: Lindeburg Manual]
#' @param nu numeric vector that contains the kinematic viscosity of the fluid
#'   (m^2/s or lbf-sec/ft^2)  [Reference: Lindeburg Manual]
#' @param Q numeric vector that contains the discharge value of the fluid
#'   (m^3/s or ft^3/s)  [Reference: Lindeburg Manual]
#'
#' @return the Reynolds number as a numeric \code{\link[base]{vector}} for Re4
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
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#'
#'
#' @examples
#' 
#' # part of Example 3 from Subramanian Pipe Flow Calculations
#'
#' # Q = 2.23 * 10 ^ - 2 ft^3/s
#' # nu = 2.40 * 10 ^ -5 ft^2/s
#' # D = 9.03 * 10 ^ -2 ft
#' 
#' library(iemisc)
#'
#' Re4(Q = 2.23 * 10 ^ -2, nu = 2.40 * 10 ^ -5, D = 9.03 * 10 ^ -2)
#'
#'
#' @seealso \code{\link{Re1}} for the additional seealso, description, details, note, and references sections, \code{\link{Re2}}, \code{\link{Re3}}
#' @export
Re4 <- function (Q, nu, D) {

checks <- c(Q, nu, D)

# Check for Q, nu, D, and units
assert_that(!any(qtest(checks, "N==3(,)") == FALSE), msg = "Either Q, nu, or D is NA, NaN, Inf, -Inf, empty, or a string. Or, there are more than or less than 3 numeric values.  Please try again.")
# only process with finite values and provide an error message if the check fails

Re4 <- (Q * 4) / (pi * D * nu)

return(Re4)

}
