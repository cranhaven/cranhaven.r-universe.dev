#' Trapezoidal cross-section for the Gauckler-Manning-Strickler equation
#'
#' This function solves for one missing variable in the Gauckler-Manning-
#' Strickler equation for a trapezoidal cross-section and uniform flow. The
#' \code{\link[stats]{uniroot}} function is used to obtain the missing parameters.
#'
#'
#'
#' @details
#' Parameters Definitions from Chow pages 7, 13, 20, 22-23
#' "The depth of flow y is the vertical distance of the lowest point of a
#' channel section from the free surface."
#'
#' "The top width Temp is the width of channel section at the free surface."
#'
#' "The water area A is water area of the flow normal to the direction of flow."
#'
#' "The wetted perimeters P is the length of the line of intersection of the
#' channel wetted surface with a cross-sectional plane normal to the direction
#' of flow."
#'
#' "The hydraulic radius R is the ratio of the water area to its wetted
#' perimeters."
#'
#' "The hydraulic radius D is the ratio of the water area to the top width."
#'
#' "The section factor for critical-flow computation Z is the product of the
#' water area and the square root of the hydraulic depth."
#'
#' "The section factor for uniform-flow computation AR^2/3 is the product of the
#' water area and the two-thirds power of the hydraulic radius."
#'
#' "A channel built with unvarying cross section and constant bottom slope is
#' called a prismatic channel. Otherwise, the channel is nonprismatic."
#'
#' "For any flow, the discharge Q at a channel section is expressed by Q = V A
#' where V is the mean velocity and A is the flow cross-sectional area normal
#' to the direction of the flow, since the mean velocity is defined as the
#' discharge divided by the cross-sectional area."
#'
#' "The effect of viscosity relative to inertia can be represented by the
#' Reynolds number. ..."
#'
#' "The effect of gravity upon the state of flow is represented by a ratio of
#' inertial forces to gravity forces. This ratio is given by the Froude
#' number. ..."
#'
#'
#'
#' The References for the following equations, include, but are not limited to:
#'  Chow pages 5, 7, 13, 21, 23, 28; Schall pages 4-17 and 5-5; Wikimedia
#'  Conversion and Manning
#'
#' Gauckler-Manning-Strickler equation is expressed as
#'
#' \deqn{V = \frac{K_n}{n}R^\frac{2}{3}S^\frac{1}{2}}
#'
#' \describe{
#'   \item{\emph{V}}{the mean velocity (m/s or ft/sec)}
#'   \item{\emph{n}}{Manning's roughness coefficient (dimensionless)}
#'   \item{\emph{R}}{the hydraulic radius (m or ft)}
#'   \item{\emph{S}}{the slope of the channel bed (m/m or ft/ft)}
#'   \item{\emph{K_n}}{the conversion constant -- 1.0 for SI and
#'        3.2808399 ^ (1 / 3) for English units -- m^(1/3)/s or ft^(1/3)/s}
#' }
#'
#'
#'
#'
#' This equation is also expressed as
#'
#' \deqn{Q = \frac{K_n}{n}\frac{A^\frac{5}{3}}{P^\frac{2}{3}}S^\frac{1}{2}}
#'
#' \describe{
#'   \item{\emph{Q}}{the discharge [m^3/s or ft^3/s (cfs)] is VA}
#'   \item{\emph{n}}{Manning's roughness coefficient (dimensionless)}
#'   \item{\emph{P}}{the wetted perimeters of the channel (m or ft)}
#'   \item{\emph{A}}{water area (m^2 or ft^2)}
#'   \item{\emph{S}}{the slope of the channel bed (m/m or ft/ft)}
#'   \item{\emph{K_n}}{the conversion constant -- 1.0 for SI and
#'        3.2808399 ^ (1 / 3) for English units -- m^(1/3)/s or ft^(1/3)/s}
#' }
#'
#'
#'
#'
#' Other important equations regarding the trapezoidal cross-section follow:
#' \deqn{R = \frac{A}{P}}
#'
#' \describe{
#'   \item{\emph{R}}{the hydraulic radius (m or ft)}
#'   \item{\emph{A}}{water area (m^2 or ft^2)}
#'   \item{\emph{P}}{the wetted perimeters of the channel (m or ft)}
#' }
#'
#'
#'
#'
#' \deqn{A = y\left(b + my\right)}
#'
#' \describe{
#'   \item{\emph{A}}{water area (m^2 or ft^2)}
#'   \item{\emph{y}}{the flow depth (normal depth in this function) [m or ft]}
#'   \item{\emph{m}}{the horizontal side slope}
#'   \item{\emph{b}}{the bottom width (m or ft)}
#' }
#'
#'
#'
#'
#' \deqn{P = b + 2y\sqrt{\left(1 + m^2\right)}}
#'
#' \describe{
#'   \item{\emph{P}}{the wetted perimeters of the channel (m or ft)}
#'   \item{\emph{y}}{the flow depth (normal depth in this function) [m or ft]}
#'   \item{\emph{m}}{the horizontal side slope}
#'   \item{\emph{b}}{the bottom width (m or ft)}
#' }
#'
#'
#'
#'
#' \deqn{B = b + 2my}
#'
#' \describe{
#'   \item{\emph{B}}{the top width of the channel (m or ft)}
#'   \item{\emph{y}}{the flow depth (normal depth in this function) [m or ft]}
#'   \item{\emph{m}}{the horizontal side slope}
#'   \item{\emph{b}}{the bottom width (m or ft)}
#' }
#'
#'
#'
#' \deqn{D = \frac{A}{B}}
#'
#' \describe{
#'   \item{\emph{D}}{the hydraulic depth (m or ft)}
#'   \item{\emph{A}}{water area (m^2 or ft^2)}
#'   \item{\emph{B}}{the top width of the channel (m or ft)}
#' }
#'
#'
#'
#' \deqn{Z = \frac{[(b + my)y]^1.5}{\sqrt{b + 2my}}}
#'
#' \describe{
#'   \item{\emph{Z}}{the Section factor (m or ft)}
#'   \item{\emph{y}}{the flow depth (normal depth in this function) [m or ft]}
#'   \item{\emph{m}}{the horizontal side slope}
#'   \item{\emph{b}}{the bottom width (m or ft)}
#' }
#'
#'
#'
#' \deqn{E = y + \frac{Q^2}{2gA^2}}
#'
#' \describe{
#'   \item{\emph{E}}{the Specific Energy (m or ft)}
#'   \item{\emph{Q}}{the discharge [m^3/s or ft^3/s (cfs)] is VA}
#'   \item{\emph{g}}{gravitational acceleration (m/s^2 or ft/sec^2)}
#'   \item{\emph{A}}{water area (m^2 or ft^2)}
#'   \item{\emph{y}}{the flow depth (normal depth in this function) [m or ft]}
#' }
#'
#'
#'
#'
#' \deqn{VH = \frac{V^2}{2g}}
#'
#' \describe{
#'   \item{\emph{VH}}{the Velocity Head (m or ft)}
#'   \item{\emph{V}}{the mean velocity (m/s or ft/sec)}
#'   \item{\emph{g}}{gravitational acceleration (m/s^2 or ft/sec^2)}
#' }
#'
#'
#'
#'
#' \deqn{w = {y}\sqrt{m^2 + 1}}
#'
#' \describe{
#'   \item{\emph{w}}{the Wetted Length (m or ft)}
#'   \item{\emph{m}}{the horizontal side slope}
#'   \item{\emph{y}}{the flow depth (normal depth in this function) [m or ft]}
#' }
#'
#'
#'
#'
#' \deqn{\\tau_0 = {\\gamma}{RS}}
#'
#' \describe{
#'   \item{\emph{\\tau_0}}{"mean boundary shear stress" (N/m^2 or lbf/ft^2)}
#'   \item{\emph{\\gamma}}{unit weight of water at the given temperature (N/m^3 or lbf/ft^3)}
#'   \item{\emph{R}}{the hydraulic radius (m or ft)}
#'   \item{\emph{S}}{the slope of the channel bed ["average bottom slope (equal to energy slope for uniform flow)"] (m/m or ft/ft)}
#' }
#'
#'
#'
#'
#' \deqn{\\tau_d = {\\gamma}{yS}}
#'
#' \describe{
#'   \item{\emph{\\tau_d}}{"shear stress in channel at maximum depth" (N/m^2 or lbf/ft^2)}
#'   \item{\emph{\\gamma}}{unit weight of water at the given temperature (N/m^3 or lbf/ft^3)}
#'   \item{\emph{y}}{the flow depth ("maximum depth of flow in the channel for the design discharge") [m or ft]}
#'   \item{\emph{S}}{the slope of the channel bed ["average bottom slope (equal to energy slope for uniform flow)"] (m/m or ft/ft)}
#' }
#'
#'
#' # where
#'
#' \deqn{\\gamma = {g}{\\rho}}
#'
#' \describe{
#'   \item{\emph{\\gamma}}{unit weight of water at the given temperature (N/m^3 or lbf/ft^3)}
#'   \item{\emph{g}}{gravitational acceleration (m/s^2 or ft/sec^2)}
#'   \item{\emph{\\rho}}{density of the fluid at a certain temperature (kg/m^3 or slugs/ft^3)}
#' }
#'
#'
#' \deqn{\\gamma = {\\rho}\frac{g}{g_c}}
#'
#' \describe{
#'   \item{\emph{\\gamma}}{unit weight of water at the given temperature (lbf/ft^3)}
#'   \item{\emph{\\rho}}{density of the fluid at a certain temperature (lbm/ft^3)}
#'   \item{\emph{g}}{gravitational acceleration (m/s^2 or ft/sec^2)}
#'   \item{\emph{gc}}{gravitational constant (32.2 lbm-ft/lbf-sec^2) used for dimensional analysis so that the Reynolds number will be dimensionless with US Customary units}
#' }
#'
#'
#'
#'
#' \deqn{K = \frac{k(A * R ^ (2 / 3))}{n}}
#'
#' \describe{
#'   \item{\emph{K}}{channel conveyance (m^3/s or ft^3/sec)}
#'   \item{\emph{k}}{unit conversion factor (1 in SI and 3.2808399^(1/3) in US Customary units}
#'   \item{\emph{A}}{water area (m^2 or ft^2)}
#'   \item{\emph{R}}{the hydraulic radius (m or ft)}
#'   \item{\emph{n}}{Manning's roughness coefficient (dimensionless)}
#' }
#'
#'
#'
#'
#' A rough turbulent zone check is performed on the water flowing in the
#' channel using the Reynolds number (Re). The Re equation follows:
#'
#' \deqn{Re = \frac{\\rho RV}{\\mu}}
#'
#' \describe{
#'   \item{\emph{Re}}{Reynolds number (dimensionless)}
#'   \item{\emph{\\rho}}{water density (kg/m^3 or slug/ft^3)}
#'   \item{\emph{R}}{the hydraulic radius (m or ft)}
#'   \item{\emph{V}}{the mean velocity (m/s or ft/sec)}
#'   \item{\emph{\\mu}}{dynamic viscosity (* 10^-3 kg/m*s or * 10^-5 lb*sec/ft^2)}
#' }
#'
#'
#'
#' A critical flow check is performed on the water flowing in the channel
#' using the Froude number (Fr). The Fr equation follows:
#'
#' \deqn{Fr = \frac{V}{\left(\sqrt{g * D}\right)}}
#'
#' \describe{
#'   \item{\emph{Fr}}{the Froude number (dimensionless)}
#'   \item{\emph{V}}{the mean velocity (m/s or ft/sec)}
#'   \item{\emph{g}}{gravitational acceleration (m/s^2 or ft/sec^2)}
#'   \item{\emph{D}}{the hydraulic depth (m or ft)}
#' }
#'
#'
#'
#' @note
#' Assumption: Surface water temperature of 20 degrees Celsius (68 degrees
#' Fahrenheit) at atmospheric pressure
#'
#' Note: Units must be consistent
#' 
#' 
#' 
#' 
#'
#'
#' @param Q numeric vector that contains the discharge value (m^3/s or ft^3/s),
#'     if known.
#' @param n numeric vector that contains the Manning's roughness coefficient n,
#'     if known.
#' @param b numeric vector that contains the bottom width, if known.
#' @param m numeric vector that contains the symmetric "cross-sectional side slope
#'     of m:V (horizontal:vertical)", if known.
#' @param m1 numeric vector that contains the non-symmetric "cross-sectional side
#'     slope of m1:V (horizontal:vertical)", if known.
#' @param m2 numeric vector that contains the non-symmetric "cross-sectional side
#'     slope of m2:V (horizontal:vertical)", if known.
#' @param Sf numeric vector that contains the bed slope (m/m or ft/ft),
#'     if known.
#' @param y numeric vector that contains the flow depth (m or ft), if known.
#' @param Temp numeric vector that contains the temperature (degrees C or degrees
#'     Fahrenheit), if known. Otherwise, the default value is 20 degrees Celsius
#'     (68 degrees Fahrenheit).
#' @param units character vector that contains the system of units [options are
#'     \code{SI} for International System of Units or \code{Eng} for English units
#'     (United States Customary System in the United States and Imperial Units in
#'     the United Kingdom)]
#' @param type character vector that contains the type of trapezoid (symmetrical
#'     or non-symmetrical). The symmetrical trapezoid uses \code{m} while the non-
#'     symmetrical trapezoid uses \code{m1} and \code{m2}.
#' @param output character vector that contains the output type, either it will
#'     be a \code{\link[base]{list}} or \code{\link[data.table]{data.table}}. The list is
#'     the easiest to obtain a singular value. Please see the examples and the
#'     vignettes.
#'
#'
#'
#' @return the missing parameters (Q, n, b, m, m1, m2, Sf, or y) & V (velocity),
#'     Flow depth (y), Bottom width (b), symmetric side slope (m), Slope (Sf),
#'     A (area), P (wetted perimeters), R (hydraulic radius), B (top width), D
#'     (hydraulic depth), w (Wetted Length), w1 (Wetted Length for a
#'     non-symmetric trapezoid), w2 (Wetted Length for a non-symmetric trapezoid),
#'     Z (Section Factor), E (Specific Energy), K (conveyance), Vel_Head (Velocity
#'     Head), Re (Reynolds number), Fr (Froude number), taud (maximum shear
#'     stress), tau0 (average shear stress) as a \code{\link[base]{list}}.
#'     Alternatively, the Flow depth (y), Flow area (A), Wetted Perimeters (P),
#'     Top Width (B), Bottom width (b), Hydraulic Radius (R), Hydraulic Depth (D),
#'     Flow Mean Velocity (V), Flow Discharge (Q), Manning's roughness coefficient
#'     (n), Slope (Sf), Temperature, Absolute Temperature, Saturated Liquid
#'     Density, Absolute or Dynamic Viscosity, Kinematic Viscosity, Froude number
#'     (Fr), Reynolds number (Re), symmetric side slope (m), non-symmetric side
#'     slope (m1), non-symmetric side slope (m2), Wetted Length (w), Wetted Length
#'     for a non-symmetric trapezoid (w1), Wetted Length for a non-symmetric
#'     trapezoid (w2), Section Factor (Z), conveyance (K), Specific Energy (E),
#'     Velocity Head (Vel_Head), Maximum Shear Stress (taud), Average Shear Stress
#'     (tau0) along with the associated units can be returned in a \code{\link[data.table]{data.table}}.
#'
#'
#'
#' @references
#' \enumerate{
#'    \item Harlan Bengtson, "Calculation of Open Channel Flow Hydraulic Radius: Calculate using Trapezoid Area", Bright Hub Engineering Hydraulics in Civil Engineering, \url{https://www.brighthubengineering.com/hydraulics-civil-engineering/67126-calculation-of-hydraulic-radius-for-uniform-open-channel-flow/}.
#'    \item Andrew Chadwick, John Morfett, and Martin Borthwick, \emph{Hydraulics in Civil and Environmental Engineering}, Fourth Edition, New York City, New York: Spon Press, 2004, pages 132-133.
#'    \item R.J. Charbeneau, "Topic 8: Open Channel Flow", CE 365K Hydraulic Engineering Design, The University of Texas at Austin Cockrell School of Engineering Department of Civil, Architectural and Environmental Engineering, \url{https://www.caee.utexas.edu/prof/maidment/CE365KSpr14/Visual/OpenChannels.pdf}.
#'    \item Ven Te Chow, Ph.D., \emph{Open-Channel Hydraulics}, McGraw-Hill Classic Textbook Reissue, New York City, New York: McGraw-Hill Book Company, 1988, pages 7-8, 13, 20-23, 28, 39-43.
#'    \item John C. Crittenden, R. Rhodes Trussell, David W. Hand, Kerry J. Howe, George Tchobanoglous, \emph{MWH's Water Treatment: Principles and Design}, Third Edition, Hoboken, New Jersey: John Wiley & Sons, Inc., 2012, pages 1861-1862.
#'    \item Prof. Dr. Aminuddin Ab Ghani, "Specific Energy & Hydraulic Jump", Universiti Sains Malaysia (USM) Engineering Campus River Engineering and Urban Drainage Research Centre (REDACE), \url{https://web.archive.org/web/20200110165556/https://redac.eng.usm.my/EAH/Handouts/Specific\%20Energy\%202011.pdf}. Retrieved thanks to the Internet Archive: Wayback Machine
#'    \item Tyler G. Hicks, P.E., \emph{Civil Engineering Formulas: Pocket Guide}, 2nd Edition, New York City, New York: The McGraw-Hill Companies, Inc., 2002, pages 423, 425.
#'    \item Gary P. Merkley, "BIE6300 - Irrigation & Conveyance Control Systems, Spring 2004", 2004, Biological and Irrigation Engineering - OCW. Course 2, \url{https://digitalcommons.usu.edu/ocw_bie/2/}.
#'    \item Dan Moore, P.E., NRCS Water Quality and Quantity Technology Development Team, Portland Oregon, "Using Mannings Equation with Natural Streams", August 2011, \url{https://web.archive.org/web/20210416091858/https://www.wcc.nrcs.usda.gov/ftpref/wntsc/H&H/xsec/manningsNaturally.pdf}. Retrieved thanks to the Internet Archive: Wayback Machine
#'    \item Robert L. Mott and Joseph A. Untener, \emph{Applied Fluid Mechanics}, Seventh Edition, New York City, New York: Pearson, 2015, pages 376, 392.
#'    \item The NIST Reference on Constants, Units, and Uncertainty, Fundamental Constants Data Center of the NIST Physical Measurement Laboratory, "standard acceleration of gravity g_n", \url{https://physics.nist.gov/cgi-bin/cuu/Value?gn}.
#'    \item James D. Schall, Everett V. Richardson, and Johnny L. Morris, U.S. Department of Transportation Federal Highway Administration & National Highway Institute (NHI) and Office of Bridge Technology (HIBTemp), \emph{Introduction to Highway Hydraulics: Hydraulic Design Series Number 4}, Fourth Edition, June 2008, pages 4-5, 4-16 - 4-17 and 5-5, \url{https://www.fhwa.dot.gov/engineering/hydraulics/pubs/08090/HDS4_608.pdf}.
#'    \item Terry W. Sturm, \emph{Open Channel Hydraulics}, 2nd Edition, New York City, New York: The McGraw-Hill Companies, Inc., 2010, pages 2, 8, 36, 102, 120, 153.
#'    \item US Department of Transportation Federal Highway Administration (FHWA), "Urban Drainage Design Manual", Hydraulic Engineering Circular No. 22, Third Edition, Publication No. FHWA-NHI-10-009 September 2009 (Revised August 2013), pages 5-7 - 5-8, \url{https://www.fhwa.dot.gov/engineering/hydraulics/library_arc.cfm?pub_number=22&id=140}.
#'    \item Ali R.Vatankhah, "Explicit solutions for critical and normal depths in trapezoidal and parabolic open channels", \emph{Ain Shams Engineering Journal}, Volume 4, Issue 1, March 2013, Pages 17-23, \url{https://www.sciencedirect.com/science/article/pii/S2090447912000329}.
#'    \item Wikimedia Foundation, Inc. Wikipedia, 15 May 2019, "Conversion of units", \url{https://en.wikipedia.org/wiki/Conversion_of_units}.
#'    \item Wikimedia Foundation, Inc. Wikipedia, 23 May 2019, "Manning formula", \url{https://en.wikipedia.org/wiki/Manning_formula}.
#' }
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
#'
#'
#' @seealso \code{\link{Manningrect}} for a rectangular cross-section, \code{\link{Manningtri}}
#'   for a triangular cross-section, \code{\link{Manningpara}} for a parabolic
#'   cross-section, and \code{\link{Manningcirc}} for a circular cross-section.
#'
#'
#'
#'
#' @examples
#' 
#' # Example 1
#'
#' library(iemisc)
#' # Exercise 4.1 from Sturm (page 153)
#'
#' uu <- Manningtrap(Q = 3000, b = 40, m = 3, Sf = 0.002, n = 0.025,
#' units = "Eng", type = "symmetrical", output = "list")
#' # Q = 3000 cfs, b = 40 ft, m = 3, Sf = 0.002 ft/ft, n = 0.025,
#' # units = English units
#' # This will solve for y since it is missing and y will be in ft
#'
#' uu$y # only returns y
#'
#' uu # returns all results
#'
#'
#'
#' # Example 2
#'
#' # Please refer to the iemisc: Manning... Examples using iemiscdata
#' # [https://www.ecoccs.com/R_Examples/Manning_iemiscdata_Examples.pdf] and iemisc:
#' # Open Channel Flow Examples involving Geometric Shapes with the
#' # Gauckler-Manning-Strickler Equation
#' # [https://www.ecoccs.com/R_Examples/Open-Channel-Flow_Examples_Geometric_Shapes.pdf]
#' # for the cross-section examples using iemiscdata
#'
#'
#'
#'
#'
#'
#'
#'
#' @importFrom data.table data.table setnames setattr
#' @importFrom fpCompare %==%
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom units set_units make_units drop_units
#' @importFrom round round_r3
#' @importFrom stats uniroot
#'
#' @export
Manningtrap <- function (Q = NULL, n = NULL, m = NULL, m1 = NULL, m2 = NULL, Sf = NULL, y = NULL, b = NULL, Temp = NULL, units = c("SI", "Eng"), type = c("symmetrical", "non-symmetrical"), output = c("list", "data.table")) {


checks <- c(Q, n, m, m1, m2, Sf, y, b)

units <- units

type <- type

output <- output


# Check
assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either Q, n, m, m1, m2, Sf, b, or y is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(qtest(units, "S==1"), msg = "There is not an units type or more than 1 units type. Please specify either 'SI' or 'Eng'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("SI", "Eng") %in% units)), msg = "The unit system has not been identified correctly as either 'SI' or 'Eng'. Please try again.")
# only process with a specified unit and provide a stop warning if not

assert_that(qtest(type, "S==1"), msg = "There is not a slope type or more than 1 slope type. Please specify either 'symmetrical' or 'non-symmetrical'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("symmetrical", "non-symmetrical") %in% type)), msg = "The type has not been identified correctly as either 'symmetrical' or 'non-symmetrical'. Please try again.")
# only process with a specified type and provide a stop warning if not

assert_that(qtest(output, "S==1"), msg = "There is not an output type or more than 1 output type. Please specify either 'list' or 'data.table'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("list", "data.table") %in% output)), msg = "The output has not been identified correctly as either 'list' or 'data.table'. Please try again.")
# only process with a specified output parameters and provide a stop warning if not


# units
if (units == "SI") {

# use the temperature to determine the density & absolute and kinematic viscosities
Temp <- ifelse(is.null(Temp), 20, Temp) # degrees C

assert_that(qtest(Temp, "N+(0,)"), msg = "Either Temp is equal to or less than 0 C, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures greater than 0 C / 32 F. Please try again.")
# only process with specified, finite values and provide an error message if the check fails

# create a numeric vector with the units of degrees Celsius
T_C <- set_units(Temp, "degree_C")


# create a numeric vector to convert from degrees Celsius to Kelvin
T_K <- T_C


# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)


# create viscosities based on temperatures
# saturated liquid density at given temperature in degrees Celsius (SI units)
rho_SI <- density_water(Temp, "SI")

# absolute or dynamic viscosity at given temperature in degrees Celsius and density of rho (SI units)
mu_SI <- dyn_visc_water(Temp, "SI")

# kinematic viscosity at given temperature in degrees Celsius and density of rho (SI units)
nu_SI <- kin_visc_water(rho_SI, mu_SI, rho_units = "kg/m^3", mu_units = "Pa*s or kg/m/s")


k <- 1

g <- 9.80665 # m / s^2

rho <- rho_SI

nu <- nu_SI

mu <- mu_SI

# unit weight of water at given temperature in degrees Celsius and density of rho (SI units)
gamma <- unit_wt(rho = rho, units = "SI")


density_water_units <- "kg/m^3"

dyn_visc_water_units <- "Pa * s or kg/m*s"

kin_visc_water_units <- "m^2/s"


result_units <- c("m", "m^2", "m", "m", "m", "m", "m", "m/s", "m^3/s", "dimensionless", "m/m", "degrees Celsius", "Kelvin", density_water_units, dyn_visc_water_units, kin_visc_water_units, "dimensionless", "dimensionless", "m/m", "m/m", "m/m", "m", "m", "m", "m", "m^3/s", "m", "m", "pascal (N/m^2)", "pascal (N/m^2)")


} else if (units == "Eng") {

# use the temperature to determine the density & absolute and kinematic viscosities
Temp <- ifelse(is.null(Temp), 68, Temp) # degrees F

assert_that(qtest(Temp, "N+(32,)"), msg = "Either Temp is equal to or less than 32 F, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures greater than 32 F / 0 C. Please try again.")
# only process with specified, finite values and provide an error message if the check fails

T_F <- Temp

# create a numeric vector with the units of degrees Fahrenheit
T_F <- set_units(T_F, "degree_F")


# create a numeric vector to convert from degrees Fahrenheit to Kelvin
T_K <- T_F


# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)


# create viscosities based on temperatures
# saturated liquid density at given temperature in degrees Fahrenheit (US Customary units)
rho_Eng <- density_water(Temp, "Eng", Eng_units = "slug/ft^3")


# absolute or dynamic viscosity at given temperature in degrees Fahrenheit and density of rho (US Customary units)
mu_Eng <- dyn_visc_water(Temp, "Eng", Eng_units = "slug/ft/s")


# kinematic viscosity at given temperature in degrees Fahrenheit and density of rho (US Customary units)
nu_Eng <- kin_visc_water(rho_Eng, mu_Eng, rho_units = "slug/ft^3", mu_units = "slug/ft/s")



k <- 3.2808399 ^ (1 / 3)

g <- 9.80665 * (3937 / 1200) # ft / sec^2

gc <- 9.80665 * (3937 / 1200) # lbm-ft/lbf-sec^2

rho <- rho_Eng

nu <- nu_Eng

mu <- mu_Eng

# unit weight of water at given temperature in degrees Fahrenheit and density of rho (US Customary units)
gamma <- unit_wt(rho = rho, units = "Eng", Eng_units = "slug/ft^3")


density_water_units <- "slug/ft^3"

dyn_visc_water_units <- "slug/ft*s"

kin_visc_water_units <- "ft^2/s"


result_units <- c("ft", "ft^2", "ft", "ft", "ft", "ft", "ft", "ft/sec (fps)", "ft^3/sec (cfs)", "dimensionless", "ft/ft", "degrees Fahrenheit", "Kelvin", density_water_units, dyn_visc_water_units, kin_visc_water_units, "dimensionless", "dimensionless", "ft/ft", "ft/ft", "ft/ft", "ft", "ft", "ft", "ft", "ft^3/sec (cfs)", "ft", "ft", "lb/ft^2", "lb/ft^2")

}



# symmetric case
if (type == "symmetrical") {

checks1 <- c(Q, n, m, Sf, y, b)

assert_that(qtest(checks1, "N==5"), msg = "There are more than or less than 5 known variables (Q, n, m, Sf, y, and/or b). Please try again.")
# only process with enough known variables and provide an error message if the check fails


if (missing(Q)) {

A <- y * (b + m * y)
P <- b + 2 * y * sqrt(1 + m ^ 2)
B <- b + 2 * m * y
R <- A / P
D <- A / B

Qfun <- function(Q) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

Quse <- uniroot(Qfun, interval = c(0.0000001, 200), extendInt = "yes")

Q <- Quse$root

V <- Q / A

w <- y * sqrt(m ^ 2 + 1)

w1 <- NA_real_

w2 <- NA_real_

m1 <- NA_real_

m2 <- NA_real_

Z <- A * R ^ (2 / 3)

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)

Re <- (rho * R * V) / mu

# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf

# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf

# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}


if(output == "list") {

return(list(Q = Q, V = V, y = y, b = b, m = m, Sf = Sf, n = n, A = A, P = P, R = R, B = B, D = D, w = w, Z = Z, E = E, K = K, Vel_Head = Vel_Head, Re = Re, Fr = Fr, taud = taud, tau0 = tau0))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = c(y, A, P, B, b, R, D, V, Q, n, Sf, Temp, drop_units(T_K), rho, mu, nu, Fr, Re, m, m1, m2, w, w1, w2, Z, K, E, Vel_Head, taud, tau0), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}


} else if (missing(n)) {

A <- y * (b + m * y)
P <- b + 2 * y * sqrt(1 + m ^ 2)
B <- b + 2 * m * y
R <- A / P
D <- A / B

nfun <- function(n) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

nuse <- uniroot(nfun, interval = c(0.0000001, 200), extendInt = "yes")

n <- nuse$root

V <- Q / A

w <- y * sqrt(m ^ 2 + 1)

w1 <- NA_real_

w2 <- NA_real_

m1 <- NA_real_

m2 <- NA_real_

Z <- A * R ^ (2 / 3)

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}



if(output == "list") {

return(list(n = n, Q = Q, V = V, y = y, b = b, m = m, Sf = Sf, A = A, P = P, R = R, B = B, D = D, w = w, Z = Z, E = E, K = K, Vel_Head = Vel_Head, Re = Re, Fr = Fr, taud = taud, tau0 = tau0))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = c(y, A, P, B, b, R, D, V, Q, n, Sf, Temp, drop_units(T_K), rho, mu, nu, Fr, Re, m, m1, m2, w, w1, w2, Z, K, E, Vel_Head, taud, tau0), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}




} else if (missing(m)) {

mfun <- function(m) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf) * (k / n)) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

muse <- uniroot(mfun, interval = c(0, 30), extendInt = "yes")

m <- muse$root

A <- y * (b + m * y)
P <- b + 2 * y * sqrt(1 + m ^ 2)
B <- b + 2 * m * y
R <- A / P
D <- A / B

V <- Q / A

w <- y * sqrt(m ^ 2 + 1)

w1 <- NA_real_

w2 <- NA_real_

m1 <- NA_real_

m2 <- NA_real_

Z <- A * R ^ (2 / 3)

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}



if(output == "list") {

return(list(m = m, Q = Q, V = V, y = y, b = b, Sf = Sf, n = n, A = A, P = P, R = R, B = B, D = D, w = w, Z = Z, E = E, K = K, Vel_Head = Vel_Head, Re = Re, Fr = Fr, taud = taud, tau0 = tau0))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = c(y, A, P, B, b, R, D, V, Q, n, Sf, Temp, drop_units(T_K), rho, mu, nu, Fr, Re, m, m1, m2, w, w1, w2, Z, K, E, Vel_Head, taud, tau0), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}



} else if (missing(b)) {

bfun <- function(b) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

buse <- uniroot(bfun, interval = c(0.0000001, 200), extendInt = "yes")

b <- buse$root

A <- y * (b + m * y)
P <- b + 2 * y * sqrt(1 + m ^ 2)
B <- b + 2 * m * y
R <- A / P
D <- A / B

V <- Q / A

w <- y * sqrt(m ^ 2 + 1)

w1 <- NA_real_

w2 <- NA_real_

m1 <- NA_real_

m2 <- NA_real_

Z <- A * R ^ (2 / 3)

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}



if(output == "list") {

return(list(b = b, Q = Q, V = V, y = y, m = m, Sf = Sf, n = n, A = A, P = P, R = R, B = B, D = D, w = w, Z = Z, E = E, K = K, Vel_Head = Vel_Head, Re = Re, Fr = Fr, taud = taud, tau0 = tau0))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = c(y, A, P, B, b, R, D, V, Q, n, Sf, Temp, drop_units(T_K), rho, mu, nu, Fr, Re, m, m1, m2, w, w1, w2, Z, K, E, Vel_Head, taud, tau0), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}



} else if (missing(y)) {

yfun <- function(y) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

yuse <- uniroot(yfun, interval = c(0.0000001, 200), extendInt = "yes")

y <- yuse$root

A <- y * (b + m * y)
P <- b + 2 * y * sqrt(1 + m ^ 2)
B <- b + 2 * m * y
R <- A / P
D <- A / B

V <- Q / A

w <- y * sqrt(m ^ 2 + 1)

w1 <- NA_real_

w2 <- NA_real_

m1 <- NA_real_

m2 <- NA_real_

Z <- A * R ^ (2 / 3)

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}




if(output == "list") {

return(list(y = y, Q = Q, V = V, b = b, m = m, Sf = Sf, n = n, A = A, P = P, R = R, B = B, D = D, w = w, Z = Z, E = E, K = K, Vel_Head = Vel_Head, Re = Re, Fr = Fr, taud = taud, tau0 = tau0))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = c(y, A, P, B, b, R, D, V, Q, n, Sf, Temp, drop_units(T_K), rho, mu, nu, Fr, Re, m, m1, m2, w, w1, w2, Z, K, E, Vel_Head, taud, tau0), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}



} else if (missing(Sf)) {

A <- y * (b + m * y)
P <- b + 2 * y * sqrt(1 + m ^ 2)
B <- b + 2 * m * y
R <- A / P
D <- A / B

Sffun <- function(Sf) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

Sfuse <- uniroot(Sffun, interval = c(0.0000001, 200), extendInt = "yes")

Sf <- Sfuse$root

V <- Q / A

w <- y * sqrt(m ^ 2 + 1)

w1 <- NA_real_

w2 <- NA_real_

m1 <- NA_real_

m2 <- NA_real_

Z <- A * R ^ (2 / 3)

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}



if(output == "list") {

return(list(Sf = Sf, Q = Q, V = V, y = y, b = b, m = m, n = n, A = A, P = P, R = R, B = B, D = D, w = w, Z = Z, E = E, K = K, Vel_Head = Vel_Head, Re = Re, Fr = Fr, taud = taud, tau0 = tau0))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = c(y, A, P, B, b, R, D, V, Q, n, Sf, Temp, drop_units(T_K), rho, mu, nu, Fr, Re, m, m1, m2, w, w1, w2, Z, K, E, Vel_Head, taud, tau0), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}
}


# non-symmetric case

} else if (type == "non-symmetrical") {

checks2 <- c(Q, n, m1, m2, Sf, y, b)

assert_that(qtest(checks2, "N==6"), msg = "There are more than or less than 6 known variables (Q, n, m1, m2, Sf, y, and/or b). Please try again.")
# only process with enough known variables and provide an error message if the check fails


if (missing(Q)) {

A <- y * (b + 0.5 * (m1 + m2) * y)
P <- b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))
B <- b + y * (m1 + m2)
R <- A / P
D <- A / B

Qfun <- function(Q) {Q - (((y * (b + 0.5 * (m1 + m2) * y) ) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

Quse <- uniroot(Qfun, interval = c(0.0000001, 200), extendInt = "yes")

Q <- Quse$root

V <- Q / A

w1 <- y * sqrt(m1 ^ 2 + 1)

w2 <- y * sqrt(m2 ^ 2 + 1)

w <- NA_real_

m <- NA_real_

Z <- ((b + (m1 + m2) * y) * y) ^ 1.5 / (sqrt(b + 2 * m1 * y) + sqrt(b + 2 * m2 * y))

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}




if(output == "list") {

return(list(Q = Q, V = V, y = y, b = b, m = m, Sf = Sf, n = n, A = A, P = P, R = R, B = B, D = D, w = w, Z = Z, E = E, K = K, Vel_Head = Vel_Head, Re = Re, Fr = Fr, taud = taud, tau0 = tau0))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = c(y, A, P, B, b, R, D, V, Q, n, Sf, Temp, drop_units(T_K), rho, mu, nu, Fr, Re, m, m1, m2, w, w1, w2, Z, K, E, Vel_Head, taud, tau0), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}



} else if (missing(n)) {

A <- y * (b + 0.5 * (m1 + m2) * y)
P <- b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))
B <- b + y * (m1 + m2)
R <- A / P
D <- A / B

nfun <- function(n) {Q - (((y * (b + 0.5 * (m1 + m2) * y) ) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

nuse <- uniroot(nfun, interval = c(0.0000001, 200), extendInt = "yes")

n <- nuse$root

V <- Q / A

w1 <- y * sqrt(m1 ^ 2 + 1)

w2 <- y * sqrt(m2 ^ 2 + 1)

w <- NA_real_

m <- NA_real_

Z <- ((b + (m1 + m2) * y) * y) ^ 1.5 / (sqrt(b + 2 * m1 * y) + sqrt(b + 2 * m2 * y))

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}





if(output == "list") {

return(list(n = n, Q = Q, V = V, y = y, b = b, m = m, Sf = Sf, A = A, P = P, R = R, B = B, D = D, w = w, Z = Z, E = E, K = K, Vel_Head = Vel_Head, Re = Re, Fr = Fr, taud = taud, tau0 = tau0))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = c(y, A, P, B, b, R, D, V, Q, n, Sf, Temp, drop_units(T_K), rho, mu, nu, Fr, Re, m, m1, m2, w, w1, w2, Z, K, E, Vel_Head, taud, tau0), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}




} else if (missing(m1)) {

m1fun <- function(m1) {Q - (((y * (b + 0.5 * (m1 + m2) * y) ) ^ (5 / 3) * sqrt(Sf) * (k / n)) / ((b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

m1use <- uniroot(m1fun, interval = c(0, 30), extendInt = "yes")

m1 <- m1use$root

A <- y * (b + 0.5 * (m1 + m2) * y)
P <- b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))
B <- b + y * (m1 + m2)
R <- A / P
D <- A / B

V <- Q / A

w1 <- y * sqrt(m1 ^ 2 + 1)

w2 <- y * sqrt(m2 ^ 2 + 1)

w <- NA_real_

m <- NA_real_

Z <- ((b + (m1 + m2) * y) * y) ^ 1.5 / (sqrt(b + 2 * m1 * y) + sqrt(b + 2 * m2 * y))

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}




if(output == "list") {

return(list(m1 = m1, Q = Q, V = V, y = y, b = b, m = m, m2 = m2, Sf = Sf, n = n, A = A, P = P, R = R, B = B, D = D, w = w, Z = Z, E = E, K = K, Vel_Head = Vel_Head, Re = Re, Fr = Fr, taud = taud, tau0 = tau0))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = c(y, A, P, B, b, R, D, V, Q, n, Sf, Temp, drop_units(T_K), rho, mu, nu, Fr, Re, m, m1, m2, w, w1, w2, Z, K, E, Vel_Head, taud, tau0), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}






} else if (missing(m2)) {

m2fun <- function(m2) {Q - (((y * (b + 0.5 * (m1 + m2) * y) ) ^ (5 / 3) * sqrt(Sf) * (k / n)) / ((b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

m2use <- uniroot(m2fun, interval = c(0, 30), extendInt = "yes")

m2 <- m2use$root

A <- y * (b + 0.5 * (m1 + m2) * y)
P <- b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))
B <- b + y * (m1 + m2)
R <- A / P
D <- A / B

V <- Q / A

w1 <- y * sqrt(m1 ^ 2 + 1)

w2 <- y * sqrt(m2 ^ 2 + 1)

w <- NA_real_

m <- NA_real_

Z <- ((b + (m1 + m2) * y) * y) ^ 1.5 / (sqrt(b + 2 * m1 * y) + sqrt(b + 2 * m2 * y))

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}




if(output == "list") {

return(list(m2 = m2, Q = Q, V = V, y = y, b = b, m = m, m1 = m1, Sf = Sf, n = n, A = A, P = P, R = R, B = B, D = D, w = w, Z = Z, E = E, K = K, Vel_Head = Vel_Head, Re = Re, Fr = Fr, taud = taud, tau0 = tau0))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = c(y, A, P, B, b, R, D, V, Q, n, Sf, Temp, drop_units(T_K), rho, mu, nu, Fr, Re, m, m1, m2, w, w1, w2, Z, K, E, Vel_Head, taud, tau0), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}





} else if (missing(b)) {

bfun <- function(b) {Q - (((y * (b + 0.5 * (m1 + m2) * y) ) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

buse <- uniroot(bfun, interval = c(0.0000001, 200), extendInt = "yes")

b <- buse$root

A <- y * (b + 0.5 * (m1 + m2) * y)
P <- b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))
B <- b + y * (m1 + m2)
R <- A / P
D <- A / B

V <- Q / A

w1 <- y * sqrt(m1 ^ 2 + 1)

w2 <- y * sqrt(m2 ^ 2 + 1)

w <- NA_real_

m <- NA_real_

Z <- ((b + (m1 + m2) * y) * y) ^ 1.5 / (sqrt(b + 2 * m1 * y) + sqrt(b + 2 * m2 * y))

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}





if(output == "list") {

return(list(b = b, Q = Q, V = V, y = y, m = m, Sf = Sf, n = n, A = A, P = P, R = R, B = B, D = D, w = w, Z = Z, E = E, K = K, Vel_Head = Vel_Head, Re = Re, Fr = Fr, taud = taud, tau0 = tau0))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = c(y, A, P, B, b, R, D, V, Q, n, Sf, Temp, drop_units(T_K), rho, mu, nu, Fr, Re, m, m1, m2, w, w1, w2, Z, K, E, Vel_Head, taud, tau0), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}



} else if (missing(y)) {

yfun <- function(y) {Q - (((y * (b + 0.5 * (m1 + m2) * y) ) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

yuse <- uniroot(yfun, interval = c(0.0000001, 200), extendInt = "yes")

y <- yuse$root

A <- y * (b + 0.5 * (m1 + m2) * y)
P <- b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))
B <- b + y * (m1 + m2)
R <- A / P
D <- A / B

V <- Q / A

w1 <- y * sqrt(m1 ^ 2 + 1)

w2 <- y * sqrt(m2 ^ 2 + 1)

w <- NA_real_

m <- NA_real_

Z <- ((b + (m1 + m2) * y) * y) ^ 1.5 / (sqrt(b + 2 * m1 * y) + sqrt(b + 2 * m2 * y))

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}



if(output == "list") {

return(list(y = y, Q = Q, V = V, b = b, m = m, Sf = Sf, n = n, A = A, P = P, R = R, B = B, D = D, w = w, Z = Z, E = E, K = K, Vel_Head = Vel_Head, Re = Re, Fr = Fr, taud = taud, tau0 = tau0))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = c(y, A, P, B, b, R, D, V, Q, n, Sf, Temp, drop_units(T_K), rho, mu, nu, Fr, Re, m, m1, m2, w, w1, w2, Z, K, E, Vel_Head, taud, tau0), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}




} else if (missing(Sf)) {

A <- y * (b + 0.5 * (m1 + m2) * y)
P <- b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))
B <- b + y * (m1 + m2)
R <- A / P
D <- A / B

Sffun <- function(Sf) {Q - (((y * (b + 0.5 * (m1 + m2) * y) ) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

Sfuse <- uniroot(Sffun, interval = c(0.0000001, 200), extendInt = "yes")

Sf <- Sfuse$root

V <- Q / A

w1 <- y * sqrt(m1 ^ 2 + 1)

w2 <- y * sqrt(m2 ^ 2 + 1)

w <- NA_real_

m <- NA_real_

Z <- ((b + (m1 + m2) * y) * y) ^ 1.5 / (sqrt(b + 2 * m1 * y) + sqrt(b + 2 * m2 * y))

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}




if(output == "list") {

return(list(Sf = Sf, Q = Q, V = V, y = y, b = b, m = m, n = n, A = A, P = P, R = R, B = B, D = D, w = w, Z = Z, E = E, K = K, Vel_Head = Vel_Head, Re = Re, Fr = Fr, taud = taud, tau0 = tau0))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = c(y, A, P, B, b, R, D, V, Q, n, Sf, Temp, drop_units(T_K), rho, mu, nu, Fr, Re, m, m1, m2, w, w1, w2, Z, K, E, Vel_Head, taud, tau0), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}

}
}
}










#' Trapezoidal cross-section for the Gauckler-Manning-Strickler equation (critical parameters)
#'
#' This function solves for one missing variable in the Gauckler-Manning-
#' Strickler equation for a trapezoidal cross-section and uniform flow. The
#' \code{\link[stats]{uniroot}} function is used to obtain the missing parameters.
#' This function provides both normal and critical parameters values.
#'
#'
#'
#' @details
#' Critical State Discussion from Chow pages 13, 63
#' "When F (Froude number) is equal to unity, ... the flow is said to be in a 
#' \emph{critical} state. If F is less than unity, ... the flow is \emph{subcritical}. If
#' F is greater than unity, ... the flow is \emph{supercritical}."
#'
#' "... the critical state of flow through a channel section is characterized
#' by several important conditions. Recapitulating, they are (1) the specific
#' energy is a minimum for a given discharge; (2) the discharge is a maximum
#' for a given specific energy; (3) the specific force is a minimum for a given
#' discharge; (4) the velocity head is equal to half the hydraulic depth in a
#' channel of small slope; (5) the Froude number is equal to unity; and (6) the
#' velocity of flow in a channel of small slope with uniform velocity
#' distribution is equal to the celerity of small gravity waves in shallow
#' water caused by local disturbances."
#' 
#' "Discussions on critical state of flow have referred mainly to a particular
#' section of a channel, known as the \emph{critical section}. If the critical state
#' of exists throughout the entire length of the channel or over a reach of the
#' channel, the flow in the channel is a \emph{critical flow}."
#' 
#'
#' 
#'
#' @param Q numeric vector that contains the discharge value (m^3/s or ft^3/s),
#'   if known.
#' @param n numeric vector that contains the Manning's roughness coefficient n,
#'   if known.
#' @param b numeric vector that contains the bottom width, if known.
#' @param m numeric vector that contains the symmetric "cross-sectional side slope
#' of m:V (horizontal:vertical)", if known.
#' @param m1 numeric vector that contains the non-symmetric "cross-sectional side
#' slope of m1:V (horizontal:vertical)", if known.
#' @param m2 numeric vector that contains the non-symmetric "cross-sectional side
#' slope of m2:V (horizontal:vertical)", if known.
#' @param Sf numeric vector that contains the bed slope (m/m or ft/ft),
#'   if known.
#' @param y numeric vector that contains the flow depth (m or ft), if known.
#' @param Temp numeric vector that contains the temperature (degrees C or degrees
#'   Fahrenheit), if known.
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units or \code{Eng} for English units
#'   (United States Customary System in the United States and Imperial Units in
#'   the United Kingdom)]
#' @param type character vector that contains the type of trapezoid (symmetrical
#'   or non-symmetrical). The symmetrical trapezoid uses \code{m} while the non-
#'   symmetrical trapezoid uses \code{m1} and \code{m2}.
#' @param critical character vector that contains the type of critical parameters
#'   calculations (\code{approximate} or \code{accurate}). The accurate calculation
#'   provides parameters where the Froude number is 1. The approximate calculation
#'   calculates the values without having the Froude number return 1.
#' @param output character vector that contains the output type, either it will be
#'   a \code{\link[base]{list}} or \code{\link[data.table]{data.table}}. The list is
#'   the easiest to obtain a singular value.
#'
#'
#'
#' @return the missing parameters (Q, n, b, m, m1, m2, Sf, or y) & V (velocity),
#'   Flow depth (y), Bottom width (b), symmetric side slope (m), Slope (Sf),
#'   A (area), P (wetted perimeters), R (hydraulic radius), B (top width), D
#'   (hydraulic depth), w (Wetted Length), w1 (Wetted Length for a
#'   non-symmetric trapezoid), w2 (Wetted Length for a non-symmetric trapezoid),
#'   Z (Section Factor), E (Specific Energy), K (conveyance), Vel_Head (Velocity
#'   Head), Re (Reynolds number), Fr (Froude number), taud (maximum shear
#'   stress), tau0 (average shear stress), yc (critical depth), Ac (critical
#'   area), Pc (critical wetted perimeters), Bc (critical top width), Rc (critical
#'   hydraulic radius), Dc (critical hydraulic depth), Vc (critical velocity), Qc
#'   (critical discharge), Sfc (critical slope), Frc (critical Froude number), Zc
#'   (critical Section Factor), Ec (critical Specific Energy) as a \code{\link[base]{list}}.
#'   Alternatively, the Flow depth (y), Flow area (A), Wetted Perimeters (P),
#'   Top Width (B), Bottom width (b), Hydraulic Radius (R), Hydraulic Depth (D),
#'   Flow Mean Velocity (V), Flow Discharge (Q), Manning's roughness coefficient
#'   (n), Slope (Sf), Temperature, Absolute Temperature, Saturated Liquid
#'   Density, Absolute or Dynamic Viscosity, Kinematic Viscosity, Froude number
#'   (Fr), Reynolds number (Re), symmetric side slope (m), non-symmetric side
#'   slope (m1), non-symmetric side slope (m2), Wetted Length (w), Wetted Length
#'   for a non-symmetric trapezoid (w1), Wetted Length for a non-symmetric
#'   trapezoid (w2), Section Factor (Z), conveyance (K), Specific Energy (E),
#'   Velocity Head (Vel_Head), Maximum Shear Stress (taud), Average Shear Stress
#'   (tau0) along with the associated units can be returned in a \code{\link[data.table]{data.table}}.
#'   Both the normal and the critical values (where present) are returned in the
#'   table.
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
#' @references
#' Ven Te Chow, Ph.D., \emph{Open-Channel Hydraulics}, McGraw-Hill Classic Textbook Reissue, New York City, New York: McGraw-Hill Book Company, 1988, pages 13, 63.
#'
#' @examples
#' 
#' # Example 1
#'
#' library(iemisc)
#' # Exercise 4.1 from Sturm (page 153)
#'
#' uuc <- Manningtrap_critical(Q = 3000, b = 40, m = 3, Sf = 0.002, n = 0.025,
#' units = "Eng", type = "symmetrical", critical = "accurate", output = "list")
#' # Q = 3000 cfs, b = 40 ft, m = 3, Sf = 0.002 ft/ft, n = 0.025,
#' # units = English units
#' # This will solve for y since it is missing and y will be in ft
#'
#' uuc$y # only returns y
#'
#' uuc # returns all results
#'
#'
#'
#' # Example 2
#'
#' # Please refer to the iemisc: Manning... Examples using iemiscdata
#' # [https://www.ecoccs.com/R_Examples/Manning_iemiscdata_Examples.pdf] and iemisc:
#' # Open Channel Flow Examples involving Geometric Shapes with the
#' # Gauckler-Manning-Strickler Equation
#' # [https://www.ecoccs.com/R_Examples/Open-Channel-Flow_Examples_Geometric_Shapes.pdf]
#' # for the cross-section examples using iemiscdata
#'
#'
#'
#'
#'
#'
#'
#' @importFrom data.table data.table setnames setattr
#' @importFrom fpCompare %==%
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom units set_units make_units drop_units
#' @importFrom round round_r3
#' @importFrom stats uniroot
#' @importFrom rivr critical_depth
#'
#' @export
Manningtrap_critical <- function (Q = NULL, n = NULL, m = NULL, m1 = NULL, m2 = NULL, Sf = NULL, y = NULL, b = NULL, Temp = NULL, units = c("SI", "Eng"), type = c("symmetrical", "non-symmetrical"), critical = c("approximate", "accurate"), output = c("list", "data.table")) {


checks <- c(Q, n, m, m1, m2, Sf, y, b)

units <- units

type <- type

output <- output

critical <- critical


# Check
assert_that(qtest(units, "S==1"), msg = "There is not an units type or more than 1 units type. Please specify either 'SI' or 'Eng'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(qtest(type, "S==1"), msg = "There is not a slope type or more than 1 slope type. Please specify either 'symmetrical' or 'non-symmetrical'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(qtest(critical, "S==1"), msg = "There is not a critical type or more than 1 critical type. Please specify either 'approximate' or 'accurate'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(qtest(output, "S==1"), msg = "There is not an output type or more than 1 output type. Please specify either 'list' or 'data.table'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(!any(qtest(checks, "N+(0,)") == FALSE), msg = "Either Q, n, m, m1, m2, Sf, b, or y is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(isTRUE(any(c("SI", "Eng") %in% units)), msg = "The unit system has not been identified correctly as either 'SI' or 'Eng'. Please try again.")
# only process with a specified unit and provide a stop warning if not

assert_that(isTRUE(any(c("symmetrical", "non-symmetrical") %in% type)), msg = "The type has not been identified correctly as either 'symmetrical' or 'non-symmetrical'. Please try again.")
# only process with a specified type and provide a stop warning if not

assert_that(isTRUE(any(c("list", "data.table") %in% output)), msg = "The output has not been identified correctly as either 'list' or 'data.table'. Please try again.")
# only process with a specified output parameters and provide a stop warning if not

assert_that(isTRUE(any(c("approximate", "accurate") %in% critical)), msg = "The critical parameters has not been identified correctly as either 'approximate' or 'accurate'. Please try again.")
# only process with a specified output parameters and provide a stop warning if not



# units
if (units == "SI") {

# use the temperature to determine the density & absolute and kinematic viscosities
Temp <- ifelse(is.null(Temp), 20, Temp) # degrees C

assert_that(qtest(Temp, "N+(0,)"), msg = "Either Temp is equal to or less than 0 C, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures greater than 0 C / 32 F. Please try again.")
# only process with specified, finite values and provide an error message if the check fails

# create a numeric vector with the units of degrees Celsius
T_C <- set_units(Temp, "degree_C")


# create a numeric vector to convert from degrees Celsius to Kelvin
T_K <- T_C


# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)


# create viscosities based on temperatures
# saturated liquid density at given temperature in degrees Celsius (SI units)
rho_SI <- density_water(Temp, "SI")

# absolute or dynamic viscosity at given temperature in degrees Celsius and density of rho (SI units)
mu_SI <- dyn_visc_water(Temp, "SI")

# kinematic viscosity at given temperature in degrees Celsius and density of rho (SI units)
nu_SI <- kin_visc_water(rho_SI, mu_SI, rho_units = "kg/m^3", mu_units = "Pa*s or kg/m/s")




k <- 1

g <- 9.80665 # m / s^2

rho <- rho_SI

nu <- nu_SI

mu <- mu_SI

# unit weight of water at given temperature in degrees Celsius and density of rho (SI units)
gamma <- unit_wt(rho = rho, units = "SI")


density_water_units <- "kg/m^3"

dyn_visc_water_units <- "Pa * s or kg/m*s"

kin_visc_water_units <- "m^2/s"


result_units <- c("m", "m^2", "m", "m", "m", "m", "m", "m/s", "m^3/s", "dimensionless", "m/m", "degrees Celsius", "Kelvin", density_water_units, dyn_visc_water_units, kin_visc_water_units, "dimensionless", "dimensionless", "m/m", "m/m", "m/m", "m", "m", "m", "m", "m^3/s", "m", "m", "pascal (N/m^2)", "pascal (N/m^2)")


} else if (units == "Eng") {

# use the temperature to determine the density & absolute and kinematic viscosities
Temp <- ifelse(is.null(Temp), 68, Temp) # degrees F

assert_that(qtest(Temp, "N+(32,)"), msg = "Either Temp is equal to or less than 32 F, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures greater than 32 F / 0 C. Please try again.")
# only process with specified, finite values and provide an error message if the check fails

T_F <- Temp

# create a numeric vector with the units of degrees Fahrenheit
T_F <- set_units(T_F, "degree_F")


# create a numeric vector to convert from degrees Fahrenheit to Kelvin
T_K <- T_F


# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)


# create viscosities based on temperatures
# saturated liquid density at given temperature in degrees Fahrenheit (US Customary units)
rho_Eng <- density_water(Temp, "Eng", Eng_units = "slug/ft^3")


# absolute or dynamic viscosity at given temperature in degrees Fahrenheit and density of rho (US Customary units)
mu_Eng <- dyn_visc_water(Temp, "Eng", Eng_units = "slug/ft/s")


# kinematic viscosity at given temperature in degrees Fahrenheit and density of rho (US Customary units)
nu_Eng <- kin_visc_water(rho_Eng, mu_Eng, rho_units = "slug/ft^3", mu_units = "slug/ft/s")




k <- 3.2808399 ^ (1 / 3)

g <- 9.80665 * (3937 / 1200) # ft / sec^2

gc <- 9.80665 * (3937 / 1200) # lbm-ft/lbf-sec^2

rho <- rho_Eng

nu <- nu_Eng

mu <- mu_Eng

# unit weight of water at given temperature in degrees Fahrenheit and density of rho (US Customary units)
gamma <- unit_wt(rho = rho, units = "Eng", Eng_units = "slug/ft^3")


density_water_units <- "slug/ft^3"

dyn_visc_water_units <- "slug/ft*s"

kin_visc_water_units <- "ft^2/s"


result_units <- c("ft", "ft^2", "ft", "ft", "ft", "ft", "ft", "ft/sec (fps)", "ft^3/sec (cfs)", "dimensionless", "ft/ft", "degrees Fahrenheit", "Kelvin", density_water_units, dyn_visc_water_units, kin_visc_water_units, "dimensionless", "dimensionless", "ft/ft", "ft/ft", "ft/ft", "ft", "ft", "ft", "ft", "ft^3/sec (cfs)", "ft", "ft", "lb/ft^2", "lb/ft^2")

}



# symmetric case
if (type == "symmetrical") {


checks1 <- c(Q, n, m, Sf, y, b)

assert_that(qtest(checks1, "N==5"), msg = "There are more than or less than 5 known variables (Q, n, m, Sf, y, and/or b). Please try again.")
# only process with enough known variables and provide an error message if the check fails



if (missing(Q)) {

A <- y * (b + m * y)
P <- b + 2 * y * sqrt(1 + m ^ 2)
B <- b + 2 * m * y
R <- A / P
D <- A / B

Qfun <- function(Q) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

Quse <- uniroot(Qfun, interval = c(0.0000001, 200), extendInt = "yes")

Q <- Quse$root

V <- Q / A

w <- y * sqrt(m ^ 2 + 1)

w1 <- NA_real_

w2 <- NA_real_

m1 <- NA_real_

m2 <- NA_real_

Z <- A * R ^ (2 / 3)

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)

Re <- (rho * R * V) / mu

# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf

# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf

# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}


# calculate the critical parameters

if(critical == "approximate") {

# critical flow area / critical top width ratio
Ac_Bc <- Q ^ 2 / g


# critical depth
ycfun <- function(yc) {Ac_Bc - ((yc * (b + m * yc)) ^ 3 / (b + 2 * m * yc))}

ycuse <- uniroot(ycfun, interval = c(0.0000001, 200), extendInt = "yes")

yc <- ycuse$root


# critical flow area
Ac <- yc * (b + m * yc)


# critical top width
Bc <- b + 2 * m * yc


# critical depth to be used in critical velocity calculation
ycV <- (yc * (b + m * yc)) / (b + 2 * m * yc)


# critical flow mean velocity
Vc <- sqrt(g * ycV)


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * (b + m * yc)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + 2 * yc * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root


# critical wetted perimeters
Pc <- b + 2 * yc * sqrt(1 + m ^ 2)


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical Froude number
Frc <- Vc / sqrt(g * yc)


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))


if(output == "list") {

return(list(Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), b = b, m = m, Sf = round_r3(Sf, d = 5), n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}


# accurate version where Froude number at critical depth point is 1
} else if (critical == "accurate") {

# critical depth
yc <- critical_depth(Q, yopt = y, g, B = b, SS = Sf)


# critical flow mean velocity
Vc <- sqrt(g * yc)


# critical Froude number
Frc <- Vc / sqrt(g * yc)

Frc


# critical flow area
Ac <- yc * (b + m * yc)


# critical top width
Bc <- b + 2 * m * yc


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * (b + m * yc)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + 2 * yc * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + 2 * yc * sqrt(1 + m ^ 2)


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))


if(output == "list") {

return(list(Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), b = b, m = m, Sf = round_r3(Sf, d = 5), n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}
}



} else if (missing(n)) {

A <- y * (b + m * y)
P <- b + 2 * y * sqrt(1 + m ^ 2)
B <- b + 2 * m * y
R <- A / P
D <- A / B

nfun <- function(n) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

nuse <- uniroot(nfun, interval = c(0.0000001, 200), extendInt = "yes")

n <- nuse$root

V <- Q / A

w <- y * sqrt(m ^ 2 + 1)

w1 <- NA_real_

w2 <- NA_real_

m1 <- NA_real_

m2 <- NA_real_

Z <- A * R ^ (2 / 3)

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}


# calculate the critical parameters

if(critical == "approximate") {

# critical flow area / critical top width ratio
Ac_Bc <- Q ^ 2 / g


# critical depth
ycfun <- function(yc) {Ac_Bc - ((yc * (b + m * yc)) ^ 3 / (b + 2 * m * yc))}

ycuse <- uniroot(ycfun, interval = c(0.0000001, 200), extendInt = "yes")

yc <- ycuse$root


# critical flow area
Ac <- yc * (b + m * yc)


# critical top width
Bc <- b + 2 * m * yc


# critical depth to be used in critical velocity calculation
ycV <- (yc * (b + m * yc)) / (b + 2 * m * yc)


# critical flow mean velocity
Vc <- sqrt(g * ycV)


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * (b + m * yc)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + 2 * yc * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + 2 * yc * sqrt(1 + m ^ 2)


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical Froude number
Frc <- Vc / sqrt(g * yc)


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))


if(output == "list") {

return(list(n = n, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), b = b, m = m, Sf = round_r3(Sf, d = 5), A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}



# accurate version where Froude number at critical depth point is 1
} else if (critical == "accurate") {

# critical depth
yc <- critical_depth(Q, yopt = y, g, B = b, SS = Sf)


# critical flow mean velocity
Vc <- sqrt(g * yc)


# critical Froude number
Frc <- Vc / sqrt(g * yc)

Frc


# critical flow area
Ac <- yc * (b + m * yc)


# critical top width
Bc <- b + 2 * m * yc


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * (b + m * yc)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + 2 * yc * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + 2 * yc * sqrt(1 + m ^ 2)


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(n = n, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), b = b, m = m, Sf = round_r3(Sf, d = 5), A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}
}


} else if (missing(m)) {

mfun <- function(m) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf) * (k / n)) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

muse <- uniroot(mfun, interval = c(0, 30), extendInt = "yes")

m <- muse$root

A <- y * (b + m * y)
P <- b + 2 * y * sqrt(1 + m ^ 2)
B <- b + 2 * m * y
R <- A / P
D <- A / B

V <- Q / A

w <- y * sqrt(m ^ 2 + 1)

w1 <- NA_real_

w2 <- NA_real_

m1 <- NA_real_

m2 <- NA_real_

Z <- A * R ^ (2 / 3)

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}


# calculate the critical parameters

if(critical == "approximate") {

# critical flow area / critical top width ratio
Ac_Bc <- Q ^ 2 / g


# critical depth
ycfun <- function(yc) {Ac_Bc - ((yc * (b + m * yc)) ^ 3 / (b + 2 * m * yc))}

ycuse <- uniroot(ycfun, interval = c(0.0000001, 200), extendInt = "yes")

yc <- ycuse$root


# critical flow area
Ac <- yc * (b + m * yc)


# critical top width
Bc <- b + 2 * m * yc


# critical depth to be used in critical velocity calculation
ycV <- (yc * (b + m * yc)) / (b + 2 * m * yc)


# critical flow mean velocity
Vc <- sqrt(g * ycV)


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * (b + m * yc)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + 2 * yc * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + 2 * yc * sqrt(1 + m ^ 2)


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical Froude number
Frc <- Vc / sqrt(g * yc)


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(m = m, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), b = b, Sf = round_r3(Sf, d = 5), n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}


# accurate version where Froude number at critical depth point is 1
} else if (critical == "accurate") {

# critical depth
yc <- critical_depth(Q, yopt = y, g, B = b, SS = Sf)


# critical flow mean velocity
Vc <- sqrt(g * yc)


# critical Froude number
Frc <- Vc / sqrt(g * yc)

Frc


# critical flow area
Ac <- yc * (b + m * yc)


# critical top width
Bc <- b + 2 * m * yc


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * (b + m * yc)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + 2 * yc * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + 2 * yc * sqrt(1 + m ^ 2)


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(m = m, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), b = b, Sf = round_r3(Sf, d = 5), n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}
}


} else if (missing(b)) {

bfun <- function(b) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

buse <- uniroot(bfun, interval = c(0.0000001, 200), extendInt = "yes")

b <- buse$root

A <- y * (b + m * y)
P <- b + 2 * y * sqrt(1 + m ^ 2)
B <- b + 2 * m * y
R <- A / P
D <- A / B

V <- Q / A

w <- y * sqrt(m ^ 2 + 1)

w1 <- NA_real_

w2 <- NA_real_

m1 <- NA_real_

m2 <- NA_real_

Z <- A * R ^ (2 / 3)

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}


# calculate the critical parameters

if(critical == "approximate") {

# critical flow area / critical top width ratio
Ac_Bc <- Q ^ 2 / g


# critical depth
ycfun <- function(yc) {Ac_Bc - ((yc * (b + m * yc)) ^ 3 / (b + 2 * m * yc))}

ycuse <- uniroot(ycfun, interval = c(0.0000001, 200), extendInt = "yes")

yc <- ycuse$root


# critical flow area
Ac <- yc * (b + m * yc)


# critical top width
Bc <- b + 2 * m * yc


# critical depth to be used in critical velocity calculation
ycV <- (yc * (b + m * yc)) / (b + 2 * m * yc)


# critical flow mean velocity
Vc <- sqrt(g * ycV)


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * (b + m * yc)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + 2 * yc * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + 2 * yc * sqrt(1 + m ^ 2)


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical Froude number
Frc <- Vc / sqrt(g * yc)


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(b = b, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), m = m, Sf = round_r3(Sf, d = 5), n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}


# accurate version where Froude number at critical depth point is 1
} else if (critical == "accurate") {

# critical depth
yc <- critical_depth(Q, yopt = y, g, B = b, SS = Sf)


# critical flow mean velocity
Vc <- sqrt(g * yc)


# critical Froude number
Frc <- Vc / sqrt(g * yc)

Frc


# critical flow area
Ac <- yc * (b + m * yc)


# critical top width
Bc <- b + 2 * m * yc


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * (b + m * yc)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + 2 * yc * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + 2 * yc * sqrt(1 + m ^ 2)


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(b = b, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), m = m, Sf = round_r3(Sf, d = 5), n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}
}



} else if (missing(y)) {

yfun <- function(y) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

yuse <- uniroot(yfun, interval = c(0.0000001, 200), extendInt = "yes")

y <- yuse$root

A <- y * (b + m * y)
P <- b + 2 * y * sqrt(1 + m ^ 2)
B <- b + 2 * m * y
R <- A / P
D <- A / B

V <- Q / A

w <- y * sqrt(m ^ 2 + 1)

w1 <- NA_real_

w2 <- NA_real_

m1 <- NA_real_

m2 <- NA_real_

Z <- A * R ^ (2 / 3)

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}


# calculate the critical parameters

if(critical == "approximate") {

# critical flow area / critical top width ratio
Ac_Bc <- Q ^ 2 / g


# critical depth
ycfun <- function(yc) {Ac_Bc - ((yc * (b + m * yc)) ^ 3 / (b + 2 * m * yc))}

ycuse <- uniroot(ycfun, interval = c(0.0000001, 200), extendInt = "yes")

yc <- ycuse$root


# critical flow area
Ac <- yc * (b + m * yc)


# critical top width
Bc <- b + 2 * m * yc


# critical depth to be used in critical velocity calculation
ycV <- (yc * (b + m * yc)) / (b + 2 * m * yc)


# critical flow mean velocity
Vc <- sqrt(g * ycV)


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * (b + m * yc)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + 2 * yc * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + 2 * yc * sqrt(1 + m ^ 2)


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical Froude number
Frc <- Vc / sqrt(g * yc)


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(y = y, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), b = b, m = m, Sf = round_r3(Sf, d = 5), n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}


# accurate version where Froude number at critical depth point is 1
} else if (critical == "accurate") {

# critical depth
yc <- critical_depth(Q, yopt = y, g, B = b, SS = Sf)


# critical flow mean velocity
Vc <- sqrt(g * yc)


# critical Froude number
Frc <- Vc / sqrt(g * yc)

Frc


# critical flow area
Ac <- yc * (b + m * yc)


# critical top width
Bc <- b + 2 * m * yc


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * (b + m * yc)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + 2 * yc * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + 2 * yc * sqrt(1 + m ^ 2)


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(y = y, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), b = b, m = m, Sf = round_r3(Sf, d = 5), n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}
}


} else if (missing(Sf)) {

A <- y * (b + m * y)
P <- b + 2 * y * sqrt(1 + m ^ 2)
B <- b + 2 * m * y
R <- A / P
D <- A / B

Sffun <- function(Sf) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

Sfuse <- uniroot(Sffun, interval = c(0.0000001, 200), extendInt = "yes")

Sf <- Sfuse$root

V <- Q / A

w <- y * sqrt(m ^ 2 + 1)

w1 <- NA_real_

w2 <- NA_real_

m1 <- NA_real_

m2 <- NA_real_

Z <- A * R ^ (2 / 3)

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}


# calculate the critical parameters

if(critical == "approximate") {

# critical flow area / critical top width ratio
Ac_Bc <- Q ^ 2 / g


# critical depth
ycfun <- function(yc) {Ac_Bc - ((yc * (b + m * yc)) ^ 3 / (b + 2 * m * yc))}

ycuse <- uniroot(ycfun, interval = c(0.0000001, 200), extendInt = "yes")

yc <- ycuse$root


# critical flow area
Ac <- yc * (b + m * yc)


# critical top width
Bc <- b + 2 * m * yc


# critical depth to be used in critical velocity calculation
ycV <- (yc * (b + m * yc)) / (b + 2 * m * yc)


# critical flow mean velocity
Vc <- sqrt(g * ycV)


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * (b + m * yc)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + 2 * yc * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + 2 * yc * sqrt(1 + m ^ 2)


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical Froude number
Frc <- Vc / sqrt(g * yc)


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(Sf = Sf, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), b = b, m = m, n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}



# accurate version where Froude number at critical depth point is 1
} else if (critical == "accurate") {

# critical depth
yc <- critical_depth(Q, yopt = y, g, B = b, SS = Sf)


# critical flow mean velocity
Vc <- sqrt(g * yc)


# critical Froude number
Frc <- Vc / sqrt(g * yc)

Frc


# critical flow area
Ac <- yc * (b + m * yc)


# critical top width
Bc <- b + 2 * m * yc


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * (b + m * yc)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + 2 * yc * sqrt(1 + m ^ 2)) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + 2 * yc * sqrt(1 + m ^ 2)


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(Sf = Sf, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), b = b, m = m, n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}
}
}



# non-symmetric case

} else if (type == "non-symmetrical") {

checks2 <- c(Q, n, m1, m2, Sf, y, b)

assert_that(qtest(checks2, "N==6"), msg = "There are more than or less than 6 known variables (Q, n, m1, m2, Sf, y, and/or b). Please try again.")
# only process with enough known variables and provide an error message if the check fails



if (missing(Q)) {

A <- y * (b + 0.5 * (m1 + m2) * y)
P <- b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))
B <- b + y * (m1 + m2)
R <- A / P
D <- A / B

Qfun <- function(Q) {Q - (((y * (b + 0.5 * (m1 + m2) * y) ) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

Quse <- uniroot(Qfun, interval = c(0.0000001, 200), extendInt = "yes")

Q <- Quse$root

V <- Q / A

w1 <- y * sqrt(m1 ^ 2 + 1)

w2 <- y * sqrt(m2 ^ 2 + 1)

w <- NA_real_

m <- NA_real_

Z <- ((b + (m1 + m2) * y) * y) ^ 1.5 / (sqrt(b + 2 * m1 * y) + sqrt(b + 2 * m2 * y))

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}


# calculate the critical parameters

if(critical == "approximate") {

# critical flow area / critical top width ratio
Ac_Bc <- Q ^ 2 / g


# critical depth
ycfun <- function(yc) {Ac_Bc - ((yc * (b + 0.5 * (m1 + m2) * yc)) ^ 3 / b + yc * (m1 + m2))}

ycuse <- uniroot(ycfun, interval = c(0.0000001, 200), extendInt = "yes")

yc <- ycuse$root


# critical flow area
Ac <- yc * (b + 0.5 * (m1 + m2) * yc)


# critical top width
Bc <- b + yc * (m1 + m2)


# critical depth to be used in critical velocity calculation
ycV <- (yc * b + yc * (m1 + m2)) / (b + yc * (m1 + m2))


# critical flow mean velocity
Vc <- sqrt(g * ycV)


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * b + yc * (m1 + m2)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical Froude number
Frc <- Vc / sqrt(g * yc)


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), b = b, m = m, m1 = m1, m2 = m2, Sf = round_r3(Sf, d = 5), n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}



# accurate version where Froude number at critical depth point is 1
} else if (critical == "accurate") {

# critical depth
yc <- critical_depth(Q, yopt = y, g, B = b, SS = Sf)


# critical flow mean velocity
Vc <- sqrt(g * yc)


# critical Froude number
Frc <- Vc / sqrt(g * yc)

Frc


# critical flow area
Ac <- yc * b + yc * (m1 + m2)


# critical top width
Bc <- b + yc * (m1 + m2)


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * b + yc * (m1 + m2)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), b = b, m = m, m1 = m1, m2 = m2, Sf = round_r3(Sf, d = 5), n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}
}


} else if (missing(n)) {

A <- y * (b + 0.5 * (m1 + m2) * y)
P <- b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))
B <- b + y * (m1 + m2)
R <- A / P
D <- A / B

nfun <- function(n) {Q - (((y * (b + 0.5 * (m1 + m2) * y) ) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

nuse <- uniroot(nfun, interval = c(0.0000001, 200), extendInt = "yes")

n <- nuse$root

V <- Q / A

w1 <- y * sqrt(m1 ^ 2 + 1)

w2 <- y * sqrt(m2 ^ 2 + 1)

w <- NA_real_

m <- NA_real_

Z <- ((b + (m1 + m2) * y) * y) ^ 1.5 / (sqrt(b + 2 * m1 * y) + sqrt(b + 2 * m2 * y))

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}


# calculate the critical parameters

if(critical == "approximate") {

# critical flow area / critical top width ratio
Ac_Bc <- Q ^ 2 / g


# critical depth
ycfun <- function(yc) {Ac_Bc - ((yc * b + yc * (m1 + m2)) ^ 3 / (b + yc * (m1 + m2)))}

ycuse <- uniroot(ycfun, interval = c(0.0000001, 200), extendInt = "yes")

yc <- ycuse$root


# critical flow area
Ac <- yc * b + yc * (m1 + m2)


# critical top width
Bc <- b + yc * (m1 + m2)


# critical depth to be used in critical velocity calculation
ycV <- (yc * b + yc * (m1 + m2)) / (b + yc * (m1 + m2))


# critical flow mean velocity
Vc <- sqrt(g * ycV)


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * b + yc * (m1 + m2)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical Froude number
Frc <- Vc / sqrt(g * yc)


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(n = n, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), b = b, m = m, m1 = m1, m2 = m2, Sf = round_r3(Sf, d = 5), A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}



# accurate version where Froude number at critical depth point is 1
} else if (critical == "accurate") {

# critical depth
yc <- critical_depth(Q, yopt = y, g, B = b, SS = Sf)


# critical flow mean velocity
Vc <- sqrt(g * yc)


# critical Froude number
Frc <- Vc / sqrt(g * yc)

Frc


# critical flow area
Ac <- yc * b + yc * (m1 + m2)


# critical top width
Bc <- b + yc * (m1 + m2)


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * b + yc * (m1 + m2)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(n = n, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), b = b, m = m, m1 = m1, m2 = m2, Sf = round_r3(Sf, d = 5), A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}
}



} else if (missing(m1)) {

m1fun <- function(m1) {Q - (((y * (b + 0.5 * (m1 + m2) * y) ) ^ (5 / 3) * sqrt(Sf) * (k / n)) / ((b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

m1use <- uniroot(m1fun, interval = c(0, 30), extendInt = "yes")

m1 <- m1use$root

A <- y * (b + 0.5 * (m1 + m2) * y)
P <- b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))
B <- b + y * (m1 + m2)
R <- A / P
D <- A / B

V <- Q / A

w1 <- y * sqrt(m1 ^ 2 + 1)

w2 <- y * sqrt(m2 ^ 2 + 1)

w <- NA_real_

m <- NA_real_

Z <- ((b + (m1 + m2) * y) * y) ^ 1.5 / (sqrt(b + 2 * m1 * y) + sqrt(b + 2 * m2 * y))

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}


# calculate the critical parameters

if(critical == "approximate") {

# critical flow area / critical top width ratio
Ac_Bc <- Q ^ 2 / g


# critical depth
ycfun <- function(yc) {Ac_Bc - ((yc * b + yc * (m1 + m2)) ^ 3 / (b + yc * (m1 + m2)))}

ycuse <- uniroot(ycfun, interval = c(0.0000001, 200), extendInt = "yes")

yc <- ycuse$root


# critical flow area
Ac <- yc * b + yc * (m1 + m2)


# critical top width
Bc <- b + yc * (m1 + m2)


# critical depth to be used in critical velocity calculation
ycV <- (yc * b + yc * (m1 + m2)) / (b + yc * (m1 + m2))


# critical flow mean velocity
Vc <- sqrt(g * ycV)


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * b + yc * (m1 + m2)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical Froude number
Frc <- Vc / sqrt(g * yc)


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(m1 = m1, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), b = b, m = m, m2 = m2, Sf = round_r3(Sf, d = 5), n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}



# accurate version where Froude number at critical depth point is 1
} else if (critical == "accurate") {

# critical depth
yc <- critical_depth(Q, yopt = y, g, B = b, SS = Sf)


# critical flow mean velocity
Vc <- sqrt(g * yc)


# critical Froude number
Frc <- Vc / sqrt(g * yc)

Frc


# critical flow area
Ac <- yc * b + yc * (m1 + m2)


# critical top width
Bc <- b + yc * (m1 + m2)


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * b + yc * (m1 + m2)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(m1 = m1, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), b = b, m = m, m2 = m2, Sf = round_r3(Sf, d = 5), n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}
}



} else if (missing(m2)) {

m2fun <- function(m2) {Q - (((y * (b + 0.5 * (m1 + m2) * y) ) ^ (5 / 3) * sqrt(Sf) * (k / n)) / ((b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

m2use <- uniroot(m2fun, interval = c(0, 30), extendInt = "yes")

m2 <- m2use$root

A <- y * (b + 0.5 * (m1 + m2) * y)
P <- b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))
B <- b + y * (m1 + m2)
R <- A / P
D <- A / B

V <- Q / A

w1 <- y * sqrt(m1 ^ 2 + 1)

w2 <- y * sqrt(m2 ^ 2 + 1)

w <- NA_real_

m <- NA_real_

Z <- ((b + (m1 + m2) * y) * y) ^ 1.5 / (sqrt(b + 2 * m1 * y) + sqrt(b + 2 * m2 * y))

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}


# calculate the critical parameters

if(critical == "approximate") {

# critical flow area / critical top width ratio
Ac_Bc <- Q ^ 2 / g


# critical depth
ycfun <- function(yc) {Ac_Bc - ((yc * b + yc * (m1 + m2)) ^ 3 / (b + yc * (m1 + m2)))}

ycuse <- uniroot(ycfun, interval = c(0.0000001, 200), extendInt = "yes")

yc <- ycuse$root


# critical flow area
Ac <- yc * b + yc * (m1 + m2)


# critical top width
Bc <- b + yc * (m1 + m2)


# critical depth to be used in critical velocity calculation
ycV <- (yc * b + yc * (m1 + m2)) / (b + yc * (m1 + m2))


# critical flow mean velocity
Vc <- sqrt(g * ycV)


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * b + yc * (m1 + m2)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical Froude number
Frc <- Vc / sqrt(g * yc)


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(m2 = m2, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), b = b, m = m, m1 = m1, Sf = round_r3(Sf, d = 5), n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}



# accurate version where Froude number at critical depth point is 1
} else if (critical == "accurate") {

# critical depth
yc <- critical_depth(Q, yopt = y, g, B = b, SS = Sf)


# critical flow mean velocity
Vc <- sqrt(g * yc)


# critical Froude number
Frc <- Vc / sqrt(g * yc)

Frc


# critical flow area
Ac <- yc * b + yc * (m1 + m2)


# critical top width
Bc <- b + yc * (m1 + m2)


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * b + yc * (m1 + m2)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(m2 = m2, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), b = b, m = m, m1 = m1, Sf = round_r3(Sf, d = 5), n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}
}



} else if (missing(b)) {

bfun <- function(b) {Q - (((y * (b + 0.5 * (m1 + m2) * y) ) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

buse <- uniroot(bfun, interval = c(0.0000001, 200), extendInt = "yes")

b <- buse$root

A <- y * (b + 0.5 * (m1 + m2) * y)
P <- b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))
B <- b + y * (m1 + m2)
R <- A / P
D <- A / B

V <- Q / A

w1 <- y * sqrt(m1 ^ 2 + 1)

w2 <- y * sqrt(m2 ^ 2 + 1)

w <- NA_real_

m <- NA_real_

Z <- ((b + (m1 + m2) * y) * y) ^ 1.5 / (sqrt(b + 2 * m1 * y) + sqrt(b + 2 * m2 * y))

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}


# calculate the critical parameters

if(critical == "approximate") {

# critical flow area / critical top width ratio
Ac_Bc <- Q ^ 2 / g


# critical depth
ycfun <- function(yc) {Ac_Bc - ((yc * b + yc * (m1 + m2)) ^ 3 / (b + yc * (m1 + m2)))}

ycuse <- uniroot(ycfun, interval = c(0.0000001, 200), extendInt = "yes")

yc <- ycuse$root


# critical flow area
Ac <- yc * b + yc * (m1 + m2)


# critical top width
Bc <- b + yc * (m1 + m2)


# critical depth to be used in critical velocity calculation
ycV <- (yc * b + yc * (m1 + m2)) / (b + yc * (m1 + m2))


# critical flow mean velocity
Vc <- sqrt(g * ycV)


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * b + yc * (m1 + m2)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical Froude number
Frc <- Vc / sqrt(g * yc)


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(b = b, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), m = m, m1 = m1, m2 = m2, Sf = round_r3(Sf, d = 5), n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}



# accurate version where Froude number at critical depth point is 1
} else if (critical == "accurate") {

# critical depth
yc <- critical_depth(Q, yopt = y, g, B = b, SS = Sf)


# critical flow mean velocity
Vc <- sqrt(g * yc)


# critical Froude number
Frc <- Vc / sqrt(g * yc)

Frc


# critical flow area
Ac <- yc * b + yc * (m1 + m2)


# critical top width
Bc <- b + yc * (m1 + m2)


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * b + yc * (m1 + m2)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(b = b, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), m = m, m1 = m1, m2 = m2, Sf = round_r3(Sf, d = 5), n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}
}



} else if (missing(y)) {

yfun <- function(y) {Q - (((y * (b + 0.5 * (m1 + m2) * y) ) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

yuse <- uniroot(yfun, interval = c(0.0000001, 200), extendInt = "yes")

y <- yuse$root

A <- y * (b + 0.5 * (m1 + m2) * y)
P <- b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))
B <- b + y * (m1 + m2)
R <- A / P
D <- A / B

V <- Q / A

w1 <- y * sqrt(m1 ^ 2 + 1)

w2 <- y * sqrt(m2 ^ 2 + 1)

w <- NA_real_

m <- NA_real_

Z <- ((b + (m1 + m2) * y) * y) ^ 1.5 / (sqrt(b + 2 * m1 * y) + sqrt(b + 2 * m2 * y))

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}


# calculate the critical parameters

if(critical == "approximate") {

# critical flow area / critical top width ratio
Ac_Bc <- Q ^ 2 / g


# critical depth
ycfun <- function(yc) {Ac_Bc - ((yc * b + yc * (m1 + m2)) ^ 3 / (b + yc * (m1 + m2)))}

ycuse <- uniroot(ycfun, interval = c(0.0000001, 200), extendInt = "yes")

yc <- ycuse$root


# critical flow area
Ac <- yc * b + yc * (m1 + m2)


# critical top width
Bc <- b + yc * (m1 + m2)


# critical depth to be used in critical velocity calculation
ycV <- (yc * b + yc * (m1 + m2)) / (b + yc * (m1 + m2))


# critical flow mean velocity
Vc <- sqrt(g * ycV)


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * b + yc * (m1 + m2)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical Froude number
Frc <- Vc / sqrt(g * yc)


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))




if(output == "list") {

return(list(y = y, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), b = b, m = m, m1 = m1, m2 = m2, Sf = round_r3(Sf, d = 5), n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}



# accurate version where Froude number at critical depth point is 1
} else if (critical == "accurate") {

# critical depth
yc <- critical_depth(Q, yopt = y, g, B = b, SS = Sf)


# critical flow mean velocity
Vc <- sqrt(g * yc)


# critical Froude number
Frc <- Vc / sqrt(g * yc)

Frc


# critical flow area
Ac <- yc * b + yc * (m1 + m2)


# critical top width
Bc <- b + yc * (m1 + m2)


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * b + yc * (m1 + m2)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(b = b, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), m = m, m1 = m1, m2 = m2, Sf = round_r3(Sf, d = 5), n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}
}


} else if (missing(Sf)) {

A <- y * (b + 0.5 * (m1 + m2) * y)
P <- b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))
B <- b + y * (m1 + m2)
R <- A / P
D <- A / B

Sffun <- function(Sf) {Q - (((y * (b + 0.5 * (m1 + m2) * y) ) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + y * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

Sfuse <- uniroot(Sffun, interval = c(0.0000001, 200), extendInt = "yes")

Sf <- Sfuse$root

V <- Q / A

w1 <- y * sqrt(m1 ^ 2 + 1)

w2 <- y * sqrt(m2 ^ 2 + 1)

w <- NA_real_

m <- NA_real_

Z <- ((b + (m1 + m2) * y) * y) ^ 1.5 / (sqrt(b + 2 * m1 * y) + sqrt(b + 2 * m2 * y))

E <- y + ((Q ^ 2) / (2 * g * A ^ 2))

Vel_Head <- (V ^ 2) / (2 * g)


# average shear stress [mean boundary shear stress]
tau0 <- gamma * R * Sf


# maximum shear stress [shear stress in channel at maximum depth]
taud <- gamma * y * Sf


# conveyance
K <- (k / n) * (A * R ^ (2 / 3))


Re <- (rho * R * V) / mu

if (Re > 2000) {

cat("\nFlow IS in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is acceptable to use.\n\n")

} else {

cat("\nFlow is NOT in the rough turbulent zone so the Gauckler-Manning-Strickler equation\n is not acceptable to use.\n\n")

}

Fr <- V / (sqrt(g * D))

if (Fr %==% 1) {

cat("\nThis is critical flow.\n\n")

} else if (Fr < 1) {

cat("\nThis is subcritical flow.\n\n")

} else if (Fr > 1) {

cat("\nThis is supercritical flow.\n\n")

}


# calculate the critical parameters

if(critical == "approximate") {

# critical flow area / critical top width ratio
Ac_Bc <- Q ^ 2 / g


# critical depth
ycfun <- function(yc) {Ac_Bc - ((yc * b + yc * (m1 + m2)) ^ 3 / (b + yc * (m1 + m2)))}

ycuse <- uniroot(ycfun, interval = c(0.0000001, 200), extendInt = "yes")

yc <- ycuse$root


# critical flow area
Ac <- yc * b + yc * (m1 + m2)


# critical top width
Bc <- b + yc * (m1 + m2)


# critical depth to be used in critical velocity calculation
ycV <- (yc * b + yc * (m1 + m2)) / (b + yc * (m1 + m2))


# critical flow mean velocity
Vc <- sqrt(g * ycV)


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * b + yc * (m1 + m2)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical Froude number
Frc <- Vc / sqrt(g * yc)


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))



if(output == "list") {

return(list(Sf = Sf, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), b = b, m = m, m1 = m1, m2 = m2, n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table
}



# accurate version where Froude number at critical depth point is 1
} else if (critical == "accurate") {

# critical depth
yc <- critical_depth(Q, yopt = y, g, B = b, SS = Sf)


# critical flow mean velocity
Vc <- sqrt(g * yc)


# critical Froude number
Frc <- Vc / sqrt(g * yc)


# critical flow area
Ac <- yc * b + yc * (m1 + m2)


# critical top width
Bc <- b + yc * (m1 + m2)


# critical slope
Sfcfun <- function(Sfc) {Q - (((yc * b + yc * (m1 + m2)) ^ (5 / 3) * sqrt(Sfc)) * (k / n) / ((b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))) ^ (2 / 3)))}

Sfcuse <- uniroot(Sfcfun, interval = c(0.0000001, 200), extendInt = "yes")

Sfc <- Sfcuse$root


# critical discharge
Qcfun <- function(Qc) {(Qc ^ 2 * B) / (g * A ^ 3) - 1}

Qcuse <- uniroot(Qcfun, interval = c(0.0000001, 200), extendInt = "yes")

Qc <- Qcuse$root



# critical wetted perimeters
Pc <- b + yc * (sqrt(m1 ^ 2 + 1) + sqrt(m2 ^ 2 + 1))


# critical hydraulic radius
Rc <- Ac / Pc


# critical hydraulic depth
Dc <- Ac / Bc


# critical section factor
Zc <- A * sqrt(A / B)


# critical Energy (minimum Energy)
Ec <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))




if(output == "list") {

return(list(Sf = Sf, Q = round_r3(Q, d = 3), V = round_r3(V, d = 3), y = round_r3(y, d = 3), b = b, m = m, m1 = m1, m2 = m2, n = n, A = round_r3(A, d = 3), P = round_r3(P, d = 3), R = round_r3(R, d = 3), B = round_r3(B, d = 3), D = round_r3(D, d = 3), w = round_r3(w, d = 3), Z = round_r3(Z, d = 3), E = round_r3(E, d = 3), K = round_r3(K, d = 3), Vel_Head = round_r3(Vel_Head, d = 3), Re = round_r3(Re, d = 0), Fr = round_r3(Fr, d = 3), taud = round_r3(taud, d = 3), tau0 = round_r3(tau0, d = 3), yc = round_r3(yc, d = 3), Ac = round_r3(Ac, d = 3), Pc = round_r3(Pc, d = 3), Bc = round_r3(Bc, d = 3), Rc = round_r3(Rc, d = 3), Dc = round_r3(Dc, d = 3), Vc = round_r3(Vc, d = 3), Qc = round_r3(Qc, d = 3), Sfc = round_r3(Sfc, d = 5), Frc = round_r3(Frc, d = 3), Zc = round_r3(Zc, d = 3), Ec = round_r3(Ec, d = 3)))

} else if (output == "data.table") {

# create data.table for displaying the results
result_table <- data.table(Parameters = c("Flow depth (y)", "Flow area (A)", "Wetted Perimeters (P)", "Top Width (B)", "Bottom width (b)", "Hydraulic Radius (R)", "Hydraulic Depth (D)", "Flow Mean Velocity (V)", "Flow Discharge (Q)", "Manning's roughness coefficient (n)", "Slope (Sf)", "Temperature", "Absolute Temperature", "Saturated Liquid Density", "Absolute or Dynamic Viscosity", "Kinematic Viscosity", "Froude number (Fr)", "Reynolds number (Re)", "symmetric side slope (m)", "non-symmetric side slope (m1)", "non-symmetric side slope (m2)", "Wetted Length (w)", "Wetted Length for a non-symmetric trapezoid (w1)", "Wetted Length for a non-symmetric trapezoid (w2)", "Section Factor (Z)", "conveyance (K)", "Specific Energy (E)", "Velocity Head (Vel_Head)", "Maximum Shear Stress (taud)", "Average Shear Stress (tau0)"), NormalValue = list(round_r3(y, d = 3), round_r3(A, d = 3), round_r3(P, d = 3), round_r3(B, d = 3), round_r3(b, d = 3), round_r3(R, d = 3), round_r3(D, d = 3), round_r3(V, d = 3), round_r3(Q, d = 3), round_r3(n, d = 3), round_r3(Sf, d = 3), round_r3(Temp, d = 0), round_r3(drop_units(T_K), d = 2), round_r3(rho, d = 3), mu, nu, round_r3(Fr, d = 3), round_r3(Re, d = 0), m = m, m1 = m1, m2 = m2, round_r3(w, d = 3), round_r3(w1, d = 3), round_r3(w2, d = 3), round_r3(Z, d = 3), round_r3(K, d = 3), round_r3(E, d = 3), round_r3(Vel_Head, d = 3), round_r3(taud, d = 3), round_r3(tau0, d = 3)), CriticalValue = list(round_r3(yc, d = 3), round_r3(Ac, d = 3), round_r3(Pc, d = 3), round_r3(Bc, d = 3), NA_real_, round_r3(Rc, d = 3), round_r3(Dc, d = 3), round_r3(Vc, d = 3), round_r3(Qc, d = 3), NA_real_, round_r3(Sfc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Frc, d = 3), NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, round_r3(Zc, d = 3), NA_real_, round_r3(Ec, d = 3), NA_real_, NA_real_, NA_real_), Units = result_units)
setnames(result_table, c("Parameters", "Normal Value", "Critical Value", "Units"))

col.names <- c("Parameters", "Normal Value", "Critical Value", "Units")


# code block below modified from data.table function
setattr(result_table, "col.names", setnames(result_table, col.names))
setattr(result_table, "class", c("data.table", "data.frame"))
result_table

}
}
}
}
}
