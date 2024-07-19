#' Saturation Vapor Pressure for Water
#'
#' This function solves for the saturation vapor pressure of water using only
#' the temperature of the water in either units of degrees Celsius, degrees
#' Fahrenheit, or Kelvin.
#'
#'
#'
#'
#' The simplified equation is expressed as
#'
#' \deqn{P_s = \exp{\frac{34.494 - \frac{4924.99}{t + 237.1}}{(t + 105)^1.57}}}
#'
#' for (t > 0 C)
#'
#' \describe{
#'   \item{\emph{P_s}}{the saturation vapor pressure (Pa or psi)}
#'   \item{\emph{t}}{the water temperature, degrees Celsius}
#' }
#'
#'
#' @note
#' Note: Please refer to the references for the formulas (Huang = Reference 1,
#' IAPWS = Reference 2, and Buck = Reference 3)
#' 
#' Note: Please refer to the iemisc: Comparing Saturated Vapor Pressure
#' Formulas to the Reference vignette for the comparisons to the reference
#' saturated vapor pressure
#' 
#' 
#'
#' @param Temp numeric vector that contains the temperature (degrees Celsius,
#'   degrees Fahrenheit, or Kelvin)
#' @param units character vector that contains the system of units (options are
#'   \code{SI} for International System of Units, \code{Eng} for English units
#'   (United States Customary System in the United States and Imperial Units in
#'   the United Kingdom), or \code{Absolute} for Absolute Units)
#' @param formula character vector that contains the source of the formula used
#'   to compute the saturation vapor pressure (options are Huang, Buck, IAPWS)
#'
#' @return the saturation vapor pressure for water as a numeric vector. The unit
#'   for \code{SI} and \code{Absolute} is Pascal (Pa), but the unit for \code{Eng}
#'   is pounds per square inch (psi). The units are not returned.
#'
#'
#'
#' @references
#' \enumerate{
#'    \item Huang, J. (2018). "A Simple Accurate Formula for Calculating Saturation Vapor Pressure of Water and Ice", \emph{Journal of Applied Meteorology and Climatology}, 57(6), 1265-1272. Retrieved Nov 4, 2021, \url{https://web.archive.org/web/20221024040058/https://journals.ametsoc.org/view/journals/apme/57/6/jamc-d-17-0334.1.xml}. Used the Internet Archive: Wayback Machine archived version for acceptance into CRAN. Used the Internet Archive: Wayback Machine archived version for acceptance into CRAN.
#'    \item The International Association for the Properties of Water and Steam. IAPWS SR1-86(1992). "Revised Supplementary Release on Saturation Properties of Ordinary Water Substance", September 1992, \url{http://www.iapws.org/relguide/Supp-sat.pdf}
#'    \item Holger Vömel, National Center for Atmospheric Research Earth Observing Laboratory, "Saturation vapor pressure formulations", \url{https://web.archive.org/web/20170623040102/http://cires1.colorado.edu/~voemel/vp.html}. Retrieved thanks to the Internet Archive: Wayback Machine
#' }
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
#'
#'
#' @examples
#'
#' # Example 1 - Example from the hydraulics package
#'
#' library(iemisc)
#'
#' vps <- hydraulics::svp(T = 10, units = "SI"); vps
#'
#' vps2 <- sat_vapor_pressure(Temp = 10, units = "SI", formula = "Huang"); vps2
#'
#'
#'
#'
#'
#' # Example 2 - from the Huang Reference
#'
#' library(iemisc)
#'
#' sat_vapor_pressure(Temp = c(0.01, seq(from = 20, to = 100, by = 20)), units
#' = "SI", formula = "Huang")
#'
#'
#'
#'
#'
#' # Example 3 - compare with saturation_pressure_H2O from aiRthermo
#'
#' install.load::load_package("iemisc", "units")
#'
#' Temp <- 40
#'
#' # create a numeric vector with the units of degrees Celsius
#' T_C <- set_units(Temp, "degree_C")
#' T_C
#'
#' # create a numeric vector to convert from degrees Celsius to Kelvin
#' T_K <- T_C
#' T_K
#'
#' # create a numeric vector with the units of Kelvin
#' units(T_K) <- make_units(K)
#'
#' pre <- aiRthermo::saturation_pressure_H2O(drop_units(T_K))
#' pre
#'
#' sat_vapor_pressure(Temp = drop_units(T_K), units = "Absolute", formula = "Huang")
#'
#'
#'
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom units set_units make_units drop_units
#' @importFrom checkmate qtest
#'
#' @export
sat_vapor_pressure <- function (Temp, units = c("SI", "Eng", "Absolute"), formula = c("Huang", "Buck", "IAPWS")) {


psi <- K <- degree_C <- Pa <- NULL
# due to NSE notes in R CMD check


units <- units

formula <- formula


# Check
assert_that(qtest(units, "S==1"), msg = "There is not an units type or more than 1 units type. Please specify either 'SI', 'Eng', or 'Absolute'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("SI", "Eng", "Absolute") %in% units)), msg = "The units system has not been identified correctly as either 'SI', 'Eng', or 'Absolute'. Please try again.")
# only process with a specified unit and provide a stop warning if not


# units
if (units == "SI") {

# use the temperature to determine the Saturation Vapor Pressure for water

if (formula == "Huang") {

assert_that(qtest(Temp, "N+(0,)"), msg = "Either Temp is less than 273.15 K / 32 F / 0 C, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures greater than 273.15 K / 32 F / 0 C. Please try again.")
# only process with specified, finite values and provide an error message if the check fails

# Saturation Vapor Pressure for water
Ps <- exp(34.494 - (4924.99 / (T + 237.1))) / ((T + 105) ^ 1.57)

return(Ps)



} else if (formula == "Buck") {

assert_that(qtest(Temp, "N+(0,)"), msg = "Either Temp is less than 273.15 K / 32 F / 0 C, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures greater than 273.15 K / 32 F / 0 C. Please try again.")
# only process with specified, finite values and provide an error message if the check fails

# Saturation Vapor Pressure for water
Ps <- 6.1121 * exp((18.678 - Temp / 234.5) * Temp / (257.14 + Temp))

# create a numeric vector with the units of hPa
Ps <- set_units(Ps, "hPa")

# create a numeric vector to convert from hPa to Pa
Ps <- Ps

# create a numeric vector with the units of Pa
units(Ps) <- make_units(Pa)

return(drop_units(Ps)) # Pa



} else if (formula == "IAPWS") {

assert_that(qtest(Temp, "N+[0.01,373.946]"), msg = "Either Temp is less than 273.16 K / 0.01 C / 32.018 F, greater than 647.096 K / 373.946 C / 705.1028 F, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures equal to or greater than 273.16 K / 0.01 C / 32.018 F & less than or equal to 647.096 K / 373.946 C / 705.1028 F. Please try again.")
# only process with specified, finite values and provide an error message if the check fails

T_C <- Temp

# create a numeric vector with the units of degrees Fahrenheit
T_C <- set_units(T_C, "degree_C")


# create a numeric vector to convert from degrees Fahrenheit to degrees Celsius
T_K <- T_C


# create a numeric vector with the units of degrees Celsius
units(T_K) <- make_units(K)


# the critical pressure of water
Pc <- 22.064 # MPa

# the critical temperature of water
Tc <- 647.096 # K

theta <- drop_units(T_K) / Tc

tau <- 1 - theta

a1 <- -7.85951783

a2 <- 1.84408259

a3 <- -11.7866497

a4 <- 22.6807411

a5 <- -15.9618719

a6 <- 1.80122502

Ps <- Pc * exp((Tc / drop_units(T_K)) * (a1 * tau + a2 * tau ^ 1.5 + a3 * tau ^ 3 + a4 * tau ^ 3.5 + a5 * tau ^ 4 + a6 * tau ^ 7.5))

# create a numeric vector with the units of MPa
Ps <- set_units(Ps, "MPa")

# create a numeric vector to convert from MPa to Pa
Ps <- Ps

# create a numeric vector with the units of Pa
units(Ps) <- make_units(Pa)

return(drop_units(Ps)) # Pa

}


} else if (units == "Eng") {

# use the temperature to determine the Saturation Vapor Pressure for water

if (formula == "Huang") {

assert_that(qtest(Temp, "N+(32,)"), msg = "Either Temp is less than 273.15 K / 32 F / 0 C, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures greater than 273.15 K / 32 F / 0 C. Please try again.")
# only process with specified, finite values and provide an error message if the check fails

T_F <- Temp

# create a numeric vector with the units of degrees Fahrenheit
T_F <- set_units(T_F, "degree_F")


# create a numeric vector to convert from degrees Fahrenheit to degrees Celsius
T_C <- T_F


# create a numeric vector with the units of degrees Celsius
units(T_C) <- make_units(degree_C)

# Saturation Vapor Pressure for water
Ps <- exp(34.494 - (4924.99 / (drop_units(T_C) + 237.1))) / ((drop_units(T_C) + 105) ^ 1.57)

# create a numeric vector with the units of Pa
Ps <- set_units(Ps, "Pa")

# create a numeric vector to convert from Pa to psi
Ps <- Ps

# create a numeric vector with the units of psi
units(Ps) <- make_units(psi)

return(drop_units(Ps)) # psi



} else if (formula == "Buck") {

assert_that(qtest(Temp, "N+(32,)"), msg = "Either Temp is less than 273.15 K / 32 F / 0 C, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures greater than 273.15 K / 32 F / 0 C. Please try again.")
# only process with specified, finite values and provide an error message if the check fails


T_F <- Temp

# create a numeric vector with the units of degrees Fahrenheit
T_F <- set_units(T_F, "degree_F")


# create a numeric vector to convert from degrees Fahrenheit to degrees Celsius
T_C <- T_F


# Saturation Vapor Pressure for water
Ps <- 6.1121 * exp((18.678 - drop_units(T_C) / 234.5) * drop_units(T_C) / (257.14 + drop_units(T_C)))

# create a numeric vector with the units of hPa
Ps <- set_units(Ps, "hPa")

# create a numeric vector to convert from hPa to Pa
Ps <- Ps

# create a numeric vector with the units of Pa
units(Ps) <- make_units(Pa)

return(drop_units(Ps)) # Pa



} else if (formula == "IAPWS") {

assert_that(qtest(Temp, "N+[32.018,705.1028]"), msg = "Either Temp is less than 273.16 K / 0.01 C / 32.018 F, greater than 647.096 K / 373.946 C / 705.1028 F, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures equal to or greater than 273.16 K / 0.01 C / 32.018 F & less than or equal to 647.096 K / 373.946 C / 705.1028 F. Please try again.")
# only process with specified, finite values and provide an error message if the check fails


T_F <- Temp

# create a numeric vector with the units of degrees Fahrenheit
T_F <- set_units(T_F, "degree_F")


# create a numeric vector to convert from degrees Fahrenheit to degrees Celsius
T_K <- T_F

# create a numeric vector with the units of degrees Celsius
units(T_K) <- make_units(K)



# the critical pressure of water
Pc <- 22.064 # MPa

# the critical temperature of water
Tc <- 647.096 # K

theta <- drop_units(T_K) / Tc

tau <- 1 - theta

a1 <- -7.85951783

a2 <- 1.84408259

a3 <- -11.7866497

a4 <- 22.6807411

a5 <- -15.9618719

a6 <- 1.80122502

Ps <- Pc * exp((Tc / drop_units(T_K)) * (a1 * tau + a2 * tau ^ 1.5 + a3 * tau ^ 3 + a4 * tau ^ 3.5 + a5 * tau ^ 4 + a6 * tau ^ 7.5))

# create a numeric vector with the units of MPa
Ps <- set_units(Ps, "MPa")

# create a numeric vector to convert from MPa to Pa
Ps <- Ps

# create a numeric vector with the units of Pa
units(Ps) <- make_units(Pa)

return(drop_units(Ps)) # Pa

}


} else if (units == "Absolute") {

# use the temperature to determine the Saturation Vapor Pressure for water


if (formula == "Huang") {

assert_that(qtest(Temp, "N+(273.15,)"), msg = "Either Temp is less than 273.15 K / 32 F / 0 C, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures greater than 273.15 K / 32 F / 0 C. Please try again.")
# only process with specified, finite values and provide an error message if the check fails

T_K <- Temp

# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)

# create a numeric vector to convert from degrees Fahrenheit to degrees Celsius
T_C <- T_K


# create a numeric vector with the units of degrees Celsius
units(T_C) <- make_units(degree_C)

# Saturation Vapor Pressure for water
Ps <- exp(34.494 - (4924.99 / (drop_units(T_C) + 237.1))) / ((drop_units(T_C) + 105) ^ 1.57)

return(Ps)



} else if (formula == "Buck") {

assert_that(qtest(Temp, "N+(273.15,)"), msg = "Either Temp is less than 273.15 K / 32 F / 0 C, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures greater than 273.15 K / 32 F / 0 C. Please try again.")
# only process with specified, finite values and provide an error message if the check fails


T_K <- Temp

# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)

# create a numeric vector to convert from degrees Fahrenheit to degrees Celsius
T_C <- T_K


# create a numeric vector with the units of degrees Celsius
units(T_C) <- make_units(degree_C)


# Saturation Vapor Pressure for water
Ps <- 6.1121 * exp((18.678 - drop_units(T_C) / 234.5) * drop_units(T_C) / (257.14 + drop_units(T_C)))

# create a numeric vector with the units of hPa
Ps <- set_units(Ps, "hPa")

# create a numeric vector to convert from hPa to Pa
Ps <- Ps

# create a numeric vector with the units of Pa
units(Ps) <- make_units(Pa)

return(drop_units(Ps)) # Pa



} else if (formula == "IAPWS") {

assert_that(qtest(Temp, "N+[273.16,647.096]"), msg = "Either Temp is less than 273.16 K / 0.01 C / 32.018 F, greater than 647.096 K / 373.946 C / 705.1028 F, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures equal to or greater than 273.16 K / 0.01 C / 32.018 F & less than or equal to 647.096 K / 373.946 C / 705.1028 F. Please try again.")
# only process with specified, finite values and provide an error message if the check fails


# the critical pressure of water
Pc <- 22.064 # MPa

# the critical temperature of water
Tc <- 647.096 # K

theta <- drop_units(T_K) / Tc

tau <- 1 - theta

a1 <- -7.85951783

a2 <- 1.84408259

a3 <- -11.7866497

a4 <- 22.6807411

a5 <- -15.9618719

a6 <- 1.80122502

Ps <- Pc * exp((Tc / drop_units(T_K)) * (a1 * tau + a2 * tau ^ 1.5 + a3 * tau ^ 3 + a4 * tau ^ 3.5 + a5 * tau ^ 4 + a6 * tau ^ 7.5))

# create a numeric vector with the units of MPa
Ps <- set_units(Ps, "MPa")

# create a numeric vector to convert from MPa to Pa
Ps <- Ps

# create a numeric vector with the units of Pa
units(Ps) <- make_units(Pa)

return(drop_units(Ps)) # Pa

}
}
}















#' Saturation Vapor Pressure for Ice
#'
#' This function solves for the saturation vapor pressure of ice using only
#' the temperature of the water in either units of degrees Celsius, degrees
#' Fahrenheit, or Kelvin.
#'
#'
#'
#'
#' The simplified equation is expressed as
#'
#' \deqn{P_s = \exp{\frac{43.494 - \frac{6545.8}{t + 278}}{(t + 868)^2}}}
#'
#' for (t <= 0 C)
#'
#' \describe{
#'   \item{\emph{P_s}}{the saturation vapor pressure (Pa or psi)}
#'   \item{\emph{t}}{the ice temperature, degrees Celsius}
#' }
#'
#'
#'
#' @param Temp numeric vector that contains the temperature (degrees Celsius,
#'   degrees Fahrenheit, or Kelvin)
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units, \code{Eng} for English units
#'   (United States Customary System in the United States and Imperial Units in
#'   the United Kingdom), or \code{Absolute} for Absolute Units]
#'
#'
#' @return the saturation vapor pressure for ice as a numeric vector. The unit
#'   for \code{SI} and \code{Absolute} is Pascal (Pa), but the unit for \code{Eng}
#'   is pounds per square inch (psi). The units are not returned.
#'
#'
#'
#' @references
#' Huang, J. (2018). "A Simple Accurate Formula for Calculating Saturation Vapor Pressure of Water and Ice", \emph{Journal of Applied Meteorology and Climatology}, 57(6), 1265-1272. Retrieved Nov 4, 2021, \url{https://web.archive.org/web/20221024040058/https://journals.ametsoc.org/view/journals/apme/57/6/jamc-d-17-0334.1.xml}. Used the Internet Archive: Wayback Machine archived version for acceptance into CRAN. Used the Internet Archive: Wayback Machine archived version for acceptance into CRAN.
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
#'
#'
#' @examples
#' # Example from the Reference
#'
#' library(iemisc)
#' 
#' sat_vapor_pressure_ice(Temp = seq(from = -100, to = 0, by = 20), units = "SI")
#' 
#' 
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom units set_units make_units drop_units
#' @importFrom checkmate qtest
#'
#' @export
sat_vapor_pressure_ice <- function (Temp, units = c("SI", "Eng", "Absolute")) {


psi <- K <- degree_C <- NULL
# due to NSE notes in R CMD check


units <- units


# Check
assert_that(qtest(units, "S==1"), msg = "There is not an units type or more than 1 units type. Please specify either 'SI', 'Eng', or 'Absolute'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("SI", "Eng", "Absolute") %in% units)), msg = "The units system has not been identified correctly as either 'SI', 'Eng', or 'Absolute'. Please try again.")
# only process with a specified unit and provide a stop warning if not


# units
if (units == "SI") {

# use the temperature to determine the Saturation Vapor Pressure for water

assert_that(qtest(Temp, "N+(,0]"), msg = "Either Temp is greater than 0 C, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures greater than 0 C / 32 F. Please try again.")
# only process with specified, finite values and provide an error message if the check fails

# Saturation Vapor Pressure for water
Ps <- exp(43.494 - (6545.8 / (T + 278))) / ((T + 868) ^ 2)

return(Ps)


} else if (units == "Eng") {

# use the temperature to determine the Saturation Vapor Pressure for water

assert_that(qtest(Temp, "N+(,32]"), msg = "Either Temp is greater than 32 F, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures greater than 32 F / 0 C. Please try again.")
# only process with specified, finite values and provide an error message if the check fails

T_F <- Temp

# create a numeric vector with the units of degrees Fahrenheit
T_F <- set_units(T_F, "degree_F")


# create a numeric vector to convert from degrees Fahrenheit to degrees Celsius
T_C <- T_F


# create a numeric vector with the units of degrees Celsius
units(T_C) <- make_units(degree_C)

# Saturation Vapor Pressure for water
Ps <- exp(43.494 - (6545.8 / (drop_units(T_C) + 278))) / ((drop_units(T_C) + 868) ^ 2)

# create a numeric vector with the units of Pa
Ps <- set_units(Ps, "Pa")

# create a numeric vector to convert from Pa to psi
Ps <- Ps

# create a numeric vector with the units of psi
units(Ps) <- make_units(psi)

return(drop_units(Ps)) # psi


} else if (units == "Absolute") {

# use the temperature to determine the Saturation Vapor Pressure for water

assert_that(qtest(Temp, "N+(,273.15]"), msg = "Either Temp is greater than 273.15 K, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures greater than 273.15 K / 32 F / 0 C. Please try again.")
# only process with specified, finite values and provide an error message if the check fails

T_K <- Temp

# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)

# create a numeric vector to convert from degrees Fahrenheit to degrees Celsius
T_C <- T_K


# create a numeric vector with the units of degrees Celsius
units(T_C) <- make_units(degree_C)

# Saturation Vapor Pressure for water
Ps <- exp(43.494 - (6545.8 / (drop_units(T_C) + 278))) / ((drop_units(T_C) + 868) ^ 2)

return(Ps)

}

}

















#' Density of Saturated Liquid Water
#'
#' This function solves for the density of water using only the temperature of
#' the water in either units of degrees Celsius, degrees Fahrenheit, or Kelvin.
#'
#'
#'
#' @details
#' The simplified equation is expressed as
#'
#' \deqn{\frac{\\rho^{t}}{\\rho_c} = 1 + b_1 \times \\tau^{1/3} + b_2 \times \\tau^{2/3} + b_3 \times \\tau^{5/3} + b_4 \times \\tau^{16/3} + b_5 \times \\tau^{43/3} + b_6 \times \\tau^{110/3}}
#'
#' where \deqn{\\rho_c = 322 \frac{kg}{m^3}}
#'
#' where \deqn{\\tau = 1 - \\theta}
#'
#' where \deqn{\\theta = \frac{T}{T_c}}
#'
#' where \deqn{T_c = 647.096 K}
#'
#' with
#'
#' \deqn{b_1 = 1.99274064}
#' \deqn{b_2 = 1.09965342}
#' \deqn{b_3 = -0.510839303}
#' \deqn{b_4 = -1.75493479}
#' \deqn{b_5 = -45.5170352}
#' \deqn{b_6 = -6.74694450 * 10 ^ 5}
#'
#' \describe{
#'   \item{\emph{\\rho' = rho^t in the equation}}{Water Density (mass divided by volume) [kg/m^3, slug/ft^3, or lbm/ft^3]}
#'   \item{\emph{\\rho_c}}{Water Density at the critical point, kg/m^3}
#'   \item{\emph{T}}{the water temperature, Kelvin}
#'   \item{\emph{T_c}}{the critical water temperature, Kelvin}
#' }
#'
#'
#'
#' @param Temp numeric vector that contains the temperature (degrees Celsius,
#'   degrees Fahrenheit, or Kelvin)
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units, \code{Eng} for English units
#'   (United States Customary System in the United States and Imperial Units in
#'   the United Kingdom), or \code{Absolute} for Absolute Units]
#' @param Eng_units character vector that contains the unit for the density of
#'   water [options are slug/ft^3 or lbm/ft^3]
#'
#'
#' @return the density as a numeric vector. The units are not returned.
#'
#'
#'
#' @references
#' IAPWS SR1-86 (1992). "Revised Supplementary Release on Saturation Properties of Ordinary Water Substance". September 1992, \url{http://www.iapws.org/relguide/Supp-sat.html}
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
#'
#' @note
#' Note: 1 lbf = 1 slug * 1 ft/sec^2, thus 1 slug = 1 lbf * sec^2 / 1 ft
#' (Reference 2)
#'
#' Thus, lbm/ft^3 = lbf*s^2/ft/ft^3
#'
#'
#'
#'
#' @examples
#' # Example 1 (Compare to reference standard in Reference paper)
#'
#' library(iemisc)
#' 
#' 273.16 # K
#' 
#' 373.1243 # K
#' 
#' 647.096 # K
#' 
#' Temp <- c(273.16, 373.1243, 647.096)
#' 
#' round::round_r3(density_water(Temp, units = "Absolute"), d = 3)
#' 
#' 
#' # Reference standard
#' 
#' 999.789 # kg/m^3
#' 958.365 # kg/m^3
#' 322 # kg/m^3
#' 
#' 
#'
#'
#'
#' # Example 2 - Example from the hydraulics package
#'
#' library(iemisc)
#' 
#' rho <- hydraulics::dens(T = 25, units = "SI"); rho
#' 
#' rho2 <- density_water(Temp = 25, units = "SI"); rho2
#' 
#' 
#'
#'
#'
#' # Example 3 - compare with densityH2Ov from aiRthermo
#' 
#' install.load::load_package("iemisc", "units")
#' 
#' Temp <- 180
#' 
#' # create a numeric vector with the units of degrees Celsius
#' T_C <- set_units(Temp, "degree_C")
#' T_C
#' 
#' # create a numeric vector to convert from degrees Celsius to Kelvin
#' T_K <- T_C
#' T_K
#' 
#' # create a numeric vector with the units of Kelvin
#' units(T_K) <- make_units(K)
#' 
#' pre <- aiRthermo::saturation_pressure_H2O(drop_units(T_K))
#' pre
#' 
#' rho_h2o <- aiRthermo::densityH2Ov(pre, drop_units(T_K), consts =
#' aiRthermo::export_constants()); rho_h2o
#' 
#' # Should not be the same as aiRthermo deals with water vapor rather than
#' # saturated liquid water
#' 
#' density_water(Temp = drop_units(T_K), units = "Absolute")
#' 
#' 
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom units set_units make_units drop_units
#' @importFrom checkmate qtest
#'
#' @export
density_water <- function (Temp, units = c("SI", "Eng", "Absolute"), Eng_units = c("slug/ft^3", "lbm/ft^3")) {


K <- slug <- ft <- lb <- NULL
# due to NSE notes in R CMD check


units <- units

ifelse(missing(Eng_units) & units == "SI" | units == "Absolute", Eng_units <- NA_character_, Eng_units <- Eng_units)


# Check
assert_that(qtest(units, "S==1"), msg = "There is not an units type or more than 1 units type. Please specify either 'SI', 'Eng', or 'Absolute'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("SI", "Eng", "Absolute") %in% units)), msg = "The units system has not been identified correctly as either 'SI', 'Eng', or 'Absolute'. Please try again.")
# only process with a specified unit and provide a stop warning if not

assert_that(qtest(Eng_units, "s==1"), msg = "There is not an Eng_units type or more than 1 Eng_units type. Please specify either 'slug/ft^3' or 'lbm/ft^3'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("slug/ft^3", "lbm/ft^3", NA_character_) %in% Eng_units)), msg = "The Eng_units has not been identified correctly as either 'slug/ft^3' or 'lbm/ft^3'. Please try again.")
# only process with a specified unit and provide a stop warning if not


# units
if (units == "SI") {

# use the temperature to determine the saturated liquid density for water

# create a numeric vector with the units of degrees Celsius
T_C <- set_units(Temp, "degree_C")


# create a numeric vector to convert from degrees Celsius to Kelvin
T_K <- T_C


# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)


assert_that(qtest(drop_units(T_K), "N+(273.155,647.096]"), msg = "Either Temp is less than 273.16 K / 0.01 C or greater than 647.096 K / 373.946 C, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures in the range of 273.16 K <= Temp <= 647.096 K. Please try again.")
# only process with specified, finite values and provide an error message if the check fails

#  the critical density of water
rhoc <- 322 # kg / m^3

# the critical temperature of water
Tc <- 647.096 # K

theta <- drop_units(T_K) / Tc

tau <- 1 - theta

b1 <- 1.99274064

b2 <- 1.09965342

b3 <- -0.510839303

b4 <- -1.75493479

b5 <- -45.5170352

b6 <- -6.74694450 * 10 ^ 5

# Density of Saturated Liquid Water
rhoprime <- rhoc * (1 + b1 * tau ^ (1 / 3) + b2 * tau ^ (2 / 3) + b3 * tau ^ (5 / 3) + b4 * tau ^ (16 / 3) + b5 * tau ^ (43 / 3) + b6 * tau ^ (110 / 3))

return(rhoprime) # kg / m^3


} else if (units == "Eng") {

# use the temperature to determine the saturated liquid density for water

T_F <- Temp

# create a numeric vector with the units of degrees Fahrenheit
T_F <- set_units(T_F, "degree_F")


# create a numeric vector to convert from degrees Fahrenheit to Kelvin
T_K <- T_F


# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)

assert_that(qtest(drop_units(T_K), "N+(273.155,647.096]"), msg = "Either Temp is less than 273.16 K / 32.018 F or greater than 647.096 K / 705.1028 F, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures in the range of 273.16 K <= Temp <= 647.096 K. Please try again.")
# only process with specified, finite values and provide an error message if the check fails


# the critical density of liquid water
rhoc <- 322 # kg / m^3

# the critical temperature of liquid water
Tc <- 647.096 # K

theta <- drop_units(T_K) / Tc

tau <- 1 - theta

b1 <- 1.99274064

b2 <- 1.09965342

b3 <- -0.510839303

b4 <- -1.75493479

b5 <- -45.5170352

b6 <- -6.74694450 * 10 ^ 5


# density of saturated liquid water

rhoprime <- rhoc * (1 + b1 * tau ^ (1 / 3) + b2 * tau ^ (2 / 3) + b3 * tau ^ (5 / 3) + b4 * tau ^ (16 / 3) + b5 * tau ^ (43 / 3) + b6 * tau ^ (110 / 3))

# create a numeric vector with the units of kg/m^3
rhoprime <- set_units(rhoprime, "kg/m^3")

if (Eng_units == "slug/ft^3") {

# create a numeric vector to convert from kg/m^3 to slug/ft^3
rhoprime_slug <- rhoprime

# create a numeric vector with the units of slug/ft^3
units(rhoprime_slug) <- make_units(slug/ft^3)

return(drop_units(rhoprime_slug)) # slug/ft^3


} else if (Eng_units == "lbm/ft^3") {

# create a numeric vector to convert from kg/m^3 to lbm/ft^3
rhoprime_lbm <- rhoprime

# create a numeric vector with the units of lbm/ft^3
units(rhoprime_lbm) <- make_units(lb/ft^3)

return(drop_units(rhoprime_lbm)) # lbm/ft^3

}


} else if (units == "Absolute") {

# use the temperature to determine the saturated liquid density for water

T_K <- Temp

assert_that(qtest(T_K, "N+(273.155,647.096]"), msg = "Either Temp is less than 273.16 K / 32.018 F or greater than 647.096 K / 705.1028 F, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures in the range of 273.16 K <= Temp <= 647.096 K. Please try again.")
# only process with specified, finite values and provide an error message if the check fails


#  the critical density of water
rhoc <- 322 # kg / m^3

# the critical temperature of water
Tc <- 647.096 # K

theta <- T_K / Tc

tau <- 1 - theta

b1 <- 1.99274064

b2 <- 1.09965342

b3 <- -0.510839303

b4 <- -1.75493479

b5 <- -45.5170352

b6 <- -6.74694450 * 10 ^ 5


# density of saturated liquid water

rhoprime <- rhoc * (1 + b1 * tau ^ (1 / 3) + b2 * tau ^ (2 / 3) + b3 * tau ^ (5 / 3) + b4 * tau ^ (16 / 3) + b5 * tau ^ (43 / 3) + b6 * tau ^ (110 / 3))

return(rhoprime)

}

}









#' Unit Weight or Specific Weight
#'
#' This function solves for the unit weight or specific weight of a substance
#' using only the substance's density and gravitational acceleration.
#'
#'
#'
#' The equation is expressed as
#'
#' \deqn{\\gamma = \frac{m * g}{V} = \\rho * g = \\rho * \frac{g}{gc}}
#'
#' \describe{
#'   \item{\emph{\\gamma}}{unit weight or specific weight (N/m^3 or lbf/ft^3)}
#'   \item{\emph{m}}{substance mass (kg, lbm, or slugs)}
#'   \item{\emph{g}}{gravitational acceleration (m/s^2 or ft/sec^2)}
#'   \item{\emph{gc}}{"unit conversion factor used to convert mass to force or vice versa" (lbm-ft/lbf-sec^2)}
#'   \item{\emph{V}}{the volume of the substance (m^3 or ft^3)}
#'   \item{\emph{\\rho}}{substance density (mass divided by volume) [kg/m^3, slug/ft^3", or lbm/ft^3]}
#' }
#'
#'
#'
#' @param rho numeric vector that contains the density
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units or \code{Eng} for English units
#'   (United States Customary System in the United States and Imperial Units in
#'   the United Kingdom)]
#' @param Eng_units character vector that contains the density English units
#'   [options are \code{lbm/ft^3} or \code{slug/ft^3}]
#'
#'
#' @return the unit weight or specific weight as a numeric vector. The units are
#'   not returned.
#'
#'
#'
#' @references
#' \enumerate{
#'    \item The Engineering ToolBox, 27 March 2022, "Water - Density, Specific Weight and Thermal Expansion Coefficients", \url{https://www.engineeringtoolbox.com/water-density-specific-weight-d_595.html}
#'    \item WikiEngineer, 27 March 2022, "Water Properties & Definitions", \url{https://web.archive.org/web/20210412034245/http://www.wikiengineer.com/Water-Resources/PropertiesAndDefinitions}. Retrieved thanks to the Internet Archive: Wayback Machine
#'    \item Wikimedia Foundation, Inc. Wikipedia, 27 March 2022, "gc (engineering)", \url{https://en.wikipedia.org/wiki/Gc_(engineering)}.
#'    \item Wikimedia Foundation, Inc. Wikipedia, 27 March 2022, "Specific weight", \url{https://en.wikipedia.org/wiki/Specific_weight}.
#' }
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
#'
#'
#'
#'
#' @examples
#' # Examples
#'
#' library(iemisc)
#' 
#' rho1 <- density_water(Temp = 68, units = "Eng", Eng_units = "slug/ft^3")
#' 
#' unit_wt(rho = rho1, units = "Eng", Eng_units = "slug/ft^3")
#' 
#' 
#' rho2 <- density_water(Temp = 68, units = "Eng", Eng_units = "lbm/ft^3")
#' 
#' unit_wt(rho = rho2, units = "Eng", Eng_units = "lbm/ft^3")
#' 
#' 
#' rho3 <- density_water(Temp = 20, units = "SI")
#' 
#' unit_wt(rho = rho3, units = "SI")
#' 
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom units set_units make_units drop_units
#' @importFrom checkmate qtest
#'
#' @export
unit_wt <- function (rho, units = c("SI", "Eng"), Eng_units = c("slug/ft^3", "lbm/ft^3")) {


ft <- s <- lb <- lbf <- slug <- NULL
# due to NSE notes in R CMD check


units <- units

ifelse(missing(Eng_units) & units == "SI", Eng_units <- NA_character_, Eng_units <- Eng_units)


# Check
assert_that(qtest(units, "S==1"), msg = "There is not an units type or more than 1 units type. Please specify either 'SI' or 'Eng'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("SI", "Eng") %in% units)), msg = "The units system has not been identified correctly as either 'SI' or 'Eng'. Please try again.")
# only process with a specified unit and provide a stop warning if not

assert_that(qtest(Eng_units, "s==1"), msg = "There is not an Eng_units type or more than 1 Eng_units type. Please specify either 'slug/ft^3' or 'lbm/ft^3'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("slug/ft^3", "lbm/ft^3", NA_character_) %in% Eng_units)), msg = "The Eng_units has not been identified correctly as either 'slug/ft^3' or 'lbm/ft^3'. Please try again.")
# only process with a specified unit and provide a stop warning if not

assert_that(!any(qtest(rho, "N+(,)") == FALSE), msg = "Either rho is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails



# units
if (units == "SI") {

g <- set_units(9.80665, "m/s^2")

rho <- set_units(rho, "kg/m^3")

gamma <- rho * g

gamma <- set_units(gamma, "kN/m^3")

return(drop_units(gamma)) # kN/m^3 or kg/m^2 * s^2


} else if (units == "Eng") {

g <- set_units(9.80665 * (3937 / 1200), ft/s^2)

gc <- set_units(9.80665 * (3937 / 1200), lb*ft/lbf/s^2)


if (Eng_units == "slug/ft^3") {

rho <- set_units(rho, slug/ft^3)

gamma <- rho * g / gc

gamma <- set_units(gamma, lbf/ft^3)

return(drop_units(gamma)) # lbf/ft^3


} else if (Eng_units == "lbm/ft^3") {

rho <- set_units(rho, lb/ft^3)

gamma <- rho * g / gc

gamma <- set_units(gamma, lbf/ft^3)

return(drop_units(gamma)) # lbf/ft^3

}

}
}











#' Specific Volume
#'
#' This function solves for the specific volume of a substance using only the
#' substance's density and the density of water.
#'
#'
#'
#' The equation is expressed as
#'
#' \deqn{SG = \frac{\\rho_s}{\\gamma_w} = \frac{\\rho_s}{\\rho_w}}
#'
#' \describe{
#'   \item{\emph{SG}}{specific volume (dimensionless)}
#'   \item{\emph{\\gamma_w}}{unit weight or specific weight of water (N/m^3 or lbf/ft^3)}
#'   \item{\emph{\\rho_s}}{substance density (mass divided by volume) [kg/m^3, slug/ft^3", or lbm/ft^3]}
#'   \item{\emph{\\rho_w}}{water density (mass divided by volume) [kg/m^3, slug/ft^3", or lbm/ft^3]}
#' }
#'
#'
#'
#' @param rho_w numeric vector that contains the density of water
#' @param rho_s numeric vector that contains the density of the substance
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units or \code{Eng} for English units
#'   (United States Customary System in the United States and Imperial Units in
#'   the United Kingdom)]
#' @param Eng_units character vector that contains the unit for the density of
#'   water [options are slug/ft^3 or lbm/ft^3]
#'
#'
#' @return the specific volume as a numeric vector
#'
#'
#'
#' @references
#' \enumerate{
#'    \item Material Properties, 27 March 2022, "Sand – Density – Heat Capacity – Thermal Conductivity", \url{https://material-properties.org/sand-density-heat-capacity-thermal-conductivity/}
#'    \item WikiEngineer, 27 March 2022, "Water Properties & Definitions", \url{https://web.archive.org/web/20210412034245/http://www.wikiengineer.com/Water-Resources/PropertiesAndDefinitions}. Retrieved thanks to the Internet Archive: Wayback Machine
#' }
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
#'
#'
#'
#'
#' @examples
#' # Examples
#'
#' install.load::load_package("iemisc", "units")
#' 
#' # The density of sand is 1500 kg/m^3 -- Reference 1
#' 
#' rho_sand <- set_units(1500, "kg/m^3")
#' 
#' 
#' # convert this density to slug/ft^3
#' rho_sand_slug <- rho_sand
#' 
#' # create a numeric vector with the units of slug/ft^3
#' units(rho_sand_slug) <- make_units(slug/ft^3)
#' 
#' 
#' # convert this density to lbm/ft^3
#' rho_sand_lbm <- rho_sand
#' 
#' # create a numeric vector with the units of lb/ft^3
#' units(rho_sand_lbm) <- make_units(lb/ft^3)
#' 
#' 
#' 
#' rho1 <- density_water(Temp = 68, units = "Eng", Eng_units = "slug/ft^3")
#' 
#' sp_gravity(rho_w = rho1, rho_s = rho_sand_slug, units = "Eng", Eng_units = "slug/ft^3")
#' 
#' 
#' rho2 <- density_water(Temp = 68, units = "Eng", Eng_units = "lbm/ft^3")
#' 
#' sp_gravity(rho_w = rho2, rho_s = rho_sand_lbm, units = "Eng", Eng_units = "lbm/ft^3")
#' 
#' 
#' rho3 <- density_water(Temp = 20, units = "SI")
#' 
#' sp_gravity(rho_w = rho3, rho_s = rho_sand, units = "SI")
#' 
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom units set_units make_units drop_units
#' @importFrom checkmate qtest
#'
#' @export
sp_gravity <- function (rho_w, rho_s, units = c("SI", "Eng"), Eng_units = c("slug/ft^3", "lbm/ft^3")) {

units <- units

ifelse(missing(Eng_units) & units == "SI", Eng_units <- NA_character_, Eng_units <- Eng_units)


# Check
assert_that(qtest(units, "S==1"), msg = "There is not an units type or more than 1 units type. Please specify either 'SI' or 'Eng'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("SI", "Eng") %in% units)), msg = "The units system has not been identified correctly as either 'SI' or 'Eng'. Please try again.")
# only process with a specified unit and provide a stop warning if not

assert_that(qtest(Eng_units, "s==1"), msg = "There is not an Eng_units type or more than 1 Eng_units type. Please specify either 'slug/ft^3' or 'lbm/ft^3'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("slug/ft^3", "lbm/ft^3", NA_character_) %in% Eng_units)), msg = "The Eng_units has not been identified correctly as either 'slug/ft^3' or 'lbm/ft^3'. Please try again.")
# only process with a specified unit and provide a stop warning if not

assert_that(!any(qtest(rho_w, "N+(,)") == FALSE), msg = "Either rho_w is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(!any(qtest(rho_s, "N+(,)") == FALSE), msg = "Either rho_s is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails



# units
if (units == "SI") {

sp_gravity <- rho_s / rho_w

return(drop_units(sp_gravity)) # dimensionless


} else if (units == "Eng") {

if (Eng_units == "slug/ft^3") {

sp_gravity <- rho_s / rho_w

return(drop_units(sp_gravity)) # dimensionless


} else if (Eng_units == "lbm/ft^3") {

sp_gravity <- rho_s / rho_w

return(drop_units(sp_gravity)) # dimensionless

}

}
}











#' Specific Volume
#'
#' This function solves for the specific volume of a substance using only the
#' substance's density
#'
#'
#'
#' The equation is expressed as
#'
#' \deqn{\\nu = \frac{1}{\\rho}}
#'
#' \describe{
#'   \item{\emph{\\rho}}{substance density (mass divided by volume) [kg/m^3, slug/ft^3", or lbm/ft^3]}
#' }
#'
#'
#'
#' @param rho numeric vector that contains the density
#'
#'
#' @return the specific volume as a numeric vector
#'
#'
#'
#' @references
#' WikiEngineer, 27 March 2022, "Water Properties & Definitions", \url{https://web.archive.org/web/20210412034245/http://www.wikiengineer.com/Water-Resources/PropertiesAndDefinitions}. Retrieved thanks to the Internet Archive: Wayback Machine
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
#'
#'
#'
#'
#' @examples
#' # Examples
#'
#' library(iemisc)
#' 
#' rho1 <- density_water(Temp = 68, units = "Eng", Eng_units = "slug/ft^3")
#' 
#' sp_volume(rho = rho1) # slug/ft^3
#' 
#' 
#' rho2 <- density_water(Temp = 68, units = "Eng", Eng_units = "lbm/ft^3")
#' 
#' sp_volume(rho = rho2) # lbm/ft^3
#' 
#' 
#' rho3 <- density_water(Temp = 20, units = "SI")
#' 
#' sp_volume(rho = rho3) # kg / m^3
#' 
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @export
sp_volume <- function (rho) {

assert_that(!any(qtest(rho, "N+(,)") == FALSE), msg = "Either rho is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

nu <- 1 / rho

return(nu)

}










#' Absolute or Dynamic Viscosity for Liquid Water
#'
#' This function solves for the absolute or dynamic viscosity of water using
#' only the temperature of the water in either units of degrees Celsius,
#' degrees Fahrenheit, or Kelvin.
#'
#'
#'
#'
#' The simplified equation is expressed as
#'
#' \deqn{\\mu_s = \frac{1}{a + bT + cT^2 + dT^3}}
#'
#' with
#'
#' \deqn{a = 557.82468}
#' \deqn{b = 19.408782}
#' \deqn{c = 0.1360459}
#' \deqn{d = -3.1160832 * 10 ^ -4}
#'
#' \describe{
#'   \item{\emph{\\mu_s}}{Water Absolute or Dynamic Viscosity (kg/m*s, slug/ft/s, or lbf*s/ft^2)}
#'   \item{\emph{T}}{the water temperature, degrees Celsius}
#' }
#'
#'
#'
#' @param Temp numeric vector that contains the temperature (degrees Celsius,
#'   degrees Fahrenheit, or Kelvin)
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units, \code{Eng} for English units
#'   (United States Customary System in the United States and Imperial Units in
#'   the United Kingdom), or \code{Absolute} for Absolute Units]
#' @param Eng_units character vector that contains the unit for the dynamic
#'   viscosity of water in the English system [options are slug/ft/s or
#'   lbf*s/ft^2]
#'
#'
#' @return the absolute or dynamic viscosity as a numeric vector. The units are
#'   not returned.
#'
#'
#'
#' @references
#' C. O. Popiel & J. Wojtkowiak (1998). "Simple Formulas for Thermophysical Properties of Liquid Water for Heat Transfer Calculations (from 0C to 150C)". \emph{Heat Transfer Engineering}, 19:3, 87-101, article from ResearchGate: \url{https://www.researchgate.net/publication/239243539_Simple_Formulas_for_Thermophysical_Properties_of_Liquid_Water_for_Heat_Transfer_Calculations_from_0C_to_150C}.
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
#'
#'
#'
#'
#' @examples
#' # Example 1 (Compare to the tabulated values in the Reference paper)
#'
#' install.load::load_package("iemisc", "data.table", "round")
#' 
#' Temp <- c(0, 0.01, 3.86, seq(5, 95, by = 5), 99.974, seq(100, 150, by = 5))
#' 
#' dynamic_viscosity <- data.table("Temperature (degrees C)" = Temp,
#' "mu (* 10 ^ 6, kg / m*s)" = round_r3(dyn_visc_water(Temp, units = "SI")
#' * 10^6, d = 1))
#' dynamic_viscosity
#' 
#' 
#'
#'
#'
#' # Example 2 - Example from the hydraulics package
#'
#' library(iemisc)
#' 
#' mu <- hydraulics::dvisc(T = 55, units = "Eng"); mu
#' 
#' mu2 <- dyn_visc_water(Temp = 55, units = "Eng", Eng_units = "lbf*s/ft^2"); mu2
#' 
#' 
#' 
#' 
#'
#' @importFrom assertthat assert_that
#' @importFrom units set_units make_units drop_units
#' @importFrom checkmate qtest
#'
#' @export
dyn_visc_water <- function (Temp, units = c("SI", "Eng", "Absolute"), Eng_units = c("slug/ft/s", "lbf*s/ft^2")) {


K <- N <- s <- m <- slug <- ft <- lbf <- degree_C <- NULL
# due to NSE notes in R CMD check


units <- units

ifelse(missing(Eng_units) & units == "SI" | units == "Absolute", Eng_units <- NA_character_, Eng_units <- Eng_units)


# Check
assert_that(qtest(units, "S==1"), msg = "There is not an units type or more than 1 units type. Please specify either 'SI', 'Eng', or 'Absolute'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("SI", "Eng", "Absolute") %in% units)), msg = "The units system has not been identified correctly as either 'SI', 'Eng', or 'Absolute'. Please try again.")
# only process with a specified unit and provide a stop warning if not


assert_that(qtest(Eng_units, "s==1"), msg = "There is not an Eng_units type or more than 1 Eng_units type. Please specify either 'slug/ft/s' or 'lbf*s/ft^2'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("slug/ft/s", "lbf*s/ft^2", NA_character_) %in% Eng_units)), msg = "The Eng_units has not been identified correctly as either 'slug/ft/s' or 'lbf*s/ft^2'. Please try again.")
# only process with a specified unit and provide a stop warning if not


# units
if (units == "SI") {

# create a numeric vector with the units of degrees Celsius
T_C <- set_units(Temp, "degree_C")


# create a numeric vector to convert from degrees Celsius to Kelvin
T_K <- T_C


# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)


assert_that(qtest(drop_units(T_K), "N+[273.15,423.15]"), msg = "Either Temp is less than 273.15 K / 0 C or greater than 423.15 K / 150 C, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures in the range of 273.15 K <= Temp <= 423.15 K. Please try again.")
# only process with specified, finite values and provide an error message if the check fails


a <- 557.82468

b <- 19.408782

c <- 0.1360459

d <- -3.1160832 * 10 ^ -4


# absolute or dynamic viscosity for water
# calculated using the temperature in degrees C

mus <- 1 / (a + b * Temp + c * Temp ^ 2 + d * Temp ^ 3)

return(mus)


} else if (units == "Eng") {


T_F <- Temp

# create a numeric vector with the units of degrees Fahrenheit
T_F <- set_units(T_F, "degree_F")


# create a numeric vector to convert from degrees Fahrenheit to degrees Celsius
T_C <- T_F


# create a numeric vector to convert from degrees Celsius to Kelvin
T_K <- T_F


# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)

# create a numeric vector with the units of degrees Celsius
units(T_C) <- make_units(degree_C)

Temp <- drop_units(T_C)

assert_that(qtest(drop_units(T_K), "N+[273.15,423.15]"), msg = "Either Temp is less than 273.15 K / 32 F or greater than 423.15 K / 302 F, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures in the range of 273.15 K <= Temp <= 423.15 K. Please try again.")
# only process with specified, finite values and provide an error message if the check fails

a <- 557.82468

b <- 19.408782

c <- 0.1360459

d <- -3.1160832 * 10 ^ -4


# absolute or dynamic viscosity for water
# calculated using the temperature in degrees C

mus <- 1 / (a + b * Temp + c * Temp ^ 2 + d * Temp ^ 3)

# create a numeric vector with the units of kg/m*s
mus <- set_units(mus, N*s/m^2) # N*s/m^2 or kg/m/s


if (Eng_units == "slug/ft/s") {

# create a numeric vector to convert from kg/m/s to slug/ft/s
mus_slug <- mus

# create a numeric vector with the units of slug/ft/s
units(mus_slug) <- make_units(slug/ft/s)

return(drop_units(mus_slug)) # slug/ft*s


} else if (Eng_units == "lbf*s/ft^2") {

# create a numeric vector to convert from kg/m/s to lbf*s/ft^2
mus_lbf <- mus

# create a numeric vector with the units of lbf*s/ft^2
units(mus_lbf) <- make_units(lbf*s/ft^2)

return(drop_units(mus_lbf)) # lbf*s/ft^2

}


} else if (units == "Absolute") {

T_K <- Temp

# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)

assert_that(qtest(drop_units(T_K), "N+[273.15,423.15]"), msg = "Either Temp is less than 273.15 K or greater than 423.15 K, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures in the range of 273.15 K <= Temp <= 423.15 K. Please try again.")
# only process with specified, finite values and provide an error message if the check fails

# create a numeric vector to convert from degrees Fahrenheit to degrees Celsius
T_C <- T_K


# create a numeric vector with the units of degrees Celsius
units(T_C) <- make_units(degree_C)

Temp <- drop_units(T_C)

a <- 557.82468

b <- 19.408782

c <- 0.1360459

d <- -3.1160832 * 10 ^ -4


# absolute or dynamic viscosity for water
# calculated using the temperature in degrees C

mus <- 1 / (a + b * Temp + c * Temp ^ 2 + d * Temp ^ 3)

return(mus)

}

}













#' Kinematic Viscosity for liquid Water
#'
#' This function solves for the kinematic viscosity of water using only
#' the water density and the dynamic viscosity.
#'
#'
#'
#'
#' The simplified equation is expressed as
#'
#' \deqn{\\nu = \frac{\\mu}{\\rho}}
#'
#' \describe{
#'   \item{\emph{\\nu}}{Water Kinematic Viscosity (m^2/s or ft^2/s)}
#'   \item{\emph{\\rho}}{Water Density (mass divided by volume), slug/ft^3}
#'   \item{\emph{\\mu}}{Water Dynamic viscosity, slug/ft/s}
#' }
#'
#'
#'
#' @param rho numeric vector that contains the water density
#' @param mu numeric vector that contains the water dynamic viscosity
#' @param rho_units character vector that contains the unit for the density of
#'   water [options are kg/m^3, lbm/ft^3, or slug/ft^3]
#' @param mu_units character vector that contains the unit for the dynamic
#'   viscosity of water [options are Pa*s or kg/m/s, lbf*s/ft^2, or slug/ft/s]
#'
#'
#' @return the kinematic viscosity as a numeric vector. The units are not
#'   returned.
#'
#' @note
#' Note: 1 lbf = 1 slug * 1 ft/sec^2, thus 1 slug = 1 lbf * sec^2 / 1 ft
#' (Reference 2)
#'
#' Thus, lbm/ft^3 = lbf*s^2/ft/ft^3
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
#' @references
#' \enumerate{
#'    \item Ven Te Chow, Ph.D., \emph{Open-Channel Hydraulics}, McGraw-Hill Classic Textbook Reissue, New York City, New York: McGraw-Hill Book Company, 1988, pages 7-8.
#'    \item Professor S.A. Kinnas, Commonly used units in CE319F (Elementary Fluid Mechanics), The University of Texas at Austin Department of Civil, Architectural and Environmental Engineering, \url{https://www.caee.utexas.edu/prof/kinnas/319LAB/notes13/units_ce319f_kinnas.pdf}.
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
#' @examples
#' # Example 1
#'
#' \donttest{
#' # See Source 1 and Source 2
#'
#' library(iemisc)
#'
#' try(kin_visc_water(mu = 34, rho = 0, rho_units = "kg/m^3", mu_units = "Pa*s or kg/m/s"))
#' }
#' 
#' 
#' 
#' 
#' # Example 2 (from the Reference)
#' 
#' install.load::load_package("iemisc", "units")
#'
#' import::from(fpCompare, "%==%")
#' 
#' 
#' # For water at 68 F (20 C), mu = 2.09 * 10 ^ -8 slug/ft/s and rho = 1.937 slug/ft^3
#' 
#' kin_visc_water(mu = 2.09 * 10 ^ -8, rho = 1.937, rho_units =
#' "slug/ft^3", mu_units = "slug/ft/s")
#' 
#' # convert the units
#' 
#' rho <- set_units(1.937, slug/ft^3)
#' 
#' mu <- set_units(2.09 * 10 ^ -8, slug/ft/s)
#' 
#' mu1 <- set_units(mu, kg/m/s)
#' 
#' rho1 <- set_units(rho, "kg/m^3")
#' 
#' kin_visc_water(mu = mu1, rho = rho1, rho_units = "kg/m^3", mu_units =
#' "Pa*s or kg/m/s")
#' 
#' mu2 <- set_units(mu, lbf*s/ft^2)
#' 
#' rho2 <- set_units(rho, lb/ft^3)
#' 
#' kin_visc_water(mu = mu2, rho = rho2, rho_units = "lbm/ft^3", mu_units =
#' "lbf*s/ft^2")
#' 
#' 
#' # compare the results of part 1 and part 3 (they should be equivalent)
#' 
#' kin_visc_water(mu = 2.09 * 10 ^ -8, rho = 1.937, rho_units = "slug/ft^3",
#' mu_units = "slug/ft/s") %==% kin_visc_water(mu = mu2, rho = rho2, rho_units =
#' "lbm/ft^3", mu_units = "lbf*s/ft^2")
#' 
#' 
#' 
#' 
#' 
#' # Example 2 - Example from the hydraulics package
#' 
#' install.load::load_package("iemisc", "units")
#'
#' import::from(fpCompare, "%==%")
#' 
#' 
#' nu <- hydraulics::kvisc(T = 55, units = "Eng", ret_units = TRUE); nu
#'
#' nus <- hydraulics::dvisc(T = 55, units = "Eng", ret_units = TRUE) /
#' hydraulics::dens(T = 55, units = "Eng", ret_units = TRUE); nus
#' 
#' 
#' # compare the results of nu and nus (they should be equivalent)
#' 
#' drop_units(nu) %==% drop_units(nus)
#' 
#' 
#' nu2 <- dyn_visc_water(Temp = 55, units = "Eng", Eng_units = "lbf*s/ft^2") /
#' density_water(Temp = 55, units = "Eng", Eng_units = "slug/ft^3"); nu2
#'
#' nus2 <- kin_visc_water(mu = dyn_visc_water(Temp = 55, units = "Eng", Eng_units =
#' "lbf*s/ft^2"), rho = density_water(Temp = 55, units = "Eng", Eng_units = "slug/ft^3"),
#' rho_units = "lbm/ft^3", mu_units = "lbf*s/ft^2"); nus2
#' 
#' # compare the results of nu2 and nus2 (they should be equivalent)
#' 
#' nu2 %==% nus2
#' 
#'
#' 
#' 
#' @importFrom assertthat assert_that
#' @importFrom units set_units make_units drop_units
#' @importFrom checkmate qtest
#'
#' @export
kin_visc_water <- function (rho, mu, rho_units = c("kg/m^3", "lbm/ft^3", "slug/ft^3"), mu_units = c("Pa*s or kg/m/s", "lbf*s/ft^2", "slug/ft/s")) {


Pa <- s <- kg <- m <- lb <- ft <- slug <- lbf <- NULL
# due to NSE notes in R CMD check


rho_units <- rho_units

mu_units <- mu_units

units <- c(rho_units, mu_units)


# Check
assert_that(qtest(rho_units, "S==1"), msg = "There is not an rho_units type or more than 1 rho_units type. Please specify either 'kg/m^3', 'lbm/ft^3', or 'slug/ft^3'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(qtest(mu_units, "S==1"), msg = "There is not an mu_units type or more than 1 mu_units type. Please specify either 'Pa*s or kg/m/s', 'lbf*s/ft^2', or 'slug/ft/s'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(qtest(rho, "N+(0,)"), msg = "Either rho is less than or equal to 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with specified, finite values and provide an error message if the check fails

assert_that(isTRUE(any(c("kg/m^3", "lbm/ft^3", "slug/ft^3") %in% rho_units)), msg = "The rho_units (density units) has not been identified correctly as either 'kg/m^3', 'lbm/ft^3', or 'slug/ft^3'. Please try again.")
# only process with a specified rho_units value and provide a stop warning if not

assert_that(isTRUE(any(c("Pa*s or kg/m/s", "lbf*s/ft^2", "slug/ft/s") %in% mu_units)), msg = "The mu_units (absolute or dynamic viscosity) has not been identified correctly as either 'Pa*s or kg/m/s', 'lbf*s/ft^2', or 'slug/ft/s'. Please try again.")
# only process with a specified mu_units value and provide a stop warning if not


if (isTRUE(all(c("kg/m^3", "lbf*s/ft^2") %in% units))) {

assert_that(all(c("kg/m^3", "lbf*s/ft^2") %in% units == FALSE), msg = "The units are mismatched. You have 'kg/m^3' and 'lbf*s/ft^2'. It should be 'kg/m^3' and 'Pa*s or kg/m/s'. Please try again.")


} else if (isTRUE(all(c("kg/m^3", "slug/ft/s") %in% units))) {

assert_that(all(c("kg/m^3", "slug/ft/s") %in% units == FALSE), msg = "The units are mismatched. You have 'kg/m^3' and 'slug/ft/s'. It should be 'kg/m^3' and 'Pa*s or kg/m/s'. Please try again.")


} else if (isTRUE(all(c("lbm/ft^3", "Pa*s or kg/m/s") %in% units))) {

assert_that(all(c("lbm/ft^3", "Pa*s or kg/m/s") %in% units == FALSE), msg = "The units are mismatched. You have 'lbm/ft^3' and 'Pa*s or kg/m/s'. It should be 'lbm/ft^3' and 'lbf*s/ft^2'. Please try again.")


} else if (isTRUE(all(c("lbm/ft^3", "slug/ft/s") %in% units))) {

assert_that(all(c("lbm/ft^3", "slug/ft/s") %in% units == FALSE), msg = "The units are mismatched. You have 'lbm/ft^3' and 'slug/ft/s'. It should be 'lbm/ft^3' and 'lbf*s/ft^2'. Please try again.")


} else if (isTRUE(all(c("slug/ft^3", "Pa*s or kg/m/s") %in% units))) {

assert_that(all(c("slug/ft^3", "Pa*s or kg/m/s") %in% units == FALSE), msg = "The units are mismatched. You have 'slug/ft^3' and 'Pa*s or kg/m/s'. It should be 'slug/ft^3' and 'slug/ft/s'. Please try again.")


} else if (isTRUE(all(c("slug/ft^3", "lbf*s/ft^2") %in% units))) {

assert_that(all(c("slug/ft^3", "lbf*s/ft^2") %in% units == FALSE), msg = "The units are mismatched. You have 'slug/ft^3' and 'lbf*s/ft^2'. It should be 'slug/ft^3' and 'slug/ft/s'. Please try again.")

}



# units
if (rho_units == "kg/m^3" & mu_units == "Pa*s or kg/m/s") {

rho <- set_units(rho, "kg/m^3")

mu <- set_units(mu, Pa*s)

units(mu) <- make_units((kg*s)/(m*s^2))

nu <- mu / rho

return(drop_units(nu))


} else if (rho_units == "lbm/ft^3" & mu_units == "lbf*s/ft^2") {

rho <- set_units(rho, lb/ft^3)

mu <- set_units(mu, lbf*s/ft^2)

nu <- mu / rho

return(drop_units(nu))


} else if (rho_units == "slug/ft^3" & mu_units == "slug/ft/s") {

rho <- set_units(rho, slug/ft^3)

mu <- set_units(mu, slug/ft/s)

nu <- mu / rho

return(drop_units(nu))

}
}










#' Water Surface Tension for Liquid Water
#'
#' This function solves for the surface tension of water using only the
#' temperature of the water in either units of degrees Celsius, degrees
#' Fahrenheit, or Kelvin.
#'
#'
#'
#'
#' The simplified equation is expressed as
#'
#' \deqn{\\sigma = \frac{1}{a + bT + cT^2 + dT^3}}
#'
#' with
#'
#' \deqn{a = 0.075652711}
#' \deqn{b = -0.00013936956}
#' \deqn{c = -3.0842103 * 10 ^ -7}
#' \deqn{d = 2.7588435 * 10 ^ -10}
#'
#' \describe{
#'   \item{\emph{\\sigma}}{Water Surface Tension (N/m or lbf/ft)}
#'   \item{\emph{T}}{the water temperature, degrees Celsius}
#' }
#'
#'
#'
#' @param Temp numeric vector that contains the temperature (degrees Celsius,
#'   degrees Fahrenheit, or Kelvin)
#' @param units character vector that contains the system of units (options are
#'   \code{SI} for International System of Units, \code{Eng} for English units
#'   (United States Customary System in the United States and Imperial Units in
#'   the United Kingdom), or \code{Absolute} for Absolute Units)
#'
#'
#' @return the surface tension as a numeric vector. The units are not returned.
#'
#'
#'
#'
#' @references
#' C. O. Popiel & J. Wojtkowiak (1998). "Simple Formulas for Thermophysical Properties of Liquid Water for Heat Transfer Calculations (from 0C to 150C)". \emph{Heat Transfer Engineering}, 19:3, 87-101, article from ResearchGate: \url{https://www.researchgate.net/publication/239243539_Simple_Formulas_for_Thermophysical_Properties_of_Liquid_Water_for_Heat_Transfer_Calculations_from_0C_to_150C}.
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
#' @examples
#' # Example (Compare to the tabulated values in the Reference paper)
#'
#' install.load::load_package("iemisc", "data.table", "round")
#' 
#' Temp <- c(0, 0.01, 3.86, seq(5, 95, by = 5), 99.974, seq(100, 150, by = 5))
#' 
#' surface_tension <- data.table("Temperature (degrees C)" = Temp, "omega (N / m)"
#' = round_r3(surf_tens_water(Temp, units = "SI"), d = 5)); surface_tension
#' 
#'
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom units set_units make_units drop_units
#' @importFrom checkmate qtest
#'
#' @export
surf_tens_water <- function (Temp, units = c("SI", "Eng", "Absolute")) {


K <- N <- m <- lbf <- ft <- degree_C <- NULL
# due to NSE notes in R CMD check


units <- units


# Check
assert_that(qtest(units, "S==1"), msg = "There is not an units type or more than 1 units type. Please specify either 'SI', 'Eng', or 'Absolute'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("SI", "Eng", "Absolute") %in% units)), msg = "The units system has not been identified correctly as either 'SI', 'Eng', or 'Absolute'. Please try again.")
# only process with a specified unit and provide a stop warning if not


# units
if (units == "SI") {

# create a numeric vector with the units of degrees Celsius
T_C <- set_units(Temp, "degree_C")


# create a numeric vector to convert from degrees Celsius to Kelvin
T_K <- T_C


# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)


assert_that(qtest(drop_units(T_K), "N+[273.15,423.15]"), msg = "Either Temp is less than 273.15 K / 0 C or greater than 423.15 K / 150 C, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures in the range of 273.15 K <= Temp <= 423.15 K. Please try again.")
# only process with specified, finite values and provide an error message if the check fails


a <- 0.075652711

b <- -0.00013936956

c <- -3.0842103 * 10 ^ -7

d <- 2.7588435 * 10 ^ -10


# surface tension for water
# calculated using the temperature in degrees C

sigmas <- a + b * Temp + c * Temp ^ 2 + d * Temp ^ 3

return(sigmas)


} else if (units == "Eng") {


T_F <- Temp

# create a numeric vector with the units of degrees Fahrenheit
T_F <- set_units(T_F, "degree_F")


# create a numeric vector to convert from degrees Fahrenheit to degrees Celsius
T_C <- T_F


# create a numeric vector to convert from degrees Celsius to Kelvin
T_K <- T_F


# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)

# create a numeric vector with the units of degrees Celsius
units(T_C) <- make_units(degree_C)

Temp <- drop_units(T_C)

assert_that(qtest(drop_units(T_K), "N+[273.15,423.15]"), msg = "Either Temp is less than 273.15 K / 32 F or greater than 423.15 K / 302 F, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures in the range of 273.15 K <= Temp <= 423.15 K. Please try again.")
# only process with specified, finite values and provide an error message if the check fails

a <- 0.075652711

b <- -0.00013936956

c <- -3.0842103 * 10 ^ -7

d <- 2.7588435 * 10 ^ -10


# surface tension for water
# calculated using the temperature in degrees C

sigmas <- a + b * Temp + c * Temp ^ 2 + d * Temp ^ 3

# create a numeric vector with the units of N/m
sigmas <- set_units(sigmas, N/m) # N*s/m^2

# create a numeric vector to convert from N/m to lbf/ft
sigmas <- sigmas

# create a numeric vector with the units of lbf/ft
units(sigmas) <- make_units(lbf/ft)

return(drop_units(sigmas)) # lbf/ft


} else if (units == "Absolute") {

T_K <- Temp

# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)

assert_that(qtest(drop_units(T_K), "N+[273.15,423.15]"), msg = "Either Temp is less than 273.15 K or greater than 423.15 K, NA, NaN, Inf, -Inf, empty, or a string. The equation is valid only for temperatures in the range of 273.15 K <= Temp <= 423.15 K. Please try again.")
# only process with specified, finite values and provide an error message if the check fails

# create a numeric vector to convert from degrees Fahrenheit to degrees Celsius
T_C <- T_K

# create a numeric vector with the units of degrees Celsius
units(T_C) <- make_units(degree_C)

Temp <- drop_units(T_C)

a <- 0.075652711

b <- -0.00013936956

c <- -3.0842103 * 10 ^ -7

d <- 2.7588435 * 10 ^ -10


# surface tension for water
# calculated using the temperature in degrees C

sigmas <- a + b * Temp + c * Temp ^ 2 + d * Temp ^ 3

return(sigmas)

}

}
