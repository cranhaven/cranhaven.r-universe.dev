#' Design of Packed Column Air Strippers
#'
#' @description
#' Calculates key parameters needed in the design of a packed column air
#' stripper according to the U.S. Army Corps of Engineers Design Guide No.
#' 1110-1-3: Air Stripping Engineering and Design (Design Guidelines). Please
#' refer to the Design Guidelines for the governing equations and background
#' information.
#'
#' 'Air stripping is the transferring of volatile components of a liquid into
#' an air stream. It is an environmental engineering technology used for the
#' purification of groundwaters and wastewaters containing volatile compounds.'
#' (Reference: Wikipedia)
#'
#'
#'
#' 
#' @param Temp numeric vector that contains the minimum Temperature (degrees
#'   Celsius, degrees Fahrenheit, or Kelvin)
#' @param pTe numeric vector that contains the total pressure of gas (air)
#'   effluent (atm)
#' @param P_atm numeric vector that contains the atmospheric pressure (atm).
#'   The default is 1 atm.
#' @param contam1 character vector that contains the name of each contaminant
#'   to be removed (may include "Total VOCs"). See the example.
#' @param Cai numeric vector that contains the concentration of each
#'   contaminant in liquid (water) influent (ug/L)
#' @param Cae numeric vector that contains the concentration of each
#'   contaminant in liquid (water) effluent (ug/L)
#' @param contam2 character vector that contains the name of each contaminant
#'   (will not include "Total VOCs"). See the example.
#' @param cas character vector that contains the CAS Number of each contaminant,
#'   if known, otherwise it can be accessed internally
#' @param Ha numeric vector that contains the Ha (Henry's Law coefficient) at
#'   the minimum Temperature \emph{T} for each contaminant (atm/mole/mole)
#' @param Q numeric vector that contains the sustained pumping rate (gallons
#'   per minute or gpm) 
#' @param loading numeric vector that contains the stripper surface loading
#'   (gpm/ft^2)
#' @param ns numeric vector that contains the number of strippers
#' @param DL numeric vector that contains the liquid diffusivity of each
#'   contaminant at the minimum Temperature \emph{T} in the same order as
#'   contam2 (m^2/s)
#' @param DG numeric vector that contains the gas diffusivity of each
#'   contaminant in air at the minimum Temperature \emph{T} and \emph{pTe} in
#'   the same order as contam2 (m^2/s)
#' @param R numeric vector that contains the stripping factor (R = 2.5 if air
#'   pollution control is required or R = 4.5 if it isn't, but in a range of 2
#'   - 5)
#' @param dP numeric vector that contains the nominal diameters for the packing
#'   material. The Design Guidelines use Jaeger Tripacks 2-in. (50.8 mm) plastic
#'   media for the packing material.
#' @param at numeric vector that contains the total surface area for the
#'   packing material (the default value is 157 m^2/m^3)
#' @param Sc numeric vector that contains the critical surface tension for the
#'   packing material (the default value is 0.033 kg/s^2)
#' @param cf numeric vector that contains the packing factor for the packing
#'   material (the default value is 15/ft)
#' @param Temp_unit character vector that contains the possible units for the
#'   water temperature [options are \code{SI} for International System of Units,
#'   \code{Eng} for English units (United States Customary System in the United
#'   States and Imperial Units in the United Kingdom), or \code{Absolute} for
#'   Absolute Units]
#' @param dP_unit character vector that contains the possible units for the
#'   nominal diameters for the packing material (inch or mm)
#' @param at_unit character vector that contains the possible units for the
#'   total surface area for the packing material (ft^2/ft^3 or m^2/m^3)
#' @param Sc_unit character vector that contains the possible units for the
#'   critical surface tension for the packing material (kg/s^2 or slug/s^2)
#' @param contaminants_table integer vector that contains 0, 1 only. 0 represents
#'   do not print the Contaminants Table and 1 is for printing the Contaminants
#'   Table.
#' @param removal_requirements_table integer vector that contains 0, 1 only. 0
#'   represents do not print the Removal Requirements Table and 1 is for printing
#'   the Removal Requirements Table.
#' @param critical_contaminant_table integer vector that contains 0, 1 only. 0
#'   represents do not print the Critical Contaminant Table and 1 is for printing
#'   the Critical Contaminant Table.
#'
#'
#'
#'
#' @return the name of the critical contaminant, molar liquid (water) flow per
#'   unit of stripper cross-sectional area (kg mole/m^2 s), molar gas (air)
#'   flow per unit of stripper cross-sectional area (kg mole/m^2 s), height of
#'   transfer unit (HTU) [m and ft], number of transfer units (NTU), packing
#'   depth (m and feet), and the air to water ratio as a \code{\link[data.table]{data.table}}.
#'   If contaminants_table = 1, provide the Contaminants Table. If
#'   removal_requirements_table = 1, provide the Removal Requirements Table. If
#'   critical_contaminant_table = 1, provide the Critical Contaminant Table.
#'
#'
#' @note
#' Please Note: Use these results as preliminary estimates only.
#' 
#' Please Note: This is not meant for any actual designs.
#' 
#' Please Note: The calculations assume dry air rather than humid air.
#' 
#' Please refer to the iemisc: Air Stripping By Packed Column Examples vignette
#' for examples
#'
#'
#'
#'
#'
#'
#' @references
#' \enumerate{
#'    \item Accu Dyne Test: Diversified Enterprises. Critical Surface Tension and Contact Angle with Water for Various Polymers, \url{https://www.accudynetest.com/polytable_03.html}.
#'    \item Design Guide No. 1110-1-3: Air Stripping Engineering and Design Appendix D: Example Air Stripping By Packed Column, Department Of The Army U.S. Army Corps of Engineers, 31 October 2001, pages D-1 - D-18, \url{https://web.archive.org/web/20240217153739/https://www.publications.usace.army.mil/Portals/76/Publications/EngineerDesignGuides/DG_1110-1-3.pdf?ver=2013-08-16-101222-003}. Retrieved thanks to the Internet Archive: Wayback Machine
#'    \item Edgar L Andreas, Design Guide No. 1110-1-3: Handbook of Physical Constants and Functions for Use in Atmospheric Boundary Layer Studies, Department Of The Army U.S. Army Corps of Engineers, October 2005, pages D-1 - D-18, \url{https://apps.dtic.mil/sti/pdfs/ADA440352.pdf}.
#'    \item EnggCyclopedia. Tutorial: air density calculation, 3 January 2022, \url{https://enggcyclopedia.com/2019/04/air-density-calculation/}.
#'    \item Harlan H. Bengtson, PhD, P.E. Continuing Education and Development, Inc., Calculation of Gas Density and Viscosity Course No: H02-008, \url{https://www.scribd.com/document/452763833/Calculation-of-Gas-Density-and-Viscosity-pdf}.
#'    \item PCA Series Packed Column Air Strippers, H2K Technologies, Inc., 2011, page 2, \url{http://www.h2ktech.com/pdf_downloads/PCA_Packed_Column_Air_Strippers.pdf}.
#'    \item Peter J. Mohr, David B. Newell, and Barry N. Taylor. Continuing Education and Development, Inc., CODATA recommended values of the fundamental physical constants: 2014, \emph{Reviews Of Modern Physics}, Volume 88, July-September 2016, \url{https://web.archive.org/web/20230608140030/https://physics.nist.gov/cuu/pdf/CODATA_JPCRD2016.pdf}. Used the Internet Archive: Wayback Machine archived version for acceptance into CRAN.
#'    \item The NIST Reference on Constants, Units, and Uncertainty, Fundamental Constants Data Center of the NIST Physical Measurement Laboratory, "standard acceleration of gravity g_n", \url{https://web.archive.org/web/20230427133623/https://physics.nist.gov/cgi-bin/cuu/Value?gn}. Used the Internet Archive: Wayback Machine archived version for acceptance into CRAN.
#'    \item Wikimedia Foundation, Inc. Wikipedia, 27 March 2022, "Air stripping", \url{https://en.wikipedia.org/wiki/Air_stripping}.
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
#' @importFrom data.table data.table setnames setattr setkey .I .EACHI :=
#' @importFrom units set_units make_units drop_units
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#' @importFrom round round_r3
#' @import chem.databases
#'
#' @export
air_stripper <- function (Temp, pTe, contam1, Cai, Cae, contam2, cas = NULL, Ha, Q, loading, ns, DL, DG, R, P_atm = NULL, dP = NULL, at = NULL, Sc = NULL, cf = NULL, Temp_unit = c("SI", "Eng", "Absolute"), dP_unit = c("inch", "mm"), at_unit = c("ft^2/ft^3", "m^2/m^3"), Sc_unit = c("kg/s^2", "slug/s^2"), contaminants_table = c(0, 1), removal_requirements_table = c(0, 1), critical_contaminant_table = c(0, 1)) {

degree_C <- J <- value <- K <- ft <- m <- kg <- s <- NULL
# due to NSE notes in R CMD check


# copy the dataset as atsdrtscald50
atsdrtscald50 <- chem.databases::atsdr_tsca_ld50_a

# copy the dataset as chemwiki
chemwiki <- chem.databases::chem_wiki



ifelse(missing(P_atm), P_atm <- 1, P_atm <- P_atm)

ifelse(missing(dP), dP <- 50.8, dP <- dP)

ifelse(missing(at), at <- 157, at <- at)

ifelse(missing(Sc), Sc <- 0.033, Sc <- Sc)

ifelse(missing(cf), cf <- 15, cf <- cf)

ifelse(missing(contaminants_table), contaminants_table <- 0, contaminants_table <- contaminants_table)

ifelse(missing(removal_requirements_table), removal_requirements_table <- 0, removal_requirements_table <- removal_requirements_table)

ifelse(missing(critical_contaminant_table), critical_contaminant_table <- 0, critical_contaminant_table <- critical_contaminant_table)

ifelse(missing(cas), cas <- atsdrtscald50[as.numeric(atsdrtscald50$"Registry Name" %inorder% contam1), "CAS"][[1]], cas <- cas)


check_numbers <- c(Temp, pTe, Cai, Cae, Ha, Q, loading, ns, DL, DG, R, dP, at, Sc, cf)

check_strings <- c(contam1, contam2, cas)


# check on check_numbers
assert_that(!any(qtest(check_numbers, "n+[0,)") == FALSE), msg = "Temp, pTe, Cai, Cae, Ha, Q, loading, ns, DL, DG, R, dP, at, Sc, and/or cf Inf, -Inf, empty, or a character string. Please try again.")
# only process with numeric values and provide an error message if the check fails

# check on check_strings
assert_that(!any(qtest(check_strings, "S+") == FALSE), msg = "contam1, contam2, and/or cas is NA, NaN, Inf, -Inf, empty, or a numeric vector. Please try again.")
# only process with character string values and provide an error message if the check fails


# the minimum Temperature Temp is in degrees Celsius
Temp_unit <- Temp_unit


# Check Temp_unit
assert_that(qtest(Temp_unit, "S==1"), msg = "There is not a Temp_unit type or more than 1 Temp_unit type. Please specify either 'SI', 'Eng', or 'Absolute'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("SI", "Eng", "Absolute") %in% Temp_unit)), msg = "The Temp_unit system has not been identified correctly as either 'SI', 'Eng', or 'Absolute'. Please try again.")
# only process with a specified Temp_unit and provide a stop warning if not


# Check dP_unit
assert_that(qtest(dP_unit, "S==1"), msg = "There is not a dP_unit type or more than 1 dP_unit type. Please specify either 'inch' or 'mm'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("inch", "mm") %in% dP_unit)), msg = "The dP_unit system has not been identified correctly as either 'inch' or 'mm'. Please try again.")
# only process with a specified dP_unit and provide a stop warning if not


# Check at_unit
assert_that(qtest(at_unit, "S==1"), msg = "There is not a at_unit type or more than 1 at_unit type. Please specify either 'ft^2/ft^3' or 'm^2/m^3'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("ft^2/ft^3", "m^2/m^3") %in% at_unit)), msg = "The at_unit system has not been identified correctly as either 'ft^2/ft^3' or 'm^2/m^3'. Please try again.")
# only process with a specified at_unit and provide a stop warning if not


# Check Sc_unit
assert_that(qtest(Sc_unit, "S==1"), msg = "There is not a Sc_unit type or more than 1 Sc_unit type. Please specify either 'kg/s^2' or 'slug/s^2'.")
# only process with enough known variables and provide an error message if the check fails

assert_that(isTRUE(any(c("kg/s^2", "slug/s^2") %in% Sc_unit)), msg = "The Sc_unit system has not been identified correctly as either 'kg/s^2' or 'slug/s^2'. Please try again.")
# only process with a specified Sc_unit and provide a stop warning if not


# check on the tables
contaminants_table <- contaminants_table

assert_that(qtest(contaminants_table, "N==1[0,1]"), msg = "contaminants_table should only be a single numeric value of 0 for no contaminants_table or 1 for a contaminants_table. Please try again.")
# only process with a single numeric value of 0 or 1 and provide an error message if the check fails



removal_requirements_table <- removal_requirements_table

assert_that(qtest(removal_requirements_table, "N==1[0,1]"), msg = "removal_requirements_table should only be a single numeric value of 0 for no removal_requirements_table or 1 for a removal_requirements_table. Please try again.")
# only process with a single numeric value of 0 or 1 and provide an error message if the check fails



critical_contaminant_table <- critical_contaminant_table

assert_that(qtest(critical_contaminant_table, "N==1[0,1]"), msg = "critical_contaminant_table should only be a single numeric value of 0 for no critical_contaminant_table or 1 for a critical_contaminant_table. Please try again.")
# only process with a single numeric value of 0 or 1 and provide an error message if the check fails




# References
# NIST reference
# standard acceleration of gravity (g) 9.80665 m / s^2

# Mohr reference
# Molar volume of ideal gas Ru * Temp / rho
# Temp = 273.15 K, rho = 100 kPa, Loschmidt constant NA / Vm
# 22.710947 x 10 ^ -3 m3 mol-1

# Mohr reference
# Molar volume of ideal gas Ru * Temp / rho
# Temp = 273.15 K, rho = 101.325 kPa, Loschmidt constant NA / Vm
# 22.413962 x 10 ^ -3 m3 mol-1

# Mohr reference
# Molar gas constant Ru 8.314 4598(48) J mol-1 K-1

# DG Reference
# the universal gas constant Ru = 0.08205746 m^3 atm / kg-mole K

# Mohr reference
# Standard-state pressure 100 kPa Exact
# Standard atmosphere 101.325 kPa Exact

# Andreas reference
# Molecular weight of air (= 28.9644 × 10-3 kg mol-1)


# standard acceleration of gravity (g)
gc <- 9.80665 # m / s^2 (NIST reference)


# obtain the chemical formulas for the contaminant 2 vector
formula <- chemwiki[chemwiki$CAS %inorder% cas, "Molecular Formula"]

# return formula as a vector
formula <- formula[, `Molecular Formula`]


# get the molecular mass for each of the contaminants and water
gmw <- chemwiki[chemwiki$CAS %inorder% cas, "Average Mass"]

# return gmw as a vector
gmw <- gmw[, `Average Mass`]


# get the mass of water
H2O <- chemwiki[`Substance Name` == "Water", "Average Mass"]

# return H2O as a vectors
H2O <- H2O[, `Average Mass`]



# the universal gas constant
Ru <- 0.08205746 # m^3 atm / kg mole K (DG reference) / atm*m^3*kg^-1*mol^-1*K^-1 # units only for Ru

Ru_use <- 8.314 # J/K·mol s[Used for calculating the density of water]




# Temp_unit
if (Temp_unit == "SI") {

# create a numeric vector with the units of degrees Celsius
T_C <- set_units(Temp, "degree_C")

# create a numeric vector to convert from degrees Celsius to Kelvin
T_K <- T_C


# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)

T_Knum <- drop_units(T_K)

T_Cnum <- drop_units(T_C)


} else if (Temp_unit == "Eng") {

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

T_Knum <- drop_units(T_K)

T_Cnum <- drop_units(T_C)


} else if (Temp_unit == "Absolute") {

T_K <- Temp

# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)

# create a numeric vector to convert from degrees Fahrenheit to degrees Celsius
T_C <- T_K

# create a numeric vector with the units of degrees Celsius
units(T_C) <- make_units(degree_C)

T_Knum <- drop_units(T_K)

T_Cnum <- drop_units(T_C)

}



# Water
# liquid density of Water at the minimum Temperature Temp (kg/m^3)
rhoL <- density_water(T_Knum, units = "Absolute")


# liquid viscosity of Water at the minimum Temperature Temp (kg/m s)
muL <- dyn_visc_water(T_Knum, units = "Absolute")


# liquid surface tension of Water at the minimum Temperature Temp (kg/s^2)
S <- surf_tens_water(T_Knum, units = "Absolute")


# molar density of Water at the minimum Temperature Temp (kg mole/m^3)
C0 <- rhoL / H2O


# Mole water
# Assume 100 L of H2O
# 100 L H2O * (1 m^3 H2O/1000 L H2O) * (1000 kg H2O/1 m^3 H2O) * (1 kg mole H2O/18.01528 kg H20) = 5.55084350617920 kg mole H2O

Mole_Water <- 100 * (1 /1000) * (1000/1) / H2O # kg*mole


# Air
# Molecular weight of air (kg/mol)
Ma <- 28.9644 * 10 ^ -3 # Andreas reference


# Air compressibility factor (Z)
Z_factor <- 0.8777 



# atmospheric pressure (atmosphere)
P_atm <- set_units(P_atm, "atm")

P_Pa <- P_atm

P_Pa <- set_units(P_Pa, "Pa")

P_Pa <- drop_units(P_Pa)

P_atm <- drop_units(P_atm)





# calculating the density of air for the given minimum Temperature
# Bengtson reference
air_crit_T <- -140.5 # C

air_crit_P <- 37.25 # atm

# ideal gas law equation is only valid where the assertions above are valid (critical temperature and pressure)
# gas (air) density at the minimum Temperature Temp and pTe (kg/m^3)

if (air_crit_T < T_Cnum & air_crit_P > P_atm) {

rhoG <- (Ma * P_Pa) / (Z_factor * Ru_use * T_Knum)


} else {

assert_that(air_crit_T < T_Cnum, msg = "The air temperature is less than the critical temperature of -140.5 `C`. Please try again.")
# only process with air temperature values greater than the critical temperature and provide a stop warning if it's note true

assert_that(air_crit_P > P_atm, msg = "The air pressure is greater than the critical air pressure of 37.25 atm. Please try again.")
# only process with air pressure values less than the critical pressure and provide a stop warning if it's note true)

}



# calculating the viscosity of air for the given minimum Temperature
TStar <- 132.5 # K

Tr <- T_Knum / TStar

rhoStar <- 314.3 # kg/m^3

H <- 6.16090 * 10 ^ -6 # Pa-s

A1 <- 0.128517

A0_5 <- 2.60661

A0 <- -1.00000

Aneg1 <- -0.709661

Aneg2 <- 0.662534

Aneg3 <- -0.197846

Aneg4 <- 0.00770147

Aneg <- c(A0, Aneg1, Aneg2, Aneg3, Aneg4)

Apos <- c(A0, A0_5, A1)

B1 <- 0.465601

B2 <- 1.26469

B3 <- -0.511425

B4 <- 0.274600

B <- c(B1, B2, B3, B4)


i <- 0:-4

nuTr <- (A1 * Tr) + (A0_5 * Tr ^ 0.5) + sum(Aneg * Tr ^ i)

rhor <- rhoG / rhoStar


j <- 1:4

delta_nurhor <- sum(B * rhor ^ j)


# gas (air) viscosity at the minimum Temperature Temp and pTe (kg/m s)
muG <- H * (nuTr + delta_nurhor) # air viscosity with units of kg/m*s

# old formulation (# Andreas reference)
# muG <- set_units(((1.458 * 10 ^ -6 * T_Knum ^ (3 / 2)) / (T_Knum + 110.4)), kg/m * s)


# D-2. Develop the Design Basis.

# a. Characterize the influent conditions and effluent requirements, including RI/FS data + total organics + background inorganics and minimum water temperature.


# set units for contamin parameters

DL <- DL # m^2/s

DG <- DG # m^2/s

Ha <- Ha # atm/mole/mole


# xai = kg mole / kg mole water
# L = kg mole / m^2 sec
# G = kg mole / m^2 sec


# Table D-1 Contaminants

contamin <- data.table(Contaminant = contam2, Formula = formula, GMW = gmw, CASNumber = cas, Ha = Ha, DL = DL, DG = DG)
setnames(contamin, 3:7, c("GMW (kg/kg-mole)", "CAS Number", "Ha (atm/mole/mole)", "Liquid Diffusivity (m^2/s)", "Gas Diffusivity (m^2/s)"))
setkey(contamin, "Contaminant")


# Round the numeric values to 2 decimal places
cols_contamin <- c("GMW (kg/kg-mole)", "Ha (atm/mole/mole)")

for (col in cols_contamin) {

idx1 <- which(!is.na(contamin[[col]]))

data.table::set(contamin, i = idx1, j = col, value = round_r3(contamin[[col]][idx1], d = 2))
}




# d. Construct a contaminant material balance for the stripping system.
contamin1_line1 <- "Total VOCs"

Cai_line1 <- NA_real_

Cae_line1 <- NA_real_


removal <- data.table(Contaminant = c(contamin1_line1, contam1), Influent = c(Cai_line1, Cai), Effluent = c(Cae_line1, Cae))
removal[, Removal := ((Influent - Effluent) / Influent) * 100]
set(removal, i = 1L, j = 2L, value = sum(removal[, Influent], na.rm = TRUE))


if (nrow(removal) == 2) {

set(removal, i = 2L, j = "xai", value = (removal[2, Influent] / contamin$"GMW (kg/kg-mole)") / Mole_Water / 10)
set(removal, i = 2L, j = "xae", value = (removal[2, Effluent] / contamin$"GMW (kg/kg-mole)") / Mole_Water / 10)
setnames(removal, c(2:ncol(removal)), c("Influent Concentration (ug/L), Cai", "Effluent Standard Concentration (ug/L), Cae", "Removal Requirement (%)", "xai (mole/mole)", "xae (mole/mole)"))

} else {

set(removal, i = 2L:nrow(removal), j = "xai", value = (removal[2:nrow(removal), Influent] / contamin$"GMW (kg/kg-mole)") / Mole_Water / 10)
set(removal, i = 2L:nrow(removal), j = "xae", value = (removal[2:nrow(removal), Effluent] / contamin$"GMW (kg/kg-mole)") / Mole_Water / 10)
setnames(removal, c(2:ncol(removal)), c("Influent Concentration (ug/L), Cai", "Effluent Standard Concentration (ug/L), Cae", "Removal Requirement (%)", "xai (mole/mole)", "xae (mole/mole)"))

}



# Round the numeric values to 1 decimal place
cols_removal1 <- "Removal Requirement (%)"

for (col in cols_removal1) {

idx2 <- which(!is.na(removal[[col]]))

data.table::set(removal, i = idx2, j = col, value = round_r3(removal[[col]][idx2], d = 1))
}



# Round the numeric values to 5 decimal places
cols_removal2 <- c("xai (mole/mole)", "xae (mole/mole)")

for (col in cols_removal2) {

idx3 <- which(!is.na(removal[[col]]))

data.table::set(removal, i = idx3, j = col, value = round_r3(removal[[col]][idx3], d = 5))
}






# D-5. Calculate the Minimum Gas Flow. Determine Gmin and the critical contaminant.

removal_use <- removal[-1, ]

setkey(removal_use, "Contaminant")

gas <- removal_use[contamin]

gas[, Diff := (gas$"Influent Concentration (ug/L), Cai" - gas$"Effluent Standard Concentration (ug/L), Cae") / gas$"Influent Concentration (ug/L), Cai"]

gas[, H_a := gas$"Ha (atm/mole/mole)" / (C0 * Ru * T_Knum)]

gas[, QGmin_QL := Diff / H_a]

max_contamin <- gas[which.max(gas$QGmin_QL), "Contaminant"][[1]]

DL <- gas[which.max(gas$QGmin_QL), "Liquid Diffusivity (m^2/s)"][[1]]

DG <- gas[which.max(gas$QGmin_QL), "Gas Diffusivity (m^2/s)"][[1]]

QGmin_QL <- gas[which.max(gas$QGmin_QL), QGmin_QL][[1]]

H_a <- gas[which.max(gas$QGmin_QL), H_a][[1]]

Cai <- gas[which.max(gas$QGmin_QL), gas$"Influent Concentration (ug/L), Cai"][[1]]

Cae <- gas[which.max(gas$QGmin_QL), gas$"Effluent Standard Concentration (ug/L), Cae"][[1]]

# change the column names
setnames(gas, c("Diff", "H_a", "QGmin_QL"), c("(Cai - Cae) / Cai", "H'a", "QGmin/QL (m^3 / m^3)"))



# Round the numeric values to 4 decimal places
cols_gas1 <- c("(Cai - Cae) / Cai", "H'a")

for (col in cols_gas1) {

idx4 <- which(!is.na(gas[[col]]))

data.table::set(gas, i = idx4, j = col, value = round_r3(gas[[col]][idx4], d = 4))
}



# Round the numeric values to 3 decimal places
cols_gas2 <- "QGmin/QL (m^3 / m^3)"

for (col in cols_gas2) {

idx5 <- which(!is.na(gas[[col]]))

data.table::set(gas, i = idx5, j = col, value = round_r3(gas[[col]][idx5], d = 3))
}









# keep some units in procedure to check for conversion errors, etc.

# Procedure

# D-3. Determine the Column Diameters

# Q sustained pumping rate (gallons per minute or gpm)
# loading stripper surface loading (gpm/ft^2)

Q <- set_units(Q, "gallon/min")

loading <- set_units(loading, "gallon/min/ft^2")

Area <- Q / loading # length ^ 2


# b. Divide the area by the number of strippers

a <- Area / ns # length ^ 2 / stripper

# c.
d <- sqrt(4 * a / pi)


# PCA Series Packed Column Air Strippers table
pca_pcas <- data.table(ModelNumber = c("PCA-1.5", "PCA-2", "PCA-3", "PCA-4", "PCA-5", "PCA-6", "PCA-8", "PCA-10", "PCA-12"), LiquidFlowRangeLow = c(1, 5, 10, 25, 40, 60, 100, 160, 225), LiquidFlowRangeHigh = c(50, 100, 250, 450, 700, 1000, 1800, 2700, 4000), TowerDiameters = c(1.5, 2:6, seq(8, 12, by = 2)), Height = c(43, 43, 44, 46, 47, 47, 48, 50, 51), PackingVolume = c(50, 100, 220, 390, 610, 870, 1550, 2430, 3490), Standardsumpholdingcapacity = c(80, 140, 317, 564, 881, 1269, 2256, 3525, 5076), LoadedWeight = c(562, 1020, 1825, 2913, 4031, 5302, 9145, 12990, 16850), OperatingWeight = c(1350, 2800, 5800, 10000, 15000, 21200, 37500, 57000, 74600))
setnames(pca_pcas, c("Model Number", "Liquid Flow Range (Low), gpm", "Liquid Flow Range (High), gpm", "Tower Diameters, ft", "Height, ft", "Packing Volume (for 30' packing height", "Standard sump holding capacity, gal", "Loaded Weight, lbs", "Operating Weight, lbs"))


# d. Bracket the calculated diameters with the nearest standard diameters.

dcol <- "Tower Diameters, ft"

# add 2.5 ft as an acceair_results standard diameters
d_col <- c(pca_pcas[, ..dcol][[1]], 2.5)

d_colDT <- data.table(d_col, value = d_col)

setkey(d_colDT, d_col)

# binary search and "roll" to the nearest neighbour
d_use <- d_colDT[d_colDT[J(d), .I, roll = "nearest", by = .EACHI]$I, value]



# D-4. Find a Suitable Packing.
# b. Find the area of the standard diameters strippers.

a_standard <- (pi * (d_use) ^ 2) / 4

units(a_standard) <- make_units(ft^2)

a_standard_SI <- a_standard

a_standard_SI <- set_units(a_standard_SI, "m^2")


# d. Calculate the surface hydraulic loading Q/A and compare the loading with various packing manufacturers’ recommendations.

QL <- Q / ns

QL_SI <- QL

QL_SI <- set_units(QL_SI, "m^3/s")

Q_SI <- QL_SI



VL <- QL / a_standard

VL_SI <- VL

VL_SI <- set_units(VL_SI, "m/s")

VL_SI <- drop_units(VL_SI)

Q_SI <- drop_units(Q_SI)




# e. Liquid mass velocity is as follows:
L <- round_r3(rhoL * VL_SI, d = 2) # kg / m^2 * s


# set units for packing materials

# dP_unit
if (dP_unit == "inch") {

# create a numeric vector with the units of inch
dP <- set_units(dP, "inch")


# create a numeric vector to convert from inch to meters
dP <- dP


# create a numeric vector with the units of meters
units(dP) <- make_units(m)

dP <- drop_units(dP)


} else if (dP_unit == "mm") {

# create a numeric vector with the units of mm
dP <- set_units(dP, "mm")


# create a numeric vector to convert from mm to meters
dP <- dP


# create a numeric vector with the units of meters
units(dP) <- make_units(m)

dP <- drop_units(dP)

}




# at_unit
if (at_unit == "ft^2/ft^3") {

# create a numeric vector with the units of ft^2/ft^3
at <- set_units(at, "ft^2/ft^3")


# create a numeric vector to convert from ft^2/ft^3 to m^2/m^3
at <- at


# create a numeric vector with the units of m^2/m^3
units(at) <- make_units(m^2/m^3)

at <- drop_units(at)


} else if (at_unit == "m^2/m^3") {

# copy the numeric vector at as it's already in the correct units
at <- at

}



# Sc_unit
if (Sc_unit == "slug/s^2") {

# create a numeric vector with the units of slug/s^2
Sc <- set_units(Sc, "slug/s^2")


# create a numeric vector to convert from slug/s^2 to kg/s^2
Sc <- Sc


# create a numeric vector with the units of kg/s^2
units(Sc) <- make_units(kg/s^2)

Sc <- drop_units(Sc)


} else if (at_unit == "kg/s^2") {

# copy the numeric vector Sc as it's already in the correct units
Sc <- Sc

}




# a. Calculate the dimensionless numbers

NRe <- (1 / at) * (VL_SI * rhoL / muL)

NFr <- at * (VL_SI ^ 2 / gc)

NWe <- (1 / at) * VL_SI ^ 2 * rhoL / gc * S

NSc <- muL / rhoL * DL



#  D-6. Calculate the Mass Transfer Rate

# i. Calculate the wetted area of the packing, a w from the dimensionless relation

aw_at <- 1 - exp(-1.45 * (Sc / S) ^ 0.75 * (NRe ^ 0.1 * NFr ^ -0.05 * NWe ^ 0.2)) # value as a percent

aw <- aw_at * at # m^2 / m^3


# k. Calculate the liquid phase mass transfer coefficient, Onda KL from the following relationship:

KL <- (0.0051 * ((VL_SI * rhoL) / (aw * muL)) ^ (2 / 3) * ((muL / (rhoL * DL)) ^ -0.5 * (at * dP) ^ 0.4)) / ((rhoL / (muL * gc)) ^ (1 / 3))


VGmin <- QGmin_QL * VL_SI

R_possible <- seq(2, 5, by = 0.5)

R_1 <- 2.5 # air pollution control is required

R_2 <- 4.5 # air pollution control is not required


VG <- R * VGmin

G <- round_r3(VG * rhoG, d = 4)

QGmin <- QGmin_QL * Q_SI

Gmin <- (QGmin * 1000) / 22.4140 # which value should be used

KG <- 5.23 * (G / (at * muG)) ^ 0.7 * (muG / (rhoG * DG)) ^ (1/3) * (at * dP) ^ -2.0 * (at * DG)

KLA <- 1 / ((1 / (H_a * KG * aw)) + (1 / (KL * aw)))

HTU <- VL_SI / KLA

HTU <- set_units(HTU, "m")

HTU_ft <- HTU

HTU_ft <- set_units(HTU, "ft")

HTU_ft <- round_r3(drop_units(HTU_ft), d = 2)

HTU <- round_r3(drop_units(HTU), d = 2)



NTU <- round_r3((R / (R - 1)) * log((((Cai / Cae) * (R - 1)) + 1) / R), d = 2)

Z <- NTU * HTU


Z <- set_units(Z, "m")

Z_ft <- Z

Z_ft <- set_units(Z, "ft")

Z_ft <- round_r3(drop_units(Z_ft), d = 2)

Z <- round_r3(drop_units(Z), d = 2)



# Water
Water <- Q_SI



# Air
Air <- (R * Gmin * 22.4140) / 1000


# Air to Water Ratio
A_W <- round_r3(Air / Water, d = 2)



# to print the final Air Results Table & for all 3 additional tables to be printed
if (contaminants_table == 1 & removal_requirements_table == 1 & critical_contaminant_table == 1) {

# for the Contaminants Table
col.names1 <- names(contamin)

# code block below modified from data.table function
setattr(contamin, "col.names1", setnames(contamin, col.names1))
setattr(contamin, "class", c("data.table", "data.frame"))
contamin



# for the Removal Requirements
col.names2 <- names(removal)

# code block below modified from data.table function
setattr(removal, "col.names2", setnames(removal, col.names2))
setattr(removal, "class", c("data.table", "data.frame"))
removal



# for the Critical Contaminant
col.names3 <- names(gas)

# code block below modified from data.table function
setattr(gas, "col.names3", setnames(gas, col.names3))
setattr(gas, "class", c("data.table", "data.frame"))
gas



# print the final Air Results Table
air_results <- data.table(cc = max_contamin, L = L, G = G, HTU = HTU, HTU_ft = HTU_ft, NTU = NTU, Z = Z, Z_ft = Z_ft, A_W = A_W)
setnames(air_results, c("Critical Contaminant", "Molar Liquid (Water) Flow per unit of Stripper Cross-Sectional Area (kg mole/m^2 s)", "Molar Gas (Air) flow per unit of Stripper Cross-Sectional Area (kg mole/m^2 s)", "Height of Transfer Unit (HTU) [m]", "Height of Transfer Unit (HTU) [ft]", "Number of Transfer Units (NTU)", "Packing Depth (m)", "Packing Depth (ft)", "Air to Water Ratio"))
col.names <- names(air_results)

# code block below modified from data.table function
setattr(air_results, "col.names", setnames(air_results, col.names))
setattr(air_results, "class", c("data.table", "data.frame"))
air_results

return(list(contamin, removal, gas, air_results))

}



# for the Contaminants Table and the Removal Requirements Table to be printed in addition to the final Air Results Table
if (contaminants_table == 1 & removal_requirements_table == 1 & critical_contaminant_table == 0) {

# for the Contaminants Table
col.names1 <- names(contamin)

# code block below modified from data.table function
setattr(contamin, "col.names1", setnames(contamin, col.names1))
setattr(contamin, "class", c("data.table", "data.frame"))
contamin



# for the Removal Requirements
col.names2 <- names(removal)

# code block below modified from data.table function
setattr(removal, "col.names2", setnames(removal, col.names2))
setattr(removal, "class", c("data.table", "data.frame"))
removal


# print the final Air Results Table
air_results <- data.table(cc = max_contamin, L = L, G = G, HTU = HTU, HTU_ft = HTU_ft, NTU = NTU, Z = Z, Z_ft = Z_ft, A_W = A_W)
setnames(air_results, c("Critical Contaminant", "Molar Liquid (Water) Flow per unit of Stripper Cross-Sectional Area (kg mole/m^2 s)", "Molar Gas (Air) flow per unit of Stripper Cross-Sectional Area (kg mole/m^2 s)", "Height of Transfer Unit (HTU) [m]", "Height of Transfer Unit (HTU) [ft]", "Number of Transfer Units (NTU)", "Packing Depth (m)", "Packing Depth (ft)", "Air to Water Ratio"))
col.names <- names(air_results)


# code block below modified from data.table function
setattr(air_results, "col.names", setnames(air_results, col.names))
setattr(air_results, "class", c("data.table", "data.frame"))
air_results

return(list(contamin, removal, air_results))

}




# for the Contaminants Table and the Removal Requirements Table to be printed in addition to the final Air Results Table
if (contaminants_table == 1 & removal_requirements_table == 0 & critical_contaminant_table == 1) {

# for the Contaminants Table
col.names1 <- names(contamin)

# code block below modified from data.table function
setattr(contamin, "col.names1", setnames(contamin, col.names1))
setattr(contamin, "class", c("data.table", "data.frame"))
contamin


# for the Critical Contaminant
col.names3 <- names(gas)

# code block below modified from data.table function
setattr(gas, "col.names3", setnames(gas, col.names3))
setattr(gas, "class", c("data.table", "data.frame"))
gas



# print the final Air Results Table
air_results <- data.table(cc = max_contamin, L = L, G = G, HTU = HTU, HTU_ft = HTU_ft, NTU = NTU, Z = Z, Z_ft = Z_ft, A_W = A_W)
setnames(air_results, c("Critical Contaminant", "Molar Liquid (Water) Flow per unit of Stripper Cross-Sectional Area (kg mole/m^2 s)", "Molar Gas (Air) flow per unit of Stripper Cross-Sectional Area (kg mole/m^2 s)", "Height of Transfer Unit (HTU) [m]", "Height of Transfer Unit (HTU) [ft]", "Number of Transfer Units (NTU)", "Packing Depth (m)", "Packing Depth (ft)", "Air to Water Ratio"))
col.names <- names(air_results)


# code block below modified from data.table function
setattr(air_results, "col.names", setnames(air_results, col.names))
setattr(air_results, "class", c("data.table", "data.frame"))
air_results

return(list(contamin, gas, air_results))

}




# for the Contaminants Table and the Removal Requirements Table to be printed in addition to the final Air Results Table
if (contaminants_table == 0 & removal_requirements_table == 1 & critical_contaminant_table == 1) {


# for the Removal Requirements
col.names2 <- names(removal)

# code block below modified from data.table function
setattr(removal, "col.names2", setnames(removal, col.names2))
setattr(removal, "class", c("data.table", "data.frame"))
removal



# for the Critical Contaminant
col.names3 <- names(gas)

# code block below modified from data.table function
setattr(gas, "col.names3", setnames(gas, col.names3))
setattr(gas, "class", c("data.table", "data.frame"))
gas



# print the final Air Results Table
air_results <- data.table(cc = max_contamin, L = L, G = G, HTU = HTU, HTU_ft = HTU_ft, NTU = NTU, Z = Z, Z_ft = Z_ft, A_W = A_W)
setnames(air_results, c("Critical Contaminant", "Molar Liquid (Water) Flow per unit of Stripper Cross-Sectional Area (kg mole/m^2 s)", "Molar Gas (Air) flow per unit of Stripper Cross-Sectional Area (kg mole/m^2 s)", "Height of Transfer Unit (HTU) [m]", "Height of Transfer Unit (HTU) [ft]", "Number of Transfer Units (NTU)", "Packing Depth (m)", "Packing Depth (ft)", "Air to Water Ratio"))
col.names <- names(air_results)


# code block below modified from data.table function
setattr(air_results, "col.names", setnames(air_results, col.names))
setattr(air_results, "class", c("data.table", "data.frame"))
air_results

return(list(removal, gas, air_results))

}



# for the Contaminants Table
if (contaminants_table == 1) {

col.names1 <- names(contamin)

# code block below modified from data.table function
setattr(contamin, "col.names1", setnames(contamin, col.names1))
setattr(contamin, "class", c("data.table", "data.frame"))
contamin



# print the final Air Results Table
air_results <- data.table(cc = max_contamin, L = L, G = G, HTU = HTU, HTU_ft = HTU_ft, NTU = NTU, Z = Z, Z_ft = Z_ft, A_W = A_W)
setnames(air_results, c("Critical Contaminant", "Molar Liquid (Water) Flow per unit of Stripper Cross-Sectional Area (kg mole/m^2 s)", "Molar Gas (Air) flow per unit of Stripper Cross-Sectional Area (kg mole/m^2 s)", "Height of Transfer Unit (HTU) [m]", "Height of Transfer Unit (HTU) [ft]", "Number of Transfer Units (NTU)", "Packing Depth (m)", "Packing Depth (ft)", "Air to Water Ratio"))
col.names <- names(air_results)

# code block below modified from data.table function
setattr(air_results, "col.names", setnames(air_results, col.names))
setattr(air_results, "class", c("data.table", "data.frame"))
air_results

return(list(contamin, air_results))

}




# for the Removal Requirements
if (removal_requirements_table == 1) {

col.names2 <- names(removal)


# code block below modified from data.table function
setattr(removal, "col.names2", setnames(removal, col.names2))
setattr(removal, "class", c("data.table", "data.frame"))


# print the final Air Results Table
air_results <- data.table(cc = max_contamin, L = L, G = G, HTU = HTU, HTU_ft = HTU_ft, NTU = NTU, Z = Z, Z_ft = Z_ft, A_W = A_W)
setnames(air_results, c("Critical Contaminant", "Molar Liquid (Water) Flow per unit of Stripper Cross-Sectional Area (kg mole/m^2 s)", "Molar Gas (Air) flow per unit of Stripper Cross-Sectional Area (kg mole/m^2 s)", "Height of Transfer Unit (HTU) [m]", "Height of Transfer Unit (HTU) [ft]", "Number of Transfer Units (NTU)", "Packing Depth (m)", "Packing Depth (ft)", "Air to Water Ratio"))
col.names <- names(air_results)


# code block below modified from data.table function
setattr(air_results, "col.names", setnames(air_results, col.names))
setattr(air_results, "class", c("data.table", "data.frame"))


return(list(removal, air_results))
}




# for the Critical Contaminant
if (critical_contaminant_table == 1) {

col.names3 <- names(gas)


# code block below modified from data.table function
setattr(gas, "col.names3", setnames(gas, col.names3))
setattr(gas, "class", c("data.table", "data.frame"))



# print the final Air Results Table
air_results <- data.table(cc = max_contamin, L = L, G = G, HTU = HTU, HTU_ft = HTU_ft, NTU = NTU, Z = Z, Z_ft = Z_ft, A_W = A_W)
setnames(air_results, c("Critical Contaminant", "Molar Liquid (Water) Flow per unit of Stripper Cross-Sectional Area (kg mole/m^2 s)", "Molar Gas (Air) flow per unit of Stripper Cross-Sectional Area (kg mole/m^2 s)", "Height of Transfer Unit (HTU) [m]", "Height of Transfer Unit (HTU) [ft]", "Number of Transfer Units (NTU)", "Packing Depth (m)", "Packing Depth (ft)", "Air to Water Ratio"))
col.names <- names(air_results)


# code block below modified from data.table function
setattr(air_results, "col.names", setnames(air_results, col.names))
setattr(air_results, "class", c("data.table", "data.frame"))


return(list(gas, air_results))
}



# print the final Air Results Table
air_results <- data.table(cc = max_contamin, L = L, G = G, HTU = HTU, HTU_ft = HTU_ft, NTU = NTU, Z = Z, Z_ft = Z_ft, A_W = A_W)
setnames(air_results, c("Critical Contaminant", "Molar Liquid (Water) Flow per unit of Stripper Cross-Sectional Area (kg mole/m^2 s)", "Molar Gas (Air) flow per unit of Stripper Cross-Sectional Area (kg mole/m^2 s)", "Height of Transfer Unit (HTU) [m]", "Height of Transfer Unit (HTU) [ft]", "Number of Transfer Units (NTU)", "Packing Depth (m)", "Packing Depth (ft)", "Air to Water Ratio"))
col.names <- names(air_results)


# code block below modified from data.table function
setattr(air_results, "col.names", setnames(air_results, col.names))
setattr(air_results, "class", c("data.table", "data.frame"))
air_results

}
