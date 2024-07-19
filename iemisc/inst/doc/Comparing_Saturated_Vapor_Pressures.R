## ----eval = FALSE, tidy = TRUE------------------------------------------------
#  install.packages(c("install.load", "iemisc", "units"))
#  # install the packages and their dependencies

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------
# load the required packages
install.load::load_package("iemisc", "units", "pander")
# load needed packages using the load_package function from the install.load package (it is assumed that you have already installed these packages)

import::from(fpCompare, "%==%")

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

install.load::load_package("iemisc", "data.table", "units", "pander")


# reference vapor pressures from the Huang reference
reference <- sort(c(611.655, 2339.32, 7384.94, 19946.4, 47414.5, 101418))

Temp <- sort(c(0.01, seq(from = 20, to = 100, by = 20)))

# hydraulics
hydraulics_svp <- hydraulics::svp(T = Temp, units = "SI")


# iemisc
iemisc_sat_vapor_pressure_huang <- sat_vapor_pressure(Temp = Temp, units = "SI", formula = "Huang")

iemisc_sat_vapor_pressure_buck <- sat_vapor_pressure(Temp = Temp, units = "SI", formula = "Buck")

iemisc_sat_vapor_pressure_iapws <- sat_vapor_pressure(Temp = Temp, units = "SI", formula = "IAPWS")



# aiRthermo

# create a numeric vector with the units of degrees Celsius
T_C <- set_units(Temp, "degree_C")
T_C

# create a numeric vector to convert from degrees Celsius to Kelvin
T_K <- T_C
T_K

# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)

aiRthermo_saturation_pressure_H2O <- aiRthermo::saturation_pressure_H2O(drop_units(T_K))



comparePress <- data.table(Reference_Pressure = reference, Hydraulics_Pressure = hydraulics_svp, Huang_Pressure = iemisc_sat_vapor_pressure_huang, Buck_Pressure = iemisc_sat_vapor_pressure_buck, IAPWS_Pressure = iemisc_sat_vapor_pressure_iapws, aiRthermo_Pressure = aiRthermo_saturation_pressure_H2O)

comparePress[, `:=` (mreHydraulics = mapply(mre, Hydraulics_Pressure, Reference_Pressure) *
100, mreHuang = mapply(mre, Huang_Pressure, Reference_Pressure) * 100,
mreBuck = mapply(mre, Buck_Pressure, Reference_Pressure) * 100, 
mreIAPWS = mapply(mre, IAPWS_Pressure, Reference_Pressure) *
100, mreaiRthermo = mapply(mre, aiRthermo_Pressure, Reference_Pressure) *
100)] # Source 1


# which row(s) has the maximum value
max_row <- pmax(comparePress$mreHydraulics, comparePress$mreHuang, comparePress$mreBuck, comparePress$mreIAPWS, comparePress$mreaiRthermo)

# which row(s) has the minimum value
min_row <- pmin(comparePress$mreHydraulics, comparePress$mreHuang, comparePress$mreBuck, comparePress$mreIAPWS, comparePress$mreaiRthermo)

# which rows are TRUE
max_row2 <- comparePress == max_row


# which rows are TRUE
min_row2 <- comparePress == min_row

comparePress[, max_mre := c(rep("mreaiRthermo", 3), rep( "mreBuck", 3))]

comparePress[, min_mre := c("mreBuck", rep("mreHydraulics / mreHuang", 4), "mreIAPWS")]

setnames(comparePress, c("Reference Pressure (Pa)", "Hydraulics Package Pressure (Pa)", "Huang Pressure (Pa)", "Buck Pressure (Pa)", "IAPWS Pressure (Pa)", "aiRthermo Pressure (Pa)", "MRE % (Hydraulics Package vs. Reference)", "MRE % (Huang vs. Reference)", "MRE % (Buck vs. Reference)", "MRE % (IAPWS vs. Reference)", "MRE % (aiRthermo vs. Reference)", "Maximum MRE % Formula", "Minumum MRE % Formula"))

pander(comparePress)

