## ----eval = FALSE, tidy = TRUE------------------------------------------------
#  install.packages(c("install.load", "iemisc", "units", "round"))
#  # install the packages and their dependencies

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------
# load the required packages
install.load::load_package("iemisc",  "units", "round")
# load needed packages using the load_package function from the install.load package (it is assumed that you have already installed these packages)

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

# 60 degrees Fahrenheit water
# new 6 in schedule-40 steel pipe
# determine the Reynolds number


# given the water flow of 750 gal / min
# create a numeric vector with the units of gallons per minute for the volumetric flow rate
Vdot <- set_units(750, gallon/min)
Vdot


# create a numeric vector with the units of cubic feet per second for the volumetric flow rate
Vdot <- Vdot

units(Vdot) <- make_units(ft^3/s)
Vdot



# given temperature of 60 degrees Fahrenheit
# create a numeric vector with the units of degrees Fahrenheit
T_F <- set_units(60, degree_F)


# create a numeric vector to convert from degrees Fahrenheit to Kelvin
T_K <- T_F
T_K

# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)
T_K

# saturated liquid density at 60 degrees Fahrenheit (SI units)
rho_SI <- density_water(drop_units(T_K), units = "Absolute")

rho_SI <- set_units(rho_SI, kg/m^3)
rho_SI


# saturated liquid density at 60 degrees Fahrenheit (US Customary units)
rho_Eng <- density_water(drop_units(T_F), units = "Eng", Eng_units = "lbm/ft^3")

rho_Eng <- set_units(rho_Eng, lb/ft^3) # lbm/ft^3
rho_Eng


# kinematic viscosity at 60 degrees Fahrenheit and density of rho (SI units)
nu_SI <- kin_visc_water(mu = dyn_visc_water(Temp = drop_units(T_K), units = "Absolute"), rho = density_water(Temp = drop_units(T_K), units = "Absolute"), rho_units = "kg/m^3", mu_units = "Pa*s or kg/m/s")

nu_SI <- set_units(nu_SI, m^2/s)
nu_SI



# kinematic viscosity at 60 degrees Fahrenheit and density of rho (US Customary units)
nu_Eng <- kin_visc_water(mu = dyn_visc_water(Temp = drop_units(T_F), units = "Eng", Eng_units = "lbf*s/ft^2"), rho = density_water(Temp = drop_units(T_F), units = "Eng", Eng_units = "lbm/ft^3"), rho_units = "lbm/ft^3", mu_units = "lbf*s/ft^2")

nu_Eng <- set_units(nu_Eng, ft^2/s)
nu_Eng


# absolute or dynamic viscosity at 60 degrees Fahrenheit and density of rho (SI units)
mu_SI <- dyn_visc_water(Temp = drop_units(T_K), units = "Absolute")

mu_SI <- set_units(mu_SI, Pa*s)
mu_SI


# absolute or dynamic viscosity at 60 degrees Fahrenheit and density of rho (US Customary units)
mu_Eng <- dyn_visc_water(Temp = drop_units(T_F), units = "Eng", Eng_units = "lbf*s/ft^2")

mu_Eng <- set_units(mu_Eng, lbf*s/ft^2)
mu_Eng


# create a numeric vector with the units of feet for the given specific roughness
epsilon <- set_units(2e-04, ft)
epsilon


# create a numeric vector with the units of feet for the given internal pipe diameter
Di <- set_units(0.5054, ft)
Di



# relative roughness (dimensionless) of the steel pipe
rel_roughness <- epsilon / Di
rel_roughness


# internal area of the steel pipe
Ai <- Di ^ 2 * pi / 4
Ai


# average velocity of the flowing water
V <- Vdot / Ai
V


# Reynolds number using the kinematic viscosity
Re_nu <- Re2(D = drop_units(Di), V = drop_units(V), nu = drop_units(nu_Eng))
Re_nu


# Reynolds number using the absolute or dynamic viscosity
Re_mu <- Re1(D = drop_units(Di), V = drop_units(V), rho = drop_units(rho_Eng), mu = drop_units(mu_Eng), units = "Eng")
Re_mu


# display Re_nu with scientific notation
format(Re_nu, scientific = TRUE)

# display Re_mu with scientific notation
format(Re_mu, scientific = TRUE)

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------
# given temperature of 22 degrees Celsius
# create a numeric vector with the units of degrees Celsius
T_C <- set_units(22, degree_C)
T_C


# create a numeric vector to convert from degrees Celsius to Kelvin
T_K <- T_C
T_K

# create a numeric vector with the units of Kelvin
units(T_K) <- make_units(K)
T_K


# saturated liquid density at 22 degrees Celsius (SI units)
rho_SI <- density_water(drop_units(T_K), units = "Absolute")

rho_SI <- set_units(rho_SI, kg/m^3)
rho_SI


# kinematic viscosity at 60 degrees Fahrenheit and density of rho (SI units)
nu_SI <- kin_visc_water(mu = dyn_visc_water(Temp = drop_units(T_K), units = "Absolute"), rho = density_water(Temp = drop_units(T_K), units = "Absolute"), rho_units = "kg/m^3", mu_units = "Pa*s or kg/m/s")

nu_SI <- set_units(nu_SI, m^2/s)
nu_SI


# absolute or dynamic viscosity at 60 degrees Fahrenheit and density of rho (SI units)
mu_SI <- dyn_visc_water(Temp = drop_units(T_K), units = "Absolute")

mu_SI <- set_units(mu_SI, Pa*s)
mu_SI



# create a numeric vector with the units of inches for the given internal pipe diameter
Di <- set_units(4, inch)
Di

# create a numeric vector with the units of meters for the given internal pipe diameter
Di <- Di
Di

units(Di) <- make_units(m)
Di


# given the mass flow rate of 0.765 kg/s (rounded in HTML)
# create a numeric vector with the units of kilograms per second for the mass flow rate
G <- set_units(0.76486004, kg/s)
G


# display the Reynolds number
re3 <- Re3(D = drop_units(Di), G = drop_units(G), mu = drop_units(mu_SI), units = "SI")
re3

# display the Reynolds number from Re3 with scientific notation
format(re3, scientific = TRUE)

