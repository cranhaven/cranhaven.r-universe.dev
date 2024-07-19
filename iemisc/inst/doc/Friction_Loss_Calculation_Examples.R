## ----eval = FALSE, tidy = TRUE------------------------------------------------
#  install.packages(c("install.load", "iemisc", "data.table", "units", "pander", "pracma"))
#  # install the packages and their dependencies

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------
# load the required packages
install.load::load_package("iemisc", "data.table", "units", "pander")
# load needed packages using the load_package function from the install.load package (it is assumed that you have already installed these packages)


import::from(pracma, newtonRaphson)
# import newtonRaphson from the pracma package

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

Re <- 400000

eps <- 0.004

D <- 1

eps / D

f2 <- f2(eps = eps, D = D, Re = Re); f2

f3 <- f3(eps = eps, D = D, Re = Re); f3

f4 <- f4(eps = eps, D = D, Re = Re); f4

f5 <- f5(eps = eps, D = D, Re = Re); f5

f6 <- f6(eps = eps, D = D, Re = Re); f6

f7 <- f7(eps = eps, D = D, Re = Re); f7

f8 <- f8(eps = eps, D = D, Re = Re); f8


# determine the relative error
acc <- 0.0287

relerror(acc, f2)

relerror(acc, f3)

relerror(acc, f4)

relerror(acc, f5)

relerror(acc, f6)

relerror(acc, f7)

relerror(acc, f8)

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

eps <- 0.00005

D <- 0.0254

Re <- 6000

# f equal to 0.0375 from Microsoft Excel Goal Seek

f2(eps = eps, D = D, Re = Re)

f3(eps = eps, D = D, Re = Re)

f4(eps = eps, D = D, Re = Re)

f5(eps = eps, D = D, Re = Re)

f6(eps = eps, D = D, Re = Re)

f7(eps = eps, D = D, Re = Re)

f8(eps = eps, D = D, Re = Re)

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------
# Please note that the Re2, f2, f3, f4, f5, f6, f7, f8, and the colebrook functions are found within the iemisc R package created by Irucka Embry


# oil
# iron-cast pipe
# find the friction loss -- the head loss is 117 meters


# given the water flow of 0.2 m^3/s
# create a numeric vector with the units of cubic meters per second for the volumetric flow rate
Vdot <- set_units(0.2, m^3/s)
Vdot


# given length of 500 m
# create a numeric vector with the units of meters
L_SI <- set_units(500, m)
L_SI


g_SI <- set_units(9.80665, m/s^2)
g_SI


# given saturated liquid density of oil (SI units)
rho_SI <- set_units(900, kg/m^3)
rho_SI


# given kinematic viscosity of oil (SI units)
nu_SI <- set_units(0.00001, m^2/s)
nu_SI



# create a numeric vector with the units of millimeters for the given specific roughness
epsilon <- set_units(0.26, mm)
epsilon

# create a numeric vector with the units of meters for the given specific roughness
epsilon <- epsilon

units(epsilon) <- make_units(m)
epsilon


# create a numeric vector with the units of millimeters for the given internal pipe diameter
Di <- set_units(200, mm)
Di


# create a numeric vector with the units of meters for the given internal pipe diameter
units(Di) <- make_units(m)
Di


# relative roughness (dimensionless) of the cast iron pipe
rel_roughness <- epsilon / Di
rel_roughness


# internal area of the cast iron pipe
Ai <- Di ^ 2 * pi / 4
Ai


# average velocity of the flowing water
V <- Vdot / Ai
V


# Reynolds number using the kinematic viscosity
Re_SI <- Re2(D = drop_units(Di), V = drop_units(V), nu = drop_units(nu_SI))
Re_SI


# Darcy friction factor (f) for cast iron pipe
# Moody equation
fr2_SI <- f2(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_SI)

# Romeo, et. al. equation
fr3_SI <- f3(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_SI)

# Žarko Ćojbašića and Dejan Brkić equation
fr4_SI <- f4(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_SI)

# Colebrook-White equation
fr5_SI <- f5(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_SI)

# Colebrook-White equation from Didier Clamond
colebrook_SI <- colebrook(Re_SI, K = drop_units(rel_roughness))

# Swamee-Jaine equation
fr6_SI <- f6(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_SI)

# Zigrang-Sylvester equation
fr7_SI <- f7(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_SI)

# Vatankhah equation
fr8_SI <- f8(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_SI)


# friction loss for cast iron pipe
hf_SI1 <- (f2(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_SI) * drop_units(L_SI) * drop_units(V) ^ 2) / (2 * drop_units(Di) * drop_units(g_SI))

hf_SI2 <- (f3(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_SI) * drop_units(L_SI) * drop_units(V) ^ 2) / (2 * drop_units(Di) * drop_units(g_SI))

hf_SI3 <- (f4(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_SI) * drop_units(L_SI) * drop_units(V) ^ 2) / (2 * drop_units(Di) * drop_units(g_SI))

hf_SI4 <- (f5(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_SI) * drop_units(L_SI) * drop_units(V) ^ 2) / (2 * drop_units(Di) * drop_units(g_SI))

hf_SI5 <- (colebrook(Re_SI, K = drop_units(rel_roughness)) * drop_units(L_SI) * drop_units(V) ^ 2) / (2 * drop_units(Di) * drop_units(g_SI))

hf_SI6 <- (f6(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_SI) * drop_units(L_SI) * drop_units(V) ^ 2) / (2 * drop_units(Di) * drop_units(g_SI))

hf_SI7 <- (f7(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_SI) * drop_units(L_SI) * drop_units(V) ^ 2) / (2 * drop_units(Di) * drop_units(g_SI))

hf_SI8 <- (f8(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_SI) * drop_units(L_SI) * drop_units(V) ^ 2) / (2 * drop_units(Di) * drop_units(g_SI))


# result table
result_table_SI <- data.table(V1 = c("Moody equation", "Romeo, et. al. equation", "Žarko Ćojbašića and Dejan Brkić equation", "Colebrook-White equation", "Colebrook-White equation from Didier Clamond", "Swamee-Jaine equation", "Zigrang-Sylvester equation", "Vatankhah equation"), V2 = c(fr2_SI, fr3_SI, fr4_SI, fr5_SI, colebrook_SI, fr6_SI, fr7_SI, fr8_SI), V3 = c(hf_SI1, hf_SI2, hf_SI3, hf_SI4, hf_SI5, hf_SI6, hf_SI7, hf_SI8))

setnames(result_table_SI, c("Darcy friction factor equation", "Darcy friction factor (f) for cast iron pipe", "Friction loss for cast iron pipe over total length"))



pander(result_table_SI)

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------
# Please note that the Re2, f2, f3, f4, f5, f6, f7, f8, and the colebrook functions are found within the iemisc R package created by Irucka Embry


# oil
# cast iron pipe
# find the head loss -- the head loss is 83.7 feet

# given the water flow of 1500 gpm (gal / min)
# create a numeric vector with the units of gallons per minute for the volumetric flow rate
Vdot <- set_units(1500, gallon/min)
Vdot


# create a numeric vector with the units of cubic feet per second for the volumetric flow rate
units(Vdot) <- make_units(ft^3/sec)
Vdot


# given length of 1600 ft
# create a numeric vector with the units of feet
L_Eng <- set_units(1600, ft)
L_Eng


# create a numeric vector for gravity (US Customary units)
g_Eng <- set_units(9.80665 * (3937 / 1200), ft/sec^2)
g_Eng


# given saturated liquid density of oil (US Customary units)
rho_Eng <- set_units(1.75, slug/ft^3)
rho_Eng


# given kinematic viscosity of oil (US Customary units)
nu_Eng <- set_units(1.15e-04, ft^2/sec)
nu_Eng


# create a numeric vector with the units of feet for the given specific roughness
epsilon <- set_units(8.5e-04, ft)
epsilon


# create a numeric vector with the units of inch for the given internal pipe diameter
Di <- set_units(8, inch)
Di


# create a numeric vector with the units of feet for the given internal pipe diameter
units(Di) <- make_units(ft)
Di


# relative roughness (dimensionless) of the cast iron pipe
rel_roughness <- epsilon / Di
rel_roughness


# internal area of the cast iron pipe
Ai <- Di ^ 2 * pi / 4
Ai


# average velocity of the flowing water
V <- Vdot / Ai
V


# Reynolds number using the kinematic viscosity
Re_Eng <- Re2(D = drop_units(Di), V = drop_units(V), nu = drop_units(nu_Eng))
Re_Eng


# Darcy friction factor (f) for cast iron pipe
# Moody equation
fr2_Eng <- f2(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_Eng)

# Romeo, et. al. equation
fr3_Eng <- f3(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_Eng)

# Žarko Ćojbašića and Dejan Brkić equation
fr4_Eng <- f4(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_Eng)

# Colebrook-White equation
fr5_Eng <- f5(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_Eng)

# Colebrook-White equation from Didier Clamond
colebrook_Eng <- colebrook(Re_Eng, K = drop_units(rel_roughness))

# Swamee-Jaine equation
fr6_Eng <- f6(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_Eng)

# Zigrang-Sylvester equation
fr7_Eng <- f7(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_Eng)

# Vatankhah equation
fr8_Eng <- f8(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_Eng)


# friction loss for cast iron pipe
hf_Eng1 <- (f2(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_Eng) * drop_units(L_Eng) * drop_units(V) ^ 2) / (2 * drop_units(Di) * drop_units(g_Eng))

hf_Eng2 <- (f3(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_Eng) * drop_units(L_Eng) * drop_units(V) ^ 2) / (2 * drop_units(Di) * drop_units(g_Eng))

hf_Eng3 <- (f4(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_Eng) * drop_units(L_Eng) * drop_units(V) ^ 2) / (2 * drop_units(Di) * drop_units(g_Eng))

hf_Eng4 <- (f5(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_Eng) * drop_units(L_Eng) * drop_units(V) ^ 2) / (2 * drop_units(Di) * drop_units(g_Eng))

hf_Eng5 <- (colebrook(Re_Eng, K = drop_units(rel_roughness)) * drop_units(L_Eng) * drop_units(V) ^ 2) / (2 * drop_units(Di) * drop_units(g_Eng))

hf_Eng6 <- (f6(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_Eng) * drop_units(L_Eng) * drop_units(V) ^ 2) / (2 * drop_units(Di) * drop_units(g_Eng))

hf_Eng7 <- (f7(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_Eng) * drop_units(L_Eng) * drop_units(V) ^ 2) / (2 * drop_units(Di) * drop_units(g_Eng))

hf_Eng8 <- (f8(eps = drop_units(epsilon), D = drop_units(Di), Re = Re_Eng) * drop_units(L_Eng) * drop_units(V) ^ 2) / (2 * drop_units(Di) * drop_units(g_Eng))


# result table
result_table_Eng <- data.table(V1 = c("Moody equation", "Romeo, et. al. equation", "Žarko Ćojbašića and Dejan Brkić equation", "Colebrook-White equation", "Colebrook-White equation from Didier Clamond", "Swamee-Jaine equation", "Zigrang-Sylvester equation", "Vatankhah equation"), V2 = c(fr2_Eng, fr3_Eng, fr4_Eng, fr5_Eng, colebrook_Eng, fr6_Eng, fr7_Eng, fr8_Eng), V3 = c(hf_Eng1, hf_Eng2, hf_Eng3, hf_Eng4, hf_Eng5, hf_Eng6, hf_Eng7, hf_Eng8))

setnames(result_table_Eng, c("Darcy friction factor equation", "Darcy friction factor (f) for cast iron pipe", "Friction loss for cast iron pipe over total length"))


pander(result_table_Eng)

