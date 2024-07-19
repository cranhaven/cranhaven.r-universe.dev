## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

# Practice Problem 14.12 from Mott (page 392)

#'
install.load::load_package("iemisc", "iemiscdata")
#'
y <- Manningcircy(y_d = 0.5, d = 6, units = "Eng")
#'
# See npartfull in iemiscdata for the Manning's n table that the
# following example uses
# Use the normal Manning's n value for 1) Corrugated Metal, 2) Stormdrain.
#'
data(npartfull)
#'
# We are using the culvert as a stormdrain in this problem
nlocation <- grep("Stormdrain", npartfull$"Type of Conduit and Description")
#'
n <- as.numeric(npartfull[nlocation, 3]) # 3 for column 3 - Normal n
#'
Manningcirc(d = 6, Sf = 1 / 500, n = n, y = y$y, units = "Eng")
# d = 6 ft, Sf = 1 / 500 ft/ft, n = 0.024, y = 3 ft, units = "Eng"
# This will solve for Q since it is missing and Q will be in ft^3/s
#'
#'
#'
# Example Problem 14.2 from Mott (pages 377-378)
#'
install.load::load_package("iemisc", "iemiscdata")
#'
y <- Manningcircy(y_d = 0.5, d = 200/1000, units = "SI")
#'
# See npartfull in iemiscdata for the Manning's n table that the
# following example uses
# Use the normal Manning's n value for 1) Clay, 2) Common drainage tile.
#'
data(npartfull)
#'
nlocation <- grep("Common drainage tile",
npartfull$"Type of Conduit and Description")
#'
n <- as.numeric(npartfull[nlocation, 3]) # 3 for column 3 - Normal n
#'
Manningcirc(Sf = 1/1000, n = n, y = y$y, d = 200/1000, units = "SI")
# Sf = 1/1000 m/m, n = 0.013, y = 0.1 m, d = 200/1000 m, units = SI units
# This will solve for Q since it is missing and Q will be in m^3/s
#'
#'
#'
# Example 4.1 from Sturm (pages 124-125)
#'
install.load::load_package("iemisc", "iemiscdata")
#'
Manningcircy(y_d = 0.8, d = 2, units = "Eng")
#'
y <- Manningcircy(y_d = 0.8, d = 2, units = "Eng")
# defines all list values within the object named y
#'
y$y # gives the value of y
#'
#'
#'
# Modified Exercise 4.1 from Sturm (page 153)
#'
install.load::load_package("iemisc", "iemiscdata")
#'
# Note: The Q in Exercise 4.1 is actually found using the Chezy equation,
# this is a modification of that problem
# See nchannel in iemiscdata for the Manning's n table that the
# following example uses
# Use the normal Manning's n value for 1) Natural streams - minor streams
# (top width at floodstage < 100 ft), 2) Mountain streams, no vegetation
# in channel, banks usually steep, trees and brush along banks submerged at
# high stages and 3) bottom: gravels, cobbles, and few boulders.
#'
data(nchannel)
#'
nlocation <- grep("bottom: gravels, cobbles, and few boulders",
nchannel$"Type of Channel and Description")
#'
n <- as.numeric(nchannel[nlocation, 3]) # 3 for column 3 - Normal n
#'
Manningcirc(Sf = 0.002, n = n, y = y$y, d = 2, units = "Eng")
# Sf = 0.002 ft/ft, n = 0.04, y = 1.6 ft, d = 2 ft, units = English units
# This will solve for Q since it is missing and Q will be in ft^3/s
#'
#'
#'
# Modified Exercise 4.5 from Sturm (page 154)
#'
install.load::load_package("iemisc", "units")
#'
#'
# create a numeric vector with the units of feet
yeng <- set_units(y$y, ft)


# create a numeric vector to convert from feet to meters
ysi <- yeng


# create a numeric vector with the units of meters
units(ysi) <- make_units(m)


# create a numeric vector with the units of feet
deng <- set_units(2, ft)


# create a numeric vector to convert from feet to meters
dsi <- deng


# create a numeric vector with the units of meters
units(dsi) <- make_units(m)
#'
Manningcirc(Sf = 0.022, n = 0.023, y = drop_units(ysi), d = drop_units(dsi), units = "SI")
# Sf = 0.022 m/m, n = 0.023, y = 0.48768 m, d = 0.6096 m, units = SI units
# This will solve for Q since it is missing and Q will be in m^3/s
#'
#'

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

#'
install.load::load_package("iemisc", "iemiscdata")
#'
# Practice Problem 14.19 from Mott (page 392)
# See nchannel in iemiscdata for the Manning's n table that the following
# example uses
# Use the minimum Manning's n value for 1) Natural streams - minor streams
# (top width at floodstage < 100 ft) Lined or Constructed Channels,
# 3) Concrete and 4) float finish.
#'
data(nchannel)
#'
nlocation <- grep("float finish", nchannel$"Type of Channel and Description")
#'
n <- as.numeric(nchannel[nlocation, 3][1]) # 3 for column 3 - Normal n
#'
tt <- Manningtrap(y = 1.5, b = 3, m = 3/2, Sf = 0.1/100, n = n,
units = "SI", type = "symmetrical", output = "list")
# y = 1.5 m, b = 3 m, m = 3/2, Sf = 0.1/100 m/m, n = 0.023, units = SI
# units
# This will solve for Q since it is missing and Q will be in m^3/s
#'
tt$Q # only returns Q
#'
tt # returns all results
#'
#'

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

install.load::load_package("iemisc", "iemiscdata")
#'
# Practice Problem 14.19 from Mott (page 392)
# See nchannel in iemiscdata for the Manning's n table that the following
# example uses
# Use the minimum Manning's n value for 1) Natural streams - minor streams
# (top width at floodstage < 100 ft) Lined or Constructed Channels,
# 3) Concrete and 4) float finish.
#'
data(nchannel)
#'
nlocationc <- grep("float finish", nchannel$"Type of Channel and Description")
#'
nc <- as.numeric(nchannel[nlocationc, 3][1]) # 3 for column 3 - Normal n
#'
ttc <- Manningtrap_critical(y = 1.5, b = 3, m = 3/2, Sf = 0.1/100, n = nc,
units = "SI", type = "symmetrical", critical = "accurate", output = "list")
# y = 1.5 m, b = 3 m, m = 3/2, Sf = 0.1/100 m/m, n = 0.023, units = SI
# units
# This will solve for Q since it is missing and Q will be in m^3/s
#'
ttc$Q # only returns Q
#'
ttc # returns all results
#'
#'

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------


install.load::load_package("iemisc", "iemiscdata")
#'
# Practice Problem 14.41 from Mott (page 393)
# See nchannel in iemiscdata for the Manning's n table that the
# following example uses
# Use the normal Manning's n value for 1) Natural streams - minor streams
# (top width at floodstage < 100 ft), 2) Excavated or Dredged Channels, 3)
# Earth, straight, and uniform, 4) clean, recently completed.
#'
data(nchannel)
#'
nlocation <- grep("clean, recently completed",
nchannel$"Type of Channel and Description")
#'
n <- as.numeric(nchannel[nlocation, 3]) # 3 for column 3 - Normal n
#'
Manningtri(Q = 0.68, m = 1.5, Sf = 0.0023, n = n, units = "Eng")
# Q = 0.68 cfs, m = 1.5, Sf = 0.002 ft/ft, n = 0.05, units = English units
# This will solve for y since it is missing and y will be in ft
#'

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

install.load::load_package("iemisc", "iemiscdata")
#'
#'
# Example Problem 14.4 from Mott (page 379)
# See nchannel in iemiscdata for the Manning's n table that the following
# example uses
# Use the normal Manning's n value for 1) Natural streams - minor streams
# (top width at floodstage < 100 ft), 2) Lined or Constructed Channels,
# 3) Concrete, and 4) unfinished.
#'
data(nchannel)
#'
nlocation <- grep("unfinished", nchannel$"Type of Channel and Description")
#'
n <- as.numeric(nchannel[nlocation, 3]) # 3 for column 3 - Normal n
#'
Manningrect(Q = 5.75, b = (4.50) ^ (3 / 8), Sf = 1.2/100, n = n, units =
"SI")
# Q = 5.75 m^3/s, b = (4.50) ^ (3 / 8) m, Sf = 1.2 percent m/m, n = 0.017,
# units = SI units
# This will solve for y since it is missing and y will be in m
#'
#'
#'
# Example Problem 14.5 from Mott (pages 379-380)
# See nchannel in iemiscdata for the Manning's n table that the following
# example uses
# Use the normal Manning's n value for 1) Natural streams - minor streams
# (top width at floodstage < 100 ft), 2) Lined or Constructed Channels,
# 3) Concrete, and 4) unfinished.
#'
data(nchannel)
#'
nlocation <- grep("unfinished", nchannel$"Type of Channel and Description")
#'
n <- as.numeric(nchannel[nlocation, 3]) # 3 for column 3 - Normal n
#'
Manningrect(Q = 12, b = 2, Sf = 1.2/100, n = n, units = "SI")
# Q = 12 m^3/s, b = 2 m, Sf = 1.2 percent m/m, n = 0.017, units = SI
# units
# This will solve for y since it is missing and y will be in m
#'
#'

