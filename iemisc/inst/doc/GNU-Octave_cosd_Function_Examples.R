## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

library("iemisc")

# Examples from GNU Octave cosd

cosd(c(0, 180, 360))


cosd(c(90, 270, 45))



try(cosd(pi * seq(0, 80, by = 10) / 180))
# gives error message since the computed value is in radians rather than degrees


cos(pi * seq(0, 80, by = 10) / 180)
# this is correct since `cos` expects the angle in radians

try(cosd(seq(0, 80, by = 10) * 180 / pi))
# converts angles in radians to degrees; however, it will still receive an error message with this current implementation. You can use the work-around below:

xx <- seq(0, 80, by = 10) * 180 / pi

cosd(xx)


try(cos(seq(0, 80, by = 10) * 180 / pi))
# converts angles in radians to degrees; however, this is incorrect since `cos` expects the angle in radians and not degrees


cosd(90)

cos(pi/2)

