## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

library("iemisc")

# Examples from GNU Octave

x <- 23.4
y <- 20
z <- 0

Rem(x, y)

Rem(y, x)

Rem(x, z)

Rem(y, z)

Rem(z, x)

Rem(z, y)

Rem (-1, 3)


# Examples from FreeMat

Rem(18, 12)

Rem(6, 5)

Rem(2 * pi, pi)

Rem(c(1, 3, 5, 2), 2)

Rem(c(9, 3, 2, 0), c(1, 0, 2, 2))

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

library("iemisc")

# Examples from FreeMat

Mod_octave(6, 5)

Mod_octave(2 * pi, pi)

Mod_octave(c(1, 3, 5, 2), 2)

Mod_octave(c(9, 3, 2, 0), c(1, 0, 2, 2))

Mod_octave(-1, 3)

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

library("iemisc")
import::from(ramify, mat)

# values from https://github.com/simaki/fracdiff

a <- mat("1, 2, 4, 7, 0")

fractdiff(x = a, d = 0.5)

