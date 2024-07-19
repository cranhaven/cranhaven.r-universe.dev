## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

library("iemisc")

import::from(gsubfn, list)
import::from(ramify, mat)

# Example from GNU Octave ndims function reference

size(matlab::ones(4, 1, 2, 1))


# Examples from GNU Octave size function reference

object1 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2, byrow = TRUE)

size(object1)


list[nr, nc] <- size(matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2,
                byrow = TRUE)); nr; nc

size(matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2, byrow = TRUE), 2)


# using ramify's mat

size(mat("1, 2; 3, 4; 5, 6"))

size(mat("1, 2; 3, 4; 5, 6"), 2)

list[nr, nc] <- size(mat("1, 2; 3, 4; 5, 6")); nr; nc

size(matlab::ones(4, 3, 4, 8), 4)

size(matlab::ones(4, 3, 4, 5), 3)

## The following can't be done currently with this function:

# list[nr, remainder] <- size(matlab::ones(2, 3, 4, 5)); nr; remainder

## As a work around to get similar results to GNU Octave, do the following:

nr <- size(matlab::ones(2, 3, 4, 5), 1); nr

remainder <- size(matlab::ones(2, 3, 4, 5), 2); remainder




# Examples from pracma size

size(1:8)

size(matrix(1:8, 2, 4))

size(matrix(1:8, 2, 4), 2)

size(matrix(1:8, 2, 4), 3)

ss <- "object"

size(ss)

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

library("iemisc")

import::from(matlab, ones)

# Example from pracma isempty

object1 <- matrix(0, 1, 0)

length_octave(object1)

object2 <- 2

length_octave(object2)

object3 <- 1:10

length_octave(object3)

object4 <- ones(3, 4)

length_octave(object4)

object5 <- "ss"

length_octave(object5)

object6 <- list(letters, b <- 2)

length_octave(object6)

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

library("iemisc")

import::from(matlab, ones)

xx <- list(1:26, 1:10)

numel(xx)


# Examples from GNU Octave numel

a <- 1

b <- ones(2, 3)

numel(a, b)


a <- 2

b <- ones(2, 3)

c <- ones(3, 4)

numel(a, b)

numel(a, b, c)

f <- matrix(c(10, 12, 23, 21, 62, 93), nrow = 2, ncol = 3, byrow = TRUE)

g <- c(2, 4)

numel(f, g)

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

library("iemisc")

# Examples from GNU Octave ndims

b <- matlab::ones(c(4, 1, 2, 1))

ndims(b)


## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

library("iemisc")

# Examples

xx <- ramify::mat("1, 2"); xx

isrow(xx)


xy <- ramify::mat("1, 2; 3, 4"); xy

isrow(xy)

## ----warning = FALSE, message = FALSE, tidy = TRUE----------------------------

library("iemisc")

# Examples

xxx <- ramify::mat("1, 2"); xxx

iscolumn(xxx)



xyy <- ramify::mat("1; 2"); xyy

iscolumn(xyy)

