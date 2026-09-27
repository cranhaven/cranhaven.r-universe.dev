library(itertools)
library(foreach)
library(abind)

x <- array(1:24, c(2, 3, 4))
y <- array(101:124, c(2, 3, 4))
mcomb <- function(...) abind(..., along=3)

########################################################################

expected.1 <- abind(x, y, along=1)

actual.1 <- array(rbind(array(x, c(dim(x)[1], prod(dim(x)[2:3]))),
                        array(y, c(dim(y)[1], prod(dim(y)[2:3])))),
                  c(dim(x)[1] + dim(y)[1], dim(x)[2:3]))
print(all(actual.1 == expected.1))

actual.1 <-
  foreach(a=iarray(x, 3), b=iarray(y, 3), .combine='c') %do% {
    rbind(a, b)
  }
dim(actual.1) <- c(4, 3, 4)
print(all(actual.1 == expected.1))

actual.1 <-
  foreach(a=iarray(x, 3), b=iarray(y, 3), .combine='mcomb',
          .multicombine=TRUE) %do% {
    rbind(a, b)
  }
print(all(actual.1 == expected.1))

########################################################################

expected.2 <- abind(x, y, along=2)

actual.2 <- array(rbind(array(x, c(prod(dim(x)[1:2]), dim(x)[3])),
                        array(y, c(prod(dim(y)[1:2]), dim(y)[3]))),
                  c(dim(x)[1], dim(x)[2] + dim(y)[2], dim(x)[3]))
print(all(actual.2 == expected.2))

actual.2 <-
  foreach(a=iarray(x, 3), b=iarray(y, 3), .combine='c') %do% {
    cbind(a, b)
  }
dim(actual.2) <- c(2, 6, 4)
print(all(actual.2 == expected.2))

actual.2 <-
  foreach(a=iarray(x, 3), b=iarray(y, 3), .combine='mcomb',
          .multicombine=TRUE) %do% {
    cbind(a, b)
  }
print(all(actual.2 == expected.2))

########################################################################

expected.3 <- abind(x, y, along=3)

actual.3 <- array(c(x, y), c(dim(x)[1:2], dim(x)[3] + dim(y)[3]))
print(all(actual.3 == expected.3))

actual.3 <-
  foreach(a=i_chain(iarray(x, 3), iarray(y, 3)), .combine='c') %do% {
    a
  }
dim(actual.3) <- c(2, 3, 8)
print(all(actual.3 == expected.3))

actual.3 <-
  foreach(a=i_chain(iarray(x, 3), iarray(y, 3)), .combine='mcomb',
          .multicombine=TRUE) %do% {
    a
  }
print(all(actual.3 == expected.3))
