library(graphpcor)

## A correlation matrix
c0 <- matrix(c(1,.8,-.625, 0.8,1,-.5, -0.625,-.5,1), 3)

## build the 'basecor'
pc.c0 <- basecor(c0) ## base as matrix
pc.c0

## elements
pc.c0$base
pc.c0$theta
pc.c0$I0

## from 'theta' 
th0 <- pc.c0$theta
pc.th0 <- basecor(th0) ## base as vector
pc.th0

## numerically the same
all.equal(c0, pc.th0$base)

## from a numeric vector (theta)
th2 <- c(-1, -0.5)
b2 <- basecor(th2, p = 3, itheta = c(2,3))
b2

## from the correlation matrix
b2c <- basecor(b2$base, itheta = c(2,3))

all.equal(th2, b2c$theta, tol = 1e-4)

## Hessian around the base (and its decomposition, etc.)
b2$I0
