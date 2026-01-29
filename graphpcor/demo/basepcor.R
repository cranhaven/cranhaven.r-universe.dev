
## p = 3, m = 2
bc <- basepcor(c(-1,-1), p = 3, itheta = c(2,3))
bc

round(solve(bc$base), 4)

all.equal(bc, basepcor(bc$base, itheta =c(2,3)))

## p = 4, m = 4
th2 <- c(0.5,-1,0.5,-0.3)
ith2 <- c(2,3,8,12)
b2 <- basepcor(th2, p = 4, itheta = ith2)
b2

Sparse(solve(b2$base), zeros.rm = TRUE)

all.equal(th2, basepcor(b2$base, itheta = ith2)$theta)

## Hessian around the base (and its decomposition, etc.)
b2$I0
