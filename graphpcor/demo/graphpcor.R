library(graphpcor)

par(mfrow = c(2, 3), mar = c(0,0,0,0))
plot(graphpcor(x~y+v, z~y+v))
plot(graphpcor(x~y,x~v,z~y,z~v))
plot(graphpcor(x~y, v~x, y~z, z~v))
plot(graphpcor(y~x, v~x, z~y, z~v))
plot(graphpcor(y~x+z, v~z+x))
plot(graphpcor(y~x+z, v~x, z~v))

## the graph in Example 2.6 of the GMRF book
g <- graphpcor(x ~ y + v, z ~ y + v)

class(g)
g

par(mfrow=c(1,1))
plot(g)

summary(g) ## the graph: nodes and edges (nodes ordered as given)

ne <- dim(g)
ne

## sometimes we need it
G <- Laplacian(g)
G 

## alternatively
all.equal(G,
          Laplacian(graphpcor(x~y, v~x, y~z, z~v)))
all.equal(G,
          Laplacian(graphpcor(x~y, x~v, z~y, v~z)))

plot(graphpcor(G)) ## from a matrix

## base model (theta for lower triangle Cholesky)
theta0l <- rep(-0.5, ne[2])

## vcov() method for graphpcor computes the correlation
##  if only theta for lower of L is provided
C0 <- vcov(g, theta = theta0l)
C0

## the precision for a correlation matrix
Q0 <- prec(g, theta = theta0l)
Q0

all.equal(C0, as.matrix(solve(Q0)))

## the Hessian matrix around a base model
I0 <- basepcor(theta0l, p = ne[1], itheta = g)
I0

## a base model can also be a matrix
## however it shall give a precision with
## same sparse pattern as the graph
all.equal(I0, basepcor(C0, p = ne[1], itheta = g))

## the 'iid' case would be
vcov(g, theta = rep(0, ne[2]))
vcov(g, theta = rep(0, sum(ne)))

## marginal variance specified throught standard errors
sigmas <- c(0.3, 0.7, 1.2, 0.5)
## the covariance
vcov(g, theta = c(log(sigmas), rep(0, ne[2]))) ## IID
vcov(g, theta = c(log(sigmas), theta0l))

vcov(g, theta = rep(-3, ne[2])) ## no edge 2~3 but high correlation!!!

