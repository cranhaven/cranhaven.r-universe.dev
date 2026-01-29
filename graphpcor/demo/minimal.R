suppressPackageStartupMessages({
	library(INLAtools)
	library(graphpcor)
	library(INLA)
})

g <- graphpcor(a ~ b + c)
g
dim(g)

cmodel <- cgeneric(g, lambda = 10, base = c(-1, -0.5))
cmodel

graph(cmodel)
prec(cmodel, theta = c(-1, -0.5))
solve(prec(cmodel, theta = c(-1, -0.5)))

cfam <- list(hyper = list(prec = list(initial = 10, fixed = TRUE)))
fit <- inla(formula = y ~ 0 + f(i, model = cmodel), 
	    data = data.frame(y = NA, i = 1:3), 
	    control.family = cfam)

stopifnot(isTRUE(fit$ok))

fit$mode$theta
solve(prec(cmodel, theta = fit$mode$theta))


