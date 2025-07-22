context('Equilibrium computation')
library(GPareto)

test_that("Calibration mode works",{
################################################################
# Example 1: KS equilibrium, 2 variables, 2 players, no filter
################################################################

# Run solver with 6 initial points, 4 iterations
# Increase n.ite to at least 10 for better results

# To use parallel computation (turn off on Windows)
library(parallel)
parallel <- TRUE #
if(parallel) ncores <- detectCores() else ncores <- 1

# Simple configuration: no filter, discretization is a 21x21 grid

# Grid definition
n.s <- rep(21, 2)
x.to.obj   <- c(1,2)
gridtype <- 'cartesian'
n.ite <- 8
target <- c(-10,-35)
Nadir <- NULL #c(Inf, log(100))

filtercontrol <- list(nsimPoints=200, ncandPoints=100,
                      filter=c("window", "window"))

calibcontrol <- list(target=target, log=TRUE, offset=0.01)

res <- solve_game(P1, equilibrium = "CKSE", crit = "sur", n.init=6, n.ite=n.ite,
                  d = 2, nobj=2, x.to.obj = x.to.obj,
                  integcontrol=list(n.s=n.s, kweights=TRUE,gridtype=gridtype, nsamp=1e3),
                  filtercontrol=filtercontrol, returncontrol=list(track.Eq="mean"),
                  ncores = ncores, trace=3, seed=1, calibcontrol=calibcontrol) #, target=target, Nadir=Nadir) 

# Get estimated equilibrium and corresponding pay-off
NE <- res$Eq.design
Poff <- res$Eq.poff

# Draw results
plotGame(res, equilibrium = "CKSE", Nadir=Nadir, calibcontrol=calibcontrol) #Nadir=c(Inf, -20))

plotGameGrid(fun=P1, n.grid=21, calibcontrol=calibcontrol, equilibrium = "KSE", integcontrol=res$integcontrol, Nadir=Nadir) #Nadir=c(Inf, -20))

# TODO add verification

})