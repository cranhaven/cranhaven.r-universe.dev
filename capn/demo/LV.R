######################################################################################
# {capn} example: Lotka-Volterra (LV)
# Updated: 3/30/2017
# Original script written by Joshua Abbott, 2015 (in MATLAB)
# Updated in R script by Seong Do Yun
# File location: system.file("demo", "LV.R", package = "capn")
######################################################################################
library(capn)
library(deSolve)
rm(list=ls())

############### Do NOT RUN: the same data provided by data("lvdata") ###############
## parameters: Prey-Predator model
# prey
r <- 0.025     # intrinsic growth rate for prey
k <- 1         # carrying capacity for prey
a <- 0.08      # predator-related mortality parameter for prey
# predator
b <- 0.05      # predator/prey uptake parameter for predator
m <- 0.01      # natural mortality for predator
# Economic program: predator with no economic value (unharvested)
gamma <- 0.005 # slope for linear predator harvest control rule
theta <- 0.005 # slope for linear prey harvest control rule
p_pred <- 0    # price per unit harvest of predator
p_prey <- 25   # price per unit harvest of prey
c_prey <- 0.1*p_prey # cost /per unit of prey effort in Schaefer model (really c/q with q=1) 
c_pred <- c_prey  # cost per unit of predator effort in Schaefer model (really c/q with q=1)
delta <- 0.03     # Discount rate

## lvaproxdata in data("lvdata")
degn <- c(20,20)  # Degree of approximation in each dimension 
ub <- c(1.5,1.5)  # upper bounds
lb <- c(0.1,0.1)  # lower bounds
# Stocks
xy <- chebgrids(degn,lb,ub,rtype='grid') # Chevyshev nodes
xs <- xy[,1]       # Prey
ys <- xy[,2]       # Predator
# Harvest
harv_prey <- (c_prey/p_prey) <= xs  # =0 if not profitable to fish for prey
harv_pred <- (c_pred/p_pred) <= ys  # =0 if not profitable to fish for predator
# Utility function, inclusive of economic program
wval <- harv_prey*(p_prey-c_prey/xs)*theta*xs +
        harv_pred*(p_pred-c_pred/ys)*gamma*ys 
# Growth function
xdot <- r*xs*(1-xs/k) - a*xs*ys - harv_prey*theta*xs 
ydot <- b*xs*ys - m*ys - harv_pred*gamma*ys

## lvsimdata.time in data("lvdata)
# time simulation
dxdyPP <- function(t, state, parms){
  with(as.list(state),{
    harv_prey <- (c_prey/p_prey) <= xs
    harv_pred <- (c_pred/p_pred) <= ys
    dx <- r*xs*(1-xs/k) - a*xs*ys - harv_prey*theta*xs
    dy <- b*xs*ys - m*ys - harv_pred*gamma*ys
    list(c(dx,dy))
  })
}
tseq <- seq(0,1000,by=10)
# Initial stock: (0.8, 0.6) 
oderes <- ode(y=c(xs=0.8,ys=0.6), times=tseq, func = dxdyPP, parms = NULL)
# xs <- oderes[,"x"]
# ys <- oderes[,"y"]
############### Do NOT RUN: the same data provided by data("lvdata") ###############

data("lvdata")
###################################################
# Time simulation

## Aproximation domain and Chevyshev polynomial orders
aproxdeg <- c(20,20)
lower <- c(0.1,0.1)
upper <- c(1.5,1.5)
delta <- 0.03
lvspace <- aproxdef(aproxdeg,lower,upper,delta)
lvaproxc <- vaprox(lvspace,lvaproxdata)
lvsim <- vsim(lvaproxc,lvsimdata.time[,2:3])

## Biomass plot
plot(lvsimdata.time[,1], lvsimdata.time[,2], type='l', lwd=2, col="blue",
     xlab="Time",
     ylab="Biomass")
lines(lvsimdata.time[,1], lvsimdata.time[,3], lwd=2, col="red")
legend("topright", c("Prey", "Predator"), col=c("blue", "red"),
       lty=c(1,1), lwd=c(2,2), bty="n")

## Shadow price-Stock1 plot
plotgen(lvsim,xlabel="Stock Size (Prey: X)",ylabel="Shadow Price")

## Shadow price-stock2 plot
plotgen(lvsim,whichs=2,xlabel="Stock Size (Predator: Y)",ylabel="Shadow Price")

## Value Function-Stock1 plot
plotgen(lvsim,ftype="vw",
        xlabel="Stock Size (Prey: X)",ylabel="Shadow Price")

## Value Function-Stock2 plot
plotgen(lvsim,ftype="vw",whichs=2,
        xlabel="Stock Size (Predator: Y)",ylabel="Shadow Price")
        
# Value Function-time plot
plotgen(lvsim,ftype="vw",tvar=lvsimdata.time[,1],
        xlabel="Time",ylabel="Value Function")

###################################################
## Stock simulation
## Simulation domain
ndim <- 50 # the number of simulation nodes
xy <- unigrids(c(ndim,ndim),(lower+0.05),(upper-0.5),rtype='grid')

## Simulation data
lvsim <- vsim(lvaproxc,xy)

## 3D-Plots 
xy2 <- unigrids(c(ndim,ndim),(lower+0.05),(upper-0.5))

## Extracting shadow price matrix
pmat1 <- vector()
pmat2 <- vector()
for (i in 0:(length(xy2[[1]])-1)){
  temp1 <- lvsim$shadowp[(ndim*i+1):(ndim*(i+1)),1]
  temp2 <- lvsim$shadowp[(ndim*i+1):(ndim*(i+1)),2]
  pmat1 <- cbind(pmat1,temp1)
  pmat2 <- cbind(pmat2,temp2)
}


## Plot the shadow price of prey (X)
nrz <- nrow(pmat1)
ncz <- ncol(pmat1)
jet.colors <- colorRampPalette( c("green", "red") ) 
nbcol <- 100
color <- jet.colors(nbcol)
zfacet <- pmat1[-1, -1] + pmat1[-1, -ncz] + pmat1[-nrz, -1] + pmat1[-nrz, -ncz]
facetcol <- cut(zfacet, nbcol)

persp(xy2[[1]],xy2[[2]],pmat1, theta=-50, phi=30, expand=0.75, border="black",
      xlab="Prey (X)", ylab="Predator (Y)", zlab="Shadow Price of Prey", ticktype = "detailed",
      col=color[facetcol])

## Plot the shadow price of predator (Y)
nrz <- nrow(pmat2)
ncz <- ncol(pmat2)
jet.colors <- colorRampPalette( c("green", "red") ) 
nbcol <- 100
color <- jet.colors(nbcol)
zfacet <- pmat2[-1, -1] + pmat2[-1, -ncz] + pmat2[-nrz, -1] + pmat2[-nrz, -ncz]
facetcol <- cut(zfacet, nbcol)

persp(xy2[[1]],xy2[[2]],pmat2, theta=-50, phi=30, expand=0.75, border="black",
      xlab="Prey (X)", ylab="Predator (Y)", zlab="Shadow Price of Y", ticktype = "detailed",
      col=color[facetcol])
