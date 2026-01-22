######################################################################################
# {capn} example: Guld of Mexico (GOM)
# Updated: 3/30/2017
# Original script written by Eli Fenichel, 2013 (in MATHEMATICA)
# Updated in R script by Seong Do Yun and Eli Fenichel
# Reference: Fenichel & Abbott (2014)
# File location: system.file("demo", "GOM.R", package = "capn")
######################################################################################
library(capn)
rm(list=ls())

## parameters from Fenichel & Abbott (2014)
r <- 0.3847
param <- as.data.frame(r)
rm(r)
param$k <- 359016000
param$q <- 0.00031729344157311126
param$price <- 2.70
param$cost <- 153.0
param$alpha <- 0.5436459179063678
param$gamma <- 0.7882
param$y <- 0.15745573410462155
param$delta <- 0.02

param$order <- 50
param$upperK <- param$k
param$lowerK <- 5*10^6
param$nodes <- 500

## functions from Fenichel & Abbott (2014)
effort <- function(s, Z){
  Z$y*s^Z$gamma
}

catch <- function(s,Z){
  Z$q*effort(s,Z)^Z$alpha*s
}

sdot <- function(s,Z){
  Z$r*s*(1-s/Z$k)-catch(s,Z)
}

profit <- function(s,Z){
  Z$price*catch(s,Z)-Z$cost*effort(s,Z)
}

dwds <- function(s,Z){
  (Z$gamma*Z$alpha+1)*Z$price*Z$q*(Z$y^Z$alpha)*(s^(Z$gamma*Z$alpha))-
  Z$gamma*Z$cost*Z$y*(s^(Z$gamma-1))
}

dwdss <- function(s,Z){
  (Z$gamma*Z$alpha+1)*Z$gamma*Z$alpha*Z$price*Z$q*(Z$y^Z$alpha)*(s^(Z$gamma*Z$alpha-1))-
  Z$gamma*(Z$gamma-1)*Z$cost*Z$y*(s^(Z$gamma-2))
}

dsdotds <- function(s,Z){
  Z$r - 2*Z$r*s/Z$k - (Z$gamma*Z$alpha+1)*Z$q*(Z$y^Z$alpha)*(s^(Z$gamma*Z$alpha))
}

dsdotdss <- function(s,Z){
  -2*Z$r/Z$k- 
  (Z$gamma*Z$alpha+1)*Z$gamma*Z$alpha*Z$q*(Z$y^Z$alpha)*(s^((Z$gamma*Z$alpha-1)))
}

## shadow prices
# prepare capN
Aspace <- aproxdef(param$order,param$lowerK,param$upperK,param$delta) #defines the approximation space
nodes <- chebnodegen(param$nodes,param$lowerK,param$upperK) #define the nodes

# prepare for simulation
simuDataV <- cbind(nodes,sdot(nodes,param),profit(nodes,param))

simuDataP <- cbind(nodes,sdot(nodes,param),
                   dsdotds(nodes,param),dwds(nodes,param))

simuDataPdot <- cbind(nodes,sdot(nodes,param),
                      dsdotds(nodes,param),dsdotdss(nodes,param),
                      dwds(nodes,param),dwdss(nodes,param))

# recover approximating coefficents
vC <- vaprox(Aspace,simuDataV)  #the approximated coefficent vector for prices

pC <- paprox(Aspace,simuDataP[,1],simuDataP[,2],
           simuDataP[,3],simuDataP[,4])  #the approximated coefficent vector for prices

pdotC <- pdotaprox(Aspace,simuDataPdot[,1],simuDataPdot[,2],
                   simuDataPdot[,3],simuDataPdot[,4],
                   simuDataPdot[,5],simuDataPdot[,6])

# project shadow prices and wealth
GOMSimV <- vsim(vC,as.matrix(simuDataV[,1],ncol=1),profit(nodes,param))
GOMSimP <- psim(pC,simuDataP[,1],profit(nodes,param),simuDataP[,2]) 
GOMSimPdot <- pdotsim(pdotC,simuDataPdot[,1],simuDataPdot[,2],simuDataPdot[,3],
                      profit(nodes,param),simuDataPdot[,5])


# Three price curves
plot(nodes,GOMSimV$shadowp, type='l', lwd=2, col="blue",
     ylim = c(0,15),
     xlab="Stock size, s",
     ylab="Shdow price")
lines(nodes, GOMSimP$shadowp, lwd=2, col="red")
lines(nodes, GOMSimPdot$shadowp, lwd=2, col="green")

