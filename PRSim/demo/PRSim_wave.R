###===============================###===============================###
### Application of PRSim.wave for different (extreme value) distributions
###===============================###===============================###

### (1) Empirical distribution
### does not allow for extrapolation to yet unobserved values
###===============================###===============================###
out <- prsim.wave(data=runoff_multi_sites, number_sim=1, marginal="empirical")

### (2) Kappa distribution (default)
### 4 parameters and flexible. Found to be performing best for 
### a set of 671 nearly natural catchments in the United States (Brunner and Gilleland 2020)
###===============================###===============================###
out <- prsim.wave(data=runoff_multi_sites, number_sim=1, marginal="kappa", GoFtest = "KS")
# out <- prsim.wave(data=runoff_multi_sites, number_sim=5, marginal="kappa",
#                   GoFtest = NULL,pars=NULL, p_val=NULL)
# simulations_multi_sites<-list(out[[1]]$simulation,out[[2]]$simulation,out[[3]]$simulation,out[[4]]$simulation)
# setwd("C:/Users/mbrunner/Documents/PRSim-devel/data")
# save(simulations_multi_sites,file='simulations_multi_sites.rda')

### (3) GEV distribution
### 3 parameters, classical extreme value distribution (Coles 2001)
###===============================###===============================###
# require("evd")
# require("ismev")
# rGEV <- function(n, theta)  rgev(n, theta[1], theta[2], theta[3])
# pGEV <- function(x, theta)  pgev(x, theta[1], theta[2], theta[3])
# GEV_fit <- function( xdat, ...)   gev.fit(xdat, show=FALSE, ...)$mle


### simulate using GEV
# out <- prsim.wave(data=runoff_multi_sites, number_sim=1, marginal="GEV", GoFtest = "KS", n_par=3)

### visualize simulations
### store stochastically simulated time series
### station 2
sim <- out[[2]]$simulation

### plot example of simulated time series
# par(mai=c(.9,.9,.1,.1))
### observed time series
plot(sim$timestamp[1:1000], sim$Qobs[1:1000], type="l", 
     xlab="Time [d]", ylab=expression(paste("Discharge [m"^3,"/s]")))
### add simulations
matlines(sim$timestamp[1:1000], sim[1:1000, grep("r", names(sim))],
         lty=1, col="gray")

### compare distributions without outliers
### without outliers
boxplot(sim$Qobs,sim$r1,outline=FALSE,col=c('black','grey'),names=c('Obs','Sim'))
### with outliers
boxplot(sim$Qobs,sim$r1,col=c('black','grey'),names=c('Obs','Sim'))


### (4) generalized Beta distribution of the second kind (Mielke 1974)
### 4 parameters: can lead to very extreme outliers, which are potentially implausible
###===============================###===============================###
# require( "GB2")
# rGB2 <- function(n, theta)  rgb2(n, theta[1], theta[2], theta[3], theta[4])
# pGB2 <- function(x, theta)  pgb2(x, theta[1], theta[2], theta[3], theta[4])
# GB2_fit <- function( xdat, ...)   ml.gb2( xdat, ...)$opt1$par
# 
# ### simulate using GB2
# out <- prsim.wave(data=runoff_multi_sites, number_sim=1, marginal="GB2", GoFtest = "KS", n_par=4)


### (5) Normal distribution (just as a reference, will not perform well because it is not extreme)
### 2 parameters, not flexible enough for daily flow
###===============================###===============================###
# library(fitdistrplus)
# rNORM <- function(n, theta)  rnorm(n, theta[1], theta[2])
# pNORM <- function(x, theta)  pnorm(x, theta[1], theta[2])
# NORM_fit <- function( xdat, ...)   fitdistr( xdat, 'normal', show=FALSE, ...)$estimate
# 
# ### run simulations using normal distribution
# out <- prsim.wave(data=runoff_multi_sites, number_sim=1, marginal="NORM", GoFtest = "KS", n_par=2)

### (6) generalized Gamma distribution (Prentice 1974)
### 3 parameters
### non-stable in optimization
###===============================###===============================###
# library(flexsurv)
# rGENGAM <- function(n, theta)  rgengamma(n, theta[1], theta[2], theta [3])
# pGENGAM <- function(x, theta)  pgengamma(x, theta[1], theta[2], theta [3])
# GENGAM_fit <- function( xdat, ...)   fitdistr(xdat, dgengamma, start=list("mu"=0,"sigma"=0.9,"Q"=0.5))$estimate
# ### depending on your data, it may be necessary to experiment with different start values
# ### as optimization may produce some 'random' errors.
# ### As optimization is not very stable, this distribution is maybe not 
# ### very useful for stochastic simulation.
# 
# ### run simulations with generalized Gamma
# out <- prsim.wave(data=runoff_multi_sites, number_sim=1, marginal="GENGAM", GoFtest = "KS", n_par=3)

### (7) Burr type XII distribution (Burr 1942)
### 3 parameters
### according to my test simulations performing very poorly.
###===============================###===============================###
# library(actuar)
# rBURR <- function(n, theta)  rburr(n, theta[1], theta[2], theta [3])
# pBURR <- function(x, theta)  pburr(x, theta[1], theta[2], theta [3])
# BURR_fit <- function( xdat, ...)   fitdist(xdat, 'burr', start = list(shape1 = 0.3, shape2 = 0.9, rate = 0.9),method='mge')$estimate
# ### using maximum gooness-of-fit estimation instead of maximum likelihood estimation
# ### because the latter is numerically unstable.
# ### run simulations with Burr type XII distribution
# out <- prsim.wave(data=runoff_multi_sites, number_sim=1, marginal="BURR", GoFtest = "KS", n_par=3)


### (8) Wakeby distribution (Hosking 1986)
### 5 parameters, very flexible
###===============================###===============================###
# library(lmomco)
# rWAK <- function(n, theta)  rlmomco(n, list(type='wak',para=theta,source='pwarwak',ifail=0,ifailtext="Successful parameter estimation."))
# pWAK <- function(x, theta)  pdfwak(x, list(type='wak',para=theta,source='pwarwak',ifail=0,ifailtext="Successful parameter estimation."))
# WAK_fit <- function( xdat, ...){
#   lmom_X <- lmoms(xdat)
#   ### fit Wakeby distribution
#   wakeby_X <- parwak(lmom_X)$para
#   return(wakeby_X)
# }
# 
# ### run simulations with Wakeby distribution
# out <- prsim.wave(data=runoff_multi_sites, number_sim=1, marginal="WAK", GoFtest = "KS", n_par=5)

### (9) Generalized Pareto
###===============================###===============================###
# ### produces NA values
# rGPD <- function(n, theta)  rgpd(n, theta[1], theta[2], theta[3])
# pGPD <- function(x, theta)  pgpd(x, theta[1], theta[2], theta[3])
# GPD_fit <- function( xdat, ...){
#   param <- gpd.fit( xdat, threshold=min(xdat))
#   theta <- c(param$threshold,param$mle)
#   return(theta)
# }   
# 
# ### run simulations with Wakeby distribution
# out <- prsim.wave(data=runoff_multi_sites, number_sim=1, marginal="GPD", GoFtest = "KS", n_par=3)


### (10) Lognormal distribution
###===============================###===============================###
# rLNORM <- function(n, theta)  rlnorm(n, theta[1], theta[2])
# pLNORM <- function(x, theta)  plnorm(x, theta[1], theta[2])
# LNORM_fit <- function( xdat, ...)   fitdistr( xdat, 'lognormal', show=FALSE, ...)$estimate
# 
# ### run simulations with Wakeby distribution
# out <- prsim.wave(data=runoff_multi_sites, number_sim=1, marginal="LNORM", GoFtest = "KS", n_par=2)


### (11) Generalized logistic
### 3 parameters
### not worth testing as it produced negative values
###===============================###===============================###
# library(glogis)
# rGLOGIS <- function(n, theta)  rglogis(n, theta[1], theta[2], theta[3])
# pGLOGIS <- function(x, theta)  plnorm(x, theta[1], theta[2], theta[3])
# GLOGIS_fit <- function(xdat, ...)   glogisfit(xdat)$coefficients
# 
# ### run simulations with Wakeby distribution
# out <- prsim.wave(data=runoff_multi_sites, number_sim=1, marginal="GLOGIS", GoFtest = "KS", n_par=3)


### (12) Pearson type III
###===============================###===============================###
# library(FAdist)
# rPEARSON <- function(n, theta)  rgamma3(n, theta[1], theta[2], theta[3])
# pPEARSON <- function(x, theta)  pgamma3(x, theta[1], theta[2], theta[3])
# PEARSON_fit <- function( xdat, ...)   fitdist(xdat, 'gamma3',start=list(shape=0.9,scale=0.9,thres=mean(xdat)),method='mge')$estimate
# ### using maximum gooness-of-fit estimation instead of maximum likelihood estimation
# ### because the latter is numerically unstable.
# 
# ### run simulations with Wakeby distribution
# out <- prsim.wave(data=runoff_multi_sites, number_sim=1, marginal="PEARSON", GoFtest = "KS", n_par=3)

