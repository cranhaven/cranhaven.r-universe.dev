###===============================###===============================###
### Application of PRSim.weather for temperature and precipitation
###===============================###===============================###
### load data for four stations
data(weather_multi_sites) ### loads data_p and data_t
# weather_multi_sites <- rep(list(rep(list(NA),times=2)),times=4)
# weather_multi_sites[[1]][[1]] <- data_t[[1]]
# weather_multi_sites[[1]][[2]] <- data_p[[1]]
# weather_multi_sites[[2]][[1]] <- data_t[[2]]
# weather_multi_sites[[2]][[2]] <- data_p[[2]]
# weather_multi_sites[[3]][[1]] <- data_t[[3]]
# weather_multi_sites[[3]][[2]] <- data_p[[3]]
# weather_multi_sites[[4]][[1]] <- data_t[[4]]
# weather_multi_sites[[4]][[2]] <- data_p[[4]]
# setwd("~/PRSim-devel/data")
# save(file='weather_multi_sites.rda',weather_multi_sites)

### (1) apply function with default distributions
### temperature: SEP, precipitation: E-GPD
### does not allow for extrapolation to yet unobserved values
###===============================###===============================###
data_t <- sapply(weather_multi_sites,function(x) x[1])
data_p <- sapply(weather_multi_sites,function(x) x[2])
# out <- prsim.weather(data_p=data_p, data_t=data_t, number_sim=5, p_margin='egpd',t_margin='sep')
### save example simulation data
# weather_sim_multi_sites <- out
# setwd("~/PRSim-devel/data")
# save(file='weather_sim_multi_sites.rda',weather_sim_multi_sites)

### (2) example with alternative distributions
### rCDF and CDF_fit need to be defined
### temperature: normal, precipitation: GEV
###===============================###===============================###
### define normal distribution
# library(fitdistrplus)
# rNORM <- function(n, theta)  rnorm(n, theta[1], theta[2])
# pNORM <- function(x, theta)  pnorm(x, theta[1], theta[2])
# NORM_fit <- function( xdat, ...)   fitdistr(xdat, 'normal', show=FALSE, ...)$estimate
# ### define GEV distribution
# require("evd")
# require("ismev")
# rGEV <- function(n, theta)  rgev(n, theta[1], theta[2], theta[3])
# pGEV <- function(x, theta)  pgev(x, theta[1], theta[2], theta[3])
# GEV_fit <- function( xdat, ...)   gev.fit(xdat, show=FALSE, ...)$mle
# 
# ### apply function using alternative distributions
# out <- prsim.weather(data_p=data_p, data_t=data_t, number_sim=1,p_margin='GEV',t_margin='NORM')
# out <- prsim.weather(data_p=data_p, data_t=data_t, number_sim=1,p_margin='NORM',t_margin='NORM')
