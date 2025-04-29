\name{weather_sim_multi_sites}
\alias{weather.sim.multi.sites}
\alias{weather_sim_multi_sites}
\docType{data}
\title{
Simulated temperature and precipitation for two grid cells
}
\description{
The dataset is generated with the package own routines and represent 5 series of 38 years of meteorological data for two grid cells
}
\usage{data("weather_sim_multi_sites")}
\format{
  Two lists (one per variable) of four elements (one per catchment), containing a data frame each holding information about the observed time series and the stochastic simulations
  \describe{
    \item{\code{YYYY}}{a numeric vector, year}
    \item{\code{MM}}{a numeric vector, month}
    \item{\code{DD}}{a numeric vector, day}
    \item{\code{timestamp}}{\code{POSIXct} vector of the daily runoff}
    \item{\code{Prec/Temp}}{observed precipitation/temperature}
    \item{\code{r1},\dots,\code{r5}}{5 simulated data series}
  }
}
\details{
The data is included to illustrate the validation and visualization routines in \code{demo("PRSim_weather-validate")}.

}
\source{
The data has been generated with 

\code{prsim.weather(data_p=data_p, data_t=data_t, number_sim=5, p_margin='egpd',t_margin='sep')} 

(default values for all other arguments).
}
\references{
Brunner, M. I., and E. Gilleland (2021). Spatial compound hot-dry events in the United States: assessment using a multi-site multi-variable weather generator, in preparation.
}
\examples{
oldpar <- par(mfrow = c(2, 1), mar = c(3, 3, 2, 1))
data(weather_sim_multi_sites)
sim <- weather_sim_multi_sites
### define plotting colors
col_sim <- adjustcolor("#fd8d3c",alpha=0.8)
col_sim_tran <- adjustcolor("#fd8d3c",alpha=0.2)
col_obs <- adjustcolor( "black", alpha.f = 0.2)
### greys
col_vect_obs <- c('#cccccc','#969696','#636363','#252525')
### oranges
col_vect_sim <- c('#fdbe85','#fd8d3c','#e6550d','#a63603')

### plot time series for multiple sites

### Temperature (first list entry)
par(mfrow=c(2,1),mar=c(3,3,2,1))
### determine ylim
ylim_max <- max(sim[[1]][[1]]$Temp)*1.5
### observed
plot(sim[[1]][[1]]$Temp[1:1000],
ylab=expression(bold(paste("Temperature [degrees]"))),
xlab="Time [d]",type="l",col=col_vect_obs[1],
ylim=c(0,ylim_max),main='Observations')
for(l in 2){
  lines(sim[[l]][[1]]$Temp[1:1000],col=col_vect_obs[l])
}
# legend('topleft',legend=c('Station 1','Station 2'
# ),lty=1,col=col_vect_obs[1:2])
### simulated (one run)
plot(sim[[1]][[1]]$r1[1:1000],
ylab=expression(bold(paste("Temperature [degrees]"))),
xlab="Time [d]",type="l",col=col_vect_sim[1],
ylim=c(0,ylim_max),main='Stochastic simulations')
for(l in 2){
  lines(sim[[l]][[1]]$r1[1:1000],col=col_vect_sim[l])
}


### precipitation (second list entry)
ylim_max <- max(sim[[1]][[2]]$Prec)*1
### observed
plot(sim[[1]][[2]]$Prec[1:1000],
ylab=expression(bold(paste("Precipitation [mm/d]"))),
xlab="Time [d]",type="l",col=col_vect_obs[1],
ylim=c(0,ylim_max),main='Observations')
for(l in 2){
  lines(sim[[l]][[2]]$Prec[1:1000],col=col_vect_obs[l])
}
# legend('topleft',legend=c('Station 1','Station 2'
# ),lty=1,col=col_vect_obs[1:2])
### simulated (one run)
plot(sim[[1]][[2]]$r1[1:1000],
ylab=expression(bold(paste("Precipitation [mm/d]"))),
xlab="Time [d]",type="l",col=col_vect_sim[1],
ylim=c(0,ylim_max),main='Stochastic simulations')
for(l in 2){
  lines(sim[[l]][[2]]$r1[1:1000],col=col_vect_sim[l])
}
par(oldpar)
}
\keyword{datasets}
