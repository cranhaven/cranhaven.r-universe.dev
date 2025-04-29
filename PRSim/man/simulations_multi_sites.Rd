\name{simulations_multi_sites}
\alias{simulations.multi.sites}
\alias{simulations_multi_sites}
\docType{data}
\title{
Simulated runoff for four catchments
}
\description{
The dataset is generated with the package own routines and represent 5 series of 38 years of runoff for four catchments
}
\usage{data("simulations_multi_sites")}
\format{
  A list of four elements (one per catchment), containing a data frame each holding information about the observed time series and the stochastic simulations
  \describe{
    \item{\code{YYYY}}{a numeric vector, year}
    \item{\code{MM}}{a numeric vector, month}
    \item{\code{DD}}{a numeric vector, day}
    \item{\code{timestamp}}{\code{POSIXct} vector of the daily runoff}
    \item{\code{Qobs}}{observed runoff}
    \item{\code{r1},\dots,\code{r5}}{5 simulated runoff series}
  }
}
\details{
The data is included to illustrate the validation and visualization routines in \code{demo("PRSim_wave-validate")}.

}
\source{
The data has been generated with 

\code{prsim.wave(data=runoff_multi_sites, number_sim=5, marginal="kappa",
                  GoFtest = NULL,pars=NULL, p_val=NULL)} 

(default values for all other arguments).
}
\references{
Brunner, M. I., A. Bárdossy, and R. Furrer (2019). Technical note: Stochastic simulation of streamflow time series using phase randomization. Hydrology and Earth System Sciences, 23, 3175-3187, https://doi.org/10.5194/hess-23-3175-2019.
}
\examples{
oldpar <- par(mfrow = c(2, 1), mar = c(3, 3, 2, 1))
### greys
col_vect_obs <- c('#cccccc','#969696','#636363','#252525')
### oranges
col_vect_sim <- c('#fdbe85','#fd8d3c','#e6550d','#a63603')
data(simulations_multi_sites)
sim <- simulations_multi_sites
dim(sim[[1]])
### plot time series for multiple sites
par(mfrow=c(2,1),mar=c(3,3,2,1))
### determine ylim
ylim_max <- max(sim[[1]]$Qobs)*1.5
### observed
plot(sim[[1]]$Qobs[1:1000],
    ylab=expression(bold(
        paste("Specific discharge [mm/d]"))),
    xlab="Time [d]",type="l",col=col_vect_obs[1],
    ylim=c(0,ylim_max),main='Observations')
for(l in 2:4){
  lines(sim[[l]]$Qobs[1:1000],col=col_vect_obs[l])
}
legend('topleft',legend=c('Station 1','Station 2',
        'Station 3','Station 4'),
    lty=1,col=col_vect_obs[1:4])
### simulated (one run)
plot(sim[[1]]$r1[1:1000],
    ylab=expression(bold(paste("Specific discharge [mm/d]"))),
    xlab="Time [d]",type="l",col=col_vect_sim[1],
    ylim=c(0,ylim_max),
    main='Stochastic simulations')
for(l in 2:4){
  lines(sim[[l]]$r1[1:1000],col=col_vect_sim[l])
}
par(oldpar)
}
\keyword{datasets}
