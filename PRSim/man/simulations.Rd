\name{simulations}
\alias{simulations}
\docType{data}
\title{
Simulated runoff
}
\description{
The dataset is generated with the package own routines and represent 5 series of 18 years of runoff
}
\usage{data("simulations")}
\format{
  A list of three elements, containing (i) a data frame with 6570 observations of the following variables
  \describe{
    \item{\code{YYYY}}{a numeric vector, year}
    \item{\code{MM}}{a numeric vector, month}
    \item{\code{DD}}{a numeric vector, day}
    \item{\code{timestamp}}{\code{POSIXct} vector of the daily runoff}
    \item{\code{deseasonalized}}{deseasonalized time series}
    \item{\code{Qobs}}{observed runoff}
    \item{\code{r1},\dots,\code{r5}}{5 simulated runoff series}
  }
  (ii) a data frame with the daily fitted kappa parameters and (iii) p-values of the daily \code{ks.test}.
  
}
\details{
The data is included to illustrate the validation and visualization routines in \code{demo("PRSim-validate")}.

}
\source{
The data has been generated with 

\code{set.seed(14); prsim( runoff[ runoff$YYYY>1999,], number_sim=5,
  KStest=TRUE)} 

(default values for all other arguments).
}
\references{
Brunner, M. I., A. Bárdossy, and R. Furrer (2019). Technical note: Stochastic simulation of streamflow time series using phase randomization. Hydrology and Earth System Sciences, 23, 3175-3187, https://doi.org/10.5194/hess-23-3175-2019.
}
\examples{
data(simulations)
names(simulations) 
sim <- simulations$simulation
dim(sim)
sim$day_id <- rep(seq(1:365), times=length(unique(sim$YYYY)))
mean_obs <- aggregate(sim$Qobs, by=list(sim$day_id), FUN=mean, simplify=FALSE)
plot(unlist(mean_obs[,2]),lty=1,lwd=1,col="black", ylab="Discharge [m3/s]",
     xlab="Time [d]", main="Mean hydrographs", ylim=c(0,22), type="l")

for(r in 7:(length(names(sim))-1)){
  mean_hydrograph <- aggregate(sim[,r], by=list(sim$day_id), FUN=mean, simplify=FALSE)
  lines(mean_hydrograph, lty=1, lwd=1, col="gray")
}
lines( mean_obs, lty=1, lwd=1, col="black")
}
\keyword{datasets}
