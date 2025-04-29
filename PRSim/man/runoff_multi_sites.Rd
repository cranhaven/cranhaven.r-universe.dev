\name{runoff_multi_sites}
\alias{runoff_multi_sites}
\alias{runoff multi sites}
\docType{data}
\title{
Sample runoff of four catchments with a similar discharge regime
}
\description{
Observed runoff data from four USGS sites.
}
\usage{data("runoff_multi_sites")}
\format{
  A list of four data frames (one list per station) of the following 4 variables.
  \describe{
    \item{\code{YYYY}}{a numeric vector, year}
    \item{\code{MM}}{a numeric vector, month}
    \item{\code{DD}}{a numeric vector, day}
    \item{\code{Qobs}}{a numeric vector, observed runoff}
  }
}
\details{
The data contains runoff for four USGS gages: (i) Calawah River near Forks, WA (USGS 12043000), (ii) NF Stillaguamish River near Arlington, WA (USGS 12167000), (iii) Nehalem River near Foss, OR (USGS 14301000), and (iv) Steamboat Creek near Glide, OR (USGS 14316700). 
}
\source{
The actual discharge data were downloaded from
\url{https://waterdata.usgs.gov/nwis}.
}
\references{
Brunner, M. I., A. Bárdossy, and R. Furrer (2019). Technical note: Stochastic simulation of streamflow time series using phase randomization. Hydrology and Earth System Sciences, 23, 3175-3187, https://doi.org/10.5194/hess-23-3175-2019.
}
\examples{
data(runoff_multi_sites)
str(runoff_multi_sites) 
runoff_multi_sites[[1]]$timestamp <- paste(runoff_multi_sites[[1]]$YYYY, 
runoff_multi_sites[[1]]$MM, runoff_multi_sites[[1]]$DD, sep=" ")
runoff_multi_sites[[1]]$timestamp <- 
as.POSIXct(strptime(runoff_multi_sites[[1]]$timestamp,format="\%Y \%m \%d", tz="GMT"))
plot(runoff_multi_sites[[1]]$timestamp[1:1000], runoff_multi_sites[[1]]$Qobs[1:1000], type="l", 
  xlab="Time [d]", ylab=expression(paste("Discharge [m"^3,"/s]")))
}
\keyword{datasets}
