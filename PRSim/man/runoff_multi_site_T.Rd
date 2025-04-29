\name{runoff_multi_site_T}
\alias{runoff_multi_site_T}
\alias{runoff multi site T}
\docType{data}
\title{
Sample runoff and temperature data of two catchments with a similar discharge regime
}
\description{
Observed runoff data from two catchments in Switzerland.
}
\usage{data("runoff_multi_site_T")}
\format{
  A list of two data frames (one list per station) of the following 5 variables.
  \describe{
    \item{\code{YYYY}}{a numeric vector, year}
    \item{\code{MM}}{a numeric vector, month}
    \item{\code{DD}}{a numeric vector, day}
    \item{\code{Qobs}}{a numeric vector, observed runoff}
    \item{\code{T}}{a numeric vector, average temperature}
  }
}
\details{
The data contains runoff for two Swiss gages: (i) Thur Andelfingen (FOEN 2044) and (ii) Alpbach Erstfeld (FOEN 2299). 
}
\source{
The actual discharge data were ordered from
\url{https://www.bafu.admin.ch/bafu/en/home/topics/water/state/data/obtaining-monitoring-data-on-the-topic-of-water/hydrological-data-service-for-watercourses-and-lakes.html}. Temperature averages were computed from E-OBS (\url{https://cds.climate.copernicus.eu/cdsapp#!/dataset/insitu-gridded-observations-europe?tab=overview}).
}
\references{
Brunner, M. I. and Eric Gilleland in preparation.
}
\examples{
data(runoff_multi_site_T)
str(runoff_multi_site_T) 
runoff_multi_site_T[[1]]$timestamp <- paste(runoff_multi_site_T[[1]]$YYYY, 
runoff_multi_site_T[[1]]$MM, runoff_multi_site_T[[1]]$DD, sep=" ")
runoff_multi_site_T[[1]]$timestamp <- 
as.POSIXct(strptime(runoff_multi_site_T[[1]]$timestamp,format="\%Y \%m \%d", tz="GMT"))
plot(runoff_multi_site_T[[1]]$timestamp[1:1000], runoff_multi_site_T[[1]]$Qobs[1:1000], type="l", 
  xlab="Time [d]", ylab=expression(paste("Discharge [m"^3,"/s]")))
}
\keyword{datasets}
