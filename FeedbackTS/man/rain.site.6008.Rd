\name{rain.site.6008}
\alias{rain.site.6008}
\docType{data}
\title{
Rainfall data at Callagiddy station
}
\description{
Rainfall data at Callagiddy station in Western Australia
(meteorological station with identifier 6008).
}

\usage{data(rain.site.6008)}
\format{
  A data frame with 36615 observations on the following 5 variables.
  \describe{
\item{\code{Station.Number}}{a numeric vector providing the identifier of the meteorological station.}
    \item{\code{Year}}{a numeric vector specifying the year of each observation.}
    \item{\code{Month}}{a numeric vector specifying the month in the year of each observation.}
    \item{\code{Day}}{a numeric vector specifying the day in the month of each observation.}
    \item{\code{Precipitation.in.the.24.hours.before.9am..local.time..in.mm}}{a numeric vector providing the precipitation level in the 24 hours before 9am, local time, in mm.}
  }
}

\source{
Soubeyrand, S., Morris, C. E. and Bigg, E. K. (2014). Analysis of fragmented time directionality in time series to elucidate feedbacks in climate data. Environmental Modelling and Software 61: 78-86.
}
\references{
Soubeyrand, S., Morris, C. E. and Bigg, E. K. (2014). Analysis of fragmented time directionality in time series to elucidate feedbacks in climate data. Environmental Modelling and Software 61: 78-86.
}
\examples{
#### load data for site 6008 (Callagiddy station)
data(rain.site.6008)

#### build KDD object from raw data (site 6008: Callagiddy station)
## using a threshold value equal to 0 to keep all days
KDDno=kdd.from.raw.data(raw.data=rain.site.6008,keyday.threshold=0,nb.days=20,
   col.series=5,col.date=c(2,3,4),na.rm=TRUE,filter=NULL)

#### compute after-before differences and plot the whole data series
plot(KDDno["day"],KDDno["before.after"][21,],type="l",xlab="Day",ylab="Daily rainfall")
   axis(3,at=c(1,365*30,365*60,365*90),labels=KDDno["year"][1]+c(0,30,60,90))
}
\keyword{datasets}
