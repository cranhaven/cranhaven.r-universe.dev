\name{Allianz}
\alias{Allianz}
\docType{data}
\title{Daily final prices (DAX) of the German stock Allianz in the years 2006 and 2007}
\description{Containing the daily final prices of the German stock Allianz in the years 2006 and 2007.}

\usage{data(Allianz)}

\format{
A data frame with 507 observations of the following 2 variables.
  \describe{
	\item{\code{Date}}{Date}
	\item{\code{ClosingPrice}}{ClosingPrice}
  }
}

\examples{
data(Allianz)

form<-'\%d.\%m.\%y'

time.Allianz <- strptime(Allianz[,1],form)

#looking for all dates in 2006
data.Allianz <- Allianz[which(time.Allianz$year==106),2]

#building differences of first order
d.Allianz <- diff(data.Allianz)

#estimating the density
density.Allianz <- pendensity(d.Allianz~1,)
plot(density.Allianz)
}
\keyword{datasets}