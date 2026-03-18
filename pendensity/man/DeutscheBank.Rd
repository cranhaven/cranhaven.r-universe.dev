\name{DeutscheBank}
\alias{DeutscheBank}
\docType{data}
\title{Daily final prices (DAX) of the German stock Deutsche Bank in the years 2006 and 2007}
\description{Containing the daily final prices of the German stock Deutsche Bank in the years 2006 and 2007.}

\usage{data(DeutscheBank)}

\format{
A data frame with 507 observations of the following 2 variables.
  \describe{
	\item{\code{Date}}{Date}
	\item{\code{ClosingPrice}}{ClosingPrice}
  }
}

\examples{
data(DeutscheBank)

form<-'\%d.\%m.\%y'

time.DeutscheBank <- strptime(DeutscheBank[,1],form)

#looking for all dates in 2006
data.DeutscheBank <- DeutscheBank[which(time.DeutscheBank$year==106),2]

#building differences of first order
d.DeutscheBank <- diff(data.DeutscheBank)

#estimating the density
density.DeutscheBank <- pendensity(d.DeutscheBank~1)
plot(density.DeutscheBank)
}
\keyword{datasets}