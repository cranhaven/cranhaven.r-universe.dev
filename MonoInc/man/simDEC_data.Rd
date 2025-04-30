\name{simDEC_data}
\alias{simDEC_data}
\docType{data}
\title{
Simulated Decreasing Data
}
\description{
This data was simulated to be monotonically decreasing. There are 500 individuals, with a random number of data points. Each individual has a two-level random effect (intercept and slope), a common intercept, and a random error term. The ages range from 0 to 10 years, which is given in months.
}
\usage{data("simDEC_data")}
\format{
  A data frame with 5505 observations on the following 3 variables.
  \describe{
    \item{\code{id}}{a numeric vector of the identification number of each individual}
    \item{\code{age}}{a numeric vector of the age in months}
    \item{\code{y}}{a numeric vector of measurements}
  }
}

\references{
http://blog.stata.com/2014/07/18/how-to-simulate-multilevellongitudinal-data/
}
\examples{
data(simDEC_data)
library(sitar)

mplot(x=age, y=y, id=id, data=simDEC_data, col=id, main="Individual Measurement Curves")	

}
\keyword{datasets}
