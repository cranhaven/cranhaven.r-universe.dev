\name{simulated_data}
\alias{simulated_data}

\docType{data}
\title{
Simulated Data
}
\description{
This data was simulated to imitate height growth of female children in electronic medical records. There are 500 individuals, with a random number of data points. Based on the CDC growth curve, each individual has a two-level random effect (intercept and slope), a common intercept, and a random error term. The ages range from 0 to 10 years, which is given in months.}

\usage{data("simulated_data")}
\format{
  A data frame with 5673 observations on the following 3 variables.
  \describe{
    \item{\code{nestid}}{a numeric vector of the identification number of each individual}
    \item{\code{age}}{a numeric vector of the age in months}
    \item{\code{height}}{a numeric vector of the height in centimeters}
  }
}

\references{
http://blog.stata.com/2014/07/18/how-to-simulate-multilevellongitudinal-data/
}
\examples{
data(simulated_data)
library(sitar)

## plot each individual growth curve
mplot(x=age, y=height, id=nestid, data=simulated_data, col=nestid, main="Growth Curves")	

}
\keyword{datasets}
\keyword{simulated}
