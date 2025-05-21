\name{elec.temp}
\alias{elec.temp}
\non_function{}
\title{Electricity usage and temperature data}
\usage{data(elec.temp)}
\description{
The \code{elec.temp} data frame has 55 observations on
monthly electricity usage and average temperature for
a house in Westchester County, New York, USA.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{usage}{monthly electricity usage (kilowatt-hours) from a house
                 in Westchester County, New York, USA.}
    \item{temp}{average temperature (degrees Fahrenheit) for the
                    corresponding month.}
  }
}
\source{
Chatterjee, S., Handcock, M. and Simonoff, J.S. (1995).
\emph{A Casebook for a First Course in Statistics
and Data Analysis}, New York: John Wiley & Sons.
}
\references{
    Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}
}
\examples{
library(SemiPar)
data(elec.temp)
attach(elec.temp)
plot(usage,temp)
}
\keyword{datasets}
