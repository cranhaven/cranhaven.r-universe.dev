\name{milan.mort}
\alias{milan.mort}
\non_function{}
\title{Milan mortality data}
\usage{data(milan.mort)}
\description{
The \code{milan.mort} data frame has data on 3652 consecutive
days (10 consecutive years: 1st January, 1980 to
30th December, 1989) for the city of Milan, Italy.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{day.num}{number of days since 31st December, 1979}
    \item{day.of.week}{1=Monday,2=Tuesday,3=Wednesday,4=Thursday,
                       5=Friday,6=Saturday,7=Sunday.}
    \item{holiday}{indicator of public holiday: 1=public holiday, 0=otherwise.}
    \item{mean.temp}{mean daily temperature in degrees Celcius.}
    \item{rel.humid}{relative humidity.}
    \item{tot.mort}{total number of deaths.}
    \item{resp.mort}{total number of respiratory deaths.}
    \item{SO2}{measure of sulphur dioxide level in ambient air.}
    \item{TSP}{total suspended particles in ambient air.}
  }
}
\source{Vigotti, M.A., Rossi, G., Bisanti, L., Zanobetti, A. and Schwartz, J.
(1996). Short term effect of urban air pollution on respiratory
health in Milan, Italy, 1980-1989. \emph{Journal of Epidemiology
and Community Health}, \bold{50}, S71-S75.
}
\references{
    Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}
}
\examples{
library(SemiPar)
data(milan.mort)
pairs(milan.mort,pch=".")
}
\keyword{datasets}
