\name{RotCtempcorrection}
\alias{RotCtempcorrection}
\title{
A function for estimating temperature effects in organic matter decomposition rates in the soil
}
\description{
This function estimates the scalar constant for temperature effects in RothC carbon turnover modelling in the soil
}
\usage{
RotCtempcorrection(temperature)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{temperature}{
mean air temperature in degrees Celsius
}
}
\details{
mean air temperature canbe monthly or annual mean temperature
}
\value{
A scalar constant for temperature effects on carbon decomposition rates
}
\references{
Burke, I., Kaye, J., Bird, S., Hall, S., McCulley, R., Sommerville, G. 2003. Evaluating and testing models of terrestrial biogeochemistry: the role of temperature in controlling decomposition, Models in ecosystem science, Princeton University Press, Princeton, New Jersey, USA, 225–253, 2003

Adair, E., Parton, W., Del Grosso, S., Silver, W., Harmon, M.,Hall, S., Burke, I., and Hart, S. 2008. Simple three-pool model accurately describes patterns of long-term litter decomposition in diverse climates, Global Change Biology, 14: 2636–2660

Coleman, K. and Jenkinson, D. 2014. ROTHC-26.3 A model for the turnover of carbon in soils: Model description and users guide (Windows version). Rothamsted Research Harpenden Herts AL5 2JQ

}
\author{
Christian Thine Omuto
}
\note{
This function can be used with monthly or annual input data to produce time-dependent scalars
The function works with temperatures greater than -18.2 degrees Celsius
}

\seealso{
\code{\link{carbonTurnover}}, \code{\link{RotCmoistcorrection}}, \code{\link{NPPmodel}}
}
\examples{
airTemp=22.1
RotCtempcorrection(airTemp)
}
