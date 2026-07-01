\encoding{utf8}
\name{sandhoppers}
\alias{sandhoppers}
\docType{data}
\title{
Behavioral plasticity of Talitrus saltator and Talorchestia brito
}
\description{
Orientation measured under natural conditions and other variables of interest for analyzing the behavioral plasticity of two sympatric sandhoppers species, Talitrus saltator and Talorchestia brito. The experiment was carried out on the exposed nontidal sand of Zouara beach located in the Tunisian northwestern coast. More details can be found in Marchetti and Scapini (2003) or Scapini et al. (2002).

}

\usage{data("sandhoppers")}
\format{
  A data frame with 1828 observations on the following 12 variables.
  \describe{
\item{\code{angle}}{Numeric vector containing the orientation angles in radians between 0 and \eqn{2\pi}.}
\item{\code{date}}{A factor where each level indicates the date when angles were measured.}
\item{\code{month}}{A factor with two levels indicating the month when angles were measured. Experiments were performed in two different periods, \code{April} and \code{October}, which were chosen for the abundance of the populations, as well as for their non-extreme and changing climatic conditions.}
\item{\code{time}}{A factor with levels \code{afternoon}, \code{morning} and \code{noon}.}
\item{\code{azim}}{A numeric vector indicating the sun azimuth. The sun position was confounded with the time of the day (morning: 100-150, noon: az=151-210 and afternoon: az=211-260 experiments).}
\item{\code{hour}}{A factor with hours when angles were measured.}
\item{\code{species}}{A factor with three levels (\code{brito}, \code{salt}, \code{ND}) indicating the specie (brito, saltator, not determined).}
\item{\code{sex}}{A factor with three levels (\code{F}, \code{M}, \code{J}) indicating the sex (female, male, J).}
\item{\code{temp}}{A numeric vector indicating the temperature (degrees centigrade).}
\item{\code{humid}}{A numeric vector indicating the air relative humidity (\%).}
\item{\code{land}}{A factor with two levels (\code{no}, \code{yes}) indicating landscape view was either permitted or screened.}
\item{\code{trap}}{A numeric vector containing the traps identifier used for capturing the sandhoppers.}
}
}

\details{
Authors thank Prof. Felicita Scapini for providing the sandhoppers data (collected under the support of the European Project ERB ICI8-CT98-0270).}
\references{
Marchetti, G. M. and Scapini, F., Use of multiple regression models in the study of sandhopper orientation under natural conditions, Estuarine, Coastal and Shelf Science, 58, 207-215 (2003).\cr
Scapini, F., Aloia, A., Bouslama, M. F., Chelazzi, L., Colombini, I., ElGtari, M., Fallaci, M. and Marchetti, G. M. Multiple regression analysis of the sources of variation in orientation of two sympatric sandhoppers, Talitrus saltator and Talorchestia brito, from an exposed Mediterranean beach, Behavioral Ecology and Sociobiology, 51(5), 403-414 (2002).
}


\examples{
data(sandhoppers)
attach(sandhoppers)
library(NPCirc)
saltator=circular(angle[(species=="salt")],type="angles",units="radians")
brito=circular(angle[(species=="brito")],type="angles",units="radians")
library(NPCirc)
oldpar<-par(mfrow=c(1,2))
plot(saltator)
plot(brito)
par(oldpar)
}
\keyword{datasets}
