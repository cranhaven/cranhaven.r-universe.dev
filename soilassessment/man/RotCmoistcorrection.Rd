\name{RotCmoistcorrection}
\alias{RotCmoistcorrection}
\title{
A function for estimating moisture effects in RothC carbon turnover modelling
}
\description{
This function estimates the scalar constant representing the moisture effects in RothC carbon turnover modelling in the soil
}
\usage{
RotCmoistcorrection(P, E, S.Thick, clay, pE, fk)
}
\arguments{
  \item{P}{
 the total rainfall amount in mm
}
  \item{E}{
 the total evapotranspiration amounts in mm. It can be pan evapotranspiration or potential evapotranspiration rate
}
  \item{S.Thick}{
 thickness of soil depth in cm (measured from the soil surface)
}
  \item{clay}{
 clay content in percent
}
  \item{pE}{
 proportion of pan evapotranspiration representing potential evapotranspiration rate.
}
  \item{fk}{
 A constant to correct for soil cover. For bare soil, fk=1.8 and for soil with cover, fk=1
}
}
\details{
E can be given as  pan evapotranspiration or potential evapotranspiration. If potential evapotranspiration is used for E, then pE = 1 and if pan evapotranspiration is used for E then pE=0.75.
}
\value{
A scalar constant for moisture effects on carbon decomposition rates
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
}
\seealso{
\code{\link{carbonTurnover}}, \code{\link{RotCtempcorrection}}, \code{\link{NPPmodel}}
}
\examples{
clay=34.5
depth=30
precip=c(73,59,63,51,52,57,34,55,58,56,76,71)
evapo=c(8,10,27,49,83,99,103,91,69,34,16,8)
inCl=data.frame(seq(1,12,1),precip,evapo)
colnames(inCl)=c("month","rain","ET")
inCl$mcor=RotCmoistcorrection(inCl$rain,inCl$ET,depth,clay,0.75,1)
inCl$mcor
}
