\name{Brook}
\alias{Brook}
\title{
Brook-Corey water retention model
}
\description{
This is a four-parameter function to determine water retention hydraulic parameters contained in the Brooks-Corey water retention model
}
\usage{
Brook(x, thr, ths, alp, nscal)
}

\arguments{
  \item{x}{
Suction potential/head as contained in the x-column of the xy water retention table or data
}
  \item{thr}{
This is the residual moisture content. It's the moisture content when suction potential is very high (almost at the drying point)
}
  \item{ths}{
This is the saturated moisture content. It's the moisture content when suction potential is very low (almost at the saturation point)
}
  \item{alp}{
It's the inverse of air-entry potential or bubbling pressure
}
  \item{nscal}{
This is a parameter or index for the pore-size distribution
}
}
\details{
A Brooks-Corey model is a type of nonlinear curve fitting model for fitting water retention characteristics using experimental data. It requires initial parameter estimates for the model to work. This can be obtained using Dstart function (e.g. thr=Dstart(data)[1]). If warnings are given during the estimation process, try to increase the iteration limit using control function. Sometimes the warnings can be suppressed using (warn=-1)/ignored since they do not terminate the process
}
\value{
A list of objects returned by standard R regression functions such as nls and lm
}
\references{
Brooks RH and Corey AT.1964. Hydraulic properties of porous medium. Hydrology Paper
Number 3. Colorado State University, USA
}
\author{
Christian Thine Omuto
}

\seealso{
\code{\link{SSvgm}}, \code{\link{SSgard}}, \code{\link{Campbel}}
}
\examples{
## Attach sample data (isric data) from the package
data(isric)
pf <- subset(isric, Sample=="Benin2")
library(minpack.lm)
brook.ns <- nlsLM(y ~ Brook(x,thr,ths,alp,nscal), data=pf,
                  control = nls.lm.control(maxiter=200),
                  start = c(thr= Dstart(pf)[1], ths = Dstart(pf)[2],
                            alp= Dstart(pf)[3], nscal=Dstart(pf)[4]-1))
summary(brook.ns)## To produce a summary of modelling results
}
\keyword{ Models }
\keyword{ manip }
