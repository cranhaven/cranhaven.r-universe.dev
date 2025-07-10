\name{SSgardner}
\alias{SSgardner}
\title{
A Gardner  water retention model
}
\description{
This is a four- parameter function to determine water retention hydraulic parameters contained in the Gardner water retention model
}
\usage{
SSgardner(x, thr, ths, alp, nscal)
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
A Gardner model is a type of nonlinear curve fitting model for fitting water retention characteristics using experimental data. It requires initial parameter estimates for the model to work. This can be obtained using Dstart function (e.g. thr=Dstart(data)[1]). If warnings are given during the estimation process, try to increase the iteration limit using control function. Sometimes the warnings can be suppressed using (warn=-1)/ignored since they do not terminate the process
}
\value{
A list of objects returned by standard R regression functions such as nls and lm
}
\references{
Gardner WR. 1958. Some steady state solutions of the unsaturated moisture flow equation with application to evaporation from a water table. Soil Science 85: 228-232
}
\author{
Christian Thine Omuto
}

\seealso{
\code{\link{SSomuto}}, \code{\link{SSvgm}}, \code{\link{Campbel}}
}
\examples{
data(isric)
isric1 <- isric[1:32,]
library(minpack.lm)
gardner <- nlsLM(y ~ SSgardner(x,thr,ths,alp,nscal), isric1)
gardner
}
\keyword{ models }
\keyword{ manip }
