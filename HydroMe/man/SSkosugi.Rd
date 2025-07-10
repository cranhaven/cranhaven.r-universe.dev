\name{SSkosugi}
\alias{SSkosugi}
\title{
A Kosugi water retention model
}
\description{
This is a four- parameter function to determine water retention hydraulic parameters contained in the Kosugi water retention model
}
\usage{
SSkosugi(x, thr, ths, alp, nscal)
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
A Kosugi model is a type of self-starting nonlinear curve fitting model for fitting water retention characteristics using experimental data. If warnings are given during the estimation process, try to increase the iteration limit using control function. Sometimes the warnings can be suppressed using (warn=-1)/ignored since they do not terminate the process
}
\value{
A list of objects returned by standard R regression functions such as nls and lm
}
\references{
Kosugi, K. 1996. Lognormal distribution model for unsaturated soil hydraulic properties. Water Resources Research 32: 2697-2703
}
\author{
Christian Thine Omuto
}
\seealso{
\code{\link{SSomuto}}, \code{\link{SSvgm}}, \code{\link{SSgard}}
}
\examples{
data(isric)
library(minpack.lm)
suppressWarnings(
kosugi.ns <- nlsLM(y ~ SSkosugi(x,thr,ths,alp,nscal),
                   data = subset(isric, Sample=="Benin2"),
                   control = nls.lm.control(maxiter=200)))
summary(kosugi.ns)
}
\keyword{ models }
\keyword{ manip }
