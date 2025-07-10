\name{SSvgm}
\alias{SSvgm}
\title{
A five-parameter van Genuchten water retention model
}
\description{
This is a five- parameter function to determine water retention hydraulic parameters contained in the van Genuchten water retention model
}
\usage{
SSvgm(input, thr, ths, alp, nscal, mscal)
}
\arguments{
  \item{input}{
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
  \item{mscal}{
This is a scaling parameter which is also related to index for pore-size distribution
}
}
\details{
Van Genuchten model is a type of self-starting nonlinear curve fitting model for fitting water retention characteristics using experimental data. In this model, the restriction of m=1-1/n has been relaxed so that m is estimated as an independent parameter of the water retention characteristics. If warnings are given during the estimation process, try to increase the iteration limit using control function. Sometimes the warnings can be suppressed using (warn=-1)/ignored since they do not terminate the process
}
\value{
A list of objects returned by standard R regression functions such as nls and lm
}
\references{
van Genuchten MT. 1980. A closed-form equation for predicting the hydraulic conductivity of unsaturated soils. Soil Science Society of America Journal 44: 892-898
}
\author{
Christian Thine Omuto
}

\seealso{
\code{\link{SSomuto}}, \code{\link{Brook}}, \code{\link{Campbel}}
}
\examples{
data(isric)
library(minpack.lm)
pf1=subset(isric,Sample=="Nicaragua21")
suppressWarnings(
vn.ns <- nlsLM(y ~ SSvgm(x,thr,ths,alp,nscal,mscal),
               data = pf1,
               control = nls.lm.control(maxiter=200)))
coef(vn.ns)
plot(fitted(vn.ns)~pf1$y)
abline(a=0,b=1,lty=20, col="blue")
}
\keyword{ models }
\keyword{ manip}
